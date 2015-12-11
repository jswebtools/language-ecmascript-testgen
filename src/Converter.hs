{-# LANGUAGE TupleSections #-}
module Converter where

import qualified Language.ECMAScript5.Syntax as ES
import Language.ECMAScript5.Syntax.Annotations (getAnnotation)
import Language.ECMAScript5.ParserState (SrcLoc (..))
import Text.Parsec.Pos (SourcePos, newPos)
import qualified Language.JavaScript.SpiderMonkey.Parser as JS
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Exit
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)
import Data.Default.Class
import Control.Applicative
import Data.Text (unpack)
import Data.Traversable (traverse)
import Data.Scientific (floatingOrInteger)

-- for testing only
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import Data.String

convertFromString :: String -> CM ES.Program
convertFromString s = eitherDecode (fromString s) >>= convert
---------------------------------

loc2span :: JS.SourceLocation -> SrcLoc
loc2span loc =
  let int32 = fromInteger . toInteger in
  case loc of
   JS.SourceLocation {} ->
     SrcLoc (int32 $ JS.line $ JS.start loc
            ,int32 $ JS.column $ JS.start loc
            ,int32 $ JS.line $ JS.end loc
            ,int32 $ JS.column $ JS.end loc
            ,JS.source loc)
   JS.NoLocation -> NoLoc

type Reason = String

type CM x = Either Reason (x SrcLoc)

convert :: JS.Program -> CM ES.Program
convert prg = (ES.Program $ loc2span $ JS.loc prg) <$> traverse statement (JS.body prg)

statement :: JS.Statement -> CM ES.Statement
statement s = case s of
  JS.EmptyStatement loc    -> pure $ ES.EmptyStmt $ loc2span loc
  JS.BlockStatement loc ss -> ES.BlockStmt (loc2span loc) <$> traverse statement ss
  JS.ExpressionStatement loc e -> ES.ExprStmt (loc2span loc) <$> expression e
  JS.IfStatement loc g t me ->
    ES.IfStmt (loc2span loc) <$> expression g <*> statement t <*>
    statement (fromMaybe (JS.EmptyStatement loc) me)
  JS.LabeledStatement loc lab s -> ES.LabelledStmt (loc2span loc) (identifier lab) <$> statement s
  JS.BreakStatement loc mlab -> pure $ ES.BreakStmt (loc2span loc) (identifier <$> mlab)
  JS.ContinueStatement loc mlab -> pure $ ES.ContinueStmt (loc2span loc) (identifier <$> mlab)
  JS.WithStatement loc g s -> ES.WithStmt (loc2span loc) <$> expression g <*> statement s
  JS.SwitchStatement loc g cases False -> ES.SwitchStmt (loc2span loc) <$> expression g <*> traverse switchCase cases
  JS.ReturnStatement loc me -> ES.ReturnStmt (loc2span loc) <$> traverse expression me
  JS.ThrowStatement loc e -> ES.ThrowStmt (loc2span loc) <$> expression e
  JS.TryStatement loc try mcatch [] mfinally ->
    ES.TryStmt (loc2span loc) <$> traverse statement (fromBlock try) <*>
    traverse catchClause mcatch <*> traverse (traverse statement) (fromBlock <$> mfinally)
  JS.WhileStatement loc g s -> ES.WhileStmt (loc2span loc) <$> expression g <*> statement s
  JS.DoWhileStatement loc s g -> ES.DoWhileStmt (loc2span loc) <$> statement s <*> expression g
  JS.ForStatement loc init mtest minc b -> ES.ForStmt (loc2span loc) <$> forInit init <*> traverse expression mtest <*> traverse expression minc <*> statement b
  JS.ForInStatement loc init obj b False ->
    let init' = case init of
          Left (JS.VariableDeclaration _ [decl] JS.DVar) ->
            ES.ForInVar <$> variableDeclarator decl
          Right e -> ES.ForInExpr <$> expression e
          _ -> Left "For in statement has multiple or non-variable declaration clauses"
    in ES.ForInStmt (loc2span loc) <$> init' <*> expression obj <*> statement b
  JS.DebuggerStatement loc -> pure $ ES.DebuggerStmt $ loc2span loc
  JS.FunctionDeclarationStatement loc func ->
    function func >>=
    \(mName, params, body) ->
     if (isNothing mName) then Left "Unsupported anonymous function statement"
     else return $ ES.FunctionStmt (loc2span loc) (fromJust mName) params body
  JS.VariableDeclarationStatement loc (JS.VariableDeclaration _ decls JS.DVar) ->
    ES.VarDeclStmt (loc2span loc) <$> traverse variableDeclarator decls
  _ -> Left $ "Statement not in ES5: " ++ (show s)

fromBlock :: JS.Statement -> [JS.Statement]
fromBlock s = case s of
  JS.BlockStatement _ ss -> ss
  _                 -> [s]

function :: JS.Function
           -> Either Reason (Maybe (ES.Id SrcLoc)
                            ,[ES.Id SrcLoc]
                            ,[ES.Statement SrcLoc])
function func =
  if ((not $ null (JS.funcDefaults func)) &&
      (isJust $ JS.funcRest func) &&
      (not $ JS.funcGenerator func))
  then Left "Non-ES5 syntax in a function declaration"
  else (identifier <$> JS.funcId func,,) <$> mapM pattern (JS.funcParams func)
       <*> (mapM statement $ fromBlock $ JS.funcBody func)

uncurry3 f (a,b,c) = f a b c

expression :: JS.Expression -> CM ES.Expression
expression e = case e of
  JS.ThisExpression loc -> pure $ ES.ThisRef $ loc2span loc
  JS.ArrayExpression loc els ->
    ES.ArrayLit (loc2span loc) <$> traverse (traverse expression) els
  JS.ObjectExpression loc props -> ES.ObjectLit (loc2span loc) <$> traverse property props
  JS.FunctionExpression loc func ->
    (uncurry3 (ES.FuncExpr (loc2span loc))) <$> function func
  JS.SequenceExpression loc es -> ES.CommaExpr (loc2span loc) <$>
                                  traverse expression es
  JS.UnaryExpression loc op True e ->
    ES.PrefixExpr (loc2span loc) (unaryOperator op) <$> expression e
  JS.BinaryExpression loc op lhs rhs ->
    ES.InfixExpr (loc2span loc) <$> binaryOperator op <*> expression lhs <*>
    expression rhs
  JS.AssignmentExpression loc op lhs rhs ->
    ES.AssignExpr (loc2span loc) (assignmentOperator op) <$> expression lhs <*> expression rhs
  JS.UpdateExpression loc op e prefix ->
    ES.UnaryAssignExpr (loc2span loc) (updateOperator op prefix) <$> expression e
  JS.LogicalExpression loc op lhs rhs ->
    ES.InfixExpr (loc2span loc) (logicalOperator op) <$> expression lhs <*>
    expression rhs
  JS.ConditionalExpression loc g te ee ->
    ES.CondExpr (loc2span loc) <$> expression g <*> expression te <*> expression ee
  JS.NewExpression loc ctor args ->
    ES.NewExpr (loc2span loc) <$> expression ctor <*> traverse expression args
  JS.CallExpression loc fun args ->
    ES.CallExpr (loc2span loc) <$> expression fun <*> traverse expression args
  JS.MemberExpression loc obj idOrExpr computed ->
    let span = loc2span loc
        obj' = expression obj
    in  case (computed, idOrExpr) of
         (False, Left id) -> (ES.DotRef span) <$> obj' <*> pure (identifier id)
         (True, Right e) -> (ES.BracketRef span) <$> obj' <*> expression e
         (True, Left id) -> (ES.BracketRef span) <$> obj' <*>
                            pure (ES.VarRef (getAnnotation $ identifier id) $ identifier id)

  JS.LiteralExpression _ lit -> pure (literal lit)
  JS.IdentifierExpression loc id -> pure $ ES.VarRef (loc2span loc) (identifier id)
  _ -> Left $ "Expression is not in ES5: " ++ (show e)

identifier :: JS.Identifier -> ES.Id SrcLoc
identifier (JS.Identifier loc name) = ES.Id (loc2span loc) (unpack name)

pattern :: JS.Pattern -> CM ES.Id
pattern (JS.IdentifierPattern _ id) = Right $ identifier id
pattern _ = Left "Non-identifier pattern"

switchCase :: JS.SwitchCase -> CM ES.CaseClause
switchCase (JS.SwitchCase loc me s) =
  let span = loc2span loc
      ss = traverse statement s
  in case me of
      Just e  -> ES.CaseClause span <$> expression e <*> ss
      Nothing -> ES.CaseDefault span <$> ss

catchClause :: JS.CatchClause -> CM ES.CatchClause
catchClause (JS.CatchClause loc pat Nothing body) =
  ES.CatchClause (loc2span loc) <$> pattern pat <*> traverse statement (fromBlock body)
catchClause _ = Left "Guarded catch clauses are not in ES5"

forInit :: JS.ForInit -> CM ES.ForInit
forInit fi = case fi of
  JS.VarInit (JS.VariableDeclaration _ decls JS.DVar) -> ES.VarInit <$> traverse variableDeclarator decls
  JS.ExprInit e -> ES.ExprInit <$> expression e
  JS.NoInit -> pure ES.NoInit
  _ -> Left "Non-variable declaration in a for init"

variableDeclarator :: JS.VariableDeclarator -> CM ES.VarDecl
variableDeclarator (JS.VariableDeclarator loc pat me) =
  ES.VarDecl (loc2span loc) <$> pattern pat <*> traverse expression me

property :: JS.Property -> CM ES.PropAssign
property (JS.Property loc name value kind) =
  let span = loc2span loc
      val  = expression value
      prop = case name of
              Left lit -> case literal lit of
                           ES.StringLit a s -> Right $ ES.PropString a s
                           ES.NumLit a n    -> Right $ ES.PropNum a n
                           _             -> Left  $ "Unsupported literal type in property name"
                             
              Right id -> let ES.Id a s = identifier id in Right $ ES.PropId a s
      e2Getter e = case e of
                    ES.FuncExpr _ Nothing [] body -> Right $ body
                    _ -> Left "Invalid setter"
      e2Setter e = case e of
                    ES.FuncExpr _ Nothing [id] body -> Right $ (id, body)
                    _ -> Left "Invalid setter"
  in case kind of
      JS.PInit -> ES.PValue span <$> prop <*> val
      JS.PGet  -> ES.PGet span <$> prop <*> (val >>= e2Getter)
      JS.PSet  -> uncurry <$> ((ES.PSet span) <$> prop) <*> (val >>= e2Setter)

literal :: JS.Literal -> ES.Expression SrcLoc
literal l = case l of
  JS.LString loc text -> ES.StringLit (loc2span loc) (unpack text)
  JS.LBool loc b -> ES.BoolLit (loc2span loc) b
  JS.LNull loc   -> ES.NullLit (loc2span loc)
  JS.LNumber loc sci -> ES.NumLit (loc2span loc) $
                        case floatingOrInteger sci of
                         Left f  -> Right f
                         Right i -> Left  i

unaryOperator :: JS.UnaryOperator -> ES.PrefixOp
unaryOperator op = case op of
  (JS.:.-:)   -> ES.PrefixMinus
  (JS.:.+:)   -> ES.PrefixPlus
  (JS.:!:)    -> ES.PrefixLNot
  (JS.:~:)    -> ES.PrefixBNot
  (JS.Typeof) -> ES.PrefixTypeof
  (JS.Void)   -> ES.PrefixVoid
  (JS.Delete) -> ES.PrefixDelete

binaryOperator :: JS.BinaryOperator -> Either Reason ES.InfixOp
binaryOperator op = case op of
  (JS.:==:) -> Right ES.OpEq
  (JS.:!=:) -> Right ES.OpNEq
  (JS.:===:)-> Right ES.OpStrictEq
  (JS.:!==:)-> Right ES.OpStrictNEq
  (JS.:<:)  -> Right ES.OpLT
  (JS.:<=:) -> Right ES.OpLEq
  (JS.:>:)  -> Right ES.OpGT
  (JS.:>=:) -> Right ES.OpGEq
  (JS.:<<:) -> Right ES.OpLShift
  (JS.:>>:) -> Right ES.OpSpRShift
  (JS.:>>>:)-> Right ES.OpZfRShift
  (JS.:+:)  -> Right ES.OpAdd
  (JS.:-:)  -> Right ES.OpSub
  (JS.:*:)  -> Right ES.OpMul
  (JS.:/:)  -> Right ES.OpDiv
  (JS.:%:)  -> Right ES.OpMod
  (JS.:|:)  -> Right ES.OpBOr
  (JS.:^:)  -> Right ES.OpBXor
  (JS.:&:)  -> Right ES.OpBAnd
  JS.In     -> Right ES.OpIn
  JS.Instanceof -> Right ES.OpInstanceof
  (JS.:..:)     -> Left "The .. operator is not in ES5"

logicalOperator :: JS.LogicalOperator -> ES.InfixOp
logicalOperator op = case op of
  (JS.:||:) -> ES.OpLOr
  (JS.:&&:) -> ES.OpLAnd

assignmentOperator :: JS.AssignmentOperator -> ES.AssignOp
assignmentOperator op = case op of
  (JS.:=:) -> ES.OpAssign
  (JS.:+=:) -> ES.OpAssignAdd
  (JS.:-=:) -> ES.OpAssignSub
  (JS.:*=:) -> ES.OpAssignMul
  (JS.:/=:) -> ES.OpAssignDiv
  (JS.:%=:) -> ES.OpAssignMod
  (JS.:<<=:) -> ES.OpAssignLShift
  (JS.:>>=:) -> ES.OpAssignSpRShift
  (JS.:>>>=:) -> ES.OpAssignZfRShift
  (JS.:|=:)   -> ES.OpAssignBOr
  (JS.:^=:)   -> ES.OpAssignBXor
  (JS.:&=:)   -> ES.OpAssignBAnd

updateOperator :: JS.UpdateOperator -> Bool -> ES.UnaryAssignOp
updateOperator op prefix = case (op, prefix) of
  ((JS.:++:), True)  -> ES.PrefixInc
  ((JS.:++:), False) -> ES.PostfixInc
  ((JS.:--:), True)  -> ES.PrefixDec
  ((JS.:--:), False) -> ES.PostfixDec
