-- | Reads a Mozilla SpiderMonkey Parser API AST from the stdin and
-- produces a source representation of language-ecmascript parser AST.
module Main where

import qualified Language.ECMAScript5.Syntax as ES
import Language.ECMAScript5.ParserState (SourceSpan(..))
import Text.Parsec.Pos (SourcePos, newPos)
import qualified Language.JavaScript.SpiderMonkey.Parser as JS
import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Exit
import Data.Maybe (fromMaybe, isJust, isNothing, fromJust)
import Control.Monad
import Data.Default.Class
import Control.Applicative ((<$>))
import Data.Text (unpack)
import Data.Traversable (traverse)

main :: IO ()
main = do json <- BS.getContents
          case eitherDecode json of
           Left msg -> hPutStr stderr ("AST parse failure: " ++ msg)
                       >> exitFailure
           Right p  -> case convert p of
                        Right es -> putStr (show es) >> exitSuccess
                        Left err -> hPutStr stderr err >> exitFailure

loc2span :: JS.SourceLocation -> SourceSpan
loc2span loc =
  let src = fromMaybe "" (JS.source loc) in
  SourceSpan (position2sourcePos src (JS.start loc), position2sourcePos src (JS.end loc))

position2sourcePos :: String -> JS.Position -> SourcePos
position2sourcePos source pos =
  newPos source (int32 $ JS.line pos) (int32 $ JS.column pos)
  where int32 = fromInteger . toInteger

type Reason = String

type CM x = Either Reason (x SourceSpan)

convert :: JS.Program -> CM ES.Program
convert prg = liftM (ES.Program $ loc2span $ JS.loc prg) $ mapM convertS (JS.body prg)

convertS :: JS.Statement -> CM ES.Statement
convertS s = case s of
  JS.EmptyStatement loc    -> return $ ES.EmptyStmt $ loc2span loc
  JS.BlockStatement loc ss -> liftM (ES.BlockStmt $ loc2span loc) $ mapM convertS ss
  JS.ExpressionStatement loc e -> liftM (ES.ExprStmt $ loc2span loc) $ convertE e
  JS.IfStatement loc g t me -> liftM3 (ES.IfStmt $ loc2span loc) (convertE g) (convertS t) (convertS $ fromMaybe (JS.EmptyStatement loc) me)
  JS.LabeledStatement loc lab s -> liftM (ES.LabelledStmt (loc2span loc) (convertI lab)) $ convertS s
  JS.BreakStatement loc mlab -> return $ ES.BreakStmt (loc2span loc) (convertI <$> mlab)
  JS.ContinueStatement loc mlab -> return $ ES.ContinueStmt (loc2span loc) (convertI <$> mlab)
  JS.WithStatement loc g s -> liftM2 (ES.WithStmt (loc2span loc)) (convertE g) (convertS s)
  JS.SwitchStatement loc g cases False -> liftM2 (ES.SwitchStmt (loc2span loc)) (convertE g) (mapM convertC cases)
  JS.ReturnStatement loc me -> liftM (ES.ReturnStmt $ loc2span loc) $ convertE `traverse` me
  JS.ThrowStatement loc e -> liftM (ES.ThrowStmt $ loc2span loc) $ convertE e
  JS.TryStatement loc try mcatch [] mfinally -> liftM3 (ES.TryStmt $ loc2span loc) (mapM convertS $ fromBlock try) (convertH `traverse` mcatch) (mapM convertS `traverse` (fromBlock <$> mfinally))
  JS.WhileStatement loc g s -> liftM2 (ES.WhileStmt $ loc2span loc) (convertE g) (convertS s)
  JS.DoWhileStatement loc s g -> liftM2 (ES.DoWhileStmt $ loc2span loc) (convertS s) (convertE g)
  JS.ForStatement loc init mtest minc b -> liftM4 (ES.ForStmt $ loc2span loc) (convertFI init) (convertE `traverse` mtest) (convertE `traverse` minc) (convertS b)
  JS.ForInStatement loc init obj b False ->
    let init' = case init of
          Left (JS.VariableDeclaration _ [decl] JS.DVar) -> liftM ES.ForInVar $ convertV decl
          Right e -> liftM ES.ForInExpr $ convertE e
          _ -> Left "For in statement has multiple or non-variable declaration clauses"
    in liftM3 (ES.ForInStmt $ loc2span loc) init' (convertE obj) (convertS b)
  JS.DebuggerStatement loc -> return $ ES.DebuggerStmt $ loc2span loc
  JS.FunctionDeclarationStatement loc func ->
    do params <- mapM convertP $ JS.funcParams func
       if ((isNothing $ JS.funcId func) && (not $ null (JS.funcDefaults func)) && (isJust $ JS.funcRest func) && (not $ JS.funcGenerator func)) then Left "Unsupported function statement syntax"
            else liftM2 (ES.FunctionStmt (loc2span loc) (convertI $ fromJust $ JS.funcId func)) (mapM convertP $ JS.funcParams func) (mapM convertS $ fromBlock $ JS.funcBody func)
  JS.VariableDeclarationStatement loc (JS.VariableDeclaration _ decls JS.DVar) -> liftM (ES.VarDeclStmt $ loc2span loc) $ mapM convertV decls
  _ -> Left $ "Statement not in ES5: " ++ (show s)

fromBlock :: JS.Statement -> [JS.Statement]
fromBlock s = case s of
  JS.BlockStatement _ ss -> ss
  _                 -> [s]

convertE :: JS.Expression -> CM ES.Expression
convertE e = undefined

convertI :: JS.Identifier -> ES.Id SourceSpan
convertI (JS.Identifier loc name) = ES.Id (loc2span loc) (unpack name)

convertP :: JS.Pattern -> CM ES.Id
convertP (JS.IdentifierPattern _ id) = Right $ convertI id
convertP _ = Left "Non-identifier pattern"

convertC :: JS.SwitchCase -> CM ES.CaseClause
convertC (JS.SwitchCase loc me s) =
  let span = (loc2span loc)
      ss = mapM convertS s
  in case me of
      Just e  -> liftM2 (ES.CaseClause span) (convertE e) ss
      Nothing -> liftM  (ES.CaseDefault span) ss

convertH :: JS.CatchClause -> CM ES.CatchClause
convertH (JS.CatchClause loc pat Nothing body) =
  liftM2 (ES.CatchClause $ loc2span loc) (convertP pat) (convertS `traverse` fromBlock body)
convertH _ = Left "Guarded catch clauses are not in ES5"

convertFI :: JS.ForInit -> CM ES.ForInit
convertFI fi = case fi of
  JS.VarInit (JS.VariableDeclaration _ decls JS.DVar) -> liftM ES.VarInit $ mapM convertV decls
  JS.ExprInit e -> liftM ES.ExprInit $ convertE e
  JS.NoInit -> return ES.NoInit
  _ -> Left "Non-variable declaration in a for init"

convertV :: JS.VariableDeclarator -> CM ES.VarDecl
convertV = undefined
