-- | Reads a Mozilla SpiderMonkey Parser API AST from the stdin and
-- produces a source representation of language-ecmascript parser AST.
module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Exit
import Converter

main :: IO ()
main = do json <- BS.getContents
          case eitherDecode json of
           Left msg -> hPutStrLn stderr ("AST parse failure: " ++ msg)
                       >> exitFailure
           Right p  -> case convert p of
                        Right es -> putStrLn (show es) >> exitSuccess
                        Left err -> hPutStrLn stderr err >> exitFailure
