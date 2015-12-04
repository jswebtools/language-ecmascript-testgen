{-# LANGUAGE OverloadedStrings #-}
-- | Reads a Mozilla SpiderMonkey Parser API AST from the stdin and
-- produces a source representation of language-ecmascript parser AST.
module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy as BS
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Converter

main :: IO ()
main = do [fileName] <- getArgs
          json <- BS.readFile fileName
          when ("FAIL" `BS.isPrefixOf` json) $ putStrLn "Nothing" >> exitSuccess
          case eitherDecode json of
           Left msg -> hPutStrLn stderr ("AST parse failure: " ++ msg)
                       >> exitFailure
           Right p  -> case convert p of
                        Right es -> putStrLn (show (Just es)) >> exitSuccess
                        Left err -> hPutStrLn stderr err >> exitFailure
