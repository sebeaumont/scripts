{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Prelude hiding (FilePath)
import System.Environment
import Shelly
import Text.Printf
import Network.URI.Encode (encodeText)
import qualified Data.Text as T
default (T.Text)


main :: IO ()
main = do
  args <- getArgs
  let terms = T.intercalate " " [T.pack a | a <- args] 
  shelly $ run_  "w3m" [search terms]

search :: T.Text -> T.Text
search s = "http://hoogle.haskell.org?q=" <> encodeText s
