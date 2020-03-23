{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where
import Prelude hiding (FilePath)
import System.Console.CmdArgs
import Shelly
import qualified Data.Text as T
default (T.Text)


data Grep = Grep
  { root :: FilePath
  , prune :: [FilePath]
  , glob :: String
  , search :: String
  } deriving (Show, Data, Typeable)

grep = Grep
  { root = "." &= typDir &= help "Starting direcory for search"
  , prune = [".stack*", ".hg", ".git", "dist*"] &= typDir &= help "Directory trees to prune"
  , search = "" &= help "pattern to grep -i for"
  , glob = "*.hs" &= help "file glob to search into"
  }
  

main :: IO ()
main = do
  args <- cmdArgs grep
  shelly $ grepper_ (T.pack $ root args) (T.pack $ glob args) (T.pack $ search args) [T.pack p | p <- prune args]

-- | excluded paths from starting dir
excluded :: [T.Text]
excluded = [".stack*", ".hg", ".git", "dist*"]

exclude :: [T.Text] -> [T.Text]
exclude [] = []
exclude (p:ps) = ["-o", "-name", p] ++ exclude ps

grepper_ :: T.Text -> T.Text -> T.Text -> [T.Text] -> Sh ()
grepper_ dir pat term (e:es) =
  run_ "find" $ concat [[dir, "-type", "d", "(", "-name", e],
                        exclude es,
                        [")", "-o", "-type", "f", "-name", pat, "-exec", "grep", "-i", term, "{}", ";", "-print"]]
  
