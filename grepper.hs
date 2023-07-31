
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
  , grepflags :: [String]
  , dryrun :: Bool
  } deriving (Show, Data, Typeable)

grep = Grep
  { root = "." &= typDir &= help "Starting direcory for search"
  , prune = [".stack*", ".hg", ".git", "dist*"] &= typDir &= help "Directory trees to prune"
  , search = "" &= help "pattern to grep for"
  , glob = "*.hs" &= help "file glob to search into"
  -- seb removed "-i" from here - prefer case sensitive grep 
  , grepflags = ["--color", "-H", "-b"] &= help "flags for grep"
  , dryrun = False &= help "print the command only"
  }
  

main :: IO ()
main = do
  args <- cmdArgs grep
  let findargs = grepper
        (T.pack $ root args)
        (T.pack $ glob args)
        (T.pack $ search args)
        [T.pack g | g <- grepflags args]
        [T.pack p | p <- prune args]

  if dryrun args
    then putStrLn ("find " ++ T.unpack (T.intercalate " " findargs))
    else shelly $ run_ "find" $ findargs

  
-- | excluded paths from starting dir
excluded :: [T.Text]
excluded = [".stack*", ".hg", ".git", "dist*"]

exclude :: [T.Text] -> [T.Text]
exclude [] = []
exclude (p:ps) = ["-o", "-name", p] ++ exclude ps

grepper :: T.Text -> T.Text -> T.Text -> [T.Text] -> [T.Text] -> [T.Text]
grepper dir pat term grepf (e:es) =
  concat [[dir, "-type", "d", "(", "-name", e],
           exclude es,
           [")", "-prune", "-o",
             "-type", "f", "-name", pat, "-exec", "grep"],
           grepf,
           ["-b", "-i", term, "{}", ";"]
         ]

{-
,"-exec", "echo", "-n", "\ESC[32m", ";",
"-print", "-exec", "echo", "\ESC[0m", ";"]]
-}
