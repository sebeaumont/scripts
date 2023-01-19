{-# LANGUAGE LambdaCase #-}
module Main where

import Data.List
import System.Environment
import Control.Exception (try)

-- | Read the lines if any error we give up
readlines :: FilePath -> IO (Either IOError [String])
readlines p = try $ (return . lines =<< readFile p)

-- | Cant believe this is not in the prelude
splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen p s =
  case dropWhile p s of
    "" -> []
    s' -> w : splitWhen p s''
      where (w, s'') = break p s'
  
-- | Split up the path variable
splitPath :: String -> [String]
splitPath = splitWhen (==':')

-- | Make a delimited path 
makePath :: [String] -> String
makePath = intercalate ":" . nub 

-- | Jolly wee strings
home, path :: String
home = "HOME" 
path = "PATH"

-- | Let's go
main :: IO ()
main = do
  h <- getEnv home
  p <- getEnv path
  -- print out new path or just the exisiting environment
  -- if things are sinister...
  readlines (h ++ "/.paths") >>= \case
    Left _ -> putStr $ p 
    Right l -> putStr $ makePath l  
    

