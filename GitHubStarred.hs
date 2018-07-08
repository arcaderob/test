{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

import Data.Aeson
import Data.Text hiding (take)
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics

data Repo =
    Repo { name     :: String
         , html_url :: String
         } deriving (Show,Generic)

instance FromJSON Repo

jsonFile :: FilePath
jsonFile = "gh.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

printRepoData :: [Repo] -> IO()
printRepoData [] = putStrLn "End of Repos."
printRepoData (r:rs) = do putStr "Repo Name: "
                          print (name r)
                          putStr "Repo URL: "
                          print (html_url r)
                          putStrLn "=========="
                          printRepoData rs

trimRepoList ::[Repo] -> Int -> IO ()
trimRepoList [] c = putStrLn "No Repos to Display."
trimRepoList rs 0 = putStrLn "No Repos to Display."
trimRepoList rs c = printRepoData (take c rs)
 
main :: IO ()
main = do
  putStrLn "Enter username:"
  name <- getLine
  putStrLn "Enter number of repos:"
  rCount :: Int <- readLn
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Repo])
  case d of
    Left err -> putStrLn err
    Right rs -> trimRepoList rs rCount