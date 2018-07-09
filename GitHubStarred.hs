{-# LANGUAGE OverloadedStrings, DeriveGeneric, ScopedTypeVariables #-}

import Data.Aeson
import Data.Text hiding (take)
import Control.Applicative
import Control.Monad
import GHC.Generics
import Network.HTTP.Simple

data Repo =
    Repo { name     :: String
         , html_url :: String
         } deriving (Show,Generic)

instance FromJSON Repo

printRepoData :: [Repo] -> IO()
printRepoData [] = putStrLn "End of Repos."
printRepoData (r:rs) = do putStr "Repo Name: "
                          print (name r)
                          putStr "Repo URL: "
                          print (html_url r)
                          putStrLn "=========="
                          printRepoData rs

trimRepoList ::[Repo] -> Int -> [Repo]
trimRepoList [] c = []
trimRepoList rs 0 = []
trimRepoList rs c = take c rs

buildUrl :: String -> String -> IO Request
buildUrl n c = parseRequest $ "https://api.github.com/users/"
                              <> n
                              <> "/starred?per_page="
                              <> c

pullFromHttp :: Request -> IO [Repo]
pullFromHttp u = do
  let request = setRequestHeader "User-Agent" ["test-app"] $ u
  response <- httpJSON request
  return $ getResponseBody response
 
main :: IO ()
main = do
  putStrLn "Enter username:"
  name <- getLine
  putStrLn "Enter number of repos:"
  rCount <- getLine
  url <- buildUrl name rCount
  rs <- pullFromHttp url
  printRepoData rs