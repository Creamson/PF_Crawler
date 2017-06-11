module UrlOps
    ( getBaseUrl
    , makeFullLink
    ) where

import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List

getBaseUrl :: String -> String
getBaseUrl link | isPrefixOf "http" link = case split of (x:y:z:_) -> x ++ "//" ++ z
                | otherwise = head $ split
                where split = splitOn "/" link

makeFullLink :: String -> String -> String
makeFullLink baseUrl ('/':'/':xs) = "http://" ++ xs
makeFullLink baseUrl ('/':link) = baseUrl ++ ('/':link)
makeFullLink baseUrl _ = baseUrl
