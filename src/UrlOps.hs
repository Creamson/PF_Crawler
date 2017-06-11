module UrlOps
    ( getBaseUrl
    , makeFullLink
    , selectLinksToVisit
    ) where

import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List

getBaseUrl :: String -> String
getBaseUrl link | isPrefixOf "http" link = case splitted of (x:y:z:_) -> x ++ "//" ++ z
                | otherwise = head $ splitted
                where splitted = splitOn "/" link

makeFullLink :: String -> String -> String
makeFullLink baseUrl ('/':'/':xs) = "http://" ++ xs
makeFullLink baseUrl ('/':link) = baseUrl ++ ('/':link)
makeFullLink baseUrl _ = baseUrl

selectLinksToVisit :: [String] -> Set.Set String -> [String]
selectLinksToVisit links visited = filter (\a -> Set.notMember a visited) links
