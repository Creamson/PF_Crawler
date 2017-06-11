module Crawler
    ( walkPage
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import qualified Data.Set as Set
import Data.List
import UrlOps

walkPage :: String -> Int -> Set.Set String -> [(String, Int)] -> IO ()
walkPage src depth visited todo = do
    let doc = readDocument [withParseHTML yes, withWarnings no, withCurl []] src
    links <- runX $ doc >>> multi (hasName "a") >>> getAttrValue "href"
    let newVisited = Set.insert src visited
    putStrLn src
    let baseUrl = getBaseUrl src
    let fullLinks = map (makeFullLink baseUrl) links
    let updatedTodo = (++) todo $ map (\x -> (x, depth-1)) (selectLinksToVisit fullLinks newVisited)
    nextPage newVisited updatedTodo

nextPage :: Set.Set String -> [(String, Int)] -> IO ()
nextPage _ [] = return ()
nextPage visited ((src, depth):todo) =
    if Set.notMember src visited && depth > 0
    then walkPage src depth visited todo
    else nextPage visited todo

selectLinksToVisit :: [String] -> Set.Set String -> [String]
selectLinksToVisit links visited = filter (\a -> Set.notMember a visited) links