module Main where

import System.Environment
import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import Data.Tree.NTree.TypeDefs
import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List

main = do
  [src, depth] <- getArgs
  walkPage src (read depth :: Int) Set.empty []

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

selectLinksToVisit :: [String] -> Set.Set String -> [String]
selectLinksToVisit links visited = filter (\a -> Set.notMember a visited) links

nextPage :: Set.Set String -> [(String, Int)] -> IO ()
nextPage _ [] = return ()
nextPage visited ((src, depth):todo) =
    if Set.notMember src visited && depth > 0
    then walkPage src depth visited todo
    else nextPage visited todo

getBaseUrl :: String -> String
getBaseUrl link = if isPrefixOf "http" link
    then (\(x:y:z:xs) -> x ++ "//" ++ z) $ splitOn "/" link
    else head $ splitOn "/" link


makeFullLink :: String -> String -> String
makeFullLink baseUrl ('/':'/':xs) = "http://" ++ xs
makeFullLink baseUrl [] = baseUrl
makeFullLink baseUrl ('/':link) = baseUrl ++ ('/':link)
makeFullLink baseUrl link = baseUrl

