module Crawler
    ( walkPage
    ) where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl
import qualified Data.Set as Set
import Data.List
import UrlOps
import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.Arrow.XmlState.TypeDefs
import Control.Arrow.IOStateListArrow

-- | Parses the page given by src and adds found links to todos with appropriate remaining depth.
-- Then calls for the next page from todos to be visited.
walkPage :: String           -- ^ Link to the page to be parsed.
         -> Int              -- ^ The remaining depth of the search, for the currently parsed page.
         -> Set.Set String   -- ^ A set of already visited links.
         -> [(String, Int)]  -- ^ So far accumulated list of links to visit, with respective remaining depths.
         -> IO ()            -- ^ No return value.
walkPage src depth visited todo = do
    let doc = parsePage src
    links <- extractLinks doc
    let newVisited = Set.insert src visited
    let baseUrl = getBaseUrl src
    let fullLinks = map (makeFullLink baseUrl) links
    let updatedTodo = mergeTodos todo fullLinks depth newVisited
    putStrLn src
    nextPage newVisited updatedTodo

-- |Takes the next link from todos and if it wasn't yet visited, parses it.
-- If visited it was, then the next link from todos is taken.
-- Should the todos list be empty, the function returns.
nextPage :: Set.Set String -> [(String, Int)] -> IO ()
nextPage _ [] = return ()
nextPage visited ((src, depth):todo) =
    if Set.notMember src visited && depth > 0
    then walkPage src depth visited todo
    else nextPage visited todo

-- |Parses the given source page to XML form using HXT.
parsePage :: String -> IOStateArrow s b XmlTree
parsePage src = readDocument [withParseHTML yes, withWarnings no, withCurl []] src

-- |Parses an XML form of a webpage's DOM structure.
-- Returns a list of href elements of \<a\> tags.
extractLinks :: IOSLA (XIOState ()) XmlTree (NTree XNode) -> IO [String]
extractLinks doc = runX $ doc >>> multi (hasName "a") >>> getAttrValue "href"

-- |'mergeTodos' adds newly found links as todos, with approprite remaining depth parameters.
mergeTodos :: [(String, Int)] -- ^ Previously gathered links to skan, with remaining depth.
           -> [String]        -- ^ Newly found links.
           -> Int             -- ^ Remaining depth on the current level of scanning.
           -> Set.Set String  -- ^ Set of already visited links.
           -> [(String, Int)] -- ^ Old and new links to skan together, with respective remaining depths.
mergeTodos todos links depth visited = (++) todos $ map (\x -> (x, depth-1)) (selectLinksToVisit links visited)

-- |Extracts links not present in the given set and returns them as a list.
selectLinksToVisit :: [String] -> Set.Set String -> [String]
selectLinksToVisit links visited = filter (\a -> Set.notMember a visited) links

