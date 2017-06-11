module UrlOps
    ( getBaseUrl
    , makeFullLink
    ) where

import qualified Data.Set as Set
import Data.List.Split (splitOn)
import Data.List

-- |Extracts the base link to a website from given url.
--For example for https://hackage.haskell.org/package/xhtml it will return https://hackage.haskell.org
getBaseUrl :: String -> String
getBaseUrl link | isPrefixOf "http" link = case split of (x:y:z:_) -> x ++ "//" ++ z
                | otherwise = head $ split
                where split = splitOn "/" link

-- | Given a path extracted from an \<a\> tag's href element and a link to its base source page,
-- returns an absolute link to a page referenced by that element.
makeFullLink :: String  -- ^ Link to the base source page (ex. stackoverflow.com).
             -> String  -- ^ Path extracted from an \<a\> tag.
             -> String  -- ^ Absolute link to the referenced page.
makeFullLink baseUrl ('/':'/':xs) = "http://" ++ xs
makeFullLink baseUrl ('/':link) = baseUrl ++ ('/':link)
makeFullLink baseUrl _ = baseUrl
