import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.List
import UrlOps

main :: IO ()
main = defaultMain [getBaseUrlTest, makeFullLinkTest, getBaseUrlQuickTest]

getBaseUrlTest :: Test
getBaseUrlTest = testGroup "Unit test of getBaseUrl"
    [testCase "getBaseUrl when url starts with http" (
        assertEqual "getBaseUrl \"https://github.com/about/press\"" "https://github.com" $ getBaseUrl "https://github.com/about/press"),
    testCase "getBaseUrl when url doesn't start with http" (
        assertEqual "getBaseUrl \"github.com/about/press\"" "github.com" $ getBaseUrl "github.com/about/press")]

makeFullLinkTest :: Test
makeFullLinkTest = testGroup "Unit test of makeFullLink"
    [testCase "makeFullLink with empty link" (
        assertEqual "makeFullLink \"https://github.com\"" "https://github.com" $ makeFullLink "https://github.com" []),
    testCase "makeFullLink when link doesn't start with /" (
        assertEqual "makeFullLink \"https://github.com\" \"about\"" "https://github.com" $ makeFullLink "https://github.com" "about"),
    testCase "makeFullLink when link starts with /" (
        assertEqual "makeFullLink \"https://github.com\" \"/about\"" "https://github.com/about" $ makeFullLink "https://github.com" "/about"),
    testCase "makeFullLink when link starts with double /" (
        assertEqual "makeFullLink \"https://github.com\" \"//about\"" "http://about" $ makeFullLink "https://github.com" "//about")]

getBaseUrlQuickTest :: Test
getBaseUrlQuickTest = testGroup "QuickCheck test of getBaseUrl without http"
   [testProperty "For strings without /" (forAll (genStr 10 (\x -> (not $ '/' `elem` x) && (not $ "http" `isPrefixOf` x))) (\value -> getBaseUrl value == value)),
   testProperty "For strings with /" (forAll (genStr 10 (\x -> ('/' `elem` x) && (not $ "http" `isPrefixOf` x))) (\value -> getBaseUrl value == takeWhile (/= '/') value))]

genStr :: Int -> (String -> Bool) -> Gen String
genStr n f = suchThat (vectorOf n arbitrary) f
