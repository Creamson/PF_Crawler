import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
import Data.List
import UrlOps

main :: IO ()
main = defaultMain [getBaseUrlTest, makeFullLinkTest, getBaseUrlQuickTest, getBaseUrlHttpQuickTest, makeFullLinkQuickTest]

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
        assertEqual "makeFullLink \"https://github.com\" \"//about.com\"" "http://about.com" $ makeFullLink "https://github.com" "//about.com"),
    testCase "makeFullLink when link absolute with http" (
        assertEqual "makeFullLink \"https://github.com\" \"https://stackexchange.com\"" "https://stackexchange.com" $ makeFullLink "https://github.com" "https://stackexchange.com"),
    testCase "makeFullLink when link absolute without http" (
        assertEqual "makeFullLink \"https://github.com\" \"stackexchange.com\"" "stackexchange.com" $ makeFullLink "https://github.com" "stackexchange.com"),
    testCase "makeFullLink when link absolute without http not base" (
        assertEqual "makeFullLink \"https://github.com\" \"stackexchange.com/helloWorld\"" "stackexchange.com/helloWorld" $ makeFullLink "https://github.com" "stackexchange.com/helloWorld")]
    
getBaseUrlQuickTest :: Test
getBaseUrlQuickTest = testGroup "QuickCheck test of getBaseUrl without http"
   [testProperty "For strings without /" (forAll (genStr 10 $ (not . containsSlash) `andFunc` (not . containsHttp))
                                         (\value -> getBaseUrl value == value)),
   testProperty "For strings with /" (forAll (genStr 10 $ containsSlash `andFunc` (not . containsHttp))
                                     (\value -> getBaseUrl value == takeToSlash value))]

getBaseUrlHttpQuickTest :: Test
getBaseUrlHttpQuickTest = testGroup "QuickCheck test of getBaseUrl with http"
   [testProperty "For strings with /" (forAll (genStr 10 $ containsSlash `andFunc` (not . containsHttp))
                                      (\value -> (getBaseUrl $ "http://" ++ value) == ("http://" ++ takeToSlash value)))]

makeFullLinkQuickTest :: Test
makeFullLinkQuickTest = testGroup "QuickCheck test of makeFullLink"
   [testProperty "For strings starts with /" (forAll (genStr 10 $ containsSlash `andFunc` (not . containsSlash . tail))
                                             (\value -> makeFullLink "https://github.com" value == "https://github.com" ++ value))]

--helpers
genStr :: Int -> (String -> Bool) -> Gen String
genStr n f = suchThat (vectorOf n arbitrary) f

takeToSlash :: String -> String
takeToSlash = takeWhile (/= '/')

andFunc :: (String -> Bool) -> (String -> Bool) -> String -> Bool
andFunc f g x = f x && g x

containsHttp :: String -> Bool
containsHttp x = "http" `isPrefixOf` x

containsSlash :: String -> Bool
containsSlash x = '/' `elem` x
