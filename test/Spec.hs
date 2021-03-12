import Control.Monad (void, zipWithM)
import Data.Either (isRight)
import Data.List (intercalate, isPrefixOf)
import Data.String.Utils (strip)
import Interpreter.Small (interpretSmall)
import Interpreter.Types (Ans)
import Parser.Small (parseSmall)
import System.Directory (getDirectoryContents)
import Test.HUnit
  ( Test (TestCase, TestList),
    assertEqual,
    assertFailure,
    runTestTT,
    (~:),
  )

data TestSpec = TestSpec {name :: String, parsed :: Bool, ran :: Bool, expected :: String, program :: String} deriving (Show)

main :: IO ()
main = do
  x <- generateTest "test/tests/"
  void $ runTestTT x

generateTest :: FilePath -> IO Test
generateTest fp = do
  files <- getDirectoryContents fp
  fileContents <- mapM (readFile . (fp ++)) (init $init files)
  let testSpecs = zipWithM generateTestSpec files fileContents
  case testSpecs of
    Right testSpecs -> return $ TestList $ map (testProgram fp) testSpecs
    Left err -> return $ TestCase $ assertFailure err

generateTestSpec :: String -> String -> Either [Char] TestSpec
generateTestSpec name content
  | length lns > 2 = TestSpec <$> Right name <*> parsed <*> ran <*> expected <*> Right content
  | otherwise = Left "not enough lines for comments"
  where
    lns = lines content
    parsed' = case head lns of
      s
        | "//" `isPrefixOf` s -> Right $ strip $ drop 2 s
        | otherwise -> Left "no line for parses"
    parsed = case parsed' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    ran' = case lns !! 1 of
      s
        | "//" `isPrefixOf` s -> Right $ strip $ drop 2 s
        | otherwise -> Left "no line for ran"
    ran = case ran' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    expected = case lns !! 2 of
      s
        | "/*" `isPrefixOf` s -> Right $ strip $ extractExpected (drop 2 $ intercalate "\n" $ drop 2 lns)
        | otherwise -> Left "no line for expected"
    extractExpected xs
      | "*/" `isPrefixOf` xs = ""
      | xs == "" = ""
      | otherwise = head xs : extractExpected (tail xs)

testProgram :: String -> TestSpec -> Test
testProgram fp testSpec = name testSpec ~: TestList ([parseTest] ++ [runTest | shouldRun] ++ [resultTest | shouldCheckResult])
  where
    pgm = parseSmall fp (program testSpec)
    parseTest = case pgm of
      Left err -> TestCase $ assertEqual (show err) (parsed testSpec) False
      Right pgm -> TestCase $ assertEqual "program parsed when it should not have" (parsed testSpec) True
    shouldRun = isRight pgm && parsed testSpec
    result = checkResult $ case pgm of
      Right pgm -> interpretSmall pgm []
    runTest = case result of
      Left err -> TestCase $ assertEqual (show err) (ran testSpec) False
      Right res -> TestCase $ assertEqual "program ran when it should not have" (ran testSpec) True
    shouldCheckResult = shouldRun && isRight result && ran testSpec
    resultTest = case result of
      Right res -> TestCase $ assertEqual "program result was not as expected" (expected testSpec) (strip res)

checkResult :: Ans -> Either String String
checkResult _ = Right ""