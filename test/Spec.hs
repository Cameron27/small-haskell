import Control.Monad
import qualified Data.ByteString.Char8 as Char8
import Data.Either
import Data.List
import Data.String.Utils
import GHC.IO.Handle
import Interpreter.Small
import Interpreter.Types
import Parser.Small
import System.Directory
import System.Exit
import System.IO
import System.IO.Silently
import Test.HUnit
import Test.Main

data TestSpec = TestSpec {name :: String, parsed :: Bool, ran :: Bool, input :: String, expected :: String, program :: String} deriving (Show)

main :: IO ()
main = do
  x <- generateTest "test/tests/"
  void $ runTestTT x

generateTest :: FilePath -> IO Test
generateTest fp = do
  files <- getFilesRecursive fp
  contents <- mapM readFile files
  let testSpecs = zipWithM generateTestSpec files contents
  case testSpecs of
    Right testSpecs -> TestList <$> mapM (testProgram fp) testSpecs
    Left err -> return $ TestCase $ assertFailure err
  where
    getFilesRecursive :: FilePath -> IO [String]
    getFilesRecursive fp = do
      all <- map (fp ++) . filter (\s -> (s /= ".") && (s /= "..")) <$> listDirectory fp
      files <- filterM doesFileExist all
      direc <- map (++ "/") <$> filterM doesDirectoryExist all
      extra <- mapM getFilesRecursive direc
      let join = files ++ concat extra
      return join

generateTestSpec :: String -> String -> Either [Char] TestSpec
generateTestSpec name content = TestSpec <$> Right name <*> parsed <*> ran <*> input <*> expected <*> Right content
  where
    lns = lines content
    drop1 = dropUntilPrefix "//" lns
    parsed' = Right $ strip $ drop 2 $ head drop1
    parsed = case parsed' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    drop2 = dropUntilPrefix "//" $ tail drop1
    ran' = Right $ strip $ drop 2 $ head drop2
    ran = case ran' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    drop3 = dropUntilPrefix "/*" $ tail drop2
    input = Right $ strip $ extractComment (drop 2 $ intercalate "\n" drop3)
    drop4 = dropUntilPrefix "/*" $ tail drop3
    expected = Right $ strip $ extractComment (drop 2 $ intercalate "\n" drop4)
    extractComment xs
      | "*/" `isPrefixOf` xs = ""
      | xs == "" = ""
      | otherwise = head xs : extractComment (tail xs)
    dropUntilPrefix prefix (x : xs)
      | prefix `isPrefixOf` x = x : xs
      | otherwise = dropUntilPrefix prefix xs
    dropUntilPrefix prefix [] = error $ "needed comments are missing in " ++ name

testProgram :: String -> TestSpec -> IO Test
testProgram fp testSpec =
  do
    let pgm = parseSmall fp (program testSpec)
    let parseTest =
          case pgm of
            Left err -> TestCase $ assertEqual (show err) (parsed testSpec) False
            Right pgm -> TestCase $ assertEqual "program parsed when it should not have" (parsed testSpec) True
    let shouldRun = isRight pgm && parsed testSpec
    result' <- case pgm of
      Right pgm -> withStdin (Char8.pack (input testSpec)) $ hCapture [stdout, stderr] $interpretSmall pgm
      Left err -> return ("", ExitFailure 2)
    let result =
          case result' of
            (x, ExitSuccess) -> Right x
            (x, _) -> Left $ getError x
    let runTest =
          case result of
            Left err -> TestCase $ assertEqual (show err) (ran testSpec) False
            Right res -> TestCase $ assertEqual "program ran when it should not have" (ran testSpec) True
    let shouldCheckResult = shouldRun && isRight result && ran testSpec
    let resultTest =
          case result of
            Right res -> TestCase $ assertEqual "program result was not as expected" (expected testSpec) (strip res)
    return $
      name testSpec
        ~: TestList (["Parsed" ~: parseTest] ++ ["Ran" ~: runTest | shouldRun] ++ ["Output" ~: resultTest | shouldCheckResult])
  where
    getError [] = ""
    getError xs
      | "Error: " `isPrefixOf` xs = xs
      | otherwise = getError $ tail xs
