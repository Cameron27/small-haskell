{-# LANGUAGE MultiWayIf #-}

import Control.Concurrent.ParallelIO
import Control.Monad
import qualified Data.ByteString.Char8 as Char8
import Data.Either
import Data.List
import Data.Maybe
import Data.String.Utils
import Interpreter.Small
import Parser.Small
import System.CPUTime
import System.Clock
import System.Console.ParseArgs
import System.Directory
import System.Environment
import System.Exit
import System.IO
import System.IO.Silently
import System.Process
import Test.HUnit
import Test.Main
import Text.Printf
import Text.Regex.TDFA
import TypeChecker.Small

data TestSpec = TestSpec {name :: String, parsed :: Bool, typed :: Bool, ran :: Bool, input :: String, expected :: String, program :: String} deriving (Show)

data Arguments = TestType | KPath
  deriving (Show, Eq, Ord)

main :: IO ()
main = do
  let testPath = "test/tests/"
  args <-
    parseArgsIO
      ArgsComplete
      [ Arg
          { argIndex = KPath,
            argAbbr = Just 'k',
            argName = Just "k-path",
            argData = argDataOptional "PATH" ArgtypeString,
            argDesc = "Path to K semantics"
          },
        Arg
          { argIndex = TestType,
            argAbbr = Nothing,
            argName = Nothing,
            argData = argDataDefaulted "TEST" ArgtypeString "haskell",
            argDesc = "The test to perform: haskell (default), k, k-parse-kast, k-parse-bison"
          }
      ]
  case getArgString args TestType of
    Just "k-parse-kast" -> kParsingSpeedTest args testPath "kast"
    Just "k-parse-bison" -> kParsingSpeedTest args testPath "bison"
    _ -> do
      x <- generateTest args testPath
      void $ runTestTT x
      stopGlobalPool

generateTest :: Args Arguments -> FilePath -> IO Test
generateTest args fp = do
  files <- getFilesRecursive fp
  contents <- mapM readFile files
  let testSpecs = zipWithM generateTestSpec files contents
  case testSpecs of
    Right testSpecs ->
      case getArgString args TestType of
        Just "k" -> do
          if gotArg args KPath then return () else error "No K path provided."
          let Just kPath' = getArgString args KPath
          let kPath = if last kPath' == '/' then kPath' else kPath' ++ "/"
          x <- parallel $ map (testKProgram kPath) testSpecs
          return $ TestList x
        Just "haskell" -> TestList <$> mapM testHaskellProgram testSpecs
        (Just s) -> error $ printf "%s is not a valid test." s
        Nothing -> error "Cannot occur."
    Left err -> return $ TestCase $ assertFailure err

getFilesRecursive :: FilePath -> IO [String]
getFilesRecursive fp = do
  all <- map (fp ++) . filter (\s -> (s /= ".") && (s /= "..")) <$> listDirectory fp
  files <- filterM doesFileExist all
  direc <- map (++ "/") <$> filterM doesDirectoryExist all
  extra <- mapM getFilesRecursive direc
  let join = files ++ concat extra
  return join

generateTestSpec :: String -> String -> Either [Char] TestSpec
generateTestSpec name content = TestSpec <$> Right name <*> parsed <*> typed <*> ran <*> input <*> expected <*> Right content
  where
    lns = lines content
    drop1 = dropUntilPrefix "//" lns
    parsed' = Right $ strip $ drop 2 $ head drop1
    parsed = case parsed' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    drop1_5 = dropUntilPrefix "//" $ tail drop1
    typed' = Right $ strip $ drop 2 $ head drop1_5
    typed = case typed' of
      Right "true" -> Right True
      Right "false" -> Right False
      _ -> Left "parsed attribute is not a boolean"
    drop2 = dropUntilPrefix "//" $ tail drop1_5
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
    dropUntilPrefix prefix [] = error $ printf "Needed comments are missing in %s." name

testHaskellProgram :: TestSpec -> IO Test
testHaskellProgram testSpec =
  do
    let pgm = parseSmall (name testSpec) (program testSpec)
    let parseTest =
          case pgm of
            Left err -> TestCase $ assertEqual (show err) (parsed testSpec) False
            Right pgm -> TestCase $ assertEqual "program parsed when it should not have" (parsed testSpec) True
    let shouldType = isRight pgm && parsed testSpec
    let typeCheckResult = case pgm of
          Right pgm -> typeCheckSmall pgm
          _ -> error "Cannot occur."
    let typedTest =
          case typeCheckResult of
            Just err -> TestCase $ assertEqual (show err) (typed testSpec) False
            Nothing -> TestCase $ assertEqual "program type checked when it should not have" (typed testSpec) True
    let shouldRun = shouldType && isNothing typeCheckResult && typed testSpec
    runResult' <- case pgm of
      Right pgm -> withStdin (Char8.pack (input testSpec)) $ hCapture [stdout, stderr] $ interpretSmall pgm
      Left err -> return ("", ExitFailure 2)
    let runResult =
          case runResult' of
            (x, ExitSuccess) -> Right x
            (x, code) -> Left (code, getError x)
    let runTest =
          case runResult of
            Left (ExitFailure 1, err) -> TestCase $ assertEqual (show err) (ran testSpec) False
            Right res -> TestCase $ assertEqual "program ran when it should not have" (ran testSpec) True
            Left (ExitFailure i, err) -> TestCase $ assertFailure $ "Unexpected exit-code: " ++ show i ++ "\n" ++ show err
            _ -> error "Cannot occur."
    let shouldCheckResult = shouldRun && isRight runResult && ran testSpec
    let regex = expected testSpec
    let resultTest =
          case runResult of
            Right res ->
              TestCase $
                assertBool
                  ("program result was not as expected\nexpected: " ++ show (expected testSpec) ++ "\n but got: " ++ show (strip res))
                  ( if regex == ""
                      then strip res == ""
                      else (strip res =~ regex) == strip res
                  )
            _ -> error "Cannot occur."
    return $
      name testSpec
        ~: TestList (["Parsed" ~: parseTest] ++ ["Typed" ~: typedTest | shouldType] ++ ["Ran" ~: runTest | shouldRun] ++ ["Output" ~: resultTest | shouldCheckResult])
  where
    getError [] = ""
    getError xs
      | "Error: " `isPrefixOf` xs = xs
      | otherwise = getError $ tail xs

-- testHaskellProgram :: TestSpec -> IO Test
-- testHaskellProgram testSpec =
--   do
--     (exitCode, stdout, stderr) <-
--       readCreateProcessWithExitCode
--         (shell $ "stack run -- " ++ name testSpec)
--         $ input testSpec ++ "\n"
--     let parseTest =
--           case exitCode of
--             ExitFailure 3 -> TestCase $ assertEqual stderr (parsed testSpec) False
--             _ -> TestCase $ assertEqual "program parsed when it should not have" (parsed testSpec) True
--     let shouldType = exitCode /= ExitFailure 3 && parsed testSpec
--     let typeTest =
--           case exitCode of
--             ExitFailure 2 -> TestCase $ assertEqual stderr (typed testSpec) False
--             _ -> TestCase $ assertEqual "program type checked when it should not have" (typed testSpec) True
--     let shouldRun = shouldType && exitCode /= ExitFailure 2 && typed testSpec
--     let runTest =
--           case exitCode of
--             ExitFailure 1 -> TestCase $ assertEqual stderr (ran testSpec) False
--             ExitSuccess -> TestCase $ assertEqual "program ran when it should not have" (ran testSpec) True
--             ExitFailure i -> TestCase $ assertFailure $ "Unexpected exit-code: " ++ show i
--     let shouldCheckResult = shouldRun && exitCode == ExitSuccess && ran testSpec
--     let regex = expected testSpec
--     let resultTest =
--           TestCase $
--             assertBool
--               ("program result was not as expected\nexpected: " ++ show (expected testSpec) ++ "\n but got: " ++ show (strip stdout))
--               ( if regex == ""
--                   then strip stdout == ""
--                   else (strip stdout =~ regex) == strip stdout
--               )
--     return $
--       name testSpec
--         ~: TestList (["Parsed" ~: parseTest] ++ ["Typed" ~: typeTest | shouldType] ++ ["Ran" ~: runTest | shouldRun] ++ ["Output" ~: resultTest | shouldCheckResult])

testKProgram :: String -> TestSpec -> IO Test
testKProgram path testSpec =
  do
    (exitCode, stdout, stderr) <-
      readCreateProcessWithExitCode
        (shell $ path ++ "run.sh -o none " ++ name testSpec)
        $ input testSpec ++ "\n"
    let parseTest =
          case exitCode of
            ExitFailure 3 -> TestCase $ assertEqual stderr (parsed testSpec) False
            _ -> TestCase $ assertEqual "program parsed when it should not have" (parsed testSpec) True
    let shouldType = exitCode /= ExitFailure 3 && parsed testSpec
    let typeTest =
          case exitCode of
            ExitFailure 2 -> TestCase $ assertEqual stderr (typed testSpec) False
            _ -> TestCase $ assertEqual "program type checked when it should not have" (typed testSpec) True
    let shouldRun = shouldType && exitCode /= ExitFailure 2 && typed testSpec
    let runTest =
          case exitCode of
            ExitFailure 1 -> TestCase $ assertEqual stderr (ran testSpec) False
            ExitSuccess -> TestCase $ assertEqual "program ran when it should not have" (ran testSpec) True
            ExitFailure i -> TestCase $ assertFailure $ "Unexpected exit-code: " ++ show i
    let shouldCheckResult = shouldRun && exitCode == ExitSuccess && ran testSpec
    let regex = expected testSpec
    let resultTest =
          TestCase $
            assertBool
              ("program result was not as expected\nexpected: " ++ show (expected testSpec) ++ "\n but got: " ++ show (strip stdout))
              ( if regex == ""
                  then strip stdout == ""
                  else (strip stdout =~ regex) == strip stdout
              )
    return $
      name testSpec
        ~: TestList (["Parsed" ~: parseTest] ++ ["Typed" ~: typeTest | shouldType] ++ ["Ran" ~: runTest | shouldRun] ++ ["Output" ~: resultTest | shouldCheckResult])

kParsingSpeedTest args fp s = do
  if gotArg args KPath then return () else error "No K path provided."
  let Just kPath' = getArgString args KPath
  let kPath = if last kPath' == '/' then kPath' else kPath' ++ "/"
  files <- getFilesRecursive fp
  contents <- mapM readFile files
  let testSpecs = zipWithM generateTestSpec files contents
  case testSpecs of
    Right testSpecs -> do
      let shellCommand =
            if
                | s == "bison" -> kPath ++ "out/interpreter/small-interpreter-kompiled/parser_PGM "
                | s == "kast" -> "kast --directory " ++ kPath ++ "out/interpreter/ -o KORE "
                | otherwise -> s ++ " is not a valid parser to test."
      putStrLn $ printf "Parsing %d files." (length files)
      start <- getTime Monotonic
      parallel $
        map
          ( \testSpec -> do
              (exitCode, stdout, stderr) <-
                readCreateProcessWithExitCode
                  (shell $ shellCommand ++ name testSpec)
                  ""
              if exitCode == ExitSuccess
                then unless (parsed testSpec) $ putStrLn $ printf "Parsed %s when it should not have." (name testSpec)
                else when (parsed testSpec) $ putStrLn $ printf "Failed to parse %s." (name testSpec)
          )
          testSpecs
      end <- getTime Monotonic
      let diff = fromIntegral (toNanoSecs $ diffTimeSpec start end) / (10 ^ 9)
      printf "Computation time: %0.3f sec\n" (diff :: Double)
      return ()
    Left err -> putStrLn err