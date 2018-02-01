-- |Netrium is Copyright Anthony Waite, Dave Hetwett, Shaun Laurens 2009-2015, and files herein are licensed
-- |under the MIT license,  the text of which can be found in license.txt
--
module Main where

import           ObservableDB
import           Paths_netrium_demo
import           UnitsDB

import           Control.Monad             (liftM, liftM2, when)
import           Data.Version
import           System.Console.GetOpt
import           System.Directory          (canonicalizePath,
                                            getTemporaryDirectory, removeFile)
import           System.Environment        (getArgs, getProgName)
import           System.Exit               (ExitCode (..), exitFailure,
                                            exitWith)
import           System.FilePath           (addExtension, dropExtension,
                                            takeDirectory, (</>))
import           System.IO                 (hClose, hPutStr, openTempFile, stderr)
import           System.Process            (runProcess, waitForProcess)
import           Text.XML.HaXml.Types
import           Text.XML.HaXml.XmlContent


data Options =
  Options
    { optObsDBs     :: [FilePath]
    , optUnitDBs    :: [FilePath]
    , optImportDirs :: [FilePath]
    , optFast       :: Bool
    , optVersion    :: Bool
    }

defaultOptions =
  Options
    { optObsDBs  = []
    , optUnitDBs = []
    , optImportDirs = []
    , optFast    = False
    , optVersion = False
    }

options :: [OptDescr (Options -> Options)]
options = [Option [] ["obs-db"]
                  (ReqArg (\ db opts -> opts { optObsDBs = db : optObsDBs opts }) "<db.xml>")
                  "use the observable database <db.xml>"
          ,Option [] ["units-db"]
                  (ReqArg (\ db opts -> opts { optUnitDBs = db : optUnitDBs opts }) "<db.xml>")
                  "use the units (products, currencies etc) database <db.xml>"
          ,Option [] ["import-dir"]
                  (ReqArg (\ db opts -> opts { optImportDirs = db : optImportDirs opts }) "DIR")
                  "Allow contracts to import modules from this directory"
          ,Option [] ["fast"]
                  (NoArg (\ opts -> opts { optFast = True }))
                  "Generate the output XML quickly but without any nice formatting"
          ,Option [] ["version"]
                  (NoArg (\ opts -> opts { optVersion = True }))
                  "Print version information"
          ]

main :: IO ()
main =
  do
    plainArgs <- getArgs
    let (optMods, args, errs) = getOpt Permute options plainArgs
    let opts = foldl (flip ($)) defaultOptions optMods
    case args of
      _ | optVersion opts         -> printVersion
      [input]         | null errs -> normalise opts input output
                                       where output = addExtension input "xml"
      [input, output] | null errs -> normalise opts input output
      _                           -> exit

exit :: IO ()
exit =
  do
    p <- getProgName
    let txt = "Usage: " ++ p ++ " [options] <input> [<output.xml>]\n\n" ++
              "Flags:"
    putStrLn $ usageInfo txt options
    exitFailure

printVersion :: IO ()
printVersion =
  do
    p <- getProgName
    putStrLn $ "netrium-demo " ++ p ++ " version " ++ showVersion version


getObservableDBs :: Options -> IO [ObservableDB]
getObservableDBs = mapM fReadXml . optObsDBs

getUnitsDBs :: Options -> IO [UnitsDB]
getUnitsDBs = mapM fReadXml . optUnitDBs


normalise :: Options -> FilePath -> FilePath -> IO ()
normalise opts input output =
  do
    tdir <- getTemporaryDirectory
    let cdir = case takeDirectory input of "" -> "."; dir -> dir

    -- read and process the various input files
    obsDBs    <- getObservableDBs opts
    unitsDBs  <- getUnitsDBs      opts
    wrapper   <- readFile =<< getDataFileName "normalise-wrapper.hs"
    contract  <- readFile input
    absOutput <- canonicalizePath output

    -- write the temporary source file
    (fp, h)  <- openTempFile tdir "norm.hs"
    hPutStr h $ generateContractProgram (optFast opts)
                                        obsDBs unitsDBs
                                        wrapper "normalise-wrapper.hs"
                                        contract input
                                        absOutput
    hClose h

    -- compile and run it
    ddir <- getDataDir
    let ghcargs = [ "runghc", "--package", "netrium-demo", "--" ]
        args    = ghcargs ++ [fp]
    print args
    ph <- runProcess "stack" args Nothing Nothing Nothing Nothing Nothing
    exit <- waitForProcess ph
    removeFile fp
    when (exit /= ExitSuccess) exitFailure


generateContractProgram :: Bool
                        -> [ObservableDB] -> [UnitsDB]
                        -> String -> FilePath
                        -> String -> FilePath
                        -> FilePath
                        -> String
generateContractProgram fast obsDBs unitsDBs
                        wrapper wrapperFile
                        contract contractFile outputFile =
  unlines
    [ "-- This is a generated file; do not edit.\n"
    , "{-# LINE 1 " ++ show wrapperFile ++ " #-}"
    , wrapper

    , "{-# LINE 1 " ++ show contractFile ++ " #-}"
    , contract

    , "{-# LINE 1 \"observables database\" #-}"
    , unlines (map compileObservableDB obsDBs)

    , "{-# LINE 1 \"units database\" #-}"
    , unlines (map compileUnitsDB unitsDBs)

    , "{-# LINE 1 \"generated contract program\" #-}"
    , "entrypoint :: Contract"
    , "entrypoint = contract"
    , "main = writeFile " ++ show outputFile ++ " $ " ++ outputCode
    ]
  where
    outputCode
      | fast
      = "PP.renderStyle PP.style { PP.mode = PP.OneLineMode } $ "
        ++ "XML.PP.document $ XML.toXml False entrypoint"

      | otherwise
      = "XML.showXml False entrypoint"
