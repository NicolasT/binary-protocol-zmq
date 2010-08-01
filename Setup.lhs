#!/usr/bin/env runhaskell
\begin{code}

import Control.Monad (unless)

import Data.Maybe (listToMaybe, fromMaybe)

import System.Directory (doesDirectoryExist, setCurrentDirectory)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (system)

import Distribution.PackageDescription
    (  BuildInfo, PackageDescription, buildable, buildInfo
     , exeName, executables)
import Distribution.Simple
    (Args, UserHooks, defaultMainWithHooks, simpleUserHooks, runTests)
import Distribution.Simple.Build (build)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(buildDir))
import Distribution.Simple.PreProcess (knownSuffixHandlers)
import Distribution.Simple.Setup (defaultBuildFlags)

main :: IO ()
main = defaultMainWithHooks hooks

hooks :: UserHooks
hooks = simpleUserHooks { runTests = testHook }

testBinaryProtocolZMQ :: a -> (BuildInfo -> a) -> PackageDescription -> a
testBinaryProtocolZMQ dflt f pd =
    fromMaybe dflt $ listToMaybe
        [ f (buildInfo exe)
        | exe <- executables pd
        , exeName exe == "test-binary-protocol-zmq"
        ]

testHook :: Args -> Bool -> PackageDescription -> LocalBuildInfo -> IO ()
testHook _ _ pd lbi = do
    let testDir = buildDir lbi </> "test-binary-protocol-zmq"

    t <- doesDirectoryExist testDir

    unless t $ do
        unless (testBinaryProtocolZMQ False buildable pd) $
            fail $ "Reconfigure with 'cabal configure -ftests' or " ++
                "'cabal install -ftests' and try again"
        putStrLn "Building tests"
        build pd lbi defaultBuildFlags knownSuffixHandlers
        putStrLn "Tests built"

    setCurrentDirectory testDir

    exitcode <- system "./test-binary-protocol-zmq"
    unless (exitcode == ExitSuccess) $
        fail "Test failed"

\end{code}
