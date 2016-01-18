{-
Copyright 2015 Arun Raghavan <mail@arunraghavan.net>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE RecordWildCards #-}

module Main where

import System.FilePath (joinPath)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Options.Applicative

import TinyXML
import TinyXMLConfig
import UCM
import XML2UCM (xml2ucm)

data Opts = Opts { optConfigFile :: String,
                   optTinyXMLFile :: String,
                   optOutputDir :: String,
                   optForce :: Bool }

parseOptions :: Parser Opts
parseOptions = Opts
           <$> strOption
               ( long "config"
              <> short 'c'
              <> metavar "FILE"
              <> help "Configuration XML file" )
           <*> strOption
               ( long "mixer-paths"
              <> short 'm'
              <> metavar "FILE"
              <> help "Mixer paths XML file" )
           <*> strOption
               ( long "output-dir"
              <> short 'o'
              <> value "-"
              <> metavar "DIR"
              <> help "Output path for UCM configuration directory" )
           <*> switch
               ( long "force"
              <> short 'f'
              <> help "Overwrite existing configuration if it exists")

run :: Opts -> IO ()
run Opts{..} = do
  xmlFile    <- readFile optTinyXMLFile
  configFile <- readFile optConfigFile
  xml        <- TinyXML.parse xmlFile
  config     <- TinyXMLConfig.parse configFile
  let stdout = optOutputDir == "-"
  let card   = TinyXMLConfig.confCardName config
  let dir    = joinPath [optOutputDir, card]
  exists     <- doesDirectoryExist dir
  case (stdout, optForce, exists) of
    (True, _, _)     -> return () -- Just write to stdout
    (_, False, True) -> error ("Error: Output directory '" ++ dir ++ "' already exists (use -f to overwrite)")
    (_, _, _)        -> createDirectoryIfMissing False dir
  mapM_ (uncurry (dumpFile stdout card optOutputDir)) (UCM.generateFiles $ xml2ucm xml config)
  where
    dumpFile :: Bool -> String -> FilePath -> FilePath -> String -> IO ()
    dumpFile False card dir file =
      writeFile (joinPath [dir, card, file])
    dumpFile True card _ file =
      dumpFileNameAndContents $ joinPath [card, file]
      where
        dumpFileNameAndContents :: String -> String -> IO ()
        dumpFileNameAndContents path contents =
          -- File path and contents in markdown formattable form
          putStrLn $ "## " ++ path ++ "\n```\n" ++ contents ++ "```\n"


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> header "xml2ucm - Android mixer path XML to ALSA UCM converter" )
