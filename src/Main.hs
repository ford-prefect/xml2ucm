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

{-# LANGUAGE RecordWildCards, DisambiguateRecordFields #-}

module Main where

import System.FilePath (joinPath)
import System.Directory (createDirectory, doesDirectoryExist)
import Options.Applicative
import Data.Maybe

import TinyXML
import TinyXMLConfig
import UCM

-- Generate UCM

generateCommand :: TinyXML.Control -> UCM.Command
generateCommand TinyXML.Control{..} = UCM.CSet controlName controlValue

lookupListOrDie :: String -> String -> [(String, a)] -> a
lookupListOrDie typ name list
  | isNothing item = error ("Could not find " ++ typ ++ ": '" ++ name ++ "'")
  | otherwise      = fromJust item
  where
    item = lookup name list

generateDefaultCommand :: TinyXML.Mixer -> TinyXML.Control -> UCM.Command
generateDefaultCommand TinyXML.Mixer{..} TinyXML.Control{controlName = name} =
  UCM.CSet name defaultValue
  where
    defaultValue = controlValue $ lookupListOrDie "control" name mixerDefaults

getPathControls :: TinyXML.Mixer -> String -> [TinyXML.Control]
getPathControls xml@TinyXML.Mixer{..} pName = let
    TinyXML.Path{..} = lookupListOrDie "path" pName mixerPaths
  in
    concatMap (getPathControls xml) pathPaths ++ pathControls

generateSequence :: (TinyXML.Control -> UCM.Command) -> TinyXML.Mixer ->  String -> [String] -> UCM.Sequence
generateSequence genCommand xml ctl =
  UCM.Sequence ctl . map genCommand . concatMap (getPathControls xml)

generateEnableSequence :: TinyXML.Mixer -> String -> [String] -> UCM.Sequence
generateEnableSequence = generateSequence generateCommand

generateDisableSequence :: TinyXML.Mixer -> String -> [String] -> UCM.Sequence
generateDisableSequence xml = generateSequence (generateDefaultCommand xml) xml

generateDevice :: TinyXML.Mixer -> TinyXMLConfig.Config -> TinyXMLConfig.Device -> UCM.Device
generateDevice xml TinyXMLConfig.Config{..} TinyXMLConfig.Device{..} =
  UCM.Device devName
             devDesc
             devPlaybackChannels
             devCaptureChannels
             devPlaybackVolume
             devCaptureVolume
             (generateEnableSequence xml confCtlDev devPaths)
             (generateDisableSequence xml confCtlDev devPaths)
             devConflicts devValues

generateModifier :: TinyXML.Mixer -> TinyXMLConfig.Config -> TinyXMLConfig.Modifier -> UCM.Modifier
generateModifier xml TinyXMLConfig.Config{..} TinyXMLConfig.Modifier{..} =
  UCM.Modifier modName
               modDesc
               modPlayDev
               modCaptureDev
               (generateEnableSequence xml confCtlDev modPaths)
               (generateDisableSequence xml confCtlDev modPaths)
               modSupports modValues

generateVerb :: TinyXML.Mixer -> TinyXMLConfig.Config -> TinyXMLConfig.UseCase -> UCM.Verb
generateVerb xml conf@TinyXMLConfig.Config{..} TinyXMLConfig.UseCase{..} =
  UCM.Verb ucName
           ucDesc
           ucPlayDev
           ucCaptureDev
           (generateEnableSequence xml confCtlDev ucPaths)
           (generateDisableSequence xml confCtlDev ucPaths)
           (map (generateDevice xml conf) ucDevices)
           (map (generateModifier xml conf) ucModifiers)
           ucValues

generateDefaults :: TinyXML.Mixer -> TinyXMLConfig.Config -> UCM.Sequence
generateDefaults TinyXML.Mixer{..} TinyXMLConfig.Config{..} =
  UCM.Sequence confCtlDev (map generateCommand' mixerDefaults)
  where
    generateCommand' (_, c) = generateCommand c

xml2ucm :: TinyXML.Mixer -> TinyXMLConfig.Config -> UCM.Config
xml2ucm xml conf@TinyXMLConfig.Config{..} =
  UCM.Config confCardName
             (map (generateVerb xml conf) confUseCases)
             (generateDefaults xml conf)

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
              <> value "."
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
  let dir = joinPath [optOutputDir, TinyXMLConfig.confCardName config]
  exists     <- doesDirectoryExist dir
  case (optForce, exists) of
    (False, True) -> error ("Error: Output directory '" ++ dir ++ "' already exists (use -f to overwrite)")
    (True, True)  -> return () -- Directory exists, but we should just write the files we want out
    (_, False)    -> createDirectory dir
  mapM_ (uncurry (dumpFile dir)) (UCM.generateFiles $ xml2ucm xml config)
  where
    dumpFile :: FilePath -> FilePath -> String -> IO ()
    dumpFile dir file =
      writeFile (joinPath [dir, file])


main :: IO ()
main = execParser opts >>= run
  where
    opts = info (helper <*> parseOptions)
      ( fullDesc
     <> header "xml2ucm - Android mixer path XML to ALSA UCM converter" )
