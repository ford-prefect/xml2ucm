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

import System.Environment (getArgs)
import System.Exit

import Data.Maybe (fromJust)

import TinyXML
import TinyXMLConfig
import UCM

-- Generate UCM

generateCommand :: TinyXML.Control -> UCM.Command
generateCommand TinyXML.Control{..} = UCM.CSet controlName controlValue

generateDefaultCommand :: TinyXML.Mixer -> TinyXML.Control -> UCM.Command
generateDefaultCommand TinyXML.Mixer{..} TinyXML.Control{controlName = name} =
  UCM.CSet name defaultValue
  where
    defaultValue = controlValue $ fromJust $ lookup name mixerDefaults -- FIXME: error handling

getPathControls :: TinyXML.Mixer -> String -> [TinyXML.Control]
getPathControls xml@TinyXML.Mixer{..} pName = let
    TinyXML.Path{..} = fromJust $ lookup pName mixerPaths -- FIXME: error handling
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
            (generateEnableSequence xml confCtlDev devPaths)
            (generateDisableSequence xml confCtlDev devPaths)
            devConflicts devValues

generateModifier :: TinyXML.Mixer -> TinyXMLConfig.Config -> TinyXMLConfig.Modifier -> UCM.Modifier
generateModifier xml TinyXMLConfig.Config{..} TinyXMLConfig.Modifier{..} =
  UCM.Modifier modName
               modPlayDev
               modCaptureDev
               (generateEnableSequence xml confCtlDev modPaths)
               (generateDisableSequence xml confCtlDev modPaths)
               modSupports modValues

generateVerb :: TinyXML.Mixer -> TinyXMLConfig.Config -> TinyXMLConfig.UseCase -> UCM.Verb
generateVerb xml conf@TinyXMLConfig.Config{..} TinyXMLConfig.UseCase{..} =
  UCM.Verb ucName
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

dumpFile :: String -> String -> IO ()
dumpFile file contents = do
  putStrLn (file ++ ":")
  putStrLn contents

main :: IO ()
main = do
  args <- getArgs
  -- FIXME: Error handling
  xmlString <- readFile (head args)
  configString <- readFile (head $ tail args)
  xml <- parseTinyXML xmlString
  config <- parseTinyXMLConfig configString
  mapM_ (uncurry dumpFile) (UCM.generateFiles $ xml2ucm xml config)
  exitSuccess
