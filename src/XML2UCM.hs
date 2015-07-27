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

module XML2UCM
( xml2ucm
) where

import Data.Maybe

import TinyXML
import TinyXMLConfig
import UCM

stringToMaybe :: String -> Maybe String
stringToMaybe "" = Nothing
stringToMaybe s  = Just s

generateCommand :: TinyXML.Control -> Maybe UCM.Command
generateCommand TinyXML.Control{..} = Just $ UCM.CSet controlName (stringToMaybe controlIndex) controlValue

lookupListOrDie :: String -> String -> [(String, a)] -> a
lookupListOrDie typ name list
  | isNothing item = error ("Could not find " ++ typ ++ ": '" ++ name ++ "'")
  | otherwise      = fromJust item
  where
    item = lookup name list

generateDefaultCommand :: TinyXML.Mixer -> TinyXML.Control -> Maybe UCM.Command
generateDefaultCommand TinyXML.Mixer{..} TinyXML.Control{controlName = name}
  | isNothing control = Nothing
  | otherwise         = Just $ UCM.CSet name (stringToMaybe . controlIndex $ fromJust control) (controlValue $ fromJust control)
  where
    control = lookup name mixerDefaults

getPathControls :: TinyXML.Mixer -> String -> [TinyXML.Control]
getPathControls xml@TinyXML.Mixer{..} pName = let
    TinyXML.Path{..} = lookupListOrDie "path" pName mixerPaths
  in
    concatMap (getPathControls xml) pathPaths ++ pathControls

generateSequence :: (TinyXML.Control -> Maybe UCM.Command) -> TinyXML.Mixer ->  String -> [String] -> UCM.Sequence
generateSequence genCommand xml ctl =
  UCM.Sequence ctl . mapMaybe genCommand . concatMap (getPathControls xml)

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
  UCM.Sequence confCtlDev (mapMaybe generateCommand' mixerDefaults)
  where
    generateCommand' (_, c) = generateCommand c

xml2ucm :: TinyXML.Mixer -> TinyXMLConfig.Config -> UCM.Config
xml2ucm xml conf@TinyXMLConfig.Config{..} =
  UCM.Config confCardName
             (map (generateVerb xml conf) confUseCases)
             (generateDefaults xml conf)

