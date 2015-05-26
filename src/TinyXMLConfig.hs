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

{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

module TinyXMLConfig
( Device (..)
, Modifier (..)
, UseCase (..)
, Config (..)
, parse
) where

import Control.Applicative ((<$>))

import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.Core

-- Device name conflictingDeviceNames tinyXMLPathNames values
data Device = Device { devName :: String,
                       devPlaybackChannels :: String,
                       devCaptureChannels :: String,
                       devPlaybackVolume :: String,
                       devCaptureVolume :: String,
                       devConflicts ::  [String],
                       devPaths :: [String],
                       devValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- Modifier name playDevice captureDevice supportedDeviceNames tinyXMLPathNames values
data Modifier = Modifier { modName :: String,
                           modPlayDev :: String,
                           modCaptureDev :: String,
                           modSupports :: [String],
                           modPaths :: [String],
                           modValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- UseCase name ctlDevice playDevice captureDevice paths devices use-cases values
data UseCase = UseCase { ucName :: String,
                         ucPlayDev :: String,
                         ucCaptureDev :: String,
                         ucPaths :: [String],
                         ucDevices :: [Device],
                         ucModifiers :: [Modifier],
                         ucValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- Config cardName useCases
data Config = Config { confCardName :: String,
                       confCtlDev :: String,
                       confUseCases :: [UseCase] } deriving (Eq, Ord, Show)

atTag :: ArrowXml a => String -> a (Data.Tree.NTree.TypeDefs.NTree XNode) XmlTree
atTag tag = getChildren >>> isElem >>> hasName tag

-- Parse the translation config XML
getConfigValue :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) (String, String)
getConfigValue = atTag "value" >>>
  proc v -> do
    vName <- getAttrValue "name" -< v
    vValue <- getAttrValue "value" -< v
    returnA -< (vName, vValue)

getConfigName :: ArrowXml a => String -> a (Data.Tree.NTree.TypeDefs.NTree XNode)String
getConfigName tag = atTag tag >>>
  proc t -> getAttrValue "name" -< t

getConfigDevice :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Device
getConfigDevice = atTag "device" >>>
  proc d -> do
    dName <- getAttrValue "name" -< d
    dPlayChannels <- getAttrValue "playback-channels" -< d
    dCapChannels <- getAttrValue "capture-channels" -< d
    dPlayVolume <- getAttrValue "playback-volume" -< d
    dCapVolume <- getAttrValue "capture-volume" -< d
    dPathNames <- listA (getConfigName "path") -< d
    dConflicts <- listA (getConfigName "conflict") -< d
    dValues <- listA getConfigValue -< d
    returnA -< Device dName dPlayChannels dCapChannels dPlayVolume dCapVolume dConflicts dPathNames dValues

getConfigModifier :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Modifier
getConfigModifier = atTag "modifier" >>>
  proc m -> do
    mName <- getAttrValue "name" -< m
    mPlay <- getAttrValue "playback-device" -< m
    mCapture <- getAttrValue "capture-device" -< m
    mPathNames <- listA (getConfigName "path") -< m
    mSupports <- listA (getConfigName "supports") -< m
    mValues <- listA getConfigValue -< m
    returnA -< Modifier mName mPlay mCapture mSupports mPathNames mValues

getConfigUseCase :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) UseCase
getConfigUseCase = atTag "use-case" >>>
  proc u -> do
    uName <- getAttrValue "name" -< u
    uPlayDev <- getAttrValue "playback-device" -< u
    uRecDev <- getAttrValue "capture-device" -< u
    uPaths <- listA (getConfigName "path") -< u
    uDevices <- listA getConfigDevice -< u
    uMods <- listA getConfigModifier -< u
    uValues <- listA getConfigValue -< u
    returnA -< UseCase uName uPlayDev uRecDev uPaths uDevices uMods uValues

getConfig :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Config
getConfig = atTag "config" >>>
  proc c -> do
    cCard <- getAttrValue "card-name" -< c
    cCtl <- getAttrValue "ctl-device" -< c
    cUseCases <- listA getConfigUseCase -< c
    returnA -< Config cCard cCtl cUseCases

parse :: String -> IO Config
parse xmlString = head <$> runX (readString [withValidate no, withRemoveWS yes] xmlString >>> getConfig)