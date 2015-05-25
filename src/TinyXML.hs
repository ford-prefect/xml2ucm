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

module TinyXML
( Control (..)
, Path (..)
, Mixer (..)
, parseTinyXML
) where

import Control.Applicative ((<$>))

import Data.Tree.NTree.TypeDefs (NTree)
import Text.XML.HXT.Core

-- Control name value
data Control = Control { controlName :: String,
                         controlValue :: String } deriving (Eq, Ord, Show)

-- Path name pathNames controls
data Path = Path { pathName :: String,
                   pathPaths :: [ String ],
                   pathControls :: [ Control ] } deriving (Eq, Ord, Show)

-- Path defaults paths
data Mixer = Mixer { mixerDefaults :: [(String, Control)],
                     mixerPaths :: [(String, Path)] } deriving (Eq, Ord, Show)

-- Parse the mixer_paths XML

atTag :: ArrowXml a => String -> a (Data.Tree.NTree.TypeDefs.NTree XNode) XmlTree
atTag tag = deep (isElem >>> hasName tag)

getControl :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Control
getControl = atTag "ctl" >>>
  proc c -> do
    cName <- getAttrValue "name" -< c
    cValue <- getAttrValue "value" -< c
    returnA -< Control cName cValue

getPathRef :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) String
getPathRef = atTag "path" >>>
  proc p -> do
    pName <- getAttrValue "name" -< p
    returnA -< pName

getPath :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Path
getPath = atTag "path" >>>
  proc p -> do
    pName <- getAttrValue "name" -< p
    pPathNames <- listA (getChildren >>> getPathRef) -< p
    pControls <- listA getControl -< p
    returnA -< Path pName pPathNames pControls

getMixer :: ArrowXml a => a (Data.Tree.NTree.TypeDefs.NTree XNode) Mixer
getMixer = atTag "mixer" >>>
  proc m -> do
    mControls <- listA getControl -< m
    mPaths <- listA getPath -< m
    returnA -< Mixer (map (\c -> (controlName c, c)) mControls)
                     (map (\p -> (pathName p, p)) mPaths)

parseTinyXML :: String -> IO Mixer
parseTinyXML xmlString = head <$> runX (readString [withValidate no, withRemoveWS yes] xmlString >>> getMixer)
