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

module UCM
( Command (..)
, Sequence (..)
, Device (..)
, Modifier (..)
, Verb (..)
, Config (..)
, generateFiles
) where

import Data.List (find)
import Data.Maybe (maybeToList)

-- CSet name value | Exec command
data Command = CSet { csetName :: String,
                      csetValue :: String } |
               Exec { execCommand :: String } deriving (Eq, Ord, Show)

-- Sequence ctl commands
data Sequence = Sequence { seqCtl :: String,
                           seqCommands :: [ Command ] } deriving (Eq, Ord, Show)

-- ice name enable disable conflicts values
data Device = Device { devName :: String,
                       devPlaybackChannels :: String,
                       devCaptureChannels :: String,
                       devPlaybackVolume :: String,
                       devCaptureVolume :: String,
                       devEnableSeq :: Sequence,
                       devDisableSeq :: Sequence,
                       devConflicts :: [String],
                       devValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- Modifier name playDev captureDev enable disable supports values
data Modifier = Modifier { modName :: String,
                           modPlayDevice :: String,
                           modCaptureDevice :: String,
                           modEnableSeq :: Sequence,
                           modDisableSeq :: Sequence,
                           modSupports :: [String],
                           modValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- Verb name playDev captureDev enable disable devices modifiers values
data Verb = Verb { verbName :: String,
                   verbPlayDevice :: String,
                   verbCaptureDevice :: String,
                   verbEnableSeq :: Sequence,
                   verbDisableSeq :: Sequence,
                   verbDevices :: [Device],
                   verbModifiers :: [Modifier],
                   verbValues :: [(String, String)] } deriving (Eq, Ord, Show)

-- Config cardName verbs
data Config = Config { confCard :: String,
                       confVerbs :: [Verb],
                       confDefaults :: Sequence } deriving (Eq, Show, Ord)

-- Write out UCM file

indentInc :: Int
indentInc = 8

quote :: String -> String
quote s =
  "\"" ++ s ++ "\""

writeLine :: Int -> String -> String
writeLine indent line = replicate indent ' ' ++ line ++ "\n"

writeStrings :: Int -> [String] -> String
writeStrings i =
  concatMap (writeLine i . quote)

writeSection :: Int -> String -> Maybe String -> (Int -> a -> String) -> a -> String
writeSection i section name objToStr obj =
  writeLine i (section ++ writeSectionName name ++ " {") ++
    objToStr (i + indentInc) obj ++
  writeLine i "}" ++
  "\n"
  where
    writeSectionName Nothing = ""
    writeSectionName (Just s) = "." ++ quote s

maybeWriteSection :: Int -> String -> Maybe String -> (Int -> [a] -> String) -> [a] -> String
maybeWriteSection _ _ _ _ [] = ""
maybeWriteSection i section name objToStr obj = writeSection i section name objToStr obj

writeList :: Int -> String -> (Int -> a -> String) -> a -> String
writeList i name objToStr obj =
  writeLine i (name ++ " [") ++
    objToStr (i + indentInc) obj ++
  writeLine i "]" ++
  "\n"

writeCommand :: Int -> Command -> String
writeCommand i control =
  writeLine i (ucmCommandToStr control)
  where
    ucmCommandToStr (CSet name value) =
      "cset " ++ quote ("name='" ++ name ++ "' " ++ value)
    ucmCommandToStr (Exec cmd) =
      "exec " ++ quote cmd

writeSequence :: Int -> Sequence -> String
writeSequence i Sequence{..} =
  writeLine i ("cdev " ++ quote seqCtl) ++
  concatMap (writeCommand i) seqCommands

writeValue :: Int -> (String, String) -> String
writeValue i (n, v) =
  writeLine i (n ++ " " ++ quote v)

writeValues :: Int -> [(String, String)] -> String
writeValues i =
  concatMap (writeValue i)

writeSequences :: Int -> Sequence -> Sequence -> String
writeSequences i en dis =
  writeList i "EnableSequence" writeSequence en ++
  writeList i "DisableSequence" writeSequence dis

filterEmpty :: [(String, String)] -> [(String, String)]
filterEmpty = filter isEmpty
  where
    isEmpty (_, "") = False
    isEmpty _       = True

-- Gets the disable sequence and adds a volume reset if required
getDisableSeq :: Config -> Device -> Sequence
getDisableSeq (Config _ _ (Sequence _ defaults)) (Device _ _ _ pVol cVol _ (Sequence ctl cmds) _ _) =
  Sequence ctl (cmds ++ maybeDisableVolume pVol ++ maybeDisableVolume cVol)
  where
    maybeDisableVolume v = maybeToList $ find (findVolume v) defaults
    findVolume v (CSet name _) = (v ++ " Volume") == name
    findVolume _ _             = False

writeDevice :: Int -> Config -> Device -> String
writeDevice i conf dev@Device{..} =
  writeSection i "SectionDevice" (Just devName) writeDevice' dev
  where
    writeDevice' i' _ =
      writeList i' "ConflictingDevice" writeStrings devConflicts ++
      writeSequences i' devEnableSeq (getDisableSeq conf dev) ++
      maybeWriteSection i' "Value" Nothing writeValues (devValues ++ genDevValues)
    genDevValues =
      filterEmpty [("PlaybackChannels", devPlaybackChannels),
                   ("CaptureChannels", devCaptureChannels),
                   ("PlaybackVolume", devPlaybackVolume),
                   ("CaptureVolume", devCaptureVolume)]

writeModifier :: Int -> Modifier -> String
writeModifier i m@Modifier{..} =
  writeSection i "SectionModifier" (Just modName) writeModifier' m
  where
    writeModifier' i' _ =
      writeList i' "SupportedDevice" writeStrings modSupports ++
      writeSequences i' modEnableSeq modDisableSeq ++
      maybeWriteSection i' "Value" Nothing writeValues modValues

writeVerb :: Int -> Verb -> String
writeVerb i Verb{..} =
  writeSequences i verbEnableSeq verbDisableSeq ++
  maybeWriteSection i "Value" Nothing writeValues (verbValues ++ genVerbValues)
  where
    genVerbValues =
      filterEmpty [("PlaybackPCM", verbPlayDevice),
                   ("CapturePCM", verbCaptureDevice)]

writeVerbFile :: Config -> Verb -> (String, String)
writeVerbFile conf verb@Verb{..} =
  (verbName,
   writeSection 0 "SectionVerb" Nothing writeVerb verb ++
   concatMap (writeDevice 0 conf) verbDevices ++
   concatMap (writeModifier 0) verbModifiers)

writeConfigVerb :: Int -> Verb -> String
writeConfigVerb i Verb{..} =
  writeLine i ("File " ++ quote verbName)

writeConfigUseCase :: Int -> Verb -> String
writeConfigUseCase i verb@Verb{..} =
  writeSection i "SectionUseCase" (Just verbName) writeConfigVerb verb

writeConfigFile :: Config -> (String, String)
writeConfigFile Config{..} =
  (confCard ++ ".conf",
   concatMap (writeConfigUseCase 0) confVerbs ++
   writeList 0 "SectionDefaults" writeSequence confDefaults)

generateFiles :: Config -> [(String, String)]
generateFiles conf@Config{..} =
  writeConfigFile conf : map (writeVerbFile conf) confVerbs
