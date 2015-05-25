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

-- CSet name value | Exec command
data Command = CSet { csetName :: String,
                      csetValue :: String } |
               Exec { execCommand :: String } deriving (Eq, Ord, Show)

-- Sequence ctl commands
data Sequence = Sequence { seqCtl :: String,
                           seqCommands :: [ Command ] } deriving (Eq, Ord, Show)

-- ice name enable disable conflicts values
data Device = Device { devName :: String,
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
  concatMap (writeLine i)

writeSection :: Int -> String -> Maybe String -> (Int -> a -> String) -> a -> String
writeSection i section name objToStr obj =
  writeLine i (section ++ writeSectionName name ++ " {") ++
    objToStr (i + indentInc) obj ++
  writeLine i "}" ++
  "\n"
  where
    writeSectionName Nothing = ""
    writeSectionName (Just s) = "." ++ quote s

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
      "cset name='" ++ name ++ "' " ++ value
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

writeDevice :: Int -> Device -> String
writeDevice i dev@Device{..} =
  writeSection i "SectionDevice" (Just devName) writeDevice' dev
  where
    writeDevice' i' _ =
      writeList i' "ConflictingDevice" writeStrings devConflicts ++
      writeSequences i' devEnableSeq devDisableSeq ++
      writeSection i' "Value" Nothing writeValues devValues

writeModifier :: Int -> Modifier -> String
writeModifier i m@Modifier{..} =
  writeSection i "SectionModifier" (Just modName) writeModifier' m
  where
    writeModifier' i' _ =
      writeList i' "SupportedDevice" writeStrings modSupports ++
      writeSequences i' modEnableSeq modDisableSeq ++
      writeSection i' "Value" Nothing writeValues modValues

writeVerb :: Int -> Verb -> String
writeVerb i Verb{..} =
  writeSequences i verbEnableSeq verbDisableSeq ++
  writeSection i "Value" Nothing writeValues verbValues

writeVerbFile :: Verb -> (String, String)
writeVerbFile verb@Verb{..} =
  (verbName,
   writeSection 0 "SectionVerb" Nothing writeVerb verb ++
   concatMap (writeDevice 0) verbDevices ++
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
  writeConfigFile conf : map writeVerbFile confVerbs
