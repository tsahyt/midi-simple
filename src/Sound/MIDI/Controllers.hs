{-# LANGUAGE PatternSynonyms #-}
-- | A module providing pattern synonyms for controller numbers as defined in
-- the MIDI 1.0 specification.
module Sound.MIDI.Controllers
(
    pattern BankSelect, 
    pattern ModWheel, 
    pattern BreathController, 
    pattern FootController, 
    pattern PortamentoTime, 
    pattern DataEntryMSB, 
    pattern ChannelVolume, 
    pattern Balance, 
    pattern Pan, 
    pattern ExpressionController, 
    pattern EffectControl1, 
    pattern EffectControl2, 
    pattern GeneralPurposeController1, 
    pattern GeneralPurposeController2, 
    pattern GeneralPurposeController3, 
    pattern GeneralPurposeController4, 
    pattern LSB0, 
    pattern LSB1, 
    pattern LSB2, 
    pattern LSB3, 
    pattern LSB4, 
    pattern LSB5, 
    pattern LSB6, 
    pattern LSB7, 
    pattern LSB8, 
    pattern LSB9, 
    pattern LSB10, 
    pattern LSB11, 
    pattern LSB12, 
    pattern LSB13, 
    pattern LSB14, 
    pattern LSB15, 
    pattern LSB16, 
    pattern LSB17, 
    pattern LSB18, 
    pattern LSB19, 
    pattern LSB20, 
    pattern LSB21, 
    pattern LSB22, 
    pattern LSB23, 
    pattern LSB24, 
    pattern LSB25, 
    pattern LSB26, 
    pattern LSB27, 
    pattern LSB28, 
    pattern LSB29, 
    pattern LSB30, 
    pattern LSB31, 
    pattern DamperPedal, 
    pattern Portamento, 
    pattern Sostenuto, 
    pattern SoftPedal, 
    pattern LegatoFootswitch, 
    pattern Hold2, 
    pattern SoundController1, 
    pattern SoundVariation, 
    pattern SoundController2, 
    pattern Timbre, 
    pattern SoundController3, 
    pattern ReleaseTime, 
    pattern SoundController4, 
    pattern AttackTime, 
    pattern SoundController5, 
    pattern Brightness, 
    pattern SoundControllers6, 
    pattern SoundControllers7, 
    pattern SoundControllers8, 
    pattern SoundControllers9, 
    pattern SoundControllers10, 
    pattern GeneralPurposeController5, 
    pattern GeneralPurposeController6, 
    pattern GeneralPurposeController7, 
    pattern GeneralPurposeController8, 
    pattern PortamentoControl, 
    pattern Effects1Depth, 
    pattern ExternalEffectsDepth, 
    pattern Effects2Depth, 
    pattern TremoloDepth, 
    pattern Effects3Depth, 
    pattern ChorusDepth, 
    pattern Effects4Depth, 
    pattern CelesteDepth, 
    pattern Effects5Depth, 
    pattern PhaserDepth, 
    pattern DataIncrement, 
    pattern DataDecrement, 
    pattern UnRegisteredParameterNumberLSB, 
    pattern UnRegisteredParameterNumberMSB, 
    pattern RegisteredParameterNumberLSB, 
    pattern RegisteredParameterNumberMSB
)
where

import Sound.MIDI.Types

pattern BankSelect :: Controller
pattern BankSelect = Controller 0

pattern ModWheel :: Controller
pattern ModWheel = Controller 1

pattern BreathController :: Controller
pattern BreathController = Controller 2

pattern FootController :: Controller
pattern FootController = Controller 4

pattern PortamentoTime :: Controller
pattern PortamentoTime = Controller 5

pattern DataEntryMSB :: Controller
pattern DataEntryMSB = Controller 6

pattern ChannelVolume :: Controller
pattern ChannelVolume = Controller 7

pattern Balance :: Controller
pattern Balance = Controller 8

pattern Pan :: Controller
pattern Pan = Controller 10

pattern ExpressionController :: Controller
pattern ExpressionController = Controller 11

pattern EffectControl1 :: Controller
pattern EffectControl1 = Controller 12

pattern EffectControl2 :: Controller
pattern EffectControl2 = Controller 13

pattern GeneralPurposeController1 :: Controller
pattern GeneralPurposeController1 = Controller 16

pattern GeneralPurposeController2 :: Controller
pattern GeneralPurposeController2 = Controller 17

pattern GeneralPurposeController3 :: Controller
pattern GeneralPurposeController3 = Controller 18

pattern GeneralPurposeController4 :: Controller
pattern GeneralPurposeController4 = Controller 19

pattern LSB0 :: Controller
pattern LSB0 = Controller 32

pattern LSB1 :: Controller
pattern LSB1 = Controller 33

pattern LSB2 :: Controller
pattern LSB2 = Controller 34

pattern LSB3 :: Controller
pattern LSB3 = Controller 35

pattern LSB4 :: Controller
pattern LSB4 = Controller 36

pattern LSB5 :: Controller
pattern LSB5 = Controller 37

pattern LSB6 :: Controller
pattern LSB6 = Controller 38

pattern LSB7 :: Controller
pattern LSB7 = Controller 39

pattern LSB8 :: Controller
pattern LSB8 = Controller 40

pattern LSB9 :: Controller
pattern LSB9 = Controller 41

pattern LSB10 :: Controller
pattern LSB10 = Controller 42

pattern LSB11 :: Controller
pattern LSB11 = Controller 43

pattern LSB12 :: Controller
pattern LSB12 = Controller 44

pattern LSB13 :: Controller
pattern LSB13 = Controller 45

pattern LSB14 :: Controller
pattern LSB14 = Controller 46

pattern LSB15 :: Controller
pattern LSB15 = Controller 47

pattern LSB16 :: Controller
pattern LSB16 = Controller 48

pattern LSB17 :: Controller
pattern LSB17 = Controller 49

pattern LSB18 :: Controller
pattern LSB18 = Controller 50

pattern LSB19 :: Controller
pattern LSB19 = Controller 51

pattern LSB20 :: Controller
pattern LSB20 = Controller 52

pattern LSB21 :: Controller
pattern LSB21 = Controller 53

pattern LSB22 :: Controller
pattern LSB22 = Controller 54

pattern LSB23 :: Controller
pattern LSB23 = Controller 55

pattern LSB24 :: Controller
pattern LSB24 = Controller 56

pattern LSB25 :: Controller
pattern LSB25 = Controller 57

pattern LSB26 :: Controller
pattern LSB26 = Controller 58

pattern LSB27 :: Controller
pattern LSB27 = Controller 59

pattern LSB28 :: Controller
pattern LSB28 = Controller 60

pattern LSB29 :: Controller
pattern LSB29 = Controller 61

pattern LSB30 :: Controller
pattern LSB30 = Controller 62

pattern LSB31 :: Controller
pattern LSB31 = Controller 63

pattern DamperPedal :: Controller
pattern DamperPedal = Controller 64

pattern Portamento :: Controller
pattern Portamento = Controller 65

pattern Sostenuto :: Controller
pattern Sostenuto = Controller 66

pattern SoftPedal :: Controller
pattern SoftPedal = Controller 67

pattern LegatoFootswitch :: Controller
pattern LegatoFootswitch = Controller 68

pattern Hold2 :: Controller
pattern Hold2 = Controller 69

pattern SoundController1 :: Controller
pattern SoundController1 = Controller 70

-- | Synonym for 'SoundController1'
pattern SoundVariation :: Controller
pattern SoundVariation = Controller 70

pattern SoundController2 :: Controller
pattern SoundController2 = Controller 71

-- | Synonym for 'SoundController2'
pattern Timbre :: Controller
pattern Timbre = Controller 71

pattern SoundController3 :: Controller
pattern SoundController3 = Controller 72

-- | Synonym for 'SoundController3'
pattern ReleaseTime :: Controller
pattern ReleaseTime = Controller 72

pattern SoundController4 :: Controller
pattern SoundController4 = Controller 73

-- | Synonym for 'SoundController4'
pattern AttackTime :: Controller
pattern AttackTime = Controller 73

pattern SoundController5 :: Controller
pattern SoundController5 = Controller 74

-- | Synonym for 'SoundController5'
pattern Brightness :: Controller
pattern Brightness = Controller 74

pattern SoundControllers6 :: Controller
pattern SoundControllers6 = Controller 75

pattern SoundControllers7 :: Controller
pattern SoundControllers7 = Controller 76

pattern SoundControllers8 :: Controller
pattern SoundControllers8 = Controller 77

pattern SoundControllers9 :: Controller
pattern SoundControllers9 = Controller 78

pattern SoundControllers10 :: Controller
pattern SoundControllers10 = Controller 79

pattern GeneralPurposeController5 :: Controller
pattern GeneralPurposeController5 = Controller 80

pattern GeneralPurposeController6 :: Controller
pattern GeneralPurposeController6 = Controller 81

pattern GeneralPurposeController7 :: Controller
pattern GeneralPurposeController7 = Controller 82

pattern GeneralPurposeController8 :: Controller
pattern GeneralPurposeController8 = Controller 83

pattern PortamentoControl :: Controller
pattern PortamentoControl = Controller 84

pattern Effects1Depth :: Controller
pattern Effects1Depth = Controller 91

-- | Synonym for 'Effects1Depth'
pattern ExternalEffectsDepth :: Controller
pattern ExternalEffectsDepth = Controller 91

pattern Effects2Depth :: Controller
pattern Effects2Depth = Controller 92

-- | Synonym for 'Effects2Depth'
pattern TremoloDepth :: Controller
pattern TremoloDepth = Controller 92

pattern Effects3Depth :: Controller
pattern Effects3Depth = Controller 93

-- | Synonym for 'Effects3Depth'
pattern ChorusDepth :: Controller
pattern ChorusDepth = Controller 93

pattern Effects4Depth :: Controller
pattern Effects4Depth = Controller 94

-- | Synonym for 'Effects4Depth'
pattern CelesteDepth :: Controller
pattern CelesteDepth = Controller 94

pattern Effects5Depth :: Controller
pattern Effects5Depth = Controller 95

-- | Synonym for 'Effects5Depth'
pattern PhaserDepth :: Controller
pattern PhaserDepth = Controller 95

pattern DataIncrement :: Controller
pattern DataIncrement = Controller 96

pattern DataDecrement :: Controller
pattern DataDecrement = Controller 97

pattern UnRegisteredParameterNumberLSB :: Controller
pattern UnRegisteredParameterNumberLSB = Controller 98

pattern UnRegisteredParameterNumberMSB :: Controller
pattern UnRegisteredParameterNumberMSB = Controller 99

pattern RegisteredParameterNumberLSB :: Controller
pattern RegisteredParameterNumberLSB = Controller 100

pattern RegisteredParameterNumberMSB :: Controller
pattern RegisteredParameterNumberMSB = Controller 101
