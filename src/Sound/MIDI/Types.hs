{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
module Sound.MIDI.Types
(
    -- * MIDI messages
    MidiMessage (..),

    -- * Basic MIDI types
    ChannelVoice (..),
    ChannelMode (..),
    SystemCommon (..),
    SystemRealTime (..),
    SystemExclusive (..),

    VendorId (..),

    -- * Numeric MIDI data
    --
    -- | Only use the direct constructors when you can assure that the values
    -- fit into 7 bits! In general you should prefer the smart constructors. For
    -- values outside of the 7 bit range, the numbers should generally wrap
    -- around, but no guarantees are made!
    Channel (..),
    mkChannel,
    Pitch (..),
    mkPitch,
    middleC,
    Patch (..),
    mkPatch,
    Velocity (..),
    mkVelocity,
    Touch (..),
    mkTouch,
    Controller (..),
    mkController,
    PositionPointer (..),
    mkPositionPointer,
    toClocks,

    -- * Helper functions
    to4Bit,
    to7Bit,
    to14Bit
)
where

import Data.Word
import Data.ByteString (ByteString)
import Data.Bits
import GHC.Generics

-- | A data type representing midi messages. Messages can be categorized into 5
-- subcategories
--
--     1. __Channel Voice Messages__. Start, stop, or alter sounds being played.
--     2. __Channel Mode Messages__. Control messages affecting the entire
--     channel.
--     3. __System Real-Time Messages__. Used by sequencers to regulate and
--     synchronize timing.
--     4. __System Common Messages__. Used for song selection, position
--     pointers, etc.
--     5. __System Exclusive Messages__. Used for device-specific extensions to
--     the MIDI protocol.
--
data MidiMessage
    = ChannelVoice ChannelVoice
    | ChannelMode ChannelMode
    | SystemCommon SystemCommon
    | SystemRealTime SystemRealTime
    | SystemExclusive SystemExclusive
    deriving (Eq, Show, Ord, Read, Generic)

-- | Type holding channel voice messages. Channel Voice messages transmit
-- real-time performance data over a single channel. Examples include "note-on"
-- messages which contain a MIDI note number that specifies the note's pitch, a
-- velocity value that indicates how forcefully the note was played, and the
-- channel number; "note-off" messages that end a note; program change messages
-- that change a device's patch; and control changes that allow adjustment of an
-- instrument's parameters.
data ChannelVoice
    = NoteOff !Channel !Pitch !Velocity
    | NoteOn !Channel !Pitch !Velocity
    | Aftertouch !Channel !Pitch !Touch
    | ControlChange !Channel !Controller !Word8
    | PatchChange !Channel !Patch
    | ChannelPressure !Channel !Touch
    | PitchBend !Channel !Word16
    deriving (Eq, Show, Ord, Read, Generic)

-- | A type for channel mode messages. Mode messages determine how an instrument
-- will receive all subsequent voice messages. This includes whether the
-- receiver will play notes monophonically or polyphonically and whether it will
-- respond only to data sent on one specific voice channel or all of them.
data ChannelMode
    = AllSoundOff !Channel
    | ResetAllControllers !Channel
    | LocalControl !Channel !Bool
    | AllNotesOff !Channel
    | OmniOff !Channel
    | OmniOn !Channel
    | MonoOn !Channel !Word8
    | PolyOn !Channel
    deriving (Eq, Show, Ord, Read, Generic)

-- | A type for system common messages. System common messages are intended for
-- all receivers in the system.
data SystemCommon
    = MTCQuarter !Word8
    | SongPosition !PositionPointer
    | SongSelect !Word8
    | TuneRequest
    | EOX
    deriving (Eq, Show, Ord, Read, Generic)

-- | System real time messages. The MIDI System Real Time messages are used to
-- synchronize all of the MIDI clock-based equipment within a system, such as
-- sequencers and drum machines. Most of the System Real Time messages are
-- normally ignored by keyboard instruments and synthesizers. To help ensure
-- accurate timing, System Real Time messages are given priority over other
-- messages, and these single-byte messages may occur anywhere in the data
-- stream (a Real Time message may appear between the status byte and data byte
-- of some other MIDI message).
data SystemRealTime
    = TimingClock
    | Start
    | Continue
    | Stop
    | ActiveSensing
    | SystemReset
    deriving (Eq, Show, Ord, Read, Generic)

-- | System exclusive messages. System Exclusive messages may be used to send
-- data such as patch parameters or sample data between MIDI devices.
-- Manufacturers of MIDI equipment may define their own formats for System
-- Exclusive data. Manufacturers are granted unique identification (ID) numbers
-- by the MMA or the JMSC, and the manufacturer ID number is included as part of
-- the System Exclusive message. See 'VendorId'.
--
-- The representation used here is deliberately generic. Special sets of system
-- exclusive messages can be implemented on top of this type.
data SystemExclusive
    = Exclusive !VendorId ByteString
    deriving (Eq, Show, Ord, Read, Generic)

-- | Data type encapsulating vendor ID numbers as used in 'SystemExclusive'.
-- They have one of two possible formats:
--
-- 1. A one byte ID (represented by 'VendorIdShort')
-- 2. A three byte ID, which must begin with @0x00@. ('VendorIdLong')
data VendorId
    = VendorIdShort !Word8
    | VendorIdLong  !Word8 !Word8
    deriving (Eq, Show, Ord, Read, Generic)

to7Bit :: Integral a => a -> Word8
to7Bit = (.&. 0x7F) . fromIntegral

to4Bit :: Integral a => a -> Word8
to4Bit = (.&. 0x0F) . fromIntegral

newtype Channel = Channel { getChannel :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkChannel :: Integral a => a -> Channel
mkChannel = Channel . to7Bit

newtype Pitch = Pitch { getPitch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkPitch :: Integral a => a -> Pitch 
mkPitch = Pitch . to7Bit

-- | The middle C on a piano as defined by the MIDI specification. This can
-- serve as a reference value for working with pitches.
middleC :: Pitch
middleC = Pitch 60

newtype Velocity = Velocity { getVelocity :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkVelocity :: Integral a => a -> Velocity 
mkVelocity = Velocity . to4Bit

newtype Touch = Touch { getTouch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkTouch :: Integral a => a -> Touch 
mkTouch = Touch . to7Bit

newtype Controller = Controller { getController :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkController :: Integral a => a -> Controller 
mkController = Controller . to7Bit

newtype Patch = Patch { getPatch :: Word8 }
    deriving (Eq, Show, Ord, Read)

mkPatch :: Integral a => a -> Patch 
mkPatch = Patch . to7Bit

newtype PositionPointer = PositionPointer { getPositionPointer :: Word16 }
    deriving (Eq, Show, Ord, Read)

to14Bit :: Integral a => a -> Word16
to14Bit = (.&. 0x3FFF) . fromIntegral

mkPositionPointer :: Integral a => a -> PositionPointer
mkPositionPointer = PositionPointer . to14Bit

-- | Convert a 'PositionPointer', used to indicate song position, to the MIDI
-- clock. Song Position Pointer is always multiplied by 6 times the MIDI clocks
-- (F8H). Thus the smallest Song Position change is 6 MIDI clocks, or 1/16 note.
toClocks :: PositionPointer -> Int
toClocks = (* 6) . fromIntegral . getPositionPointer
{-# INLINEABLE toClocks #-}
