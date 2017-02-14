module Sound.MIDI
(
    -- * Encoding
    encodeMidi,
    encodeMidi',
    encodeMidi1,
    encodeMidi1',

    -- * Decoding
    decodeMidi,
    decodeMidi1,
    partitionRealtime,

    -- * Parser and Serializer
    --
    -- | Top level parsing and serializing tools are exposed here. For
    -- fine-grained access, see "Sound.MIDI.Parser" and "Sound.MIDI.Serialize".
    midiParser,
    midiSerializer,

    -- * Re-exports
    module Sound.MIDI.Types
)
where

import Control.Applicative
import Data.Functor.Identity
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (toLazyByteString, Builder)
import qualified Data.Attoparsec.ByteString as A

import Sound.MIDI.Types
import qualified Sound.MIDI.Parser as P
import qualified Sound.MIDI.Serialize as S

-- | Encode some collection of 'MidiMessage's to a lazy 'BL.ByteString'
encodeMidi :: Foldable t => t MidiMessage -> BL.ByteString
encodeMidi = toLazyByteString . foldMap S.midiMessage

-- | Strict version of 'encodeMidi'
encodeMidi' :: Foldable t => t MidiMessage -> BS.ByteString
encodeMidi' = BL.toStrict . encodeMidi

-- | Encode a single message to a lazy 'BL.ByteString'
encodeMidi1 :: MidiMessage -> BL.ByteString
encodeMidi1 = encodeMidi . Identity

-- | Strict version of 'encodeMidi1'
encodeMidi1' :: MidiMessage -> BS.ByteString
encodeMidi1' = encodeMidi' . Identity

-- | Decode raw MIDI data from a strict 'ByteString'. Any incomplete data at the
-- beginning will be skipped! This function assumes a normalized MIDI stream,
-- i.e. one in which events are /not/ interrupted by real-time events!
decodeMidi :: BS.ByteString -> Either String [MidiMessage]
decodeMidi = A.parseOnly (P.skipToStatus *> some P.midiMessage)

-- | Decode one event from raw MIDI data in a strict 'ByteString'. Any
-- incomplete data at the beginning will be skipped!
decodeMidi1 :: BS.ByteString -> Either String MidiMessage
decodeMidi1 = A.parseOnly (P.skipToStatus *> P.midiMessage)

-- | Partition an event stream into real-time events and other messages. The
-- first parameter returned will be the real-time events, the second element
-- will be the rest of the stream. Note that this effectively normalizes the
-- second element.
partitionRealtime :: BS.ByteString -> (BS.ByteString, BS.ByteString)
partitionRealtime = BS.partition isRT
    where isRT x = x >= 0xF8 && x <= 0xFF

midiParser :: A.Parser MidiMessage
midiParser = P.midiMessage

midiSerializer :: MidiMessage -> Builder
midiSerializer = S.midiMessage
