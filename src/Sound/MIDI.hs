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

    -- * Parser and Serializer
    midiParser,
    midiSerializer
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
-- beginning will be skipped!
decodeMidi :: BS.ByteString -> Either String [MidiMessage]
decodeMidi = A.parseOnly (P.skipToStatus *> some P.midiMessage)

-- | Decode one event from raw MIDI data in a strict 'ByteString'. Any
-- incomplete data at the beginning will be skipped!
decodeMidi1 :: BS.ByteString -> Either String MidiMessage
decodeMidi1 = A.parseOnly (P.skipToStatus *> P.midiMessage)

midiParser :: A.Parser MidiMessage
midiParser = P.midiMessage

midiSerializer :: MidiMessage -> Builder
midiSerializer = S.midiMessage
