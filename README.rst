midi-simple
===========

A simple and minimal MIDI library for Haskell, written with real-time
applications in mind.  It provides fast decoding and encoding of (normalized)
MIDI messages as output by e.g. JACK.

Currently, the MIDI 1.0 specification is supported. No extensions (e.g. General
MIDI) are implemented at the moment, but may be added in the future.

In particular, timing information, relative or absolute, is out of scope for
this project, as it messages are timed in the way they arrive in a real-time
setting. There are currently also no plans to support MIDI files, although this
library can be used as a base for building support for such a use case.

Usage
~~~~~

Most intended use cases should be covered by the `Sound.MIDI` module. The
modules `Sound.MIDI.Parser` and `Sound.MIDI.Serialize` export the innards of the
parser/serializer respectively for when they are needed.

Performance
~~~~~~~~~~~

In my benchmarks, the library has proven to be sufficient for real-time use.
Parsing of common MIDI events such as Note On/Off or Pitch Bend runs at around
50-60ns per event.
