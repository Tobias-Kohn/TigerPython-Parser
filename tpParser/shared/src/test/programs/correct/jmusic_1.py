# rowYourBoat.py
# Demonstrates how to build a musical canon.

from music import *

# Create the necessary musical data
rowYourBoatScore = Score("Row Your Boat", 108.0)  # tempo is 108 bpm

flutePart    = Part(FLUTE, 0)        # flute part on channel 0
trumpetPart  = Part(TRUMPET, 1)      # trumpet part on channel 1
clarinetPart = Part(CLARINET, 2)     # clarinet part on channel 2

themePhrase = Phrase(0.0)            # theme starts at the beginning

# "Row, row, row your boat gently down the stream"
pitches1   = [C4, C4, C4,  D4, E4, E4,  D4, E4,  F4, G4]
durations1 = [QN, QN, DEN, SN, QN, DEN, SN, DEN, SN, HN]

# "merrily, merrily, merrily, merrily"
pitches2   = [C5,  C5,  C5,  G4,  G4,  G4,  E4,  E4,  E4,  C4,
              C4,  C4]
durations2 = [ENT, ENT, ENT, ENT, ENT, ENT, ENT, ENT, ENT, ENT,
              ENT, ENT]

# "life is but a dream."
pitches3   = [G4,  F4, E4,  D4, C4]
durations3 = [DEN, SN, DEN, SN, HN]

# add the notes to the theme
themePhrase.addNoteList(pitches1, durations1)
themePhrase.addNoteList(pitches2, durations2)
themePhrase.addNoteList(pitches3, durations3)

# make two new phrases and change start times to make a round
response1Phrase = themePhrase.copy()
response2Phrase = themePhrase.copy()

response1Phrase.setStartTime(4.0)     # start after 4 quarter notes
response2Phrase.setStartTime(8.0)     # start after 8 quarter notes

# play different parts in different registers
Mod.transpose(themePhrase, 12)         # one octave higher
Mod.transpose(response2Phrase, -12)    # one octave lower

# play each phrase twice
Mod.repeat(themePhrase, 2)
Mod.repeat(response1Phrase, 2)
Mod.repeat(response2Phrase, 2)

# add phrases to corresponding parts
flutePart.addPhrase(themePhrase)
trumpetPart.addPhrase(response1Phrase)
clarinetPart.addPhrase(response2Phrase)

# add parts to score
rowYourBoatScore.addPart(flutePart)
rowYourBoatScore.addPart(trumpetPart)
rowYourBoatScore.addPart(clarinetPart)

# play score
Play.midi(rowYourBoatScore)

# write score to MIDI file
Write.midi(rowYourBoatScore, "rowYourBoat.mid")
