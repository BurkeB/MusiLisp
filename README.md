MusiLisp
==========

A program to create 16-Bit .wav music files from tone-lists.


About Musilisp
=======================

This program is part of my participation in the http://lispinsummerprojects.org/ Lisp-Contest.

Due to various reasons (Study, Children) I'm not able to completely finish this project until the end of the Contest.

But my goal is to implement these features:

* Console programm to write tones to a .wav file
* Define functions with which the tones will be synthesized. For example Sine, Triangle, Sawtooth, Square Wave or self defined functions with manually added overtones or combinations of various functions.
* Write melodies with list of notes. The first version will only allow absolute defined notes (a' = 440hz | a = 220hz). Number at the end of a note will define the length.
* Tempo will be a parameter of the Creatorfunction. Time signature will always be 4/4.



How to use Musilisp
=======================

Simple Test with Bourree
------------------------

First load and interpret the files in this order:
1. functions_tone.lisp
2. functions_instruments.lisp
3. musilisp.lisp
4. (bourree.lisp)

Now you can generate Bourrée by Johann Sebastian Bach with this function call:

(musilisp "bourree_sinus.wav" (bourree) :bpm 150 :instrument #'make-mysin-octave)

musilisp is the function which generates a wave file with a given melody, tempo and instrument. 

bourree is the function from bourree.lisp which generates a list of two strings. Bourree is a musical piece with two voices, so it consists of two strings.

Your own song
-------------

If you want to generate your own song you can simply enter a String, for example "c4 e4 g4 c'1" or if you want to use two voices you can enter a list of two strings, for example (list "c4 e4 g4 c'1" "c16 e16 g16 e16 e16 g16 h16 g16 g16 h16 d'16 h16 c1")

Then you simple enter it as the parameter of the musilisp function with some other parameters like tempo and instrument:

(musilisp "simple_arpeggio.wav" (list "c4 e4 g4 c'1" "c16 e16 g16 e16 e16 g16 h16 g16 g16 h16 d'16 h16 c1") :bpm 60 :instrument #'make-mysin-octave)


To-Do
=======================

These functions will be implemented in future versions:
* Support of polyphonic Music (Lists of tone-lists)
* * Writing Chords instead of Melody
* * Generate a Walking-Bass-Line to a Chord-Sequence
* Allow tempo changes inside a melody
* Allow time signature changes (insige a melody)
* Setting of reference-notes to transpose a melody 
* Musilisp as a Webservice. Simple form to enter a Melody, which creates a download of a Wave-File or play the file directly with a musicplayer Plugin.
* Play created music directly with Soundcard
* ....