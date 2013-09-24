MusiLisp
==========

A program to create .wav music files from tone-lists.


=======================
About Musilisp
=======================

This program is part of my participation in the http://lispinsummerprojects.org/ Lisp-Contest.
Due to various reasons (Study, Children) I'm not able to completely finish this project until the end of the Contest.
But my goal is to implement these features:
-Console programm to write tones to a .wav file
-Define functions with which the tones will be synthesized. For example Sine, Triangle, Sawtooth, Square Wave or self defined functions with manually added overtones or combinations of various functions.
-Write melodies with list of notes. The first version will only allow absolute defined notes (a' = 440hz | a = 220hz). Number at the end of a note will define the length.
-Tempo will be a parameter of the Creatorfunction. Time signature will always be 4/4.



=======================
How to use Musilisp
=======================

.....



=======================
To-Do
=======================

These functions will be implemented in future versions:
-Support of polyphonic Music (Lists of tone-lists)
--Writing Chords instead of Melody
--Generate a Walking-Bass-Line to a Chord-Sequence
-Allow tempo changes inside a melody
-Allow time signature changes (insige a melody)
-Setting of reference-notes to transpose a melody 
-Musilisp as a Webservice. Simple form to enter a Melody, which creates a download of a Wave-File or play the file directly with a musicplayer Plugin.
-Play created music directly with Soundcard
