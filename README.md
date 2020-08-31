
Fort Assault
============

Retro remake of a fairly well-known 1980s computer game.

![Main screen](/WebImages/Image1.jpg)

"You can't use Functional Programming to make games!" -- Anon.

Well yes you can, and no there isn't too much garbage collection 
going on, in fact there's almost none.

This is a Jonathan-kills-many-birds exercise, involving many technologies.

Technology:

    - Windows desktop (F# / .Net Core / SDL2-CS / SDL2)
    - Linux desktop (F# / .Net Core / SDL2-CS / SDL2)
    - [Forthcoming!] In the browser (F# / Fable / Web Canvas / HTML)  (if not also Bolero)

In other words, one game, one language, its goes three places.

I also have distilled a re-usable game algorithm library out of this.

Screenshots
===========

![Game screenshot](/WebImages/Image2.jpg)

![Game screenshot](/WebImages/Image3.jpg)

![Game screenshot](/WebImages/Image4.jpg)

![Game screenshot](/WebImages/Image5.jpg)


Functional Programming
----------------------
In Functional Programming, the data model is READ ONLY, for which F# has 100% support.  

This program achieves that in the FsAnyGame and FsFortAssault libraries, where the
visible data types are read-only throughout.

In functional programming you do not take parameters and fiddle around with their
values!  Instead, you take parameters (or "values" as they are known), and read
their state.  You then calculate a new desired state, and return that from your
function.

This architectural approach cascades from the overview level (that I call the 
Storyboard) into the currently active screen, and down into the fine detail 
of the data that makes the screen uniquely special.

If you don't wish to calculate a new state for something (because conceptually 
it hasn't changed), then simply return the original input as-is!

You also try not to have side-effects by peppering code with OS calls to write things 
to the Standard Out, or to files, or drawing on the display.  Inevitably, some of
these activities are needed, and we can do these things in F# because it's an 
impure Functional Programming language, but I have, by design, captured activities 
like drawing to the screen, and sectioned them off from main processing.



Architecture
------------

FsAnyGame           | Game algorithm library, based on F# core library only.
FsAnyGameTests      | Automation Tests for FsAnyGame.  Top priority of all the tests.
FsFortAssault       | Host-environment-agnostic library, based on F# core library only.
FsFortAssaultSDL2CS | Game entry point, .Net Core, for Desktop Windows (and hopefully Linux-- but not tested yet!)
FsSDL2              | F#-to-SDL2 smoothing library.  Done via the (third party) SDL2-CS interop library.
FsXUnitExtensions   | Jonathan's attempt at making XUnit tests nicer on F#, in lieu of looking for a proper F# test lib.

Note - The Fable in-browser version is under development on a separate Spike repo, because 
this is my first time using Fable, and I need to investigate a ton of stuff.  Well, a small
wheelbarrow of stuff really.  Fable will be coming to this repo in due course.


Pure Functional Programming Notes
---------------------------------
FsAnyGame           | 100% PFP
FsFortAssault       | 100% PFP
FsFortAssaultSDL2CS | The main loop is a loop, so not PFP


Source code guide
=================

Main and Message Loop
---------------------
FsFortAssaultSDL2CS/Program.fs has the "main" function.
It loads all the resources, and drops into "MainLoopProcessing", which 100% old-school message loop.

The message loop stores the address of the current game state 'let mutable screenState ='.
The game state is immutable, but the loop's pointer to the game state is mutable.

SDL2 event handling is done in a way that tries to make it as easy as possible for
pure-functional code to deal with the keyboard input without having to maintain
extra state flags for itself.  See type 'InputEventData'.

Game engine
-----------
FsFortAssault assembly is the game engine, which is NEITHER desktop nor web aware.

The main loop calls the engine (in FsFortAssault) every 50th of a second to draw the 
display 'RenderStoryboard'.  The main loop also calls NextStoryboardState every
50th of a sceond to process the accumulated keyboard events that have happened during 
the previous 1/50th of a second, and calculate the new game state.

Event handling
--------------
To be clear- there are only TWO events that the game engine ever sees:

    - Draw the screen please
    - Frame advance by 1/50th of a second please

The Player's input is supplied into the Frame advance handler, and NOT supplied
as separate events (it doesn't fit well with Functional Programming).

The Storyboard and Screens
--------------------------
FsFortAssault/Storyboard.fs is the central orchestrator of how the game progresses between the
title screen, and the many varied scenario screens that the game has.  The Storyboard is
a Discriminated Union (DU) type, with one case for each "screen".  Each case contains the Data
Model Type for the screen.  The Storyboard tries to know as little as possible about the
screen's Data Models, but it has to know some minor stuff.  Very certainly, the screens
are designed to know NOTHING about the data models of any of the other screens, and
nothing of the Storyboard DU.  The screens also try to know as little as possible about the
order in which the screens progress.  That is for the Storyboard to know where possible.

Developer Shortcuts
-------------------
In development, you can shortcut the game to a screen by altering the "NewStoryboard" function.
This calls the "Shortcut" function, and you can choose various start-state cases for
development purposes.  You must set this back to 'RunGameNormally' for release.

Functions a Screen module must provide
--------------------------------------
The screens have a constructor called "New...screen name...", and a frame-advance function
called every 1/50th of a second, called "Next...screen name...State".  They also have
a rendering function called "Render...screen name..." which must take the screen's data
model, and draw the screen by calling using the 'render' function supplied to it by the
hosting framework.

The Display
-----------
The game uses a 320 x 200 virtual coordinate system for the display, which the host is
permitted to map to larger integral size multiples for the actual sprites, as it sees
fit.  In this implementation I only use a 1:1 correspondance between the supplied PNG images
in the Images folder, and the virtual coordinate system.





Future Framework Notes
----------------------
I would like to split FsFortAssaultSDL2CS into a framework assembly that will be the back-end
of any F# game on Windows/Linux Desktop where SDL2 is assumed.  All of the game specific
stuff would be removed.

I need to write another game in order to distill the requirements for this.
The framework is small, and this might not be meaningfully achieveable, perhaps
to create new games, copy-paste-and-change will do.  In any case, the FsAnyGame 
library is more important that this aim.



Test notes
----------
No test framework for FsFortAssault.  Instead, just the brutal strong typing of F# demonstrating
use of features like Units Of Measure, and wrapping basic types in Discriminated-Unions-Of-Just-One-Thing.
Ultimately I don't care that much about this particular game, it's single-author, and will 
serve its purpose to gestate the FsAnyGame library.



