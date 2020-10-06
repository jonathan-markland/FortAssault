
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
    - [Untested] Linux desktop (F# / .Net Core / SDL2-CS / SDL2) 
    - In the browser (F# / Fable / Web Canvas / HTML)
	- TODO:  F#-Bolero in-browser solution

In other words, one game, one language, its goes many places.

This is a research project also:

	- Practice F#
	- Distill a game library from this
	- Distill desktop and web mini-frameworks from this
	- Use in other similar games

Screenshots
===========

![Game screenshot](/WebImages/Image2.jpg)

![Game screenshot](/WebImages/Image3.jpg)

![Game screenshot](/WebImages/Image4.jpg)

![Game screenshot](/WebImages/Image5.jpg)

Browser version
---------------
This is in the FortAssaultWeb folder. It shares the FsAnyGame and 
FsFortAssault assemblies by relative path inclusion of the projects.

It's all node.js / npm stack, so you need these.

To start the server:

	cd FortAssaultWeb
	npm start

Then navigate to http://localhost:8080

TODO: I must find a better solution to copying the 
Desktop version's image files manually!

To obtain the production web site file set, run this command in 
the solution root folder:

	npx webpack --mode production

Build Desktop Linux Version
---------------------------
Requires SDL2 libraries from distro provider, eg: Ubuntu:

	sudo apt-get install libsdl2-dev
	sudo apt-get install libsdl2-image-dev
	sudo apt-get install libsdl2-ttf-dev
	sudo apt-get install libsdl2-mixer-dev
	cd FsFortAssaultDesktopLinux
	dotnet run

Build Desktop Windows Version
-----------------------------
I just use Visual Studio 2019, Community Edition.

Set 'FsFortAssaultDesktopWindows' as the startup project.

About Functional Programming for Newcomers
==========================================
In the discipline of Pure Functional Programming, the data model is always 
READ ONLY.  F# has 100% support in the language and in its core library for this
kind of programming, although it will allow you to have READ-WRITE data if you
need it.  It is a hybrid "Functional-First" language.

In functional programming you do *not* pass addresses of things, and fiddle around 
with their state by assignments, property-setters, or calling methods!  Instead
you only ever READ them.  Indeed, *nobody* can tamper with values after the
construction phase of a new data item.

Your function progresses by reading its inputs, calculating a new desired result, 
and returning that as its result:

	- Parameters input -> Parameters processed -> New output returned.

If you don't wish to calculate a new state for something (because conceptually 
it hasn't changed), then simply return the original input as-is!

Ideology
--------
In functional programming, the default data model is READ ONLY data because,
ideologically, it is attempting to model the mathematical concept of a *value*.
In mathematics:

	- The number 7 is a *value*
	- The number 7 *just exists*, it is not considered to be *stored* anywhere
	- The number 7 does not have an *address*, so cannot be *re-assigned* to be some other number at any point
	- The number 7 will always be 7, and never any other number.

Inside the computer, functional programming makes compromises because, of course 
things must have addresses!  But in making the data types read-only after initial
construction, subsequent understanding of the program can be simplified.

In a .Net context, F# lets you control whether your types are implemented by 
containment (like C# structs), or whether they are indirectly on the managed
heap (like C# class).  But, I think it's fair to say, that the concept of an
address is diminished in Functional Programming.

Simplify program comprehension
------------------------------
Understanding can be simplified because, once you know a parameter type is read-only,
you know that the parameter can only ever be an INPUT to the function.  

Understanding can also be simplified because sharing data that's read-only for 
everyone, is safe and pretty uncontroversial.

In C++/C#/Java the parameters usually have some mutability about them, perhaps as 
part of their OO interfaces.  This way, parameters could be inputs, but they also 
could be OUTPUTS, or even a hybrid of both!  You can no real way of knowing whether 
that parameter object could have its value altered without tracing where it is 
passed, and who might alter it.

Performance
-----------
Sometimes newcomers are concerned about "creating new things all the time because
you can't change existing things".

There are many approaches to coming to terms with non-mutability:

	- Mutability isn't as commonly needed as you think.
	  What about that mutable C# object instance that is NEVER mutated?
	  That would be ONE read-only F# record instance, with no net change
	  in the amount of data stored.

	- Design for re-use of existing data.
	  Do all of the Space Invader's fields change?  Or just the (x,y)?
	  Split data into several record types, from most-mutable to least-mutable.
	  Forward-link them in that order, and "replace" only the front-end.
	  
	- Don't *store* what you can *calculate*.
	  My "Flickbooks" do this for large parts of the animation of this game.

	- Use the stack for temporary values

A counterpoint is that in imperative programming, since data is almost always
mutable by default, if you really don't want anyone else to modify your data, you
have to make a *copy*.  Copies are expensive.

Use of Functional Programming in this game
------------------------------------------

The main use of mutability is in the main loop where a mutable pointer points to
the current game state record on the heap, and for key state recording.  All other 
mutability is pretty strictly disguised, so the caller wouldn't ever know, or be
accidentally affected by it.

This architectural approach cascades from the overview level (that I call the 
Storyboard) into the currently active screen, and down into the fine detail 
of the data that makes the screen uniquely special.

You also try not to have side-effects by peppering code with OS calls to write things 
to the Standard Out, or to files, or drawing on the display.  Inevitably, some of
these activities are needed, and we can do these things in F# because it's an 
impure Functional Programming language, but I have, by design, captured activities 
like drawing to the screen, and sectioned them off from main processing.


Architecture
------------

| Assembly                | Purpose                                                                                            |
|-------------------------|----------------------------------------------------------------------------------------------------|
| FsAnyGame               | Game algorithm library, based on F# core library only.                                             |
| FsAnyGameTests          | Automation Tests for the complex bits of FsAnyGame.  Top priority of all tests.                    |
| FsDesktopGameFramework  | Game mini-framework for desktop Windows and Linux, using SDL2.                                     |
| FsFortAssaultDesktop    | Fort Assault Linux/Windows Desktop version main program and resources.                             |
| FsFortAssaultEngine     | Game engine, host-environment-agnostic.                                                            |
| FsFortAssaultWeb        | Fort Assault web version, currently integrating the web framework (for now).  Using Web Canvas     |
| FsXUnitExtensions       | Jonathan's attempt at making XUnit tests nicer on F#, in lieu of looking for a proper F# test lib. |
| SDL2-CS                 | A (hopefully temporary) copy of the SDL2-CS framework, until the author puts that on NuGet! *hint* |

The Web framework needs separating out of FsFortAssaultWeb into a new library "FsWebGameFramework".

Developer Screen Shortcuts
--------------------------
In development, you can shortcut the game to a screen by altering the "NewStoryboard" function.
This calls the "Shortcut" function, and you can choose various start-state cases for
development purposes.  You must set this back to 'RunGameNormally' for release.

Manual Test assistance
----------------------
SHORT_PLAYTHROUGH : A special mode for testing, enabled by inserting the following
into FsFortAssaultEngine.fsproj:

```
  <PropertyGroup Condition="'$(Configuration)|$(Platform)'=='Debug|AnyCPU'">
    <DefineConstants>TRACE;SHORT_PLAYTHROUGH</DefineConstants>
  </PropertyGroup>
```

Automation Test notes
---------------------
I only really care about the FsAnyGame library.  It will get more tests, particularly
because it is a re-use focus point.

FortAssault itself is something of a personal research project, and low priority 
for automation tests.  I am happy to present this game for people to play, but it is 
not likely to get more investment from me after it has served its purpose of
practising and refining F# techniques and spinning off a mini library+framework
that targets desktop and web environments.

Program-correctness notes
-------------------------
In spite of no test framework for FsFortAssault, I make *fairly* good use of the brutal 
strong typing of F# demonstrating use of features like Units Of Measure, and wrapping 
basic types in Discriminated-Unions-Of-Just-One-Thing.  

F# has some superb features to support strength-under-refactoring, such as the exhausive 
case checking of "match" when used with Discriminated Unions, and the exhaustive field 
checking of record construction.  Even 'if' is checked:  Both the 'if' and 'else' arms
must return the same type.  This is a good way of avoiding stupid accidents under 
program change.
