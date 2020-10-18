
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
===============
This is in the FortAssaultWeb folder.

It's all node.js / npm stack, so you need these.

To start the server:

	cd root-of-repository-folder
	npm start

Then navigate to http://localhost:8080

The file 'webpack.config.js' contains a link to the F#
project that contains the entry point for the web version.

Build the production website
----------------------------
To obtain the production web site file set, run this command
from the solution root folder:

	npx webpack --mode production

The production web site ends up in the "wwwroot" folder as
specified by the webpack.config.js file.

Notes
-----
NB: The image files in wwwroot/Images are the ones that the
Desktop version links to, so that there is only one image set.

On the F# side, the 'web' code can be built by both the .Net
compiler and the Fable compiler, but you cannot run the .DLL
file made by the .Net compiler as it expects to interface
with Javascript.

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

Set 'FsFortAssaultDesktop' as the startup project.


Architecture
------------

| Assembly                | Purpose                                                                                            |
|-------------------------|----------------------------------------------------------------------------------------------------|
| FsAnyGame               | Game algorithm library, based on F# core library only.                                             |
| FsAnyGameTests          | Automation Tests for the complex bits of FsAnyGame.  Top priority of all tests.                    |
| FsDesktopGameFramework  | Game mini-framework for desktop Windows and Linux, using SDL2.                                     |
| FsWebGameFramework      | Game mini-framework for web browsers using Fable, and Web Canvas.                                  |
| FsFortAssaultDesktop    | Fort Assault Linux/Windows Desktop version main program.                                           |
| FsFortAssaultEngine     | Game engine, host-environment-agnostic.                                                            |
| FsFortAssaultWeb        | Fort Assault web version main program.                                                             |
| FsXUnitExtensions       | Jonathan's attempt at making XUnit tests nicer on F#, in lieu of looking for a proper F# test lib. |
| SDL2-CS                 | A (hopefully temporary) copy of the SDL2-CS framework, until the author puts that on NuGet! *hint* |

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
There is only a set of tests for some features of the shared library that are more
"difficult".  I make no bones that the main game has no tests.  This has been a
research project to get to grips with the technology stack.  Working alone on this
has made tests less urgent.  With the stack tamed, and the library and framework
shaping up nicely, the next game may have more of a test framework.

F# has some superb features to support strength-under-refactoring which is why I
use this.


