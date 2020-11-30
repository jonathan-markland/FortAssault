
Retro Games
===========

Retro remakes for various 1980s computer games.

![Main screen](/ReadmeImages/Image1.jpg)

"You can't use Functional Programming to make games!" -- Anon.

Well yes you can, and no there isn't too much garbage collection 
going on, in fact there's almost none.

This is a Jonathan-kills-many-birds exercise, involving many technologies.

Technology:

    - Windows desktop (F# / .Net Core / SDL2-CS / SDL2)
    - [Untested] Linux desktop (F# / .Net Core / SDL2-CS / SDL2) 
    - In the browser (F# / Fable / Web Canvas / HTML)
	- TODO:  F#-Bolero in-browser solution

In other words, retro games, one language, go many places.

This is a research project also:

	- Practice F#
	- Distill a game library from this
	- Distill desktop and web mini-frameworks from this
	- Use in other similar games

Fort Assault Screenshots
========================

![Game screenshot](/ReadmeImages/Image2.jpg)

![Game screenshot](/ReadmeImages/Image3.jpg)

![Game screenshot](/ReadmeImages/Image4.jpg)

![Game screenshot](/ReadmeImages/Image5.jpg)

Pac Man Screenshots
===================

![Game screenshot](/ReadmeImages/Pacman1.png)

![Game screenshot](/ReadmeImages/Pacman2.png)

Building and running browser versions
=====================================
These are in the <game name>Web folders.

It's all node.js / npm stack, so you need these installed.

build_production.bat
--------------------

Builds the bundle.js in the wwwroot folder.
Everything else needed is already in the wwwroot folder,
although I accept this is probably not standard usage.
The wwwroot folder is self-contained and could become
become part of another larger web site.
	
serve_production.bat
--------------------

Serves the wwwroot folder at http://localhost:8080

serve_development.bat
---------------------

Uses WebPack to serve partially from wwwroot, but the
Fable compiler's output is served from Webpack's RAM.
This supports live recompiling and refreshing of the
browser if the F# code changes.  Also at http://localhost:8080
	
Notes
-----
NB: The image files in wwwroot/Images are the ones that the
Desktop version links to, so that there is only one image set.

On the F# side, the 'web' code can be compiled by both the .Net
compiler and the Fable compiler, but you cannot run the .DLL
file made by the .Net compiler as it expects to interface
with Javascript.

Building desktop version on Linux
---------------------------------
Requires SDL2 libraries from distro provider, eg: Ubuntu:

	sudo apt-get install libsdl2-dev
	sudo apt-get install libsdl2-image-dev
	sudo apt-get install libsdl2-ttf-dev
	sudo apt-get install libsdl2-mixer-dev
	cd FsFortAssaultDesktopLinux
	dotnet run

Build desktop version on Windows
--------------------------------
I just use Visual Studio 2019, Community Edition.
I provide SDL2.dll and other DLLs.
Set '<game name>Desktop' as the startup project.


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
For Fort Assault:

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


