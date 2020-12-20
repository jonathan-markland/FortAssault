
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

Erases and re-builds the wwwroot folder.

The needed files are copied into wwwroot, and the Fable
compiler runs to generate a hash-stamped JS file
from the F# source code.

Each game has its own wwwroot folder, which is self-
contained, and can be pasted into a larger web site
that links to the index.html file.
	
serve_production.bat
--------------------

Run build_production.bat first!

Serves the wwwroot folder at http://localhost:8080

serve_development.bat
---------------------

Run build_production.bat first, so that the support
files are created within the wwwroot folder.  It is 
worthy of note that the main.<hash>.js file is *not* used,
because WebPack starts the Fable compiler, and the Fable 
compiler's output (main.js) is served directly from 
Webpack's RAM.

This supports live recompiling and refreshing of the
browser if the F# code changes.  

This serves the wwwroot folder at:  http://localhost:8080

Notes
-----
On the F# side, the 'web' code can be compiled by the .Net
compiler, which can be useful for error checking, even though
it can never yield a useful program binary.

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

| Shared Assemblies       | Purpose                                                                                            |
|-------------------------|----------------------------------------------------------------------------------------------------|
| GameLib                 | Game algorithm library, based on F# core library only.                                             |
| GameLibDesktopFramework | Game mini-framework for desktop Windows and Linux, using SDL2.                                     |
| GameLibTests            | Automation Tests for the complex bits of GameLib.                                                  |
| GameLibWebFramework     | Game mini-framework for web browsers using Fable, and Web Canvas.                                  |
| SDL2-CS                 | A (hopefully temporary) copy of the SDL2-CS framework, until the author puts that on NuGet! *hint* |
| XunitExtensionsLib      | Jonathan's attempt at making XUnit tests nicer on F#, in lieu of looking for a proper F# test lib. |

| Game Assemblies         | Purpose                                                                                            |
|-------------------------|----------------------------------------------------------------------------------------------------|
| FortAssaultDesktop      | Fort Assault Linux/Windows Desktop version main program.                                           |
| FortAssaultEngine       | Fort Assault Game engine, host-environment-agnostic.                                               |
| FortAssaultWeb          | Fort Assault web version main program.                                                             |
| PacmanDesktop           | Pac Man Linux/Windows Desktop version main program.                                                |
| PacmanEngine            | Pac Man Game engine, host-environment-agnostic.                                                    |
| PacmanEngineTests       | Pac Man engine tests.                                                                              |
| PacmanWeb               | Pac Man web version main program.                                                                  |

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

Future Major Tasks
------------------

Another game!  Adding more games helps determine requirements for the
shared library and framework.

Add more test framework for the shared game library and framework.

Design for, and include the following:
- Sound support
- Runtime-generated bitmap images.  This would allow for more variety without
  the need for manually-drawn image files.

F#-Bolero (.Net WebAssembly) version, as a technology demonstrator.
