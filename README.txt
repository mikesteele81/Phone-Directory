The Phone Directory application package
=======================================

This is the Phone Directory application for generating a nicely formatted
2-page .pdf file of names and phone numbers organized in groups.

Phone Directory stores everything in the JSON format.  This format is easily
editable by humans, and is also supported by many programming languages.
Because of this, it would be easy to combine Phone Directory with another
tool.

More information may be found in the user manual, which should have been
installed along with this README file.  You may also find resources on the
application's home page ([1]).

Please send questions and suggestions to:

Michael Steele <mikesteele81@gmail.com>

Installation instructions for the Phone Directory application
=============================================================

Building from source
--------------------

Phone Directory is a haskell application.  The easiest way to build it is to
first install the Haskell Platform from ([2]) and then execute the following
from the command line:

    cabal fetch pdirectory
    cabal configure --user
    cabal build

At this point you can either execute `cabal install`, but if you are using
Windows the best thing would be to generate an installer from the 'nsis'
folder.

Preparing an installer for Windows
----------------------------------

Currently cabal has some limitations in what can be done, so there are a few
manual steps that must be followed to create an installer.

running 'cabal build' will compile the application, but doesn't know how to
insert the icon resource into the executable:

    windres.exe "data/images/icon.rc" "src/icon.o"
    cd src
    ghc --make Main.hs icon.o -optl-mwindows
    strip -s Main.exe
    move Main.exe "../dist/build/pdirectory/pdirectory.exe"

Now that this is done, you can execute the NSIS installer script from the nsis
folder.

Installing from a Windows binary
--------------------------------

This is the preferred method if you are using Windows.  Download the installer
from ([1]) and run it.



[1] Phone Directory home page
    http://www.michaelsteele.us/phonedirectory/

[2] Haskell Platform
    http://hackage.haskell.org/platform/
