# For whoever is willing to learn Haskell!
# Haskell - I'll enhance the repo as the time is going on
Getting started with Haskell

The Haskell Platform
The worst part of learning a new programming language is getting your development environment set up for the first time. Fortunately, and somewhat surprisingly, this isn’t a problem at all with Haskell. The Haskell community has put together a single, easily installable package of useful tools referred to as the Haskell Platform. The Haskell Platform is the “batteries included” model of packaging a programming language.

The Haskell Platform includes the following:

The Glasgow Haskell Compiler (GHC)
1. An interactive interpreter (GHCi)
2. The stack tool for managing Haskell projects
3. A bunch of useful Haskell packages

The Haskell Platform can be downloaded from www.haskell.org/downloads#platform. From there, follow the directions for installing on your OS of choice. This book uses Haskell version 8.0.1 or higher.



 INTERACTING WITH HASKELL—GHCI
One of the most useful tools for writing Haskell programs is GHCi, an interactive interface for GHC. Just like GHC, GHCi is started with a simple command: ghci. When you start GHCi, you’ll be greeted with a new prompt:

$ ghci
GHCi>

This book indicates when you’re using GHCi by using GHCi> for lines you input and a blank for lines that are output by GHCi. The first thing to learn about any program you start from the command line is how to get out of it! For GHCi, you use the :q command to exit:

$ ghci
GHCi> :q
Leaving GHCi.

Working with GHCi is much like working with interpreters in most interpreted programming languages such as Python and Ruby. It can be used as a simple calculator:

GHCi> 1 + 1
2
You can also write code on the fly in GHCi:

GHCi> x = 2 + 2
GHCi> x
4

Prior to version 8 of GHCi, function and variable definitions needed to be prefaced with a let keyword. This is no longer necessary, but many Haskell examples on the web and in older books still include it:

GHCi> let f x = x + x
GHCi> f 2
4

The most important use of GHCi is interacting with programs that you’re writing. There are two ways to load an existing file into GHCi. The first is to pass the filename as an argument to ghci:

$ ghci hello.hs
[1 of 1] Compiling Main
Ok, modules loaded: Main.
The other is to use the :l (or :load) command in the interactive session:

$ ghci
GHCi> :l hello.hs
[1 of 1] Compiling Main
Ok, modules loaded: Main.
In either of these cases, you can then call functions you’ve written:

GHCi> :l hello.hs
GHCi> main
"Hello World!"
