# Hash 
> A shell written in Haskell.

## Features
> Describes proposed application and its goals in 300 words.

Our objective is to implement a shell program in Haskell that supports history-based autocomplete, a la the [fish shell](https://fishshell.com/).
Key features:
* Provide an interactive CLI
* Allow the user to run both predefined commands and arbitrary programs written in the fish language.
* Provide the user with history-based autocomplete
* Give the user suggestions after an invalid command
* Customize the colors, prompt and environment vars for the shell using config files

We define the following milestones for the project:
* Week of 11/19
    - Narrow down the libraries we might use/refer  (starting with the shortlist below).
        * Richard will look at procex.
        * Chi-Cheng will look at shellmet. 
        * Sung-Yan will look at coquina.
        * Tanmay will look at brick
    - Implement the interactive CLI and support for some/all predefined commands. At a minimum, the user should be able to echo back their input.
    - Add unit tests for the features.
* Week of 11/26
    - Support all predefined commands.
    - Support the ability to run arbitrary programs.
    - Implement command suggestions after an error.
    - Add unit tests for the features.
* Week of 12/3
    - Implement history-based autocomplete.
    - Add unit tests for the features.
* Week of 12/10
    - Buffer in case of procrastination/other delays.
    - More testing.
    - Presentation prep.

## Library Usage/ Library References
* [brick](https://github.com/jtdaugherty/brick/)
* [shellmet](https://hackage.haskell.org/package/shellmet)
* [coquina](https://hackage.haskell.org/package/coquina)
* [procex](https://hackage.haskell.org/package/procex)

## Reproduction
* [cabal](https://www.haskell.org/cabal/) or [stack](https://docs.haskellstack.org/en/stable/README/)
## Unit Tesing
* [QuickCheck](https://hackage.haskell.org/package/QuickCheck)

## Collaboration
* Tanmay Gujar
* Richard Gu
* Chi-Cheng Lee
* Sung-Yan Hsieh

## Architecture

## Challenges
