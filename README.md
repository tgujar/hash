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
* [bytestring-trie](https://hackage.haskell.org/package/bytestring-trie)
* [haskeline](https://hackage.haskell.org/package/haskeline)

## Reproduction
* [stack](https://docs.haskellstack.org/en/stable/README/)
## Unit Tesing
* [QuickCheck](https://hackage.haskell.org/package/QuickCheck)

## Collaboration
* Tanmay Gujar
* Richard Gu
* Chi-Cheng Lee
* Sung-Yan Hsieh

## Architecture
We divided the project into four main parts: Parser, Evaluator, History, and the REPL.

* Parser

* Evaluator

* History
- Core Design

I knew early on that I wanted the history to be represented as a trie, so that querying for matches given a prefix would be efficient.
Thus, I leveraged the Data.Trie library and built my query and upsert functions on top of it.

To query the history structure, one provides a prefix and gets a list of matches back, sorted by frequency.

To update the history, one provides a string to be added in with an initial frequency count of 1. If the string already exists, then its count is just incremented by 1 instead.

- Persistence

History is persisted as a file, where each line records one of the user's inputs.
On startup, this file is read into the application and serves as the initial history.
On exit, the final history is written out into this file.

This logic was handled by the Haskeline library.

- User Interface

I was not certain of how I wanted the user to actually be able to request the autocomplete. Tab-based completion tends to be token-based in shells, rather than line-based.
However, in the interest of time, I decided to leverage Haskeline's tab completion functionality and complete only whole lines at once instead.

* REPL

Haskeline, out of the box, provides a minimal REPL without the E (RPL?). Thus, all we had to do was take the input string and feed it into the evaluator and proceed from there.

The main design decision here was what state to pass between iterations and how. The History Trie obviously had to be passed between iterations explicitly; the default history functionality of Haskeline is just a log in list form. Then, state for the evaluator had to also be passed around. Together, these formed a neat little tuple of state data.

One of the things that Haskeline does by default is the automatic adding of user input to its internal history log. I had to turn this off and add to the history explicitly because we didn't want to save invalid inputs.

## Challenges
* See Richard's report below for his thoughts

## (Late) Milestone Report (Richard)

* Week of 11/19
    - I spent the first week or so not doing much of anything.
* Week of 11/26
    - I decided to use a Trie as the backing data structure for tracking history.
    - I found the Data.Trie library which suited my needs. From there, it was a matter of adding upsert and query functions.
* Week of 12/3
    - Mostly studied for finals. Thought about how to actually set up the interpreter loop and call my history functions.
* Week of 12/10
    - Found the Haskeline library which handles most of the boilerplate of getting a REPL going.
    - Plugged my history functions into Haskeline's system for tab-triggered auto-complete.
    - Added some property tests for the history trie.
    - Integrated history and parse-eval pieces together into the REPL.
    - Wrote tests for parse-eval.

In summary, the scope of my work included the history-based autocomplete as well as the REPL interface itself.
Throughout the past month, I did not properly account for the sudden spike in workload toward the end of the quarter.
However, because Haskeline is an amazing library, I was able to get a basic REPL working in short order, thus giving us some semblance of a shell.
From there, I was able to leverage monad transformers to integrate my history trie and other state pieces from the evaluator portion of the project.

However, due to time constraints, we had to cut many fluff features from our shell. However, I still was able to deliver a functioning autocomplete and REPL.

The main challenges, other than procrastination, that I faced in this process were three-fold:
* Learning syntax, especially for monad transformers, was challenging. This was just something that got easier with time.
* Integrating the evaluator with the REPL was difficult since the evaluator interface was not set in stone until very late. I wouldn't say this was "solved," per se; rather, it solved itself once the design crystallized.
* Devising a way to test IO output was very confusing. In the end, I settled on having a print out of "all passed" or something along those lines, so that at least we didn't have to click "run in GHCI" a bunch of times.