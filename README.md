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
    - Core Design
         
        The basic implementation of the design of parser is based on the parse.hs in hw2. Since the parser in the homework has limited supporting types, we define the types and functions such as if..else, while loops, comment block supports, set builtin, commands for "cd" and "echo" and prefix and infix operators.

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
* See everyone's report below for their thoughts


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


## (Late) Milestone Report (Chi-Cheng)

* Week of 11/19
    - I tried to figure out what we would like to have in our program and checked different libraries.
* Week of 11/26
    - I decided to make our program able to run binary executable files
    - I searched for specific libraries that can help us reach the goal and then found a library called Turle.
* Week of 12/3
    - I checked the functions supported by Turtle and learned its syntax.
* Week of 12/10
    - I discussed with Tanmay and David to see if Turtle can be embedded in to the parser and the evaluator.
    - I read the code about the parser and the evaluator and came up with a method to utilize the Turtle library.
    - I wrote some psuedo codes and comments that Tanmay and David can follow and modify their parts.
    - I prepared the codes to utilize the Turtle library after the parser and evaluator is done.
    - Tanmay and David found another library that is more compatible with his implementation, so the Turtle library is eventually not used.

In conclusion, my work is to study how haskell can run binary executable files. Therefore, I checked different libraries and decided to use the Turtle library.
However, I need the help with the parser and the evaluator to run the Turtle library, but it seemed that it couldn't be easily embedded into the codes.
Thus, I decided to design the implementation and write the psuedo codes for my teammates, so they would know what to do.

Eventually, even though I figured out how the library could be utilized, my teammates found another way that required less work and decided not to use Turtle.
I think the main challange for me was to find out the way that helped my teammates work less, and I didn't do a great job. Maybe I should have communicated and discussed more about the details with my teammates, so we would have saved more time and also avoid repetitive work.


## (Late) Milestone Report (Sung-Yan)

* Week of 11/19
    - Checking out the libraries, and realized that we would not use the libraries we choosed previously.
* Week of 11/26
    - Understanding fish shell syntax and its supporting types.
    - Working on the hw3, which serves as the base of evaluator in our project.
* Week of 12/3
    - Looking into parsec library.
    - Constructed prototype for parser and evaluator(adding prefix and infix operator supporting, if, while, echo and string parsing functions). 
* Week of 12/10
    - Working on the implementation for the Turtle library usage.(We didn't use it at last.)
    - Fixing on operator supporting in evaluator.
    - Add the statement parsers that matches the syntax in FISH by using parsec library.
    - Add unit tests on parser.

I collaborated with Tanmay for working on the evaluator and parser. We introduced a lot of usage on Parsec library, which allows us to realize the functionality similar to FISH shell. The concept of our working type is that I create the first version of the implementation of both files, then Tanmay glued up and devised them and add more features, and I keep working on top of them and so on. 

One of my challenges is handling the errors for parser, I was unable to apply the [Parsec.Error](https://hackage.haskell.org/package/parsec-3.1.15.0/docs/Text-Parsec-Error.html) library into our project due to limited time. We somehow removed other feature such as function parsing also but the basic expectation of shell was completed. The greatest challenges for me is the time allocation for the project, I should take more time working on the project before the final week. Since Richard's feature is based on the completion of evaluator and parser, the timeline for supporting basic predefined commands is far from our expectation, this lead to the soaring workload for everyone in the last week of project. Besides, I failed on integrating Chi-Cheng's Turtle library idea, the better integration would also be solved by the timing of working on the project.