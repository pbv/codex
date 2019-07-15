---
title: Codex User Guide
author: Pedro Vasconcelos <pbv@dcc.fc.up.pt>, University of Porto, Portugal. 
date: July 2019 (version 0.9.5)
...


# Introduction

*Codex* is a web system for setting up programming exercises with
automatic assessment. The main features are:

*Simple exercise authoring*

:   The exercise statement is written human-readable plain-text file;
that can easily be transfered, kept in a version repository,
compared for changes, etc.;

*Assessing program fragments*

:    Exercise can assess only individual functions, classes or methods
rather than complete programs;

*Provides automatic feedback*

:    Rather than just report an *accept/reject* result,
Codex can use automatic testing techniques such as
[properties](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
or [documentation-tests](https://docs.python.org/3.7/library/doctest.html)
to report failure example to students.

*Multiple types of exercises*

:     Codex supports testing code, mutiple-choice
and fill-in questionaries.


Codex is intended for learning environments rather than programming
contests (for the later, check out [Mooshak](https://mooshak.dcc.fc.up.pt/)).  The system is currently being used at the Faculty of
Science of the University of Porto for teaching introductory courses
on Python and C programming.

## Pages and exercises

A Codex repository is  organized as set of interlinked
hypertext pages.
*Pages* can contain text, links, tables, images and mathematical formulas
written in [Markdown](https://en.wikipedia.org/wiki/Markdown).

Markdown is a
[lightweight text markup syntax](https://en.wikipedia.org/wiki/Markdown)
designed by John Gruber.
Codex uses the 
[Pandoc library](http://hackage.haskell.org/package/pandoc) for parsing
Markdown which suports many extensions. In particular, it
support LaTeX mathematics (rendered using
[MathJaX](http://www.mathjax.org))
For more details on the Pandoc-flavoured Markdown
syntax accepted, check the [Pandoc user
manual](http://pandoc.org/MANUAL.html#pandocs-markdown).

Pages can be just plain documents or marked as *exercises*;
the later allow student's submissions and trigger automatic assessment.
Submissions are kept in a persistent database and students
can review previous their submissions attempts (thought not
anyone else's). The administrator can view and manage 
all submissions (e.g. re-evaluate or generate printouts).

Comments in Markdown pages can be written in HTML-style:

~~~
<!-- This is the begining of comment;
     it can go on for multiple lines -->
~~~

Note that, unlike common Markdown readers, raw HTML markup
is **not** allowed (it will be rendered as plain text).

Codex uses Pandoc's YAML extension to specify metadata for pages,
(e.g. set the type of execise, the  programming 
languages allowed, the period for valid submissions, etc).
Metadata blocks are delimited between `---` and 
`...`:

    ---
    tester: doctest
    language: python
    valid: "after 25/05/2019 and before 15/06/2019"
    ...

Such blocks can occur anywhere in a document; several metadata
blocks are equivalent to a single block of all collected fields.
(We suggest placing metadata only at the beginning or the the end of
the document.)
 
Exercises  are marked by the  `tester` field; the
particular tester specifies the type of exercise and how assement
is done (see the [section on Testers](#testers)).

<!--
Exercises are identified by the page's *request path* i.e. the
relative path after `/page`; e.g. an exercise at the URL
`/page/foo/bar.md` is identified as `foo/bar.md`.  This means that the
exercise can be edited (e.g. to correct mistakes); however, if the
file is renamed or moved any existing submissions will no longer be
associated to the the modified exercise.

Users may submit multiple attempts for exercises. 
Previous submissions are kept in a persistent SQLite
database; only an administrator can remove or 
re-evaluate submissions.
-->

## Document structure

All page files, images, etc. should be
maintained inside the `public` directory and 
the starting page after login is the `index.md` page;
this should link exercises or other sub-directories.
For example, for 3 exercises `worksheet1.md`, `worksheet2.md` and
`worksheet3.md`, a minimal `index.md` page could be:

~~~{.boxed}
# Welcome!

This is minimal index page. Here is a list of available exercises:

* [](worksheet1.md){.ex}
* [](worksheet2.md){.ex}
* [](worksheet3.md){.ex}
~~~

Links to exercise pages are marked with class attribute `.ex`; this
fills-in automatically the exercise title (read from the markdown
document) and a summary of previous submissions by the current user
(read from the submission database).

The adminstrator can edit the index page to the order
of exercises, or group exercises using sections and sub-pages.  It is also
possible to add plain Markdown pages for documentation, or links to
external resources.

Note that:

1. Exercise pages can be accessed by their URL even if they
   are not explicitly linked to the index; this can be used
   test exercises before making them visible to students;
2. If you intend to group exercises using sub-directories
   make sure *you include an `index.md` for each sub-directory*.

## An example exercise

Consider the following simple Python programming exercise: *write a function to
compute rounded-down integral square roots*.

First we set up an *exercise page*
`sqroot.md`; this contains the exercise description
shown to the student (and some metadata fields):

~~~{.boxed}
---
tester: doctest
language: python
...

# Compute rounded-down integer square roots

Write a function `root(x)` that computes the square root
of an integer number rounded-down to the nearest integer
value.

If the argument `x` is negative the function should
throw a `ValueError` exception.
~~~

We also need a *doctest script* `sqroot.tst` specifying the
tets cases to try and expected results:

~~~{.boxed}
>>> root(0)
0
>>> root(1)
1
>>> root(2)
1
>>> root(4)
2
>>> root(5)
2
>>> root(10)
3
>>> root(25)
5
>>> root(-1)
Traceback (most recent call last):
  ...
ValueError: math domain error
~~~

These two files are all that we need.
For each student submission the tests in the *doctest* script
will be tried in order;
testing terminates immediately if any of tests fails (with a wrong
answer or a runtime exception) and the failed test case is used to
produce a report, e.g.:

~~~{.boxed}
Failed example:
    root(2)
Expected:
    1
Got:
    1.4142135623730951
~~~


# Reference guide

## Generic metadata 

The following metadata fields apply to all exercises types.

`title`

:      Specifies the title for exercise links; if this is missing,
the title is the first header in the document.

`tester`

:      Specify the type of exercise (described in the following subsection).


`valid`

:   Specify constraints on the time and maximum number for submission attempts; the default is no time constraint and an unlimited number of submssions.  Some examples:
    
    ```
    valid: "after 08:00 15/02/2019"
    valid: "after 08:00 15/02/2019 and before 12:00 15/02/2019"
    valid: "before 16/02/2019"
    valid: "before 16/02/2019 and max_attempts 20"
    ```
 
    Date and times are relative to the server timezone.  <!-- You
    may also use named events defined in a `events.cfg` configuration
    file, e.g.
    
    ```
    valid: "before #week1"
    ```
    
    where the `events.cfg` file contains a line such as
    
    ```
    week1 = 16/02/2019
    ```
	-->


`feedback`

:     A boolean value (`yes`/`true` or `no`/`false`) controlling whether
detailed feedback is shown (e.g. show the test case in case of test failure);
the default is feedback enabled.

`code`

:      Specify an initial code skeleton for programming exercises; use an indented block for multiple lines, e.g.:

    ````
    code: |
      ~~~
      def distance(x1, y1, x2, y2):
          #
          # complete this definition
          #
      ~~~
    ````
   Note the vertical bar (`|`) and indentation in the above example.



<!--
The following fields are specific to programming languages.

### Python-specific fields

`doctest`

:     Specifies the file path for a *doctest* script for testing Python
submissions; if omitted, this defaults to the file name for exercise
page with extension replaced by `.tst`, e.g. the default doctest for
`foo/bar.md` is `foo/bar.tst`.


### Haskell- and C-specific fields

`quickcheck`

:     Specifies the file name for a Haskell file containing
QuickCheck script for Haskell or C submission testing.


`maxSuccess`

:     Number of QuickCheck tests run.

`maxSize`

:     Maximum size for QuickCheck generated test data.

`maxDiscardRatio`

:     Maximum number of discarded tests per successful test before giving up.

`randSeed`

:     Integer seed value for pseudo-random test data generation;
use if you want to ensure reproducibility of QuickCheck tests.

-->

# Assement and Feedback 

## Results

Codex assesses submissions using a *tester*; for
programming exercises this typically requires running a test suite (either
provided by the author or randomly-generated).
The result 
is a *classification labels*, a *validity check* and a (possibly empty)
*detail text report*.  Classification labels are similar to the ones used for
[ICPC programming contests](https://icpc.baylor.edu/worldfinals/rules):

*Accepted*

:   The submission passed all tests.

*WrongAnswer*

:   The submission was rejected because it failed at least one
test.

*CompileError*

:   The submission was rejected because it caused a static error (e.g.
compiler error)

*RuntimeError*

:     The submission was rejected because it caused a runtime error
(e.g. runtime exception, segmentation fault)

*RuntimeLimitExceeded*, *MemoryLimitExceeded*

:     The submission was reject because it tried to use too much computing resources; this usually signals an erroneous program (e.g. non-terminating).

*MiscError*

:     Some other error (e.g. incorrect metadata, test files, etc.)


*Evaluating*

:     Temporary label assigned during evaluation.


A submission is marked *Valid* if the it
respects the constraints on time and maximum attempts or
*Invalid* with a suitable message otherwise.
Note that Codex always evaluates submissions (reporting
feedback if enabled) and checks validity independent from classification:
e.g. a program that passes all tests but is submitted too late will
be classified *Accepted (Invalid: Late submission)*.[^1]

[^1]: This behaviour allows students to retry past exercises and
  leaves a supervisor freedom to decide how to consider
  such submissions. 


<!--
When submissions are rejected because of a wrong answer, the text
report includes a human-readable description of a failed test case; this
is intended for the student to use as a starting point for
understanding the problem and debugging.

Note that  regardless of the time interval specified in the
exercise; however:

* it will hide feedback for early submissions
  until the start of submission interval;
* late submissions are assessed as usual but
  additionally labelled *Overdue*[^1].

-->


## Testers

### `stdio`

Tests complete programs that read input from `stdin` and produce output
on `stdout`; specific fields:

`languages`

:   List all allowed languages for this exercise; e.g.:
    
    ```
    languages: [c, java, python, haskell]
    ```

    If only a single language is allowed, the singular option `language` may be used instead:

    ```
    language: c
    ```

`inputs`

:    List of input files in order; glob patterns are allowed; e.g.
     
     ```
     inputs: [ "exercise1/tests/example-in.txt", "exercise1/tests/input*.txt" ]
     ```

`outputs`

:    List of expected output files in order; glob patterns are allowed; e.g.
     
     ```
     outputs: [ "exercise1/tests/example-out.txt", "exercise1/tests/output*.txt" ]
     ```
     
     The number of output files much be the same as the number of inputs.

`files`

:    List of extra files that will be copied to the temporary working
directory while running the tests.

`arguments`

:    List of files with command-line arguments for each of the test cases
defined by input/output pairs; by default, the command-line
arguments are empty.

### `doctest`

Test Python code using
the [`doctest` library](https://docs.python.org/3/library/doctest.html)
for unit-testing 
simple functions, methods or classes. Extra options:

`language`

:     Should be `python` (must be included).

`tests`

:     Optional filename for doctests; by default this is the same
file as the exercise with `.tst` as extension.

`linter`

:     Boolean value specifying whether to run an external
      compile-time linter such as
      an [optional static type checker](http://mypy-lang.org/));
      the linter command is specified in the global configuration file;
      by default, the linter is not run.

`linter-args`

:     Optional command-line arguments to pass the linter.

### `quickcheck`


Test Haskell or C code
using a [custom version](https://github.com/pbv/codex-quickcheck). of
the [QuickCheck library](http://hackage.haskell.org/package/QuickCheck).
Extra options:

`language`

:     Languages accepted: `haskell` or `c`.

`properties`

:     Haskell file with QuickCheck specification (generators and properties);
defaults to the page filename with extension replaced by `.hs`.

`maxSuccess`

:     Number of tests to run.

`maxSize`

:     Maximum size for generated test data.

`maxDiscardRatio`

:     For conditional generators: the maximum number of discarded tests per successful test before giving up.

`randSeed`

:     Integer seed value for pseudo-random data generation;
use if you want to ensure reproducibility of tests.


### `quiz`

Multiple-choice and fill-in questionaries with optional shuffling of
questions and answers. Extra options:

`shuffle-questions`

:      Boolean value; specifies whether the order of questions should be
shuffled; defaults to `false`. By default shuffling uses a
deterministic algorithm based on the user login.

`shuffle-answers`

:      Boolean value; specifies whether the order of answers within each
question should be shuffled; default to `false`.  By default shuffling
uses a deterministic algorithm based on the user login.

`random-seed`

:    Integer value; fix the PRNG seed used for shuffled instead of computing
it from the user login.


`correct-weight`

:     Fractional number from 0 to 1; this is the scoring weight
for a correct item in a multiple-choice question. 
By default the correct item(s) score 1.

`incorrect-weight`

:     Fractional number from 0 to 1; this is the scoring weight
for an incorrect to a multiple-choice question.
By default the score for each incorrect item is such that
the sum of incorrect items equals minus the sum of corrects ones,
e.g. for a three option question with a single correct option,
an incorrect option scores $-1/2$. To change
change the penalty for wrong opttion to $-1/3$:

     ```
     incorrect-weight: -1/3
     ```
 
<!--

## Specifying test cases

### Python

Test cases for Python submissions are specified
using the [*doctest* library](https://docs.python.org/3.7/library/doctest.html).


Some advices:

1. order the test cases such that simplest input values occur first;
2. be aware that *doctest* employs a straight textual matching of outputs
(e.g. `0` and `0.0` are distinct);
3. make sure you normalize floating-point results
to avoid precision issues, e.g. use `round`(..., *n-decimals*`)`
4. to discourage students from "fixing" submissions by copy-pasting
failed tests, it is best to gerate a large number (50-100) of test
cases (write a Python script);
5. you can test the correct handling of invalid situations by requiring
that proper exceptions are thrown;
6. you can omit test cases from the report
by setting `feedback: no` in the exercise metadata.


### Haskell

Haskell submissions can be tested using the
[QuickCheck library](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html).
A QuickCheck specification consists of a set of *properties* and possibly
*test data generators*; the QuickCheck library includes
default generators for basic types, tuples, lists, etc.

Consider the an example exercise: merging elements from two ordered lists;
the page text file is as follows:

~~~~{style="margin:2em; padding:1em; width:50em; border: solid;border-width:1px;"}
---
exercise: true
language: haskell
quickcheck: merge.hs
...

# Merge two ordered lists

Write a function `merge :: Ord a => [a] -> [a] -> [a]`{.haskell}
that merges two lists in ascending order; the result list
should preserve the ordering and contain all elements from
both lists.
~~~~

The QuickCheck script `merge.hs` is a straightforward translation
of the above specification:

~~~~{.haskell style="margin:2em; padding:1em; width:50em; border: solid;border-width:1px;"}
import Test.QuickCheck
import Data.List ((\\))

-- 1) merge preserves ordering
prop_ordered :: OrderedList Int -> OrderedList Int -> Bool
prop_ordered (Ordered xs) (Ordered ys)
    = ascending (Submit.merge xs ys)

-- 2) merge preserves elements
prop_elements :: OrderedList Int -> OrderedList Int -> Bool
prop_elements (Ordered xs) (Ordered ys)
    = permutation (Submit.merge xs ys) (xs ++ ys)

-- auxiliary definitions
-- check if a list is in ascending order
ascending xs = and (zipWith (<=) xs (tail xs))
-- check if two lists are permutations
permutation xs ys = null (xs \\ ys) && null (ys \\ xs)
~~~~

Some remarks:

* the user submission is implicitly imported qualified as a module `Submit`;
* all properties (e.g. functions starting with `prop_`) will be tested;
* note that although `merge` is polymorphic, we need to choose
a monomorphic type for testing (`Int`); 
* we used the default generators for `Int` and `OrderedList` wrapper 
defined in the QuickCheck library (no need to define a custom generator);


### C

TO BE DONE 

-->


----

Pedro Vasconcelos, 2017.



