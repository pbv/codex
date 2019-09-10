---
title: A Guide to Writting Codex Exercises
author: Pedro Vasconcelos <pbv@dcc.fc.up.pt>, University of Porto, Portugal. 
date: July 2019 (version 0.9.5)
...


# Introduction

## Overview

*Codex* is a web system for setting up programming exercises with
automatic assessment. The main features are:

*Simple exercise authoring*

:    Exercise descriptions are written in a human-readable Markdown
format that can easily be copied, mailed, kept in a version repository,
compared for changes, etc.;

*Allows assessing program units*

:    Exercises can assess separate functions, classes or methods
rather than just complete programs;

*Provides automatic feedback*

:    Rather than report just an *accept/reject* result,
Codex can use automatic testing techniques such as
[properties](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html)
or [documentation-tests](https://docs.python.org/3.7/library/doctest.html)
to report failure example to students.

*Allows multiple types of exercises*

:     Codex supports testing code in several programming language and also
mutiple-choice and fill-in questionaries.


Codex is intended for learning environments rather than programming
contests (for the later, check out
[Mooshak](https://mooshak.dcc.fc.up.pt/)).  The system is currently
being used at the Faculty of Science of the University of Porto for
introductory courses on Python and C programming.

## Pages and exercises

A Codex exercise repository is organized as set of interlinked
hypertext pages.  Pages are written in
[Markdown](https://en.wikipedia.org/wiki/Markdown), 
a lightweight text markup syntax originally proposed by John Gruber.
Codex uses the 
[Pandoc library](http://hackage.haskell.org/package/pandoc) for parsing
Markdown which suports many extensions. In particular, it
support LaTeX mathematics (rendered using
[MathJaX](http://www.mathjax.org)).
For more details on the Pandoc-flavoured Markdown, check the [Pandoc user
manual](http://pandoc.org/MANUAL.html#pandocs-markdown).

Pages can be plain static documents or marked as *exercises*;
the later allow student's submissions and trigger automatic assessment.
Submissions are kept in a persistent database and students
can review  their previous attempts.
The administrator can view and manage 
all submissions (e.g. re-evaluate or generate printouts).

Comments can be written in HTML-style:

~~~
<!-- This is a comment; it can span multiple lines -->
~~~

Note that, unlike other Markdown readers, raw HTML markup
is **not** allowed (it will be rendered as plain text).

Codex uses Pandoc's YAML extension to specify metadata for pages
e.g., define the exercise tester, programming 
languages allowed, period for valid submissions, etc.
Metadata blocks are delimited between "`---`" and 
"`...`":

    ---
    tester: doctest
    language: python
    valid: "after 25/05/2019 and before 15/06/2019"
    ...

Metadata blocks can occur anywhere in a document; several 
blocks are equivalent to a single one with the (left-biased)
union of all fields.
(*Sugestion*: include
metadata at the beginning or the the end of the document.)
 
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

Note that the links above are marked with a special 
class attribute `".ex"`; this
fills-in the exercise title automatically (read from the markdown
document) and a summary of previous submissions by the current user
(read from the submission database).

The administrator should compose index pages to define order
of exercises, or group exercises using sections and sub-pages.  It is also
possible to add plain Markdown pages for documentation, or links to
external resources.

Note that:

1. Exercise pages can be accessed by their URL even if they
   are not explicitly linked to the index; this can be used
   test exercises before making them visible to students;
2. If you intend to group exercises using sub-directories
   make sure *you include an `index.md` for each sub-directory*.

# Writting exercises

## An example programming exercise

Consider the following simple Python programming exercise: *write a
function `root` to compute rounded-down integral square roots*.

First we set up an *exercise page* `sqroot.md`; this contains the
exercise description shown to the student (and some metadata fields):

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

Some remarks on writting *doctest* cases:

1. The first test case that fails is reported, thus
   so the *doctest* script should the include
   the simplest cases first;
2. Be aware that the `doctest` library
   employs a textual matching of outputs
   (e.g. `0` and `0.0` are considered distinct); make sure you normalize
   floating-point results (e.g. using `round`)
4. To discourage students from "overfitting" solutions
   to pass just the failing tests, it is best to generate
   a large number (50-100) of cases (write a Python script);
5. Alternatively, you can hide test cases by setting
   the metadata field "`feedback: no`";
6. It is also possible test error handling by requiring
   that proper exceptions are thrown &mdash; see the [`doctest` documentation](https://docs.python.org/3/library/doctest.html#what-about-exceptions);

### An example quiz

Setting up a multiple-choice or fill-in quiz requires
only a Markdown page:

~~~{.boxed}
---
tester: quiz
shuffle-questions: no
shuffle-answers: yes
...

# Example quiz

This is an example of multiple-choice and fill-in questions.

<!-- Each question starts with a header (any level)
     with class attribute "question" -->

## Question 1 {.question answer=a}

Consider a relation $R \subseteq \mathbb{Z}\times\mathbb{Z}$ 
definied by $x R y \iff \exists k \in\mathbb{Z} : y = k\times x$.
What properties does this relation have?

(a) reflexive, transitive e anti-symetric
(b) reflexive, transitive e symetric
(c) reflexive, not transitive and symetric

<!-- The next question allows multiple selections -->

## Question 2 {.question .multiple answer=a answer=c answer=e}

Select **all** true statements from the choices bellow.

(a) If cats have wings then dogs have wings
(b) If bird have wings thgen cats have wings 
(c) If cats have wings then birds have wings
(d) Snakes have legs if and only if mice have tails
(e) If frogs have fur and mice have eyes, then sharks have no teeth

<!-- A fill-in question; answers are compared textually ignoring spaces -->

## Question 3 {.question .fillin answer="42"}

According the "The Hitchhiker's Guide to the Galaxy", what is the
answer to the fundamental question about life, the universe and
everything?
~~~

Each question has a unique identifier;
when these are not explicitly defined in the headers (as above)
Pandoc generates identifiers automatically 
based on the header text (e.g. `#question-1`, `#question-2`,
`#question-3`).
If the header has no text, then the automatic identifiers
will be `#section`, `#section-1`, `#section-1`, etc.
You can explicitly set header identifiers as follows:

~~~
## {#q1 .question answer=a}
~~~

If the metadata options `shuffle-questions` and `shuffle-answers` are
enabled Codex will shuffle questions and/or answers. By default,
neither questions nor answers are shuffled.  Shuffling is
deterministic for each student (i.e. it depends only on the user
login).

It is also possible to group variants of each question
so that only one is randomly choosen for each student, e.g.:

~~~{.boxed}
# {#questionA1 .question group="A" ...}

<!-- first variant for A -->

# {#questionA2 .question group="A" ...}

<!-- second variant for A -->

# {#questionA3 .question group="A" ...}

<!-- third variant for A -->

# {#questionB1 .question group="B" ...}

<!-- first variant for B -->

# {#questionB2 .question group="B" ...}

<!-- second variant for B -->

# {#questionC .question ...}

<!-- this question has no group, so it always included -->
~~~

From the above quiz, three questions will be chosen: one from
`#questionA1`, `#questionA2` and `#questionA3`; one of `#questionB1`
and `#questionB2`; and finally `#questionC`.  As with shuffling, the choice
is pseudo-random but deterministic and depends only on the user login.
Grouping can also be combined with shuffling, changing the order of
questions and/or answers.


### Testing Haskell code using Quickcheck

A QuickCheck specification is a Haskell main module that acts as a
"test harness" for the student's code. It can define properties and
possibly data generators; the Codex QuickCheck library includes
default generators for basic types (tuples, lists, etc.)  and
combinators for building custom ones.

Consider an example exercise:

> Write a Haskell function `strong :: String -> Bool`
> that checks if a string is a *strong password* using
> the following criteria:
>
> 1. it must have at least 6 characteres in length;
> 2. it must have an uppercase letter, a lowercase letter and a digit.

The QuickCheck specification for this exercise is as follows:

~~~~{.boxed .haskell}
module Main where

import Codex.QuickCheck
import Data.Char
import Submission (strong) -- student's solution

-- | reference solution 
strong_spec :: String -> Bool
strong_spec xs
   = length xs >= 6 && any isUpper xs && any isLower xs && any isDigit xs

-- | a generator for suitable strings
asciiString = listOf (choose ('0', 'z'))

-- | correctness property: for all above strings,
-- the submission yields the same result as the reference solution
prop_correct
  = forAllShrink "str" asciiString shrink $
    \xs -> strong xs ?== strong_spec xs <?> "strong"
                    
main = quickCheckMain prop_correct
~~~~

Some remarks:

1. The student's code is always imported
   from a separate module; names should be explicitly imported or qualified
   to prevent name colisions;
2. `asciiString` is a custom generator 
   for strings with charateres from `0` to `z`:
   this generate letters, digits and some other simbols as well;
3. The operator `?==` asserts that the left-hand side
   equals the (expected) right-hand side
5. The operator `<?>` names the test for reporting;
4. Finally, the `quickCheckMain` driver function handles
   setting up of the testing parameters and checks the property.

The use of shrinking simplifies failing test cases
automatically; for example, consider the following submission
exhibiting a common logical error:

~~~{.boxed .haskell}
forte :: String -> Bool
   = length xs >= 6 && any (\x -> isUpper x || isLower x || isDigit x) xs
   -- WRONG!
~~~ 

Because of shrinking, the specification above QuickCheck *always* finds a
simple counter-example:

~~~{.boxed}
*** Failed! (after 12 tests and 6 shrinks): 
Expected:
	False
Got:
	True
Testing forte with:
str = "aaaaaa"
~~~

### Testing C code Using QuickCheck 

It is also possible to use QuickCheck to test C code using the Haskell
FFI. The test specification written in Haskell and uses the
Haskell-to-C FFI to call the student's code.

Consider the C version of the strong password example:

> Write a C function `int strong(char *str)`
> that checks if a NUL-terminated string is a *strong password* using
> the following criteria:
>
> 1. it must have at least 6 characteres in length;
> 2. it must have an uppercase letter, a lowercase letter and a digit.
>
> The result should be 1 the string satifies the above criteria
> and 0 otherwise.

The previous specification is adapted to test C as follows:

~~~{.boxed .haskell}
module Main where

import Codex.QuickCheck
import Control.Monad
import Control.Exception
import System.IO.Unsafe
import Data.Char

foreign import ccall "forte" c_forte :: CString -> IO CInt
   -- use FFI to import students' submission

-- | functional wrapper over C code
c_forte_wrapper :: String -> CInt
c_forte_wrapper str = unsafePerformIO $ 
  withCString str $ \ptr -> do
     r <- c_forte ptr
     str' <- peekCAString ptr
     unless (str == str') $
          throwIO $ userError "modified argument string"
     return r

-- | functional specification
forte_spec :: String -> Bool
forte_spec xs
   = length xs >= 6 && any isUpper xs && any isLower xs && any isDigit xs

prop_correct
  = forAllShrink "str" asciiString shrink $
    \xs -> c_forte_wrapper xs ?== fromIntegral (fromEnum (forte_spec xs))
           <?> "forte" 

asciiString = listOf (choose ('0', 'z'))
                      
main = quickCheckMain prop_correct
~~~

Note that, along with function correctness,
the wrapper code above also checks that student code does
not overwrite the string buffer. 

# Reference guide

## Results

Codex assesses submissions using a *tester*; for
programming exercises this typically requires running a test suite (either
provided by the instrutor or randomly-generated).
The result is a *classification label*, a *validity check* and
a *detail report*.

Classification labels are similar to the ones used for
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


Independentely of the classification above,
the submission is marked *Valid* if it
respects the constraints on time and maximum attempts (specified
as metadata) and
*Invalid* otherwise (with a suitable message).

Note that Codex always evaluates submissions (and reports
feedback if is enabled); this means
that a late submission that passes all tests will
be classified *Accepted (Invalid: Late submission)*.[^1]
This behaviour is intencional: it
allows students to retry past exercises and
leaves the instructor freedom on how to consider
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

## General metadata 

The following metadata fields apply to all exercises types.

`title`

:      Specifies the exercise title (used for hypertext links); if
this is missing, then the first header in the exercise description
is used.

`tester`

:      Specify the type of exercise (described in the following subsection).


`valid`

:   Specify constraints on the time and maximum number for submission attempts; the default is no time constraint and an unlimited number of submssions.  Some examples:
    
    ```
    valid: "after 08:00 15/02/2019"
    valid: "after 08:00 15/02/2019 and before 12:00 15/02/2019"
    valid: "before 16/02/2019"
    valid: "before 16/02/2019 and attempts 20"
    ```
 
    Date and times are relative to the server timezone.

<!-- You
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

:      Specify an initial code skeleton for programming exercises;
use an indented block for multiple lines, e.g.:

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
for unit-testing simple functions, methods or classes. Extra options:

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
using a [custom version](https://github.com/pbv/codex-quickcheck) of
the [QuickCheck library](http://hackage.haskell.org/package/QuickCheck).
Extra metata options:

`language`

:     Languages accepted: `haskell` or `c`. 

`properties`

:     Haskell file with QuickCheck specification (generators and properties);
defaults to the page filename with the extension replaced by `.hs`.

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

:     Fractional number from 0 to -1; this is the scoring weight
for an incorrect to a multiple-choice question.
By default the score for each incorrect item is such that
the sum of incorrect items equals minus the sum of corrects ones,
e.g. for a three option question with a single correct option,
an incorrect option scores $-1/2$. To change
change the penalty for wrong opttion to $-1/3$:

     ```
     incorrect-weight: -1/3
     ```

`feedback`

:    Boolean value specifiying whether feedback on the replies should be
given; should be set to `false` in in exam setting so that no feedback
on correct/incorrect answers is given.

`printout`

:    Boolean value specifiying whether to include a detailed
transcript of questions and answer when producing a printout
report (e.g. for exams); if set to `false` then the
report will only include a summary (number of answered, correct
and incorrect replies and a percentage score).


----

Pedro Vasconcelos, 2019.



