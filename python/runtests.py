#!/usr/bin/python3
# 
# Run doctests on a Python file
# Pedro Vasconcelos, 2013-2025
#
import os
import sys
import doctest

if len(sys.argv) < 2:
    sys.stderr.write("usage: "+sys.argv[0]+" <python-file> <doctest-file> ?<doctest-file>? ...\n")
    sys.exit (-1)

# scripts = sys.argv[1]
inpfile = sys.argv[1]
docfiles = sys.argv[2:]

(pydir, pyfile) = os.path.split(inpfile)
(pymod, pyext) = os.path.splitext(pyfile)

# setup module search path
# add submissions directory to module search path
sys.path.insert(0, pydir)

# lower recursion depth for shorter stack traces on unbounded recursions
# sys.setrecursionlimit(100)    

# avoid cluttering submission directory with bytecode files
sys.dont_write_bytecode = True

tstmod = __import__(pymod, globals(), locals(), [], 0)

flags = (doctest.IGNORE_EXCEPTION_DETAIL|
         doctest.ELLIPSIS|
         doctest.NORMALIZE_WHITESPACE|
         doctest.REPORT_ONLY_FIRST_FAILURE)

total_failed = 0
total_tests = 0
for docfile in docfiles:
    name = os.path.basename(docfile)
    (fails,tests) = doctest.testfile(docfile,
                                     name=name,
                                     module_relative=False, 
                                     extraglobs=vars(tstmod), 
                                     report=False,
                                     verbose=False,
                                     optionflags=flags)
    total_failed += fails
    total_tests += tests

print(f'{total_tests} tests, {total_failed} failed.')
    
if total_failed > 0:
    sys.exit(1)
else:
    sys.exit(0)
