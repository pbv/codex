#!/usr/bin/python3
# 
# Run a doctest suite on a Python file
# Pedro Vasconcelos, 2013-2025
#
import os
import sys
import argparse
import doctest

parser = argparse.ArgumentParser( prog='runtests',
                                  description='runner for Python doctests')
parser.add_argument('--verbose', action='store_true')
parser.add_argument('--all-fails', action='store_true')
parser.add_argument('--limit-tests', type=int)
parser.add_argument('inpyfile')
parser.add_argument('docfiles', nargs='*')

args = parser.parse_args(sys.argv[1:])

# parse the doctests and check the maximum test limit
num_tests = 0
for docfile in args.docfiles:
    with open(docfile, "r") as f:
        txt = f.read()
        out = doctest.DocTestParser().get_examples(txt, name=docfile)
        num_tests += len(out)

if args.limit_tests:
    if num_tests > args.limit_tests:
        sys.stderr.write(f'Maximum number of tests exceeded: {args.limit_tests}\n')
        sys.exit (-1)

if num_tests == 0:
    sys.stderr.write('No tests defined\n')
    sys.exit(-1)
        
(pydir, pyfile) = os.path.split(args.inpyfile)
(pymod, pyext) = os.path.splitext(pyfile)

# setup module search path
# add submissions directory to module search path
sys.path.insert(0, pydir)

# avoid cluttering submission directory with bytecode files
sys.dont_write_bytecode = True
tstmod = __import__(pymod, globals(), locals(), [], 0)

flags = (doctest.IGNORE_EXCEPTION_DETAIL|
         doctest.ELLIPSIS|
         doctest.NORMALIZE_WHITESPACE|
         (doctest.REPORT_ONLY_FIRST_FAILURE if not args.all_fails else 0))

total_failed = 0
total_tests = 0
for docfile in args.docfiles:
    name = os.path.basename(docfile)
    (fails,tests) = doctest.testfile(docfile,
                                     name=name,
                                     module_relative=False, 
                                     extraglobs=vars(tstmod), 
                                     report=False,
                                     verbose=args.verbose,
                                     optionflags=flags)
    total_failed += fails
    total_tests += tests

print(f'{total_tests} tests, {total_failed} failed.')
    
if total_failed > 0:
    sys.exit(1)
else:
    sys.exit(0)
