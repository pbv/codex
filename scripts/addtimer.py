#
# Add timer bonus to a user, keeping all other metadata values intact
# PBV, 2026
#

import argparse
import subprocess
import sys
import json

parser = argparse.ArgumentParser(prog='Add Timer bonus',
                                 description='Add bonus to students in Codex exams time')
parser.add_argument('-u', '--user')
parser.add_argument('-x', '--extratime')
parser.add_argument('-d', '--database')
args = parser.parse_args()

db = args.database
user = args.user
extra = args.extratime

if db is None or user is  None:
    print("no user or database specified; exiting")
    sys.exit(-1)
   
out = subprocess.run(['snap-auth-cli', '-r', '-u', user, '-s', '-f', db], capture_output=True)
obj = json.loads(out.stdout)

meta = {}
for k,v in obj['meta'].items():
    if k != "timerBonus":
        meta[k] = v
meta['timerBonus'] = extra

cmdline = ['snap-auth-cli', '-m', '-u', user, '-s', '-f', db]
for k,v in meta.items():
    cmdline.extend(['-k', k, '-v', v])

print(' '.join(cmdline))
subprocess.run(cmdline)










