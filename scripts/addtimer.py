#
# Add extra time to a user, keeping all other metadata values intact
# PBV, 2026
#

import argparse
import subprocess
import sys
import json

class NominalDiffTime:
    def __init__(self, txt):
        "Parse a nomimal diff time string."
        if len(txt)==0:
            raise ValueError('empty string')
        lit = float(txt[:-1])
        if txt[-1] == 's':
            self.value= lit
        elif txt[-1] == 'm':
            self.value= lit*60
        elif txt[-1] == 'h':
            self.value = lit*3600
        else:
            raise ValueError(f'invalid time: {txt}')

    def __add__(self,other):
        if not isinstance(other, NominalDiffTime):
            raise TypeError()
        return NominalDiffTime(f'{self.value + other.value}s')
        
    def __repr__(self):
        return f'{self.value}s'


def main():
    global parser
    args = parser.parse_args()
    db = args.database
    user = args.user

    if db is None or user is None:
        print("no user or database specified; exiting")
        sys.exit(-1)

    if args.add is None and args.remove is None:
        print("no time to add of remove; exiting")
        sys.exit(-1)

    if args.remove is None:
        offset = NominalDiffTime(args.add)
    else:
        offset = NominalDiffTime('-' + args.remove)
        
    out = subprocess.run(['snap-auth-cli', '-r', '-u', user, '-s', '-f', db], capture_output=True)
    obj = json.loads(out.stdout)

    meta = {}
    for k,v in obj['meta'].items():
        meta[k] = v
    old = NominalDiffTime(meta.get('timeDiffExtra','0s'))
    meta['timeDiffExtra'] = str(offset + old)

    cmdline = ['snap-auth-cli', '-m', '-u', user, '-s', '-f', db]
    for k,v in meta.items():
        cmdline.extend(['-k', k, '-v', v])

    print(' '.join(cmdline))
    subprocess.run(cmdline)

parser = argparse.ArgumentParser(prog='Add Timer bonus',
                                 description='Add time bonus Codex user')
parser.add_argument('-u', '--user')
parser.add_argument('-d', '--database')
parser.add_argument('-a', '--add', help='time offset to add')
parser.add_argument('-r', '--remove', help='time offset to remove')

if __name__=='__main__':
    main()

    









