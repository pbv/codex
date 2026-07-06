#
# Add extra time to a user, keeping all other metadata values intact
# PBV, 2026
#

import argparse
import subprocess
import random
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

def load_meta(user, db):
    "Load the meta data for a user; create the user if does not exist."
    cmdline = ['snap-auth-cli', '-r', '-u', user, '-s', '-f', db]
    print(' '.join(cmdline))
    out = subprocess.run(cmdline, capture_output=True)
    if out.returncode != 0:
        # user does not exist, create one
        # choose a dummy random local password;
        # this is just so that snap-auth-cli creates the user
        # the user will be able to login with the LDAP password
        passwd = str(random.randint(1000000, 9999999))
        cmdline = ['snap-auth-cli', '-c', '-u', user, '-p', passwd, '-s', '-f', db]
        print(' '.join(cmdline))
        subprocess.run(cmdline)
        meta = dict()
    else:
        obj = json.loads(out.stdout)
        meta = dict()
        for k,v in obj['meta'].items():
            meta[k] = v
    return meta

def store_meta(user, db, meta):
    "Store the meta data for a user."
    cmdline = ['snap-auth-cli', '-m', '-u', user, '-s', '-f', db]
    for k,v in meta.items():
        cmdline.extend(['-k', k, '-v', v])
    print(' '.join(cmdline))
    subprocess.run(cmdline)
    

def main():
    global parser
    args = parser.parse_args()
    db = args.database
    user = args.user

    if db is None or user is None:
        print("no user or database specified; exiting")
        sys.exit(-1)

    meta = load_meta(user, db)       
    if args.reset_start:
        if 'firstLogin' in meta:
            print(f'deleting firstLogin for user {user}')        
            del meta['firstLogin']
            store_meta(user, db, meta)                        
        else:
            print(f'no firstLogin for user {user}; exiting')
            sys.exit(-1)
    elif args.reset_offset:
        if 'timeDiffExtra' in meta:
            print(f'deleting timeDiffExtra for user {user}')
            del meta['timeDiffExtra']
            store_meta(user, db, meta)            
        else:
            print(f'no timeDiffExtra for user {user}; exiting')
            sys.exit(-1)            
    elif args.add is not None:
        print(f'adding {args.add} offset for user {user}')
        offset = NominalDiffTime(args.add)
        time = NominalDiffTime(meta.get('timeDiffExtra','0s'))
        meta['timeDiffExtra'] = str(offset + time)
        store_meta(user, db, meta)
        sys.exit(0)



parser = argparse.ArgumentParser(prog='Add Timer bonus',
                                 description='Add time bonus Codex user')
parser.add_argument('-u', '--user')
parser.add_argument('-d', '--database')
parser.add_argument('-a', '--add', help='time offset to add')
parser.add_argument('-z', '--reset-offset', action='store_true', help='reset the time offset')
parser.add_argument('-x', '--reset-start', action='store_true', help='reset the start time')

if __name__=='__main__':
    main()

    









