#!/usr/bin/env python3
#
# Script to generate and assign 2FA single-use login tokens for Codex system.
#
# Each user gets a configurable number of random alphanumeric tokens.
# Tokens are stored in the 'two_fa_tokens' table of the submissions DB
#
# Pedro Vasconcelos, 2026
#

from datetime import datetime, timezone
import argparse
import random
import string
import sqlite3
import sys
import os

# Characters used for token generation (no ambiguous chars)
# Exclude ambiguous characters: O, 0, I, 1
TOKEN_CHARS = ''.join(c for c in string.ascii_uppercase + string.digits if c not in 'O0I1')
TOKEN_LENGTH = 8

def generate_token():
    """Generate a single random token."""
    return ''.join(random.choice(TOKEN_CHARS) for _ in range(TOKEN_LENGTH))

def generate_tokens(count):
    """Generate a list of unique random tokens."""
    tokens = set()
    while len(tokens) < count:
        tokens.add(generate_token())
    return list(tokens)

def cmd_generate(conn, args):
    """Generate a number of unique random tokens."""
    print(args)
    if args.seed:
        random.seed(args.seed)
    tokens = generate_tokens(args.count)
    cursor = conn.cursor()
    for token in tokens:
        cursor.execute(
            """INSERT INTO two_fa_tokens (token, used, used_by, used_at) 
               VALUES (?, ?, ?, ?)""", (token,0,None,None))
    print(f"Generated {args.count} tokens.")


def cmd_list(conn, args):
    """List all tokens and their status."""
    cursor = conn.cursor()
    cursor.execute("SELECT token, used, used_by, used_at FROM two_fa_tokens")
    rows = cursor.fetchall()

    if not rows:
        print("No tokens found.")
        return

    print("-" * 70)
    for (token, used, used_by, used_at) in rows:
        if used_by == None: used_by = '-'
        if used_at == None: used_at = '-'
        print(f"{token} {used} {used_by} {used_at}")
    print("-" * 70)


def cmd_assign(conn, args):
    """Manually assign a token (for debugging)."""
    cursor = conn.cursor()
    [user, token] = args.assign

    cursor.execute(
        "SELECT id FROM two_fa_tokens WHERE token = ?",
        (token, )
    )
    row = cursor.fetchone()
    if not row or row[0] is None:
        print(f"Error: token '{token}' is not valid", file=sys.stderr)
        sys.exit(1)


    # Consume the token
    time = datetime.now(timezone.utc)
    cursor.execute(
        "UPDATE two_fa_tokens SET used = 1, used_by = ?, used_at = ? WHERE id = ?",
        (user, str(time), row[0])
    )
    print(f"Assigned token '{token}' to user '{user}'.")


def cmd_remove(conn, args):
    """Remove all tokens from a user."""
    [user] = args.remove
    cursor = conn.cursor()
    cursor.execute(
        "UPDATE two_fa_tokens SET used = 0, used_by = NULL, used_at = NULL WHERE used_by = ?",
        (user,)
    )
    print(f"Removed all tokens from user '{user}'.")


def main():
    parser = argparse.ArgumentParser(
        description='Generate and manage login tokens for Codex two-factor authentication.'
    )
    parser.add_argument('--database', default='submissions.db',
                        help='Path to the SQLite database')
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument('--gen', nargs=1, metavar='NUMBER', 
                       help='Generate new tokens')
    group.add_argument('--list', action='store_true',
                       help='List all users and their token status')
    group.add_argument('--assign', nargs=2, metavar=('USER', 'TOKEN'),
                       help='Assign a specific token to a user')
    group.add_argument('--remove', nargs=1, metavar='USER',
                       help='Remove all tokens from a user')
    
    args = parser.parse_args()


    if not os.path.exists(args.database):
        print(f"Error: database '{args.database}' not found", file=sys.stderr)
        sys.exit(1)

    conn = sqlite3.connect(args.database)
    try:
        if args.gen:
            cmd_generate(conn, args)
        elif args.list:
            cmd_list(conn, args)
        elif args.assign:
            cmd_assign(conn, args)
        elif args.remove:
            cmd_remove(conn, args)
        conn.commit()
    finally:
        conn.close()


if __name__ == '__main__':
    main()
