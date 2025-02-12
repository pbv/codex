
# Instalation instructions for the Codex exercise system

## Requirements

*Codex* was developed in a Linux system (Ubuntu) but should work
identically in most Linux distributions. It *should* be possible to get
it to work in MacOS, but I don't use Apple computers and can't help you
with that.

Compiling from source requires a computer with at least 8Gb of RAM and
about 10 Gb of free disk space (for the tools and dependencies) and
takes about 45 mins (mostly because all the dependencies) on a 10
year-old Core I5 Thinkpad with a SATA SSD drive. Recompiling should be
much faster because the libraries are cached.

## Install the Haskell tool stack

Don't use the Haskell packages that are shipped with your
distribution. Instead, you should follow the instructions in
[GHCup](https://www.haskell.org/ghcup/) to install the Haskell
development tools, in particular `cabal` and `stack`.

The particular version of GHC choosen is not very important because
`stack` will download the specific version of compiler and all
libraries specified in `stack.yaml`.

You don't have to delete any system-wide Haskell packages: `stack`
will simply ignore them and use the ones it downloaded.

## Compiling Codex from source

In the project root directory:

~~~bash
$ stack setup
$ stack build    # takes a while
$ stack install
~~~

You might get an error message regarding missing C libraries, in
particular `sqlite3`. In Ubuntu/Debian systems these can be installed
using the following command:

~~~bash
$ sudo apt install libsqlite3-dev
~~~

If all goes well, you'll be able to run the server locally from the
project root directory:

~~~bash
$ codex-server
~~~

## Compiling and installing the sandboxing utility

Codex uses a separate command-line utility for sandboxing exercise
execution. This is included in the `SafeExec` directory. Run the
following commands from the project root directory to compile and
install it:

~~~bash
$ cd SafeExec
$ make
$ sudo cp safeexec /usr/local/bin   # or some other system-wide location
$ sudo chown root:root /usr/local/bin/safeexec
$ sudo chmod ug+s /usr/local/bin/safeexec
~~~

The executable needs to be in the search path of the the Codex server.
The last command sets the SETUID bit on the executable so that it can
reduce memory and time limits and change user and group.

## Compiling and installing the user DB utility 

Codex does not include mechanisms for creating, delete and changing
users.  There is a separate utility from the Snap web framework for
doing that called `snap-auth-cli`.  You could alternatively use an
LDAP service, but this is less practical in testing.

To compile and install it, run the following commands (outside of the
Codex project):

~~~bash
$ git clone https://github.com/dzhus/snap-auth-cli
$ cd snap-auth-cli
$ stack build
$ stack install
~~~

After this, you can create one or more test users in the Codex database.
Change directory to the Codex project root and run the following command:

~~~bash
$ snap-auth-cli -s -f submissions.db -c -u test -p <password>
~~~

*If you get an error message because the `submissions.db` file is
missing, simply run `codex-server` to create an empty database file.*

This creates a user with login `test` and your choosen password. You
now should be able to run the Codex server and login -- only to get an error
message because there are no exercises pages to serve!  The next
section addresses that.


## Trying some example exercises

There is a separate repository for [example
exercises](https://github.com/pbv/codex-examples). Clone it and 
create a symbolic link to the `public` directory inside the Codex root:

~~~bash
$ git clone https://github.com/pbv/codex-examples
$ cd codex 
$ ln -s ../codex-examples/public public
~~~

You should now be able to login, navigate the exercises pages and
submit attempts.

-----

Pedro Vasconcelos, 2025.


