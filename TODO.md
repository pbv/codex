

# Missing user funcionality

* Allow restricting visible pages by user (page prefixes)
* Allow getting events from Database

# Missing admin functionalty

* no user-friendly interface to add/remove user roles (e.g. administrator)
* no way to create files (other than by uploading)
* no way to delete submissions - DONE
* no way to change classifications/timing of submissions - DONE (re-evaluation)
* implement export submissions (e.g. CSV) - DONE
* implement printout submissions - DONE
* no way to download files

# Known bugs

* re-submiting exercises as admin does not maintain the original user id
* possible security flaw: all files types (including specifications, test cases,
  solutions, etc) are served by the public interface

# SQL Evaluators
* sql/utils.py - `exec_multi_query()` uses a lot of resources. Try not use
  cursors
* sql/eval-schema.py - check indexes and foreign keys

# Markdown
* allow get title from pages without exercises
* put color of accepted/wrong answer in exercises links
* allow specify exercise number (A, B, C / 1, 2, 3). In this case the full exercise title becomes "A - Title"
* allow link to exercises without the submissions counter
* allow link to exercises showing only the number
