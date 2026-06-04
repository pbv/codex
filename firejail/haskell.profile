#
# Firejail profile for running Haskell submissions
# Pedro Vasconcelos, 2026
#

quiet
net none
private
whitelist /tmp/codex
read-write /tmp/codex

rlimit-nproc 1000
rlimit-cpu 2
rlimit-as 1G
rlimit-fsize 100M
timeout 00:00:10
