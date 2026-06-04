#
# Firejail profile for running Codex submissions
# Pedro Vasconcelos, 2026
#

quiet
net none
private
whitelist /tmp/codex
read-write /tmp/codex

rlimit-nproc 1024
rlimit-cpu 1
rlimit-as 100M
rlimit-fsize 100K
timeout 00:00:10
