#
# Firejail profile for running Java submissions
# Pedro Vasconcelos, 2026
#

quiet
net none
private
whitelist /public/codex
read-only /public/codex
whitelist /tmp/codex
read-write /tmp/codex

rlimit-nproc 2000
rlimit-cpu 4
rlimit-as 8G
rlimit-fsize 20M
timeout 00:00:10
