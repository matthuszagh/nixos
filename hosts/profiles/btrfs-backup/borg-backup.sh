#!/bin/sh

SNAPSHOT_DATETIME=$(find /.snapshots/home/ * -maxdepth 0 -name "home\.*" | tail -n 1 | awk '{print substr ($0, 23, 42)}')
SNAPSHOT_FILE="/.snapshots/home/home.${SNAPSHOT_DATETIME}"

# Setting this, so the repo does not need to be given on the commandline:
export BORG_REPO=fs828tyj@fs828tyj.repo.borgbase.com:repo

# See the section "Passphrase notes" for more infos.
export BORG_PASSPHRASE=$(cat ../../../secrets/borg-backup-password)

# some helpers and error handling:
info() { printf "\n%s %s\n\n" "$( date )" "$*" >&2; }
trap 'echo $( date ) Backup interrupted >&2; exit 2' INT TERM

info "Starting backup"

# Backup the most important directories into an archive named after
# the machine this script is currently running on:

borg create                             \
    --verbose                           \
    --filter AME                        \
    --list                              \
    --stats                             \
    --show-rc                           \
    --compression lz4                   \
    --exclude-caches                    \
    ::'{hostname}-${SNAPSHOT_DATETIME}' \
    ${SNAPSHOT_FILE}                    \

backup_exit=$?

info "Pruning repository"

# Use the `prune` subcommand to maintain 7 daily, 4 weekly and 6 monthly
# archives of THIS machine. The '{hostname}-' prefix is very important to
# limit prune's operation to this machine's archives and not apply to
# other machines' archives also:

borg prune                          \
    --list                          \
    --prefix '{hostname}-'          \
    --show-rc                       \
    --keep-daily    7               \
    --keep-weekly   4               \
    --keep-monthly  6               \

prune_exit=$?

# use highest exit code as global exit code
global_exit=$(( backup_exit > prune_exit ? backup_exit : prune_exit ))

if [ ${global_exit} -eq 0 ]; then
    info "Backup and Prune finished successfully"
elif [ ${global_exit} -eq 1 ]; then
    info "Backup and/or Prune finished with warnings"
else
    info "Backup and/or Prune finished with errors"
fi

exit ${global_exit}
