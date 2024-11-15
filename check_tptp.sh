#!/usr/bin/env bash
basedir="$(dirname "$0")"
. "$basedir/include.sh"

echo -n "$1, "
if "$basedir/parse_tptp.sh" "$@" > /dev/null 2> /dev/null ;
then
    echo "OK"
else
    echo "ERROR"
fi
