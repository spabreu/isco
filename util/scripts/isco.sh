# /bin/sh
# $Id$

# -----------------------------------------------------------------------------
#  ISCO is Copyright (C) 1998-2001 Salvador Abreu
#  
#     This program is free software; you can redistribute it and/or
#     modify it under the terms of the GNU General Public License as
#     published by the Free Software Foundation; either version 2, or
#     (at your option) any later version.
#  
#     This program is distributed in the hope that it will be useful,
#     but WITHOUT ANY WARRANTY; without even the implied warranty of
#     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
#     General Public License for more details.
#  
#     You should have received a copy of the GNU General Public License
#     along with this program; if not, write to the Free Software
#     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
#     02111-1307, USA.
#  
#  On Debian GNU/Linux systems, the complete text of the GNU General
#  Public License can be found in `/usr/share/common-licenses/GPL'.
# -----------------------------------------------------------------------------

# -- isco.sh ------------------------------------------------------------------
#
# isco [FLAGS] FILES
#
# FILES is a list of files or directories:
#   DIRECTORY -> compile (with ISCO) and load
#   FILE.isco -> compile (with ISCO) and load
#   FILE.*    -> load
# FLAGS may be:
#   --output=FILE   Place executable into FILE
#   -o FILE         Same as --output=FILE
#   --compile, -c   Compile all files

PREFIX=/usr
LIBDIR=/usr/lib/isco


PROG=${0##*/}
FILE=
OUTPUT=

while [ ! -z "$1" ]; do
  case $1 in

  --help|-h)
    cat <<EOF
usage: $PROG ... [to be written]
EOF
    exit 1;;

  --output=*)
    OUTPUT=${1#--output=};
    shift;;

  -o)
    OUTPUT=$2;
    shift;
    shift;;

  -*)
    echo 1>&2 illegal flag: $1. try $PROG --help for information.;
    exit 1;;

  *.sql)
    FILE=$1;
    [ -z "$PREFIX" ] && { PREFIX=${FILE##*/}; PREFIX=${PREFIX%.sql}; };
    [ -z "$OUTPUT" ] && OUTPUT=${PREFIX}.isco;
    shift;;

  *)
    echo 1>&2 illegal argument: $1. try $PROG --help for information.;
    exit 1;;
  esac
done

if [ -z "$FILE" -o -z "$PREFIX" ]; then
  echo 1>&2 no file specified. try $PROG --help for information.
  exit 1
fi

LC_ALL=en_US
export LC_ALL

$COMPILER -c $FILES > $OUTFILE
$GPLC -o $OUTPUT $PREFIXFILES $OUTFILE $LIBS

# Local Variables:
# mode: shell-script
# mode: font-lock
# End:
