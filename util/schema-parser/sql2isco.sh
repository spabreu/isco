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

PROG=${0##*/}
FILE=
PREFIX=
OUTPUT=
UNIT=

while [ ! -z "$1" ]; do
  case $1 in

  --help|-h)
    cat <<EOF
usage: $PROG [ FLAG ]... FILE.sql
where FLAG may be:
  -o, --output=OUTFILE   Output result to OUTFILE
  -p, --prefix=PREFIX    Set class prefix to 'PREFIX_'
  -u, --unit=UNIT        Set unit/external access to 'UNIT'
  -g, --postgres=UNIT    Set unit/external (using PostgreSQL) access to 'UNIT'

defaults: OUTFILE defaults to FILE.isco
	  PREFIX defaults to FILE
	  UNIT defaults to FILE
EOF
    exit 1;;

  --output=*)
    OUTPUT=${1#--*=};
    shift;;

  -o)
    OUTPUT=$2;
    shift;
    shift;;

  --prefix=*)
    PREFIX=${1#--*=};
    shift;;

  -p)
    PREFIX=$2;
    shift;
    shift;;

  --unit=*)
    UNIT=${1#--*=};
    shift;;

  -u)
    UNIT=$2;
    shift;
    shift;;

  --postgres=*)
    PGSQL="${1#--*=}, postgres";
    shift;;

  -g)
    PGSQL="$2, postgres";
    shift;
    shift;;


  *.sql)
    FILE=$1;
    [ -z "$PREFIX" ] && { PREFIX=${FILE##*/}; PREFIX=${PREFIX%.sql}; };
    [ -z "$OUTPUT" ] && OUTPUT=${PREFIX}.isco;
    [ -z "$UNIT" ]   && UNIT=${PREFIX};
    shift;;

  *)
    echo 1>&2 unknown argument: $1. try $PROG --help for information.; exit 1;;
  esac
done

if [ -z "$FILE" -o -z "$PREFIX" ]; then
  echo 1>&2 no file specified. try $PROG --help for information.
  exit 1
fi

LC_ALL=en_US
export LC_ALL

if [ -z "$PGSQL" ]; then
  echo "external(${PREFIX}, odbc(${UNIT}))." > ${OUTPUT};
else
  echo "external(${PREFIX}, odbc(${PGSQL}))." > ${OUTPUT};
fi

JAVA=${JAVA-java}
CLASSPATH=@LIBDIR@:/usr/share/CUP:${CLASSPATH} ${JAVA} \
    Main "${PREFIX}" "${UNIT}" < ${FILE} | \
  @LIBDIR@/stack-to-isco >> ${OUTPUT}

# Local Variables:
# mode: font-lock
# End:
