# $Id$

# -----------------------------------------------------------------------------
#  ISCO is Copyright (C) 1998-2003 Salvador Abreu
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

SHELL = /bin/bash

GPLC = /usr/bin/gplc-cx
PROLOG = gprolog-cx
INSTALL = /usr/bin/install -c

PROG = isco

PREFIX = /usr
LIBDIR = $(PREFIX)/lib/$(PROG)
DESTDIR = $(PREFIX)/bin

all: $(PROG)

install: all
	[ -d $(DESTDIR) ] || mkdir -p $(DESTDIR)
	$(INSTALL) -m 555 $(PROG) $(DESTDIR)/prog

clean:
	rm -f *~ \#* *.o

# $Log$
# Revision 1.3  2003/01/07 00:13:26  spa
# *** empty log message ***
#
# Revision 1.2  2003/01/07 00:01:18  spa
# *** empty log message ***
#

# Local variables:
# mode: font-lock
# End:
