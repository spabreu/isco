% $Id$

% -----------------------------------------------------------------------------
%  ISCO is Copyright (C) 1998-2001 Salvador Abreu
%  
%     This program is free software; you can redistribute it and/or
%     modify it under the terms of the GNU General Public License as
%     published by the Free Software Foundation; either version 2, or
%     (at your option) any later version.
%  
%     This program is distributed in the hope that it will be useful,
%     but WITHOUT ANY WARRANTY; without even the implied warranty of
%     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%     General Public License for more details.
%  
%     You should have received a copy of the GNU General Public License
%     along with this program; if not, write to the Free Software
%     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
%     02111-1307, USA.
%  
%  On Debian GNU/Linux systems, the complete text of the GNU General
%  Public License can be found in `/usr/share/common-licenses/GPL'.
% -----------------------------------------------------------------------------

% == ISCO back-end for unixodbc ===============================================

:- unit(odbc(C)).

exec(Q, R) :- odbc_alloc_stmt(C, R), odbc_exec_direct(R, Q).
fetch(R) :- odbc_fetch(R).
get_data(R, X, T, V) :- odbc_get_data(R, X, T, V).

ntuples(R, N) :- odbc_row_count(R, N).
oid(_, 0) :- format('[warning: OID implicitly used with ODBC!]\n', []).


% $Log$
% Revision 1.2  2003/03/12 19:02:13  spa
% Add ntuples/2 and fake oid/2.
%
% Revision 1.1.1.1  2003/01/06 14:27:21  spa
% Imported into CVS
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
