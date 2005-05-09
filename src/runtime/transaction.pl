% $Id$

% -----------------------------------------------------------------------------
%  ISCO is Copyright (C) 1998-2005 Salvador Abreu
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

% == ISCO transaction support =================================================

:- unit(transaction(CONN)).

% -----------------------------------------------------------------------------

internal :- g_read(isco_isco, CONN).
class(C) :- isco_get_connection(C, CONN).

% -----------------------------------------------------------------------------

begin :-
	isco_be_exec(CONN, "begin transaction", R),
	isco_be_fetch(CONN, R) -> true ; true.

commit :-
	isco_be_exec(CONN, "end transaction", R),
	isco_be_fetch(CONN, R) -> true ; true.
end :- commit.

rollback :-
	isco_be_exec(CONN, "rollback transaction", R),
	isco_be_fetch(CONN, R) -> true ; true.
abort :- rollback.

% atomic(GOAL) :-
% 	isco_term_expansion(GOAL, GOAL1),
% 	begin,
% 	( catch(GOAL1, _EX, fail) -> commit ; abort ).

% $Log$
% Revision 1.1  2005/05/09 22:47:42  spa
% Initial release
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
