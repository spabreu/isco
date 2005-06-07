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

:- unit(transaction(CONNs)).

% -----------------------------------------------------------------------------

use(internal) :- !,
	g_read(isco_isco, CONN),
	ol_insert(CONNs, CONN).
use(class(C)) :- !,
	:# isco_get_connection(C, CONN),
	ol_insert(CONNs, CONN).
use(db(DB)) :- !,
	atom_concat(isco_, DB, C),
	:# isco_connection(C, CONN),
	ol_insert(CONNs, CONN).
use(CONN) :- !,
	ol_insert(CONNs, CONN).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

using(X) :- ol_member(X, CONNs).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lock :- ol_close(CONNs).

% -----------------------------------------------------------------------------

transaction_command(C) :-
	format_to_codes(S, "%s transaction", [C]),
	command_on_list(CONNs, S).

command_on_list(L, _) :- var(L), !.
command_on_list([], _) :- !.
command_on_list([L|Ls], C) :-
	( integer(L) -> 
	    isco_be_exec(L, C, R),
	    ( isco_be_fetch(L, R) -> true ; true ), ! ; true ),
	command_on_list(Ls, C).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

begin    :- transaction_command("begin").
commit   :- transaction_command("commit").
rollback :- transaction_command("rollback").
except   :- ol_insert(CONNs, except).

try(GOAL) :-
	:# isco_term_expansion(GOAL, GOAL1),
	begin,
	( ol_memberchk(except, CONNs) ->
	    EX_GOAL = (rollback, throw(EX)) ;
	    EX_GOAL = (rollback, fail) ),
	( catch(GOAL1, EX, EX_GOAL) -> commit ; rollback, fail ).

% -----------------------------------------------------------------------------

ol_add(L, I) :- var(L), !, L=[I|_].
ol_add([_|L], I) :- ol_add(L, I).

ol_member(_,  L) :- var(L), !, fail.
ol_member(X, [X|_]).
ol_member(X, [_|L]) :- ol_member(X, L).

ol_memberchk(_,  L) :- var(L), !, fail.
ol_memberchk(X, [X|_]) :- !.
ol_memberchk(X, [_|L]) :- ol_memberchk(X, L).

ol_insert(L, I) :- ol_memberchk(I, L), !.
ol_insert(L, I) :- ol_add(L, I).

ol_close([]) :- !.
ol_close([_|L]) :- ol_close(L).

% -----------------------------------------------------------------------------

% $Log$
% Revision 1.7  2005/06/07 14:14:31  spa
% try/1 now incorporates the old try_ex/1 funcionality, by means of a
% paramenter transmitted by the "except/0" goal.
%
% Revision 1.6  2005/06/07 13:54:30  spa
% typo.
%
% Revision 1.5  2005/06/07 13:54:12  spa
% end/0 and abort/0 are now gone.
%
% Revision 1.4  2005/06/07 13:53:40  spa
% lazy calls in use/1.
%
% Revision 1.3  2005/05/10 11:48:26  spa
% Mucho changed:
% - channel specifier preds now clauses for use/1
% - factored "begin", "end", etc. into transaction_command/1
% - act on a list of connections (allow for multiple concurrent transactions)
%
% Revision 1.2  2005/05/09 22:56:16  spa
% db/1 added.
%
% Revision 1.1  2005/05/09 22:47:42  spa
% Initial release
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
