% $Id$ -*-Prolog-*-

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

% == ISCO Symbol Table support ================================================

:- include(ops).

insert(T, KV) :- var(T), !, T=[KV|_].
insert([K=_Vt|_], K=_V) :- format("error: duplicate entry ~w~n", [K]), !, fail.
insert([_|T], KV) :- !, insert(T, KV).

lookup(T, _KV, _) :- var(T), !, fail.
lookup([Kt=Vt|_], K, Kt=Vt) :- possibly_equal(Kt, K), !.
lookup([_|T], K, KVt) :- !, lookup(T, K, KVt).

possibly_equal(X, Y) :- not_equal(X, Y), !, fail.
possibly_equal(_, _).

not_equal(X, X) :- !, fail.
not_equal(_, _).

% -- Utilities ----------------------------------------------------------------

ol_add(L, I) :- var(L), !, L=[I|_].
ol_add([_|L], I) :- ol_add(L, I).

ol_memberchk(_,  L) :- var(L), !, fail.
ol_memberchk(X, [X|_]) :- !.
ol_memberchk(X, [_|L]) :- ol_memberchk(X, L).

ol_member(_,  L) :- var(L), !, fail.
ol_member(X, [X|_]).
ol_member(X, [_|L]) :- ol_member(X, L).

ol_length(L, N) :- ol_length(L, 0, N).

    ol_length(L, N, N) :- var(L), !.
    ol_length([], N, N) :- !.
    ol_length([_|L], N, M) :- N1 is N+1, ol_length(L, N1, M).

ol_close([]) :- !.
ol_close([_|L]) :- ol_close(L).

isco_error(FORMAT, ARGS) :- format(FORMAT, ARGS).

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
