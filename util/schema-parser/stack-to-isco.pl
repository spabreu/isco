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

:- op(580, xfx, (:)).

% -- Top loop -----------------------------------------------------------------

top_level :- stack_to_isco.

% -- Read stack ops and act upon them -----------------------------------------

stack_to_isco :- stack_to_isco([]).

stack_to_isco(S) :- read(T), stack_to_isco(T, S, '').


stack_to_isco(end_of_file, _, _) :- !.

stack_to_isco(unit(FOO), STACK, _) :-
	!,
	atom_concat(FOO, '_', FOO_),
	read(NEXT),
	stack_to_isco(NEXT, STACK, FOO_).

stack_to_isco((BEFORE -> AFTER ; ACTION), STACK, UNIT) :-
	STACK=BEFORE,
	!,
	stack_format(ACTION, UNIT),
	read(NEXT),
	stack_to_isco(NEXT, AFTER, UNIT).

stack_to_isco((BEFORE -> AFTER), STACK, UNIT) :-
	STACK=BEFORE,
	!,
	read(NEXT),
	stack_to_isco(NEXT, AFTER, UNIT).

% -- Pretty print a class definition ------------------------------------------

stack_format(class(NAME:SUPER, WHENCE, FIELDs), U) :-
	format("~n%% -- ~w~w class definition~n~n", [U,NAME]),
	format("~w~nclass ~w~w:~w~w.~n", [WHENCE, U,NAME, U,SUPER]),
	stack_format_fields(FIELDs, inherit).

stack_format(class(NAME, WHENCE, FIELDs), U) :-
	format("~n%% -- ~w~w class definition~n~n", [U,NAME]),
	format("~w~nclass ~w~w.~n", [WHENCE, U,NAME]),
	stack_format_fields(FIELDs, root).


stack_format_fields([], _).
stack_format_fields([F|Fs], I) :- stack_format_field(F, I), stack_format_fields(Fs, I).

stack_format_field(classe:text, inherit) :- !.
stack_format_field(NAME:TYPE, _) :-
	format("  field ~w: ~w.~n", [NAME, TYPE]).

% $Log$
% Revision 1.1  2003/01/06 14:27:10  spa
% Initial revision
%
% Revision 1.4  2001/04/19 12:56:14  spa
% Copyright.
%
% Revision 1.3  2001/04/17 07:19:54  spa
% New syntax: use module name as a prefix for the relations, with an underline.
%
% Revision 1.2  2001/04/16 23:02:21  spa
% Working version, with MODULE(RELATION) syntax.
%
% Revision 1.1  2001/04/11 13:12:40  spa
% Initial version.
%

% Local variables:
% mode: prolog
% mode: font-lock
% End:
