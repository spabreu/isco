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

% == ISCO Compiler Top Level ==================================================

:- include('ops').

% -- Compiler top-level driver ------------------------------------------------

isco_dir(DIR, ST) :-
	isco_load_dir(DIR, P),
	parser :> parse(A, P, []),
	isco_apt(A, ST),
	!,
	isco_schema(ST) :> emit,
	isco_prolog(ST) :> emit.

isco(P, ST) :-
	parser :> parse(A, P, []),
	isco_apt(A, ST).

% -- isco_load(-PROGRAM) ------------------------------------------------------
%
% Reads in the program as a list of clauses.

isco_load_dir(PROGRAM) :-
	isco_load_dir('.', PROGRAM).

isco_load_dir(PATH, PROGRAM) :-
	directory_files(PATH, LIST),
	isco_load_files(LIST, PATH, [], PROGRAM).

isco_load_files([], _, P, P).
isco_load_files([F|Fs], PATH, Pin, Pout) :-
	sub_atom(F, _, _, 0, '.isco'), !,
	atom_concat(PATH, '/', P),
	atom_concat(P, F, FILENAME),
	see(FILENAME),
	read_term(NEXT, [variable_names(NVs)]),
	isco_load(NEXT, NVs, Pin, Pint),
	seen,
	isco_load_files(Fs, PATH, Pint, Pout).
isco_load_files([_F|Fs], PATH, Pin, Pout) :-
	isco_load_files(Fs, PATH, Pin, Pout).

isco_load_files([], P, P).
isco_load_files([F|Fs], Pin, Pout) :-
	see(F),
	read_term(NEXT, [variable_names(NVs)]),
	isco_load(NEXT, NVs, Pin, Pint),
	seen,
	isco_load_files(Fs, Pint, Pout).

isco_load(PROGRAM) :-
	read_term(NEXT, [variable_names(NVs)]),
	isco_load(NEXT, NVs, [], PROGRAM).

isco_load(end_of_file, _V, PROGRAM, PROGRAM) :- !.
isco_load(include(FILE), _V, Pin, Pout) :- !,
	isco_load((:-include(FILE)), _V, Pin, Pout).
isco_load((:-include(FILE)), _V, Pin, Pout) :- !,
	( file_permission(FILE, read) ->
	    seeing(PREV_INPUT),
	    see(FILE),
	    read_term(NEXT, [variable_names(NVs)]),
	    isco_load(NEXT, NVs, Pin, Pout),
	    seen,
	    see(PREV_INPUT)
	;
	    format("warning: ~w unreadable.~n", [FILE]),
	    read_term(NEXT, [variable_names(NVs)]),
	    isco_load(NEXT, NVs, Pin, Pout) ).
isco_load(T, Vs, Pin, [T/Vs|Pout]) :-
	read_term(NEXT, [variable_names(NVs)]),
	isco_load(NEXT, NVs, Pin, Pout).

% $Log$
% Revision 1.1  2003/01/06 14:27:14  spa
% Initial revision
%
% Revision 1.11  2001/05/07 14:49:06  spa
% Include revision.pl.
%
% Revision 1.10  2001/05/02 18:03:02  spa
% Copyright information is now elsewhere.
%
% Revision 1.9  2001/04/30 22:25:58  spa
% Initial banner message.
%
% Revision 1.8  2001/04/29 07:40:10  spa
% Provide isco_load_files/3, for use by the stand-alone top-level in cmdline.pl
%
% Revision 1.7  2001/04/22 13:37:28  spa
% Separate analysis phase from code generation (cut).
%
% Revision 1.6  2001/04/19 14:51:08  spa
% Produce the schema as well as the code.
%
% Revision 1.5  2001/04/19 12:54:14  spa
% Copyright.
% New top-level predicate isco_dir/2.
% Include run-time support.
%
% Revision 1.4  2001/04/16 22:32:59  spa
% Include 'code-gen.pl' as well.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
