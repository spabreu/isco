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

% == Prolog code generation (directives) for ISCO =============================

:- unit(directives(ST)).

emit :-
	( g_read(isco_cx_unit, UNIT),
	  UNIT = 0 -> true ;
	  format(':- unit(~q).~n~n', [UNIT]) ),
	isco_lib_directory(D),
	isco_revision(REV),
	format(':- initialization(isco_check_revision(~q)).~n', [REV]),
	isco_prolog_code(ST), nl,
	format(':- include(''~w/ops.pl'').~n', [D]),
	format(':- discontiguous(isco_setup_connection/2).~n~n', []).


isco_prolog_code(EST) :- var(EST), !.
isco_prolog_code([special(_TYPE)=SST|Ss]) :-
	isco_prolog_specials(SST),
	!,
	isco_prolog_code(Ss).
isco_prolog_code([_|Ss]) :-
	isco_prolog_code(Ss).

% -- Generate code for general clauses and directives -------------------------

isco_prolog_specials(EST) :- var(EST), !.
isco_prolog_specials([]).
isco_prolog_specials([directive(_, BODY)|Ss]) :- !,
	write(':- '), portray_clause(BODY), nl,
	isco_prolog_specials(Ss).
isco_prolog_specials([_|Ss]) :- isco_prolog_specials(Ss).


% -----------------------------------------------------------------------------

% $Log$
% Revision 1.4  2008/06/17 11:42:44  spa
% emit :-unit declarations.
%
% Revision 1.3  2003/03/12 19:06:06  spa
% Simplified unit name...
%
% Revision 1.2  2003/03/05 01:12:41  spa
% support oid= and instanceOf= arguments.
% support redefinition of arguments, namely for default values.
%
% Revision 1.1.1.1  2003/01/06 14:27:17  spa
% Imported into CVS
%

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% End:
