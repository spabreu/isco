% $Id$

% -----------------------------------------------------------------------------
%  ISCO is Copyright (C) 1998-2003 Salvador Abreu
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

% == Inheritance code generation for ISCO =====================================

% -- Unit parameters: ---------------------------------------------------------
%  C1 - the superclass, which is inherited by C2
%  C2 - the subclass, which inherits from C1

:- unit(inherit(C1, C2)).

% -----------------------------------------------------------------------------
% clause(-CLAUSE)
% CLAUSE gets bound to whatever is needed to access subclasses of C1
%
% The point about C1 and C2: CLAUSE is a solution if:
% - C2 is an immediate subclass of C1 and:
%   - C1 and C2 are of different sources (eg. regular vs. computed), or
%   - C1 doesn't have auto_inheritance
% 
% - C2 is a subclass+ of C1 and:
%   - C1 has auto_inheritance and
%   - C1 and C2 are of different sources (eg. regular vs. computed)

clause(CLAUSE) :-
	st(ST),
	lookup(ST, C1, _=NET1),
	lookup(NET1, fields, fields=Fs1),
	isco_nondupe_fields(Fs1, FF1),
	ol_length(FF1, NFs1),
	functor(C1, HEAD, NFs1),
	isco_locate_generator(C1, GEN1),
	( GEN1 :> isco_auto_inheritance -> AI1=yes ; AI1=no ),
	!,
	some_subclass(C1, NET1, C2),
	lookup(ST, C2, _=NET2),
	isco_locate_generator(C2, GEN2),
	( GEN2 :> isco_auto_inheritance -> AI2=yes ; AI2=no ),
	% ---------------------------------------------------------------------
	inherit(AI1, AI2, GEN1, GEN2, NET1, NET2),
	% ---------------------------------------------------------------------
	lookup(NET2, fields, fields=Fs2),
	isco_nondupe_fields(Fs2, FF2),
	ol_length(FF2, NFs2),
	functor(C2, BODY, NFs2),
	HEAD =.. [_|ARGS1],
	BODY =.. [_|ARGS2],
	once(append(ARGS1, _, ARGS2)), % merge variables
	CLAUSE = (HEAD :- BODY).

inherit(yes, yes, GEN, GEN, _, _) :-
	!,			% identical auto generators
	fail.			% => no need to do anything

inherit(yes, no, _, _, _, NET2) :-
	lookup(NET2, superclass, superclass=C1). % C2 immediate subclass of C1

inherit(yes, _, _, _, _, _) :-
	isco_source(C1, S1),
	isco_source(C2, S2),
	S1 \= S2.


% $Log$
% Revision 1.1  2003/04/14 23:51:18  spa
% Initial version.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% comment-column: 48
% End:

