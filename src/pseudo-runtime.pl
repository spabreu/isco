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

% == ISCO pseudo-run-time support =============================================

% This unit can be used to simulate the predicates available during runtime,
% for perusal by the compiler.

:- include('ops').

:- unit(pseudo_rt(ST)).

isco_class(CLASS, ARITY) :-
	ol_member(CLASS=[class|NET], ST),
	lookup(NET, fields, fields=Fs),
	isco_nondupe_fields(Fs, FF),
	ol_length(FF, ARITY).

isco_sequence(NAME, TYPE) :-
	ol_member(sequence(NAME)=ATTRs, ST),
	( ol_memberchk(external(DB, SNAME), ATTRs) ->
	    TYPE = external(DB, SNAME)
	;   TYPE = regular ).

isco_superclass(C, SC) :-
	ol_member(C=[class|NET], ST),
	lookup(NET, super, super=SC),
	( var(SC) -> fail
	; member(C,SC) ).

isco_field(CLASS, FIELD, POS, NFB, KIND, TYPE) :-
	ol_member(CLASS=[class|NET], ST),
	lookup(NET, fields, fields=Fs),
	fail.			% FIXME: finish this code...


% isco_compound_key(CLASS, KEY).
% isco_compound_index(CLASS, KEY).
% isco_field_domain(_, _, _, _).
% isco_field_default(_, _, _).
% isco_field_unique(_, _).
% isco_field_not_null(_, _).
% isco_field_key(_, _).
% isco_field_index(_, _).

isco_classtype(CLASS, CLASSTYPE).


% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% outline-regexp: "% \\(--\\|==\\) "
% comment-column: 48
% End:
