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

:- unit(schema(ST)).

% == Prolog code generation (schema) for ISCO =================================

% This part creates clauses for meta-information about the currently loaded
% ISCO program.
% 
% The predicates defined here are:
%  isco_class(CLASSNAME).
%  isco_superclass(CLASS, SUPERCLASS).
%  isco_field(CLASS, FIELD, POSITION, TYPE).
%

emit :-
	isco_schema_header(schema, "=", ""),
	isco_schema(class),
	isco_schema(sequence),
	isco_schema(superclass),
	isco_schema(fields),
	isco_schema(field),
	isco_schema(key),
	isco_schema(index),
	isco_schema(attr(domain)),
	isco_schema(attr(defaults)),
	isco_schema(attr(unique)),
	isco_schema(attr(not_null)),
	isco_schema(attr(key)),
	isco_schema(attr(index)),
	isco_schema(classtype).


isco_schema(TYPE) :-
	isco_schema_header(TYPE),
	isco_schema(ST, TYPE, DONE),
	isco_schema_trailer(DONE, TYPE).


isco_schema(EOT, _, _) :- var(EOT), !.
isco_schema([],  _, _).
isco_schema([CNAME=NET|Ss], TYPE, DONE) :-
	isco_schema_entry(TYPE, CNAME, NET, DONE),
	!,
	isco_schema(Ss, TYPE, DONE).
isco_schema([_|Ss], TYPE, DONE) :-
	isco_schema(Ss, TYPE, DONE).


% -- Nice header for each section ---------------------------------------------

isco_schema_header(TYPE) :- isco_schema_header(TYPE, "-", "\n").

isco_schema_header(TYPE, DASH, NL) :-
	format_to_atom(C, "%% ~2c ISCO ~w declarations. ", [DASH, TYPE]),
	atom_length(C, CLENGTH),
	format("~s~w~*c~n~n", [NL, C, 79-CLENGTH, DASH]).

% -- Produce a fake clause for an information predicate -----------------------

isco_schema_trailer(DONE, _) :- nonvar(DONE), !.
isco_schema_trailer(_, TYPE) :- isco_schema_trailer(TYPE).


isco_schema_trailer(sequence) :- !,
	portray_clause((isco_sequence(_, _) :- !, fail)), nl.

isco_schema_trailer(superclass) :- !,
	portray_clause((isco_superclass(_, _) :- !, fail)), nl.

isco_schema_trailer(key) :- !,
	portray_clause((isco_compound_key(_, _) :- !, fail)), nl.

isco_schema_trailer(index) :- !,
	portray_clause((isco_compound_index(_, _) :- !, fail)), nl.

isco_schema_trailer(attr(A)) :- !,
	isco_schema_pattern(A, _, _, HEAD, _), % just get pattern
	portray_clause((HEAD :- !, fail)), nl.

isco_schema_trailer(TYPE) :-
	format('%% (trailer for ~w)~n', [TYPE]).

% -- Process a class definition -----------------------------------------------

isco_schema_entry(_, special(_NAME), _METHOD, _) :- !.

isco_schema_entry(_, external(_NAME), _METHOD, _) :- !.

isco_schema_entry(sequence, sequence(NAME), ATTRs, ok) :- !,
	( ol_memberchk(external(DB, SNAME), ATTRs) ->
	    TYPE = external(DB, SNAME)
	; TYPE = regular ),
	portray_clause(isco_sequence(NAME, TYPE)), nl.

isco_schema_entry(sequence, _, _, _) :- !.

isco_schema_entry(_, sequence(_NAME), _ATTRs, _) :- !.

isco_schema_entry(classtype, NAME, NET, ok) :-
	lookup(NET, atrib, atrib=ATTRs),	% external class?
	ol_memberchk(external(DB, REL), ATTRs),
	!,
	portray_clause(isco_classtype(NAME, external(DB, REL))), nl.

isco_schema_entry(classtype, NAME, NET, ok) :-
	lookup(NET, rules, rules=RULES),	% computed class?
	nonvar(RULES),
	!,
	portray_clause(isco_classtype(NAME, computed)), nl.

isco_schema_entry(classtype, NAME, _NET, ok) :- !,
	portray_clause(isco_classtype(NAME, regular)), nl.

isco_schema_entry(class, CNAME, NET, ok) :-
	lookup(NET, fields, fields=Fs),
	isco_nondupe_fields(Fs, FF),
	ol_length(FF, NFs), !,
	portray_clause(isco_class(CNAME, NFs)), nl.

isco_schema_entry(superclass, CNAME, NET, DONE) :-
	lookup(NET, super, super=SC), !,
	( var(SC) -> true
	; SC=[] -> true
	; DONE=ok,
	  ( member(C,SC),
	    portray_clause(isco_superclass(CNAME, C)), nl,
	    fail -> true ; true ) ).

isco_schema_entry(fields, CNAME, NET, ok) :-
	lookup(NET, fields, fields=_Fs), !,
	atom_concat('isco_field_', CNAME, CFNAME),
	HEAD = isco_field(CNAME, N,P,T),
	BODY =.. [ CFNAME, N,P,T ],
	portray_clause((HEAD :- BODY)), nl.

isco_schema_entry(field, CNAME, NET, ok) :-
	lookup(NET, fields, fields=Fs), !,
	atom_concat('isco_field_', CNAME, CFNAME),
	isco_schema_field(Fs, CFNAME).

isco_schema_entry(key, CNAME, NET, ok) :-
	lookup(NET, atrib, atrib=ATTRs),	% external class?
	ol_memberchk(key(FLIST), ATTRs), !,
	portray_clause(isco_compound_key(CNAME, FLIST)), nl.

isco_schema_entry(index, CNAME, NET, ok) :-
	lookup(NET, atrib, atrib=ATTRs), !,
	ol_member(index(FLIST), ATTRs),		% BEWARE: nondet!
	portray_clause(isco_compound_index(CNAME, FLIST)), nl.

isco_schema_entry(attr(ATTRIBUTE), CNAME, NET, DONE) :-
	lookup(NET, fields, fields=Fs), !,
	isco_schema_attr(Fs, ATTRIBUTE, CNAME, DONE).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_schema_field([],  _).
isco_schema_field([f(POS,NAME,TYPE,ATTRs)|Fs], CFNAME) :-
	isco_schema_field(Fs, CFNAME),		% reverse order
	( lookup(ATTRs, dupe, dupe=yes) ->
	    true				% duplicate field: ignore
	;   HEAD =.. [CFNAME, NAME, POS, TYPE],
	    portray_clause(HEAD), nl ).


isco_schema_attr([], _, _, _).
isco_schema_attr([F|Fs], SET, CNAME, DONE) :-
	isco_schema_attr(Fs, SET, CNAME, DONE),	% reverse order
	( isco_schema_pattern(SET, CNAME, F, CLAUSE) ->
	    portray_clause(CLAUSE), nl, DONE=ok
	; true ).


% -- Patterns for producing code based on field attributes --------------------

isco_schema_pattern(SET, CNAME, F, CLAUSE) :-
	isco_schema_pattern(SET, CNAME, F, CLAUSE, ok).

isco_schema_pattern(domain, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_domain(CNAME, FNAME, XCNAME, XFNAME), OK) :-
	( lookup(ATTRs, dupe, dupe=yes) -> fail	% ignore dups
	; ol_memberchk(internal(XCNAME,XFNAME), ATTRs) -> OK=ok ; OK=no ).

isco_schema_pattern(defaults, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_default(CNAME, FNAME, VALUE), OK) :-
	( lookup(ATTRs, dupedIn, dupedIn=DUPEDIN) ->
	    ( ol_memberchk(CNAME, DUPEDIN) -> fail
	    ;	ol_memberchk(default(VALUE), ATTRs) -> OK=ok ; OK=no )
	;   ( ol_memberchk(default(VALUE), ATTRs) -> OK=ok ; OK=no ) ).

isco_schema_pattern(unique, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_unique(CNAME, FNAME), OK) :-
	   ( ol_memberchk(unique, ATTRs) -> OK=ok ; OK=no ).

isco_schema_pattern(not_null, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_not_null(CNAME, FNAME), OK) :-
	   ( ol_memberchk(not_null, ATTRs) -> OK=ok ; OK=no ).

isco_schema_pattern(key, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_key(CNAME, FNAME), OK) :-
	   ( ol_memberchk(key, ATTRs) -> OK=ok ; OK=no ).

isco_schema_pattern(index, CNAME, f(_,FNAME,_,ATTRs),
	isco_field_index(CNAME, FNAME), OK) :-
	   ( ol_memberchk(index, ATTRs) -> OK=ok ; OK=no ).


% -----------------------------------------------------------------------------

% $Log$
% Revision 1.4  2003/03/12 19:05:55  spa
% Simplified unit name...
%
% Revision 1.3  2003/03/05 01:12:41  spa
% support oid= and instanceOf= arguments.
% support redefinition of arguments, namely for default values.
%
% Revision 1.2  2003/02/28 23:39:27  spa
% isco_schema_attr/4: first clause had an argument too many!
%
% Revision 1.1  2003/01/06 15:15:01  spa
% *** empty log message ***
%
% Revision 1.1.1.1  2003/01/06 14:27:18  spa
% Imported into CVS
%
% Revision 1.17  2001/08/24 18:15:34  spa
% Take note of "index" attributes (class and field).
%
% Revision 1.16  2001/08/07 18:28:18  spa
% * apt.pl: isco_rule_rw_goal_args/9: produce ORDER_BY+MASK even if
%   ORDER_BY is empty.  Effect in clause bodies.
% * runtime.pl: isco_term_expansion/2: handle ':\' goal: it's now postfix,
%   as per the docs.  Added ':+' goal, together with corresponding
%   operator in ops.pl.
% * runtime.pl: isco_odbc_format/3: use double-quotes in result.
% * code-gen-sql.pl: isco_sql_code/{0,2}: generate sequences before classes.
% * code-gen-schema.pl: isco_schema/1, isco_schema_entry/5,
%   isco_schema_trailer/1: generate code for compound key schema.
% * apt.pl: isco_apt/2: preload symbol table with 'isco' database.
% * runtime.pl: new isco_schema_class/1, for topologic sort of classes by
%   dependency.
%
% Revision 1.15  2001/06/23 23:23:49  spa
% isco_sequence/1 is now isco_sequence/2 again.
% properly generate isco_superclass/2; really.
%
% Revision 1.14  2001/06/21 18:33:05  spa
% isco_schema/4: allow isco_schema_entry/5 to fail (ignore entry).
% isco_schema_entry/5: do the right thing when there are no superclass entries.
%
% Revision 1.13  2001/06/15 13:26:58  spa
% isco_sequence/2 should have been isco_sequence/1!
%
% Revision 1.12  2001/06/13 11:22:54  spa
% Added trailer for sequences.
%
% Revision 1.11  2001/05/19 00:48:49  spa
% Initial model and implementation for sequences.
%
% Revision 1.10  2001/05/11 15:39:41  spa
% Use ol_memberchk/3 instead of memberchk/3 to avoid binding the list tail.
% isco_schema_trailer/1 should never fail!
% isco_schema_pattern/6: new arg indicates success or failure, predicate
% always succeeds now.
%
% Revision 1.9  2001/05/09 11:13:41  spa
% Include fake clauses for missing meta-predicates.
%
% Revision 1.8  2001/05/08 12:55:10  spa
% Present many more attributes.
%
% Revision 1.7  2001/05/07 14:35:52  spa
% Output class type (external, regular, computed...).
%
% Revision 1.6  2001/05/03 13:45:31  spa
% Deal with clauses and directives in the source.
%
% Revision 1.5  2001/04/27 12:56:59  spa
% Recovered: isco_schema_entry/4 for defaults.
%
% Revision 1.4  2001/04/22 13:35:29  spa
% Deal with default declarations.
% Buglet: superclass schema generation was non-deterministic!
% Factored schema output predicates based on field attributes.
% Field lists are supposed to come as closed lists, here.
%
% Revision 1.3  2001/04/21 00:27:28  spa
% Now generates domain-related predicates, to allow for dependency analysis
% when creating tables and integrity constraint functions.
%
% Revision 1.2  2001/04/19 14:50:24  spa
% Fields are now double-indexed.
% Nicer header.
%
% Revision 1.1  2001/04/19 14:17:14  spa
% Initial version.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% End:
