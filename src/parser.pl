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

:- unit(parser).

% -- ISCO Parser --------------------------------------------------------------
%
% Constructs the APT from an input stream of pairs TERM/VARIABLES.
%
% VARIABLES is in the form list(NAME=VAR)
%
% APT node types:
%
%  class(CLASSNAME, SUPERCLASSES, ATTRS, DEF)
%    CLASSNAME: atom_name
%    SUPERCLASSES: list(atom_name)
%    ATTRS: list(atom_attribute)
%    DEF: body
%
%  data(CLASSNAME, FIELDLIST, VALUELIST)
%
%  rule(CLAUSE)
%
%  atom_attribute: oneof([final, abstract])
%

:- include(ops).

% == parse(-APT, +PROGRAM, ?REST) =============================================
%
% DCG to actually parse an ISCO program.

parse([DEF|DEFs]) --> isco_def(DEF), !, parse(DEFs).
parse([]) --> [].

% == parse a definition =======================================================

isco_def(external(NAME, METHOD)) --> [ external(NAME, METHOD) / _ ], !.
isco_def(class(NAME, ATTRs, SUPERs, DEF)) -->
	ic_head(NAME, HATTRs, SUPERs), !,
	ic_fields(DEF, DEF1),
	ic_attrs(ATTRs, HATTRs),
	isco_data(NAME, DEF1, DEF2),
	isco_rules(NAME, DEF2, []).
isco_def(sequence(NAME, ATTRs)) -->
	[ sequence(ATTRx, NAME) / _ ],
	{ is_attrs(ATTRx, ATTRs) },
	!.
isco_def(sequence(NAME, ATTRs)) -->
	[ sequence(NAME) / _ ],
	is_attrs(ATTRs),
	!.
isco_def(clause(HEAD, BODY)) --> [ (HEAD :- BODY) / _ ], !.
isco_def(directive(GOAL)) --> [ (:- GOAL) / _ ], !.
isco_def(clause(HEAD, true)) --> [ HEAD / _ ].

% -- parse the head of a class definition -------------------------------------

ic_head(N, As, SCs) --> [ (Ax class N : Ss) / _], !,
	{ ic_attrs(Ax, As), ic_supers(Ss, SCs) }.
ic_head(N, [], SCs) --> [ (class N : Ss)   / _ ], !, { ic_supers(Ss, SCs) }.
ic_head(N, As, [])  --> [ (Ax class N)   / _   ], !, { ic_attrs(Ax, As)   }.
ic_head(N, [], [])  --> [ (class N)   / _      ], !.

% -- parse the fields in a class definition -----------------------------------
%
% **WARNING** the fields must come in reverse order, so that we can later
% stick inherited field lists at the end of the subclass field lists and share
% the data structures.
%

ic_fields([F|Fs], Fx) --> ic_field(F), !, ic_fields(Fs, Fx).
ic_fields(Fs, Fs)     --> [].


ic_field(field(NAME, int, ATTRs)) -->
	[ (field NAME : DEC_TYPE) / _VNs ],
	{ DEC_TYPE =.. [serial | ARGS] },
	!,
	{ ic_field_sequence(ARGS, ATTRs, XATTRs) },
	if_attrs(XATTRs).
ic_field(field(NAME, TYPE, ATTRs)) -->
	[ (field NAME : TYPE) / _VNs ],
	{ atom(TYPE) },
	!,
	{ type(TYPE) },
	if_attrs(ATTRs).
ic_field(field(NAME, _TYPE, ATTRs)) -->
	[ (field NAME: C.F) / _VNs ],
	{ (atom(C) ; C=..[_,_]), atom(F) },
	!,
	{ ATTRs = [internal(C, F) | OATTRs] },
	if_attrs(OATTRs).
ic_field(field(NAME, _TYPE, ATTRs)) -->
	[ (field NAME: [V|Vs]) / _VNs ],
	!,
	{ ATTRs = [constant([V|Vs]) | OATTRs] },
	if_attrs(OATTRs).
ic_field(field(NAME, int, ATTRs)) -->
	[ (NAME : DEC_TYPE) / _VNs ],
	{ DEC_TYPE =.. [serial | ARGS] },
	!,
	{ ic_field_sequence(ARGS, ATTRs, XATTRs) },
	if_attrs(XATTRs).
ic_field(field(NAME, TYPE, ATTRs)) -->
	[ (NAME : TYPE) / _VNs ],
	{ atom(TYPE) },
	!,
	{ type(TYPE) },
	if_attrs(ATTRs).
ic_field(field(NAME, _TYPE, ATTRs)) -->
	[ (NAME: C.F) / _VNs ],
	{ (atom(C) ; C=..[_,_]), atom(F) },
	!,
	{ ATTRs = [internal(C, F) | OATTRs] },
	if_attrs(OATTRs).
ic_field(field(NAME, _TYPE, ATTRs)) -->
	[ (NAME: [V|Vs]) / _VNs ],
	!,
	{ ATTRs = [constant([V|Vs]) | OATTRs] },
	if_attrs(OATTRs).
ic_field(field(NAME, _TYPE, ATTRs)) --> [ (field NAME) / _VNs],
	if_attrs(ATTRs).

if_attrs([A|As]) --> if_attr(A), !, if_attrs(As).
if_attrs(_) --> [].				% open tail...

if_attr(key) --> [ (key) / _VNs ].		% unique index
if_attr(index) --> [ (index) / _VNs ].		% non-unique index
if_attr(indexed) --> [ (index) / _VNs ].	% non-unique index
if_attr(internal(CLASS, FIELD)) -->
	[ (domain CLASS.FIELD) / _VNs ],
	{ atom(CLASS), atom(FIELD) }, !.
if_attr(constant([V|Vs])) -->
	[ (domain [V|Vs]) / _VNs ].
if_attr(not_null)   --> [ not_null / _VNs ].
if_attr(default(V)) --> [ default(V) / _VNs ].
if_attr(unique)     --> [ unique / _VNs ].

% -- parse the extra attributes (eg. "key") in a class definition -------------

ic_attrs([A|As], T) --> [ A / _VNs ], { ic_attr(A) }, !, ic_attrs(As, T).
ic_attrs(As, As) --> [].

ic_attr(key(_KEY)).				% unique index
ic_attr(index(_KEY)).				% non-unique index
ic_attr(mutable).
ic_attr(append).
ic_attr(static).
ic_attr(final).
ic_attr(external(_REF)).

% -- make up the sequence attributes in a field declaration -------------------

ic_field_sequence([], [sequence(new)|As], As) :- !.
ic_field_sequence([NAME], [sequence(external(NAME))|As], As) :- atom(NAME), !.
ic_field_sequence([CN.FN], [sequence(ref(CN, FN))|As], As) :- !.

% -- parse the extra attributes in a sequence definition ----------------------

is_attrs([A|As]) --> [ A / _VNs ], { is_attr(A) }, !, is_attrs(As).
is_attrs([AA|As]) --> [ A / _VNs ], { is_attr(A,AA) }, !, is_attrs(As).
is_attrs([]) --> [].

is_attrs((A,Ax), [A|As]) :- is_attr(A), !, is_attrs(Ax, As).
is_attrs((AA,Ax), [A|As]) :- is_attr(AA,A), !, is_attrs(Ax, As).
is_attrs(A, [A]) :- is_attr(A), !.
is_attrs(AA, [A]) :- is_attr(AA, A), !.

is_attr(external(_DBREF)).
is_attr(external(_DBREF,_SEQ)).
is_attr(min(_MIN)).
is_attr(max(_MAX)).
is_attr(inc(_INC)).
is_attr(start(_START)).
is_attr(cache(_CACHE)).
is_attr(cycle).

is_attr(ext(DBREF), external(DBREF)).
is_attr(ext(DBREF,SEQ), external(DBREF,SEQ)).
is_attr(minimum(MIN), min(MIN)).
is_attr(maximum(MAX), max(MAX)).
is_attr(increment(INC), inc(INC)).
is_attr(step(INC), inc(INC)).

% == parse a data definition ==================================================

isco_data(CLASS, [data(CLASS, FIELDs, VALUEs)|DEFs], DEFs) -->
	id_head(FIELDs),
	id_items(FIELDs, VALUEs), !.
isco_data(_CLASS, DEFs, DEFs) --> [].

id_head(_FIELDs) --> [ (data) / _VNs ], !.
id_head(FIELDs) -->  [ DATA / _VNs ], { DATA =.. [data|FIELDs] }.

id_items(_Fs, []) --> [ end / _VNs ], !.
id_items(Fs,  [Vs|Vss]) -->
	[ Vx / _VNs ],
	{ id_item_line(Fs, Vx, Vs) },
	id_items(Fs, Vss).

id_item_line([F|Fs], (V,Vx), [F=V|Vs]) :- !, id_item_line(Fs, Vx, Vs).
id_item_line([F], V, [F=V]).

% == parse a rule definition ==================================================

isco_rules(_C, [rule(select, BODY, VARIABLES)|DEFs], FDEFs) -->
	[ (rule :- BODY) / VARIABLES ], !,
	isco_rules(_C, DEFs, FDEFs).
isco_rules(_C, [rule(insert, BODY, VARIABLES)|DEFs], FDEFs) -->
	[ (rule :+ :- BODY) / VARIABLES ], !,
	isco_rules(_C, DEFs, FDEFs).
isco_rules(_C, [rule(delete, BODY, VARIABLES)|DEFs], FDEFs) -->
	[ (rule :\ :- BODY) / VARIABLES ], !,
	isco_rules(_C, DEFs, FDEFs).
isco_rules(_C, [rule(update, BODY, VARIABLES)|DEFs], FDEFs) -->
	[ (rule @:= :- BODY) / VARIABLES ], !,
	isco_rules(_C, DEFs, FDEFs).
isco_rules(_C, DEFs, DEFs) --> [].

% -- Utilities ----------------------------------------------------------------

ic_attrs([X|Y], [X|Y]) :- !.
ic_attrs((A,B), [A|Bs]) :- !, ic_attrs(B, Bs).
ic_attrs(X, [X]).

ic_supers(X, [X]) :- atom(X), !.
ic_supers(X, X).

type(int).
type(float).
type(bool).
type(text).
type(term).
type(date).
type(datetime).
type(dt).					% same as 'datetime'
%type(serial).				% like 'int', with default
type(X) :- var(X), !.
type(X) :- format("illegal ISCO type: ~w~n", [X]).

% $Log$
% Revision 1.4  2003/09/23 12:28:21  spa
% WIP: update for computed classes.
% fix term type: should not create atoms!
%
% Revision 1.3  2003/04/15 15:03:31  spa
% - rules now have a type (select, delete, insert)
%
% Revision 1.2  2003/03/07 09:59:58  spa
% term type.
% delete done as select(oid)+delete(oid).
%
% Revision 1.1.1.1  2003/01/06 14:27:17  spa
% Imported into CVS
%
% Revision 1.18  2001/08/24 18:16:49  spa
% Take note of "index" attributes (class and field).
%
% Revision 1.17  2001/06/13 08:45:56  spa
% Support for "serial" argument type (automatic sequence).
% Argument attribute list now open-ended.
% Support simplified syntax for arguments (no "field" or "arg" keyword).
%
% Revision 1.16  2001/05/22 14:38:18  spa
% Terminal attributes for sequences must return a LIST of attributes,
% not the attribute itself!
%
% Revision 1.15  2001/05/19 01:09:13  spa
% Allow step(X) as a synonym for inc(X) in sequences.
%
% Revision 1.14  2001/05/19 01:03:49  spa
% Allow alternate syntax for sequence attributes (infix).
%
% Revision 1.13  2001/05/19 00:55:40  spa
% Sequences: exchange order of DBREF,SEQ in external/2.
%
% Revision 1.12  2001/05/19 00:49:57  spa
% Initial model and implementation for sequences.
%
% Revision 1.11  2001/05/03 13:47:41  spa
% Deal with clauses and directives in the source.
%
% Revision 1.10  2001/04/23 22:10:25  spa
% New attribute: static.
%
% Revision 1.9  2001/04/22 21:54:14  spa
% Parse 'mutable' and 'append' class attributes.
%
% Revision 1.8  2001/04/19 12:52:40  spa
% Copyright.
%
% Revision 1.7  2001/04/16 22:31:26  spa
% Typos.
% Support "external" declarations.
% More built-in "SQL compatible" types.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
