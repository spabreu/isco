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

% == ISCO Abstract Parse Tree analysis ========================================

% General information:
%
% Symbol table structure:
%
%  open list of pairs NAME=VALUE
%    NAME may be any term
%    VALUE also, in particular it may be another symbol table
%
%
% Topmost symbol table has classes as NAMEs and other symbol tables as VALUEs.
% Some particular values:
% 
% fields=FList
%    where FList is a (open, in the case of subclasses) list of fields
% Each field (member of FList) is:
%  f(ORDER, NAME, TYPE, ATTRS), where:
%    ORDER is unbound or an integer (the position) for serializing fields
%    NAME  is the name of the field
%    TYPE  is the type of the field.  This is an ISCO basic type.
%    ATTRS is a list of attributes, which includes:
%      key if this field is a key to the relation
%      constant(VALUES) if this field must map to the indicated values:
%       VALUES is a list of (atomic) values.
%      internal(CLASS, FIELD) if this field must belong to CLASS.FIELD.
%      hidden if this field is to be hidden from regular uses
%      skip if this field is *not* to be counted
%
% super=SUPERCLASSNAME
% name=CLASSNAME
% 
% atrib=ATTRIBUTE_LIST
%  Where ATTRIBUTE_LIST is a list of class attributes (see parser.pl)
%
% rules=RULE_LIST
% prules=RULE_LIST
%  Rules.  First form is raw from source; second is after de-iscofication.

:- include(ops).

isco_apt(APT, ST) :-
	isco_apt_names(APT, ST),		% basic name analysis
	isco_apt_inherits(ST, ST),		% inheritance (fill in fields)
	isco_apt_zap_fields(ST), 		% 
	isco_apt_types(ST, ST),			% type checking
	isco_apt_number(ST),			% number fields
	isco_apt_rules(ST, ST).			% rule pre-processing

% -- Construct the symbol table and perform name analysis from the APT --------

isco_apt_names([], _ST) :- !.
isco_apt_names([C|Cs], ST) :-
	isco_apt_name(C, ST),
	isco_apt_names(Cs, ST).


% == Analysis for top-level nodes =============================================

isco_apt_name(external(NAME, METHOD), ST) :-
	insert(ST, external(NAME)=METHOD),	% special name to avoid...
	!.					% ...namespace collision.

isco_apt_name(class(NAME, ATTRs, SUPERs, DEFs), ST) :-
	insert(ST, NAME=NET),			% NET: Name Entry Symbol Table
	!,
	insert(NET, name=NAME),			% redundant but convenient...
	insert(NET, super=SUPERs),
	insert(NET, atrib=ATTRs),
	insert(NET, fields=FIELDs),
	insert(NET, rules=_RULEs),
	isco_apt_field_defs(DEFs, NAME, NET, ST),
	isco_apt_default_fields(SUPERs, FIELDs, NAME),
	isco_apt_name_defs(DEFs, NAME, NET, ST).

isco_apt_name(clause(HEAD, BODY), ST) :-
	!,
	( lookup(ST, special(clauses), special(clauses)=CLST) -> true
	; insert(ST, special(clauses)=CLST) ),
	ol_add(CLST, clause(HEAD, BODY, _)).

isco_apt_name(directive(GOAL), ST) :-
	!,
	( lookup(ST, special(directives), special(directives)=DST) -> true
	; insert(ST, special(directives)=DST) ),
	ol_add(DST, directive(GOAL, _)).

isco_apt_name(sequence(NAME, ATTRs), ST) :-
	!,
	insert(ST, sequence(NAME)=ATTRs).

isco_apt_name(_, _).				% anything else: ignore?

% == Analysis for DEF parts of nodes (fields, data, rules...) =================

isco_apt_field_defs([DEF|DEFs], CNAME, NET, ST) :-
	isco_apt_field_defs(DEFs, CNAME, NET, ST),
	isco_apt_field_def(DEF, CNAME, NET, ST).
isco_apt_field_defs([], _CNAME, _NET, _ST).


isco_apt_name_defs([DEF|DEFs], CNAME, NET, ST) :-
	isco_apt_name_def(DEF, CNAME, NET, ST),
	isco_apt_name_defs(DEFs, CNAME, NET, ST).
isco_apt_name_defs([], _CNAME, _NET, _ST).


% -- Fill in default fields ---------------------------------------------------

% Don't do it if this has superclasses.

isco_apt_default_fields(SUPERs, _, _) :- var(SUPERs), !.
isco_apt_default_fields([_|_], _, _) :- !.	% it's a subclass: inherit
isco_apt_default_fields(_, FST, _CNAME) :-	% not a subclass: extra fields.
	insert(FST, f(2, instanceOf, text, [hidden=yes|_])),
	insert(FST, f(1, oid, int, [hidden=yes|_])).


% -- field DEF for field node -------------------------------------------------

isco_apt_field_def(field(FNAME, FTYPE, FATTRIB), CNAME, NET, ST) :- !,
	lookup(NET, fields, fields=FST),	% *** TBD ***
	insert(FST, f(_ORDER, FNAME, FTYPE, FATTRIB)),
	isco_apt_field_attrib_defs(FATTRIB, CNAME, FNAME, NET, ST).

isco_apt_field_def(_, _CNAME, _NET, _ST).


isco_apt_field_attrib_defs(EOF, _CNAME, _FNAME, _NET, _ST) :- var(EOF), !.
%isco_apt_field_attrib_defs([], _CNAME, _FNAME, _NET, _ST) :- !.
isco_apt_field_attrib_defs([A|As], CNAME, FNAME, NET, ST) :-
	isco_apt_field_attrib_def(A, As, CNAME, FNAME, NET, ST),
	isco_apt_field_attrib_defs(As, CNAME, FNAME, NET, ST).


isco_apt_field_attrib_def(sequence(SEQT), AsT, CNAME, FNAME, NET, ST) :-
	isco_apt_field_seq_attrib_def(SEQT, AsT, CNAME, FNAME, NET, ST).
isco_apt_field_attrib_def(key,             _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(index,           _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(internal(_C,_F), _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(constant(_VS),   _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(not_null,        _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(default(_V),     _AsT, _CNAME, _FNAME, _NET, _ST).
isco_apt_field_attrib_def(unique,          _AsT, _CNAME, _FNAME, _NET, _ST).


isco_apt_field_seq_attrib_def(new, AsT, CNAME, FNAME, _NET, ST) :-
	format_to_atom(SN, "is__~w_~w", [CNAME, FNAME]),
	insert(ST, sequence(SN)=[]),
	ol_add(AsT, default(nextval(SN))).
isco_apt_field_seq_attrib_def(ref(CN,FN), AsT, _CNAME, _FNAME, _NET, _ST) :-
%%% FIXME: Check that CN.FN is indeed a sequence and get its real name!
	format_to_atom(SN, "is__~w_~w", [CN, FN]),
	ol_add(AsT, default(nextval(SN))).
isco_apt_field_seq_attrib_def(external(SN), AsT, _CNAME, _FNAME, _NET, ST) :-
	( lookup(ST, sequence(SN), _=SEQAs) ->
	    ( ol_memberchk(external(_,SN_X), SEQAs) -> true ; SN_X=SN )
	; insert(ST, sequence(SN)=[]), SN_X=SN ),
	ol_add(AsT, default(nextval(SN_X))).

% -- name DEF for field node --------------------------------------------------

isco_apt_name_def(field(_FNAME, _FTYPE, _FATTRIB), _CNAME, _NET, _ST) :- !.

% -- DEF for data node --------------------------------------------------------

isco_apt_name_def(data(_CLASS, FIELDs, VALUEs), _CNAME, NET, _ST) :-
	insert(NET, data(FIELDs, VALUEs)).

% -- DEF for rule node --------------------------------------------------------

isco_apt_name_def(rule(B, V), _CNAME, NET, _ST) :-
	lookup(NET, rules, rules=Rs),
	isco_apt_add_rule(Rs, rule(B,V)).

isco_apt_add_rule(Rs, R) :- var(Rs), !, Rs=[R|_].
isco_apt_add_rule([_|Rs], R) :- isco_apt_add_rule(Rs, R).

% == Preprocess rule nodes into proper clauses ================================

isco_apt_rules(X, _ST) :- var(X), !.
isco_apt_rules([external(_NAME)=_EXT|Ds], ST) :- !,
	isco_apt_rules(Ds, ST).
isco_apt_rules([sequence(_NAME)=_DEF|Ds], ST) :- !,
	isco_apt_rules(Ds, ST).
isco_apt_rules([special(_TYPE)=CST|Ds], ST) :- !,
	isco_special(CST, ST),
	isco_apt_rules(Ds, ST).
isco_apt_rules([NAME=NET|Ds], ST) :-
	lookup(NET, rules, rules=Rs),
	insert(NET, prules=_RT),
	isco_apt_rule_list(Rs, NAME, NET, ST), !,
	isco_apt_rules(Ds, ST).

isco_apt_rule_list(ERs, _, _, _) :- var(ERs), !.
isco_apt_rule_list([], _, _, _).
isco_apt_rule_list([R|Rs], NAME, NET, ST) :-
	isco_apt_rule(NAME, R, NET, ST),
	!,
	isco_apt_rule_list(Rs, NAME, NET, ST).
isco_apt_rule_list([R|Rs], NAME, NET, ST) :-
	format("%% failing rule def: ~w~n", [R]),
	isco_apt_rule_list(Rs, NAME, NET, ST).

isco_apt_rule(NAME, rule(BODY, VARs), NET, ST) :-
	isco_field_vars(NET, FVARs),
	isco_merge_vars(VARs, FVARs),
	isco_rule_head(NAME, FVARs, NHEAD),
	isco_rule_body(BODY, ST, NBODY),
	lookup(NET, prules, prules=RT),
	insert(RT, rule(NHEAD, NBODY)).


isco_special(ESL, _ST) :- var(ESL), !.
isco_special([],  _ST).
isco_special([clause(_HEAD, IBODY, OBODY)|Cs], ST) :-
	isco_rule_body(IBODY, ST, OBODY),
	!,
	isco_special(Cs, ST).
isco_special([directive(IDIR, ODIR)|Ds], ST) :-
	isco_rule_body(IDIR, ST, ODIR),
	!,
	isco_special(Ds, ST).


% -- Process the head (create term with bound parameters) ---------------------

isco_rule_head(PRED, VARs, HEAD) :-
	isco_var_list(VARs, VL),
	HEAD =.. [PRED | VL].

isco_var_list(Vs, []) :- var(Vs), !.
isco_var_list([], []) :- !.
isco_var_list([_=V|Vs], [V|LO]) :- isco_var_list(Vs, LO).

% -- Process the body (locate the calls, take care of variables) --------------

isco_rule_body(VG, _ST, NVG) :- var(VG), !,	% variable goal.
	NVG = (isco_term_expansion(VG, VGx), VGx).

isco_rule_body((G1,G2), ST, (NG1,NG2)) :- !,
	isco_rule_body(G1, ST, NG1),
	isco_rule_body(G2, ST, NG2).

isco_rule_body((G1;G2), ST, (NG1;NG2)) :- !,
	isco_rule_body(G1, ST, NG1),
	isco_rule_body(G2, ST, NG2).

isco_rule_body((G1->G2), ST, (NG1->NG2)) :- !,
	isco_rule_body(G1, ST, NG1),
	isco_rule_body(G2, ST, NG2).

isco_rule_body(\+(G1), ST, \+(NG1)) :- !,
	isco_rule_body(G1, ST, NG1).

isco_rule_body(call(G1), ST, NG1) :- !,
	isco_rule_body(G1, ST, NG1).

isco_rule_body(catch(G1,E,G2), ST, catch(NG1,E,NG2)) :- !,
	isco_rule_body(G1, ST, NG1),
	isco_rule_body(G2, ST, NG2).

isco_rule_body(setof(A,G,As), ST, setof(A,NG,As)) :- !,
	isco_rule_body(G, ST, NG).

isco_rule_body(bagof(A,G,As), ST, bagof(A,NG,As)) :- !,
	isco_rule_body(G, ST, NG).

isco_rule_body(findall(A,G,As), ST, findall(A,NG,As)) :- !,
	isco_rule_body(G, ST, NG).

isco_rule_body(X^G, ST, X^NG) :- !,
	isco_rule_body(G, ST, NG).

isco_rule_body(G, ST, NG) :-
	functor(G, F, _A),			% is this goal for...
	lookup(ST, F, F=NET),			% ...an ISCO predicate?
	!,
	isco_rule_rw_goal(G, NET, NG).

isco_rule_body(B, _, B).			% fallback to no changes.


% -- Extract the variable names for the fields --------------------------------

isco_field_vars(NET, FVARs) :-
	lookup(NET, fields, fields=FIELDs),
	isco_field_var_list(FIELDs, [], FVARs).

isco_field_var_list(V, L, L) :- var(V), !.
isco_field_var_list([], L, L).
isco_field_var_list([f(_,Vn,_T,_A)|Fs], LI, LO) :-
	lower_upper_atom(Vn, VN),
	isco_field_var_list(Fs, [VN=_|LI], LO).

% -- Merge the field and rule variables ---------------------------------------

isco_merge_vars([], _) :- !.
isco_merge_vars([N=V|FVs], Vs) :-
	lookup(Vs, N, N=V), !,
	isco_merge_vars(FVs, Vs).
isco_merge_vars([_N=_V|FVs], Vs) :-
	isco_merge_vars(FVs, Vs).

% -- Rewrite a call to an ISCO predicate --------------------------------------
%
% Input: goal in non-positional format, 
% Output: regular goal

isco_rule_rw_goal(GOAL, NET, NGOAL) :-
	lookup(NET, fields, fields=Fs),
	ol_length(Fs, NFs),
	GOAL=..[F|As],				% functor & input args
	functor(IGOAL, F, NFs),			% skel for output goal
	isco_rule_rw_goal_args(As, Fs, 1, _MODE, IGOAL, USED, ORDER, 0, MASK),
	( ORDER=[], MASK=0 ->
	    NGOAL=IGOAL
	; IGOAL =.. IGOAL_,
	  isco_order_clause(ORDER, ORDER_CLAUSE),
	  append(IGOAL_, [ORDER_CLAUSE+MASK], NGOAL_),
	  NGOAL =.. NGOAL_ ),
	isco_rule_rw_fill_blanks(USED, NFs, NGOAL).

isco_rule_rw_goal_args(As, _, _, _, _, [], [], M, M) :- var(As), !.
isco_rule_rw_goal_args([], _, _, _, _, [], [], M, M) :- !.
isco_rule_rw_goal_args([A|As], Fs, _, nonpos, NGOAL, [P|Ps], O, M, Mo) :-
	nonvar(A),
	( isco_order_by_item(A, FNAME=FVALUE, Oi) -> O = [Oi|Os]
	; A=(FNAME=FVALUE), O=Os ),
	!,
	ol_close(Fs),
	( ol_memberchk(f(P,FNAME,_TYPE,_ATTRs), Fs) ->
	    arg(P, NGOAL, FVALUE),
	    Mi is M \/ (1 << P)
	; isco_error("illegal field name in rule body: ~w", [FNAME]), Mi=M ),
	isco_rule_rw_goal_args(As, Fs, _, nonpos, NGOAL, Ps, Os, Mi, Mo).
isco_rule_rw_goal_args([A|As], _Fs, N, pos, NGOAL, [N|Ps], [], _, 0) :- !,
	arg(N, NGOAL, A),
	N1 is N+1,
	isco_rule_rw_goal_args(As, _Fs, N1, pos, NGOAL, Ps, [], 0, _).

isco_rule_rw_fill_blanks(L, NF, G) :-
	format("%% fill_blanks(~w, ~w, ~w).~n", [L, NF, G]).

%isco_rule_rw_fill_blanks([], 0, _) :- !.
%isco_rule_rw_fill_blanks([], N, T) :-
%	arg(N, T, special(no_arg)),
%	N1 is N-1,
%	isco_rule_rw_fill_blanks([], N1, T).
%isco_rule_rw_fill_blanks([P|


% == Perform inheritance propagation on the symbol table ======================
%
% usage: isco_apt_inherits(?VST, -ST)
%  VST is initially ST, but will be traversed
%  ST is the top-level symbol table, for reference
%
% all entries will have their field lists updated as per the inheritance
% characteristics.
%
% as a side-effect, every class will be inserted in its superclass as an extra
% symbol-table contained in the 'subs' attribute.

isco_apt_inherits(VST, _ST) :- var(VST), !.
isco_apt_inherits([K=V|Es], ST) :-
	isco_apt_inherit(K, V, ST),
	isco_apt_inherits(Es, ST).


isco_apt_inherit(NAME, NET, ST) :-		% inherit for some class
	lookup(NET, super, super=[SUPER]),
	!,
	lookup(ST, SUPER, SUPER=SUPERdef),
	lookup(SUPERdef, fields, fields=SUPERs),
	( lookup(SUPERdef, subs, subs=DESC) -> true ;
	    insert(SUPERdef, subs=DESC) ),
	insert(DESC, NAME),			% mark as subclass
	lookup(NET, fields, fields=FIELDs),
	isco_inherit_supers(FIELDs, SUPERs).
isco_apt_inherit(_, _, _).

isco_inherit_supers(CFs, SFs) :- var(CFs), !, CFs=SFs.
isco_inherit_supers([_CF|CFs], SFs) :- isco_inherit_supers(CFs, SFs).


% == Mark useless fields as "skip" ============================================

isco_apt_zap_fields(ST) :- var(ST), !.
isco_apt_zap_fields([K=V|Es]) :-
	isco_apt_zap_fields(K, V),
	isco_apt_zap_fields(Es).

isco_apt_zap_fields(_NAME, NET) :-
	lookup(NET, subs, subs=DESC),
	\+ DESC=[],				% it's got subclasses
	lookup(NET, fields, fields=FLIST),
	ol_memberchk(f(_,instanceOf,_,ALIST), FLIST),
	member(skip=yes, ALIST), !.
isco_apt_zap_fields(_, _).



% == Perform type analysis and checking on the symbol table ===================

isco_apt_types(ST, _) :- var(ST), !.
isco_apt_types([E|Es], ST) :- isco_type(E, ST), !, isco_apt_types(Es, ST).



isco_type(external(_)=_, _ST) :- !.
isco_type(special(_)=_, _ST) :- !.
isco_type(sequence(_)=_, _ST) :- !.
isco_type(C=NET, ST) :-
	lookup(NET, fields, fields=LFs),
	isco_field_types(LFs, C, ST).

isco_field_types([], _, _).
isco_field_types([F|Fs], C, ST) :-
	isco_field_type(F, C, ST), !,
	isco_field_types(Fs, C, ST).

isco_field_type(f(_,_,T,_), _, _) :- nonvar(T), !.
isco_field_type(f(_,F,T,A), C, ST) :-
	ol_memberchk(internal(CLASS, FIELD), A), !,
	isco_internal_domain_field_type(T, CLASS, FIELD, C, F, ST).
isco_field_type(f(_,_,T,A), _, _) :-
	ol_memberchk(constant(VALUEs), A), !,
	VALUEs = [V|_],
	isco_literal_type(V, T).
isco_field_type(f(_,N,T,_A), C, _) :-
	isco_default_type(T),
	format("%% untyped field ~w.~w (default '~w' used)~n", [C, N, T]).


isco_internal_domain_field_type(T, CLASS, FIELD, C, F, ST) :-
	lookup(ST, CLASS, CLASS=NET),
	( lookup(NET, super, super=S) ; S=[] ), !,
	isco_internal_domain_in_classes([CLASS|S], CLASS, FIELD, T, C, F, ST).
isco_internal_domain_field_type(_T, CLASS, FIELD, C, F, _ST) :-
	format("%% unknown class in internal domain: ~w.~w: <~w>.~w~n",
	           [C, F, CLASS, FIELD]).

isco_internal_domain_in_classes([], CLASS, FIELD, _, C, F, _) :-
	format("%% unknown field in internal domain: ~w.~w: <~w.>~w)~n",
		[C, F, CLASS, FIELD]), !, fail.
isco_internal_domain_in_classes([CL|_CLs], _CLASS, FIELD, T, _C, _F, ST) :-
	lookup(ST, CL, CL=NET),
	lookup(NET, fields, fields=LFs),
	ol_memberchk(f(_, FIELD, T, _), LFs), !.
isco_internal_domain_in_classes(CLASSES, CLASS, FIELD, _T, C, F, _ST) :-
	var(CLASSES), !,
	format("%% unknown class in internal domain: ~w.~w: <~w.>~w)~n",
		[C, F, CLASS, FIELD]), !, fail.
isco_internal_domain_in_classes([_CL|CLs], CLASS, FIELD, T, C, F, ST) :-
	isco_internal_domain_in_classes(CLs, CLASS, FIELD, T, C, F, ST).

% == Number arguments in relations ============================================

isco_apt_number(ST) :- var(ST), !.
isco_apt_number([E|Es]) :- isco_number(E), !, isco_apt_number(Es).


isco_number(external(_)=_) :- !.
isco_number(special(_)=_) :- !.
isco_number(sequence(_)=_) :- !.
isco_number(CNAME=NET) :-
	lookup(NET, fields, fields=LFs),
	isco_field_numbers(LFs, 0, _, CNAME).

isco_field_numbers([], N, N, _).
% isco_field_numbers([F|Fs], N, N1, CNAME) :-
% 	F=(_,_,_,FAs), lookup(FAs, skip, _), !, % marked to be skipped?
% 	isco_field_numbers(Fs, N, N1, CNAME). % if so, ignore
isco_field_numbers([F|Fs], N, KF, CNAME) :-
	isco_field_numbers(Fs, N, N1, CNAME),
	F=f(KF,NAME,TYPE,ATTRs),
	( memberchk(f(K,NAME,TYPE,ATTRs2), Fs) ->
	    KF=K,				% repeated field (default?)
	    insert(ATTRs, dupe=yes),
	    member(dupedIn=DUPEDIN, ATTRs2),
	    insert(DUPEDIN, CNAME)		% remember WHO redefines it.
	;
	    KF is N1+1,				% genuine new field...
	    isco_field_number(F, KF, CNAME) ).

isco_field_number(f(N,_,_,_), N, _) :- !.
isco_field_number(f(K,NAME,TYPE,_ATTRS), N, CNAME) :-
	format("%% attempt to renumber field ~w (~w.~w: ~w) as ~w~n",
		[K, CNAME, NAME, TYPE, N]).

% == Utilities ================================================================

isco_literal_type(X, int) :- integer(X), !.
isco_literal_type(dt(Y,M,D), date) :- integer(Y), integer(M), integer(D), !.
isco_literal_type(dt(Y,M,D,HH,MM,SS), datetime) :-
	integer(Y), integer(M), integer(D),
	integer(HH), integer(MM), integer(SS), !.
isco_literal_type(X, text) :- atom(X), !.
isco_literal_type(X, codes) :- list(X), !.
isco_literal_type(_, term).


isco_default_type(text).


isco_nondupe_fields(Fs, FF) :- isco_skip_fields(Fs, dupe=yes, FF).
isco_skip_fields(Fs, FF)    :- isco_skip_fields(Fs, skip=yes, FF).


isco_skip_fields(Fs, ATTR, FF) :-
	findall(F, (ol_member(F, Fs),
		       F=f(_,_,_,AL),
		       \+ ol_memberchk(ATTR, AL)), FF).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_database_type(C, DBTYPE, ST) :-
	lookup(ST, C, C=NET),
	lookup(NET, atrib, atrib=As),
	( ol_memberchk(external(EXT, _), As) ->
	    lookup(ST, external(EXT), external(EXT)=DBTYPE)
	; ol_memberchk(computed, As) -> DBTYPE=computed
	; DBTYPE=isco ),
	!.

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

lower_upper_atom(LC, UC) :-
	name(LC, LCL),
	lower_upper_string(LCL, UCL),
	name(UC, UCL).

lower_upper_string([LC|LCs], [UC|UCs]) :-
	islower(LC), UC is LC-32, !,
	lower_upper_string(LCs, UCs).
lower_upper_string([NL|LCs], [NL|UCs]) :-
	lower_upper_string(LCs, UCs).
lower_upper_string([], []).


isco_display_st(T) :- isco_display_st(T, 0).

isco_display_st(ST, _) :- var(ST), !.
isco_display_st([], _) :- !.
isco_display_st([N=V|Ss], L) :- !,
	format("~n~*c~w:", [L*3, 32, N]),	% indent
	isco_display_st_v(V, L),
	isco_display_st(Ss, L).
isco_display_st([S|Ss], L) :- !,
	format("~n~*c~w", [L*3, 32, S]),	% indent
	isco_display_st(Ss, L).

isco_display_st_v(VV, _) :- var(VV), !.
isco_display_st_v(Vs, L) :- functor(Vs, '.', 2), !,
	L1 is L+1,
	isco_display_st(Vs, L1).
isco_display_st_v(V, _) :- format(" ~w", [V]).

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% outline-regexp: "% \\(--\\|==\\) "
% End:
