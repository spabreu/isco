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

% == Prolog code generation (clauses) for ISCO ================================

:- unit(isco_prolog(ST)).

emit :- DASH="=",
	format_to_atom(C, "%% ~2c ISCO clause definitions. ", [DASH]),
	atom_length(C, CLENGTH),
	format("~n~w~*c~n~n", [C, 79-CLENGTH, DASH]),
	isco_prolog_code(ST).


isco_prolog_code(EST) :- var(EST), !.
isco_prolog_code([external(NAME)=METHOD|Ss]) :-
	atom_concat('isco_', NAME, ISCO_NAME),
	isco_prolog_external(METHOD, NAME, ISCO_NAME),
	!,
	isco_prolog_code(Ss).
isco_prolog_code([sequence(NAME)=ATTRs|Ss]) :-
	isco_prolog_sequence(NAME, ATTRs),
	!,
	isco_prolog_code(Ss).
isco_prolog_code([special(_TYPE)=SST|Ss]) :-
	isco_prolog_specials(SST, separate),
	!,
	isco_prolog_code(Ss).
isco_prolog_code([CNAME=NET|Ss]) :-
	isco_prolog_class(CNAME, NET),
	!,
	isco_prolog_code(Ss).

% -- Generate access code for any class ---------------------------------------

isco_prolog_class(CNAME, NET) :-		% external DB classes
	lookup(NET, atrib, atrib=As),
	ol_memberchk(external(XID, XNAME), As),
	lookup(NET, fields, fields=Fs),
	!,
	isco_nondupe_fields(Fs, FF),
	ol_length(FF, NFs),
	isco_prolog_class_header(external, CNAME, NET),
	( lookup(ST, external(XID), external(_)=XDEF) ->
	    atom_concat('isco_', XID, ISCO_XID),
	    isco_prolog_external_class(CNAME, NFs, FF, XDEF, ISCO_XID, XNAME),
	    isco_prolog_class_inheritance(CNAME, NET, XDEF),
	    isco_prolog_code_update(CNAME, NET, NFs, FF),
	    isco_prolog_code_delete(CNAME, NET, NFs, FF),
	    isco_prolog_code_insert(CNAME, NET, NFs, FF)
	; format("error: external database ~w not declared.~n", [XID]),
	  fail ).

isco_prolog_class(CNAME, NET) :-		% computed classes
	lookup(NET, prules, prules=RULES),
	nonvar(RULES),
	!,
	isco_prolog_class_header(computed, CNAME, NET),
	isco_prolog_computed_class(RULES),
	isco_prolog_class_inheritance(CNAME, NET).

isco_prolog_class(CNAME, NET) :-		% regular class
	lookup(NET, fields, fields=Fs),
	!,
	isco_nondupe_fields(Fs, FF),
	ol_length(FF, NFs), !,
	isco_prolog_class_header(regular, CNAME, NET),
	isco_prolog_internal_class(CNAME, NFs, Fs),
	isco_prolog_class_inheritance(CNAME, NET, isco),
	isco_prolog_code_update(CNAME, NET, NFs, FF),
	isco_prolog_code_delete(CNAME, NET, NFs, FF),
	isco_prolog_code_insert(CNAME, NET, NFs, FF),
	format('\n%% [end ~w]\n', [CNAME]).

% -- Generate code for general clauses and directives -------------------------

isco_prolog_specials(EST, _) :- var(EST), !.
isco_prolog_specials([],  _).
isco_prolog_specials([clause(HEAD, _, BODY)|Ss], FA) :-
	functor(HEAD, NF, NA), (NF/NA = FA -> true ; nl),
	portray_clause((HEAD :- BODY)), nl,
	isco_prolog_specials(Ss, NF/NA).
isco_prolog_specials([directive(_, _BODY)|Ss], _) :-
	isco_prolog_specials(Ss, separate).


% -- Generate code to access a computed relation ------------------------------

isco_prolog_class_header(TYPE, CNAME, NET) :-
	lookup(NET, super, super=SCNAME),
	lookup(NET, fields, fields=Fs),
	format_to_atom(COMMENT, "%% -- ~w class ~w. ", [TYPE, CNAME]),
	atom_length(COMMENT, CLENGTH),
	format("~n~w~*c~n", [COMMENT, 79-CLENGTH, "-"]),
	( SCNAME=[] -> true
	; ol_length(SCNAME, 1) -> format("%% superclass: ~w.~n", SCNAME)
	; format("%% superclass: ~w.~n", [SCNAME]) ),
	( lookup(NET, subs, subs=SUBCs) -> 
	    ol_close(SUBCs),
	    format("%% subclasses: ~w.~n", [SUBCs])
	; true ),
	isco_prolog_class_header_fields(Fs),
	nl.


isco_prolog_class_header_fields(Fs) :- var(Fs), !.
isco_prolog_class_header_fields([]) :- !.
isco_prolog_class_header_fields([f(NUM,NAME,TYPE,ATTRs)|Fs]) :-
	isco_prolog_class_header_fields(Fs), !,
	( lookup(ATTRs, dupe, dupe=yes) -> true
	;   format("%%    ~2w. ~w: ~w.~n", [NUM, NAME, TYPE]) ).


isco_prolog_computed_class(EOR) :- var(EOR), !.
isco_prolog_computed_class([]).
isco_prolog_computed_class([rule(HEAD,BODY) | Rs]) :-
	isco_prolog_computed_class_rule(HEAD, BODY),
	!,
	isco_prolog_computed_class(Rs).


isco_prolog_computed_class_rule(HEAD, BODY) :-
	portray_clause((HEAD :- BODY)), nl.

% -- Generate code to access a regular class ----------------------------------

isco_prolog_internal_class(CNAME, NFs, Fs) :-
	isco_prolog_class_head(CNAME, NFs, Fs, HEAD, VARs, OC_VAR),
	isco_prolog_class_body(VARs, CNAME, HEAD, BODY, CH, OC_VAR),
	HEAD =.. [HF | HA],
	HEAD1 =.. [HF, CH | HA],
	BODY1 = (isco_connection(CH), HEAD1),
	append(HEAD0_, [_], [HF|HA]), HEAD0 =.. HEAD0_,
	append(HEAD0_, [[]+0], BODY0_), BODY0 =.. BODY0_,
	HEAD0 =.. [HPx, _, _ | HAx],	% includes oid and instanceOf
	HEADx =.. [HPx | HAx],
	BODYx = HEAD0,
	portray_clause((HEADx :- BODYx)), nl,
	portray_clause((HEAD0 :- BODY0)), nl,
	portray_clause((HEAD :- BODY1)), nl,
	portray_clause((HEAD1 :- BODY)), nl.


isco_prolog_class_head(CNAME, NFs, Fs, HEAD, HVARs, OC_VAR) :-
	NFs1 is NFs+1,
	functor(HEAD, CNAME, NFs1),
	arg(NFs1, HEAD, OC_VAR),
	isco_prolog_ichf(Fs, HEAD, HVARs).

isco_prolog_ichf(EOF, _, _) :- var(EOF), !.
isco_prolog_ichf([], _, _).
isco_prolog_ichf([f(NUM,NAME,TYPE,_)|Fs], HEAD, HVARs) :-
	( lookup(HVARs, NUM, NUM=f(VAR,NAME,TYPE)) -> true
	; insert(HVARs, NUM=f(VAR,NAME,TYPE)) ),
	arg(NUM, HEAD, VAR), !,
	isco_prolog_ichf(Fs, HEAD, HVARs).


% -- isco_prolog_class_body(Vs, RNAME, HEAD, GOAL, CH, OC_VAR+MASK) -----------
%
% Generate the class head and body for the "select" function.  Parameters:
% Vs -> list of variables
%       ( from the symbol table, as a list of POS=f(VAR,NAME,TYPE)
% RNAME -> relation name
% HEAD, GOAL -> clause head and body, resp.
% CH -> back-end channel variable
% OC_VAR -> ORDER BY clause
% MASK -> output variable selection mask

isco_prolog_class_body(Vs, RNAME, HEAD, GOAL, CH, OC_VAR+MASK) :-
	functor(HEAD,CNAME,_),
	( isco_database_type(CNAME, DBTYPE, ST),
	  isco_auto_inheritance(DBTYPE, DESCEND) -> true ; DESCEND='' ),
	GOAL = (
	  ( MASK = 0 -> NMASK = -1 ; NMASK=MASK ),
	  MASK1 is NMASK /\ \6, % 6 is: 2 forced args (2^N-1)<<1
	  MASK2 is NMASK \/ 6,
	  isco_mask_to_var_list(HEAD, _, MASK2, VL),
	  isco_mask_to_var_list(HEAD, _, MASK1, VL1),
	  reverse(VL1, VL1r),
	  isco_var_list_to_select(VL1r, ", ", "", SELf),
	  format_to_codes(SQLin, 'select o.oid, c.relname as instanceOf~s from ~w~w o, pg_class c where c.oid=o.tableoid', [SELf, RNAME, DESCEND]),
	  G1 ),
	isco_where_clause(Vs, CH, G1, G2, SQLin, SQLout),
	G2 = (append(SQLout, OC_VAR, SQLfinal),
	      ( g_read(isco_debug_sql, 1) ->
		  format('sql: ~s~n', [SQLfinal]) ; true ),
	      isco_be_exec(CH, SQLfinal, SH),
	      isco_be_ntuples(CH, SH, NT),
	      g_assign(isco_ntuples, NT),
	      isco_be_fetch(CH, SH),
	      BODY),
	isco_prolog_class_body_fields(Vs, BODY, CH, SH, VL, MASK2).

isco_prolog_class_body_fields(EOV, true, _, _, _, _) :- var(EOV), !.
isco_prolog_class_body_fields([], true, _, _, _, _).
isco_prolog_class_body_fields([P=f(V,N,T)|VARs], GOAL, CH, SH, VL, MASK) :-
	isco_odbc_type(T, OT), odbc_type(OT, OTn),
	( isco_odbc_conv(T) -> CONV=yes ; CONV=no ),
	GOAL = (isco_be_get_arg(MASK, N, P, CH, SH, OTn, CONV, V, T, VL), Gs),
	isco_prolog_class_body_fields(VARs, Gs, CH, SH, VL, MASK).


% -- SQL WHERE clause generation ----------------------------------------------

isco_where_clause(Vs, C, Gin, Gout, SQLin, SQLout) :-
	isco_where_clause(Vs, C, 'and', _, Gin, Gout, SQLin, SQLout).
%	isco_where_clause(Vs, C, 'where', _, Gin, Gout, SQLin, SQLout).


isco_where_clause(Vs, _, P, P, G, G, WC, WC) :- var(Vs), !.
isco_where_clause([], _, P, P, G, G, WC, WC).
isco_where_clause([_P=f(V,N,T)|Vs], C, Pin, Pout, Gin, Gout, WCin, WCout) :-
	Gin = (isco_where_var(C, V, N, T, Pin, Pint, WCin, WCint), Gint),
	isco_where_clause(Vs, C, Pint, Pout, Gint, Gout, WCint, WCout).



% -- Generate code to access an external database -----------------------------

isco_prolog_external_class(CNAME, NFs, Fs, _XDEF, XID, XNAME) :-
	isco_prolog_class_head(CNAME, NFs, Fs, HEAD, VARs, OC_VAR),
	isco_prolog_class_body(VARs, XNAME, HEAD, BODY, CH, OC_VAR),
	HEAD =.. [HF | HA],
	HEAD1 =.. [HF, CH | HA],
	BODY1 = (isco_connection(XID, CH), HEAD1),
	append(HEAD0_, [_], [HF|HA]), HEAD0 =.. HEAD0_,
	append(HEAD0_, [[]+0], BODY0_), BODY0 =.. BODY0_,
	HEAD0 =.. [HPx, _, _ | HAx],	% includes oid and instanceOf
	HEADx =.. [HPx | HAx],
	BODYx = HEAD0,
	portray_clause((HEADx :- BODYx)), nl,
	portray_clause((HEAD0 :- BODY0)), nl,
	portray_clause((HEAD :- BODY1)), nl,
	portray_clause((HEAD1 :- BODY)), nl.


% -- Generate code for external database declarations -------------------------

isco_prolog_external(SPEC, NAME, ISCO_NAME) :-
	format_to_atom(COMMENT, "%% -- Access to ~w external database. ",
			    [NAME]),
	atom_length(COMMENT, CLENGTH),
	format("~n~w~*c~n", [COMMENT, 79-CLENGTH, "-"]),
	isco_prolog_external(SPEC, ISCO_NAME, HEAD, BODY),
	nl,
	portray_clause((HEAD :- BODY)),
	nl.


isco_prolog_external(odbc(DB), ISCO_NAME, HEAD, BODY) :-
	HEAD=isco_setup_connection(ISCO_NAME, odbc(H)),
	BODY=odbc_connect(DB, H).

isco_prolog_external(odbc(DB,_METHOD), ISCO_NAME, HEAD, BODY) :-
	HEAD=isco_setup_connection(ISCO_NAME, odbc(H)),
	BODY=odbc_connect(DB, H).

isco_prolog_external(postgres(DB), ISCO_NAME, HEAD, BODY) :-
	HEAD=isco_setup_connection(ISCO_NAME, pg(H)),
	name(DB, DBCODES),
	BODY=pq_open("", 0, DBCODES, H).

isco_prolog_external(postgres(DB, HOST), ISCO_NAME, HEAD, BODY) :-
	HEAD=isco_setup_connection(ISCO_NAME, pg(H)),
	name(DB, DBCODES),
	name(HOST, HOSTCODES),
	BODY=pq_open(HOSTCODES, 0, DBCODES, H).


% -- Generate code to insert a new tuple --------------------------------------

isco_prolog_code_insert(CNAME, NET, NFs, Fs) :-
%%DBG	format('%% [~w]\n', [isco_prolog_code_insert(CNAME,NET,NFs,Fs)]), %%DBG
	lookup(NET, atrib, atrib=As),
	\+ ol_memberchk(static, As),		% only static is out...
	( ol_memberchk(external(XID, XNAME), As) ; XID=isco, XNAME=CNAME ),
	!,
%	isco_skip_fields(Fs, skip=yes, Fs1),	% w/o instanceOf
	isco_skip_fields(Fs, hidden=yes, Fs2),	% w/o instanceOf and oid
%%DBG	format('%% [isco_skip_fields IN: ~w]\n', [Fs]), %%DBG
%%DBG	format('%% [isco_skip_fields OUT1: ~w]\n', [Fs1]), %%DBG
%%DBG	format('%% [isco_skip_fields OUT2: ~w]\n', [Fs2]), %%DBG
%	NFs1 is NFs - 1,			% w/o instanceOf
	atom_concat('isco_insert__', CNAME, ISCO_NAME),
	functor(HEAD, ISCO_NAME, NFs),
	HEAD =.. [HF | HA], HEAD1 =.. [HF, CH | HA], % tack on channel
	( XID=isco ->
	    BODY1 = (isco_connection(CH), HEAD1)
	;   atom_concat('isco_', XID, ISCO_XID),
	    BODY1 = (isco_connection(ISCO_XID, CH), HEAD1) ),
	isco_prolog_class_argnames(Fs2, [], ANLIST),
	XNAME_ARGS =.. [XNAME | ANLIST],
	format_to_codes(Q0, 'insert into ~w values', [XNAME_ARGS]),
	BODY = (QPFX = Q0, BODY2),
	reverse(Fs, RFs),			% *CROCK*
	arg(1,HEAD,OID),			% **OBNOXIOUS CROCK**
	isco_prolog_code_insert_body(CNAME,NET,RFs,_,QPFX,HEAD,BODY2,CH,OID),
	nl,
	portray_clause((HEAD :- BODY1)), nl,
	portray_clause((HEAD1 :- BODY)), nl.
isco_prolog_code_insert(_, _, _, _, _).



isco_prolog_code_insert_body(_CN, _NET, [], _, Q, _HEAD, BODY, CH, OID) :-
	BODY = (append(Q, ")", QF),
		   ( g_read(isco_debug_sql, 1) ->
		       format('sql: ~s~n', [QF]) ; true ),
		   isco_be_exec(CH, QF, R),
		   isco_be_oid(CH, R, OID)).

isco_prolog_code_insert_body(CN, NET, [F|Fs], P, Q, HEAD, BODY, CH, O) :-
	F=f(POS,FNAME,TYPE,ATTRs),
	arg(POS, HEAD, Vin),
	( ol_memberchk(default(nextval(SEQ)), ATTRs) ->
	    atom_concat(SEQ, '_current', SEQ_CURR),
	    SEQ_CURR_GOAL =.. [ SEQ_CURR, CH, Vin ],
	    BODY = (BODY0,
		    ( var(Vin) -> SEQ_CURR_GOAL ; true ) )
	; BODY = BODY0 ),
	( ol_memberchk(hidden=yes, ATTRs) ->
	    BODY0 = BODY1,
	    Qx = Q
	;
	    BODY0 = (isco_insert_arg_default(CN, FNAME, Vin, Vout),
			isco_odbc_format(TYPE, Vout, V),
			BODY2),
	    ( var(P) ->
		BODY2 = (format_to_codes(Qx, '~s (~s', [Q, V]), BODY1), P=comma
	    ;	BODY2 = (format_to_codes(Qx, '~s, ~s', [Q, V]), BODY1) ) ),
	isco_prolog_code_insert_body(CN, NET, Fs, P, Qx, HEAD, BODY1, CH, O).

% -- Generate code to delete a tuple ------------------------------------------

% Tuple delete predicates:
% name: isco_delete__NAME
% arity: ARITY
% args: the WHERE clause

isco_prolog_code_delete(CNAME, NET, NFs, Fs) :-
	lookup(NET, atrib, atrib=As),
	ol_memberchk(mutable, As),
	( ol_memberchk(external(XID, XNAME), As) ; XID=isco, XNAME=CNAME ),
	!,
	% ---------------------------------------------------------------------
	atom_concat('isco_delete__', CNAME, ISCO_NAME),
	isco_prolog_class_head(CNAME, NFs, Fs, HEAD0, VARs, OC_VAR),
	isco_prolog_class_body(VARs, XNAME, HEAD0, Bselect, CH, OC_VAR),
	% ---------------------------------------------------------------------
	HEAD0 =.. [_|HA],
	HEAD =.. [ISCO_NAME | HA],
	HA = [OID, IOF | _],			% pin down oid and iOf
	HEAD1 =.. [ISCO_NAME, CH | HA],
	( XID=isco ->
	    BODY1 = (isco_connection(CH), HEAD1)
	;   atom_concat('isco_', XID, ISCO_XID),
	    BODY1 = (isco_connection(ISCO_XID, CH), HEAD1) ),
	% ---------------------------------------------------------------------
	Bdelete = (
	    format_to_codes(QUERYd, 'delete from ~w where oid=~w', [IOF, OID]),
	    ( g_read(isco_debug_sql, 1) ->
	      format('sql: ~s~n', [QUERYd]) ; true ),
	    isco_be_exec(CH, QUERYd, _) ),
	BODY = (Bselect, Bdelete),
	% ---------------------------------------------------------------------
	nl,
	portray_clause((HEAD :- BODY1)), nl,	% ... & mask
	portray_clause((HEAD1 :- BODY)), nl.	% ... & DB channel
isco_prolog_code_delete(_, _, _, _).


% -- Generate code to update a tuple ------------------------------------------

% Schema for tuple update predicates:
% name: isco_update__NAME
% arity: 2*ARITY+1
% args: 1st half is for the SET part
%       2nd half is for the WHERE clause
%       last arg is the goal to try before updating
%
% Done at compile time:
% - all the SET skeleton, except for the values
% Done at runtime:
% - the rest.
%
% This is now adapted from the "delete" code.

isco_prolog_code_update(CNAME, NET, NFs, Fs) :-
	lookup(NET, atrib, atrib=As),
	ol_memberchk(mutable, As),		% only for mutable classes
	( ol_memberchk(external(XID, XNAME), As) ; XID=isco, XNAME=CNAME ),
	!,
	atom_concat('isco_update__', CNAME, ISCO_NAME),
	ARITY is NFs+1+(NFs-2)+1,		% WHERE v, MASK, SET v, GOAL
	functor(HEADNC, ISCO_NAME, ARITY),
	HEADNC =.. [HF | HA], HEAD1 =.. [HF, CH | HA],
	( XID=isco ->
	    BODY1 = (isco_connection(CH), HEAD1)
	;   atom_concat('isco_', XID, ISCO_XID),
	    BODY1 = (isco_connection(ISCO_XID, CH), HEAD1) ),
	nl,
	portray_clause((HEADNC :- BODY1)),
	!,
%% -- Legend for body variables -----------------------------------------------
%%
%% BODY = (construct query Q0 = QPFX),
%% B1   = (construct SET part, will build Q_S, args in QA_S, uses H_S)
%% B2   = (construct WHERE part, will build Q_W, args in QA_W, uses H_W)
%% B3   = (finalize query and do the thing)
	% ---------------------------------------------------------------------
	isco_prolog_class_head(CNAME, NFs, Fs, HEAD0, VARs, OC_VAR),
	isco_prolog_class_body(VARs, XNAME, HEAD0, Bselect, CH, OC_VAR),
	% ---------------------------------------------------------------------
	HEAD0 =.. [_|HAx],
	HAx = [OID, IOF | _],			% pin down oid and iOf
	HEAD2 =.. [ISCO_NAME, CH | HA],
	% ---------------------------------------------------------------------
	NFsm2 is NFs-2,
	append(HAx, _, HA),
	append(HA_WM, [HA_M], HAx),
	append(HA_WMS, [HA_G], HA),		% HA_G: head arg for GOAL
	length(HA_S, NFsm2),			% HA_W: head args for WHERE...
	append(HA_W, [HA_M|HA_S], HA_WMS),	% ...are at the end of HA.
	BODY = ((Bselect,
		 format_to_codes(Q0, 'update ~w set', [IOF]),
		 B1)),
	H_S =.. [CNAME, _, _ | HA_S],		% head for SET (fake OID, IOF).
	isco_update_set(NFsm2, Fs, H_S, B1, B2, Q0, Q1),
	B2 = ((call(HA_G) ->
	       format_to_codes(QUERY, '~s where oid=~w', [Q1, OID]),
	       ( g_read(isco_debug_sql, 1) ->
		   format('sql: ~s~n', [QUERY]) ; true ),
	       isco_be_exec(CH, QUERY, _SH)
	      ;	  fail )),
	nl,
	portray_clause((HEAD2 :- BODY)),
	nl.
isco_prolog_code_update(_, _, _, _).


% -- SET part of tuple update -------------------------------------------------

%% -- Arguments: --------------------------------------------------------------
%%   -N			Number of fields yet to be seen
%%   -Fs		Field list
%%   -H			Where the arguments should come from (clause head)
%%   -PF		Prefix (either '' or ',')
%%   +Gin			Start of body
%%   +Gout		Body continuation
%%   +SCin		SET clause (previous)
%%   +SCout		SET clause (result)

isco_update_set(N, Fs, H, Gin, Gout, SCin, SCout) :-
	isco_update_set(N, Fs, H, "", Gin, Gout, SCin, SCout).

%%DBG isco_update_set(A, B, C, D, E, F, G, H) :-	% debug code
%%DBG	write(isco_update_set(A, B, C, D, E, F, G, H)), nl, fail.

isco_update_set(0, _, _, _, G, G, SC, SC) :- !.
isco_update_set(N, [f(P,FN,T,_A)|Fs], H, PF, Gin, Gout, SCin, SCout) :-
	arg(P, H, V),
	Gin = (isco_update_set_var(V, FN, T, PF, PFnew, SCin, SCint), Gint),
	N1 is N-1,
	isco_update_set(N1, Fs, H, PFnew, Gint, Gout, SCint, SCout).

% -- Clauses to get the list of argument names from a class -------------------

isco_prolog_class_argnames(EOL, L, L) :- var(EOL), !.
isco_prolog_class_argnames([],  L, L) :- !.
isco_prolog_class_argnames([f(_,NAME,_,_)|FL], Lin, Lout) :-
	ol_memberchk(f(_,NAME,_,_), FL), !, % ignore dupes (override...)
	isco_prolog_class_argnames(FL, Lin, Lout).

isco_prolog_class_argnames([f(_,NAME,_,_)|FL], Lin, Lout) :-
	isco_prolog_class_argnames(FL, [NAME|Lin], Lout).


% -- Clauses to model inheritance (subtypes) ----------------------------------

isco_prolog_class_inheritance(_CNAME, _NET, XDEF) :-
	isco_auto_inheritance(XDEF, _), !.
isco_prolog_class_inheritance(CNAME, NET, _XDEF) :-
	isco_prolog_class_inheritance(CNAME, NET).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_auto_inheritance(isco, '*') :- !.	% be pessimistic, it does no harm.
isco_auto_inheritance(odbc(_, postgres(V)), '') :- nonvar(V), V @>= '7.0', !.
isco_auto_inheritance(odbc(_, postgres(_)), '*') :- !.
isco_auto_inheritance(odbc(_, postgres), '*') :- !.
isco_auto_inheritance(PG, '*') :- nonvar(PG), PG=..[postgres|_], !.


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_prolog_class_inheritance(CNAME, NET) :-
	lookup(NET, subs, subs=SUBCLASSES),
	lookup(NET, atrib, ATTRs),
	\+ ol_memberchk(final, ATTRs),
	!,
	lookup(NET, fields, fields=FIELDS),
	ol_length(FIELDS, NF),
	isco_prolog_class_inheritance_list(SUBCLASSES, CNAME, NF).
isco_prolog_class_inheritance(_, _).

isco_prolog_class_inheritance_list(ECL, _, _) :- var(ECL), !.
isco_prolog_class_inheritance_list([], _, _) :- !.
isco_prolog_class_inheritance_list([C|_], CNAME, NF) :-
	lookup(ST, C, C=NET),
	isco_prolog_class_inheritance_one(CNAME, C, NF, NET),
	fail.
isco_prolog_class_inheritance_list([_|Cs], CNAME, NF) :-
	isco_prolog_class_inheritance_list(Cs, CNAME, NF).

isco_prolog_class_inheritance_one(CN, SN, NF, NET) :-
	lookup(NET, fields, fields=FIELDS),
	ol_length(FIELDS, NFSUB),
	NAH is NF+2,
	NAB is NFSUB+2,
	functor(HEAD, CN, NAH),
	functor(BODY, SN, NAB),
	HEAD =.. [_|HA],
	BODY =.. [_|HB],
	isco_prolog_class_inheritance_matcher(HA, HB),
	!,
	portray_clause((HEAD :- BODY)), nl.


isco_prolog_class_inheritance_matcher([AA], HB) :- !, append(_, [AA], HB).
isco_prolog_class_inheritance_matcher([AA|HA], [AA|HB]) :-
	isco_prolog_class_inheritance_matcher(HA, HB).


% -- Generate code for sequences ----------------------------------------------

isco_prolog_sequence(NAME, ATTRs) :-
	( ol_memberchk(external(XDBREF,SEQ), ATTRs) ->
	    atom_concat('isco_', XDBREF, DBREF), SNAME=SEQ
	; ol_memberchk(external(XDBREF), ATTRs) ->
	    atom_concat('isco_', XDBREF, DBREF), SNAME=NAME
	; DBREF=isco_isco, SNAME=NAME ),
	odbc_type(integer, TYPE),
	format_to_atom(COMMENT, "%% -- ~w sequence. ", [NAME]),
	atom_length(COMMENT, CLENGTH),
	format("~n~w~*c~n", [COMMENT, 79-CLENGTH, "-"]),
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	atom_concat(NAME, '_current', SEQ_C),
	atom_concat(NAME, '_next', SEQ_N),
	HEAD =.. [ NAME, H_V ],
	format_to_codes(Q_S, "select setval('~w', ~~w)", [SNAME]),
	BODY = (nonvar(H_V), !, integer(H_V),
		format_to_codes(Q_SX, Q_S, [H_V]),
		isco_connection(DBREF, H_C),
		isco_be_exec(H_C, Q_SX, _H_S)),
	DEFAULT_BODY =.. [ SEQ_N, H_V ],
	nl,
	portray_clause((HEAD :- BODY)), nl,
	portray_clause((HEAD :- DEFAULT_BODY)), nl,
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	HEAD_C =.. [SEQ_C, HC_C, HC_V],
	HEAD_C0 =.. [SEQ_C, HC_V],
	format_to_codes(Q_C, 'select currval(''~w'')', [SNAME]),
	BODY_C = (isco_be_exec(HC_C, Q_C, HC_S),
		  isco_be_fetch(HC_C, HC_S),
		  isco_be_get_data(HC_C, HC_S, 1, TYPE, HC_V)),
	BODY_C0 = (isco_connection(DBREF, HC_C),
		   HEAD_C),
	nl,
	portray_clause((HEAD_C0 :- BODY_C0)), nl,
	portray_clause((HEAD_C :- BODY_C)), nl,
% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
	HEAD_N =.. [SEQ_N, HN_C, HN_V],
	HEAD_N0 =.. [SEQ_N, HN_V],
	format_to_codes(Q_N, 'select nextval(''~w'')', [SNAME]),
	BODY_N = (isco_be_exec(HN_C, Q_N, HN_N),
		  isco_be_fetch(HN_C, HN_N),
		  isco_be_get_data(HN_C, HN_N, 1, TYPE, HN_V)),
	BODY_N0 = (isco_connection(DBREF, HN_C),
		   HEAD_N),
	nl,
	portray_clause((HEAD_N0 :- BODY_N0)), nl,
	portray_clause((HEAD_N :- BODY_N)), nl.

% -----------------------------------------------------------------------------

% $Log$
% Revision 1.10  2003/03/09 01:54:11  spa
% New code for update!
%
% Revision 1.9  2003/03/07 23:01:05  spa
% Sequence ops must yield strings, not atoms.
%
% Revision 1.8  2003/03/07 15:30:27  spa
% *** empty log message ***
%
% Revision 1.7  2003/03/07 09:59:58  spa
% term type.
% delete done as select(oid)+delete(oid).
%
% Revision 1.6  2003/03/05 01:12:41  spa
% support oid= and instanceOf= arguments.
% support redefinition of arguments, namely for default values.
%
% Revision 1.5  2003/02/28 22:24:17  spa
% make clean limpa mesmo :)
%
% Revision 1.4  2003/02/14 14:22:45  spa
% Offset in column index due to OID being included in query.
%
% Revision 1.3  2003/01/19 08:28:38  spa
% Code for 'select' now grabs OID as well.
%
% Revision 1.2  2003/01/17 14:48:09  spa
% isco_prolog_code_insert_body/8: use strings instead of atoms.
%
% Revision 1.1.1.1  2003/01/06 14:27:21  spa
% Imported into CVS
%
% Revision 1.22  2001/08/24 22:28:57  spa
% Add support for retrieving sequence numbers when inserting new tuples.
%
% Revision 1.21  2001/08/23 23:47:55  spa
% Access to the database now performed via the ISCO back-end
% predicates (isco_be_*).
%
% Revision 1.20  2001/08/21 13:26:11  spa
% isco_prolog_class_argnames/3: new predicate to account for indeterminacy in
% argument placement (PostgreSQL bug.)
%
% Revision 1.19  2001/06/12 09:14:49  spa
% Generate code for delete (:\ operator).
%
% Revision 1.18  2001/05/22 15:37:51  spa
% External dbname needs 'isco_' prepended! (isco_prolog_sequence/3).
%
% Revision 1.17  2001/05/19 00:55:57  spa
% Sequences: exchange order of DBREF,SEQ in external/2.
%
% Revision 1.16  2001/05/19 00:48:31  spa
% Initial model and implementation for sequences.
%
% Revision 1.15  2001/05/17 22:52:51  spa
% Properly determine whether to use '*' for DBMS-side inheritance: it was
% using the table name instead of the ISCO predicate name to find out.
%
% Revision 1.14  2001/05/15 09:21:59  spa
% Support for SELECT queries w/o all variables (MASK).
%
% Revision 1.13  2001/05/11 15:35:34  spa
% Use ol_memberchk/3 instead of memberchk/3 to avoid binding the list tail.
% isco_prolog_class_inheritance/{3,4}: only emit code if the relation is not
% for a Postgres table.
% isco_auto_inheritance/2: new argument indicates what suffix to add to the
% classname in select queries to get the desired behaviour.
%
% Revision 1.12  2001/05/10 23:54:50  spa
% Added "ORDER BY" operators.
% Fix inheritance clause arity.
%
% Revision 1.11  2001/05/09 17:05:14  spa
% Enable run-time echo of SQL queries (controlled by g_assign'ing 1 to
% the atom isco_debug_sql).
%
% Revision 1.10  2001/05/08 12:54:34  spa
% Explicit inheritance is now conditional on back-end type.  For example,
% PostgreSQL 7 does inheritance by itself and by default.
%
% Revision 1.9  2001/05/04 15:53:56  spa
% Reverse order of INSERT fields, as they have implicit names.
%
% Revision 1.8  2001/05/03 13:42:06  spa
% Deal with clauses and directives in the source.
%
% Revision 1.7  2001/04/30 17:24:10  spa
% Implement inheritance (subtyping) at the Prolog level.
%
% Revision 1.6  2001/04/27 14:59:39  spa
% Initial code to perform updates.
%
% Revision 1.5  2001/04/23 22:10:04  spa
% isco_odbc_conv/3 was being mis-used in isco_prolog_class_body_fields/3.
% insert code now generated if class is not 'static'.
%
% Revision 1.4  2001/04/22 21:53:49  spa
% Support tuple insertion.
%
% Revision 1.3  2001/04/19 14:49:41  spa
% Add a "nice" header...
%
% Revision 1.2  2001/04/19 12:51:54  spa
% Copyright.
%
% Revision 1.1  2001/04/19 12:38:30  spa
% Initial version.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% End:
