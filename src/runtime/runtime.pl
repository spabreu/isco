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

% == ISCO run-time support ====================================================

:- include('ops').

% -- Link to external connections ---------------------------------------------

isco_connection(C) :- isco_connection(isco_isco, C). % default connection

isco_connection(ID, C) :- g_read(ID, C), C \= 0, !.
isco_connection(ID, C) :-
	isco_setup_connection(ID, C),
	g_assign(ID, C).

isco_get_connection(R, C) :- isco_classtype(R, regular), !,
	isco_connection(C).
isco_get_connection(R, C) :- isco_classtype(R, external(DB, _)), !,
	atom_concat('isco_', DB, CNAME),
	isco_connection(CNAME, C).
isco_get_connection(_, _).

% -- Transactions -------------------------------------------------------------

isco_transaction :- isco_connection(C), isco_transaction(C).
isco_transaction(C) :-
	isco_be_exec(C, "begin transaction", R),
	isco_be_fetch(C, R) -> true ; true.

isco_commit :- isco_connection(C), isco_commit(C).
isco_commit(C) :-
	isco_be_exec(C, "end transaction", R),
	isco_be_fetch(C, R) -> true ; true.

isco_rollback :- isco_connection(C), isco_rollback(C).
isco_rollback(C) :-
	isco_be_exec(C, "rollback transaction", R),
	isco_be_fetch(C, R).

isco_atomic(GOAL) :- isco_connection(C), isco_atomic(C, GOAL).
isco_atomic(C, GOAL0) :-
	isco_transaction(C),
	isco_term_expansion(GOAL0, GOAL),
	( catch(GOAL, _EX, fail) -> isco_commit(C) ; isco_rollback(C) ).

% -- ISCO/Prolog update goal call ---------------------------------------------

%% Examples:
%%	foobar @ (xpto=X, zzz=Y) := (ahh=10)		-- simple (1)
%%	foobar @ (xpto<X, zzz>ahh) := (ahh=zzz+ahh)	-- complex (2)
%% Initially only type (1) queries are to be supported.

% -- ISCO/Prolog non-positional argument goal call ----------------------------

RELNAME @ ARGS :-
	isco_term_expansion((RELNAME @ ARGS), GOAL), !,
	call(GOAL).

% -- ISCO/Prolog insert goal --------------------------------------------------

RELNAME_ARGS := NEWARGS :-
	isco_term_expansion((RELNAME_ARGS := NEWARGS), GOAL), !,
	call(GOAL).

RELNAME_ARGS :+ :-
	isco_term_expansion((RELNAME_ARGS :+), GOAL), !,
	call(GOAL).

% -- ISCO/Prolog delete goal --------------------------------------------------

RELNAME_ARGS :\ :-
	isco_term_expansion((RELNAME_ARGS :\), GOAL), !,
	call(GOAL).

% -- Create tables in SQL from the Prolog database ----------------------------

isco_create_table(RELNAME) :-
	isco_create_table(RELNAME, _).

isco_create_table(RELATION, SQL) :-
	isco_classtype(RELATION, regular),
	open_output_codes_stream(OUTPUT),
	current_output(OLDOUTPUT),
	set_output(OUTPUT),
	catch(isco_sql_code(RELATION), _, true),
	close_output_codes_stream(OUTPUT, SQL),
	set_output(OLDOUTPUT),
	isco_connection(C),
	odbc_alloc_stmt(C, S),
	( g_read(isco_debug_sql, 1) -> format('sql: ~s', [SQL]) ; true ),
	odbc_exec_direct(S, SQL),
	odbc_release_stmt(S).

% -- Utilities for above ------------------------------------------------------

isco_arg_list(ARGS, GOAL, RELNAME) :-
	isco_arg_list(ARGS, GOAL, RELNAME, 0, 0, _),
	!.

isco_arg_list(ARGS, GOAL, RELNAME) :-
	isco_class(RELNAME, NARGS),
	isco_positional_arg_list(POS, NARGS, ARGS, GOAL),
	( fd_domain(POS, [1, 3]) -> true
	;   NSEEN is NARGS-POS+1,
	    NARGSm2 is NARGS-2,
	    throw(invalid(positional_arg_list(length(NSEEN)),
			  relation(RELNAME,length(NARGS,NARGSm2)))) ).


isco_arg_list(AAs, GOAL, RELNAME, BASE, MASK, MASKo) :-
	nonvar(AAs),				% catch VAR where we should
	AAs = (A,As),				% have non-var...
	!,
	isco_arg(A, GOAL, RELNAME, BASE, MASK, MASKi),
	isco_arg_list(As, GOAL, RELNAME, BASE, MASKi, MASKo).
isco_arg_list(A, GOAL, RELNAME, BASE, MASK, MASKo) :-
	isco_arg(A, GOAL, RELNAME, BASE, MASK, MASKo).


isco_arg(VARARG, _GOAL, RELNAME, _BASE, MASK, MASK) :-
	var(VARARG), !,
	format("isco: warning: variable field provided to class ~w.~n",
		[RELNAME]).
isco_arg(ARG=VALUE, GOAL, RELNAME, BASE, MASKi, MASKo) :-
	isco_field(RELNAME, ARG, POS, _TYPE),
	POSITION is POS + BASE,
	MASKo is MASKi \/ (1 << POS),
	arg(POSITION, GOAL, VALUE).

%% FIXME: deal with ARG>VALUE, etc.


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_positional_arg_list(POS, N, AAs, G) :-
	nonvar(AAs), AAs = (A,As),
	!,
	isco_positional_arg_list(POS0, N, As, G),
	POS is POS0-1,
	arg(POS, G, A).

isco_positional_arg_list(POS, POS, A, G) :-
	arg(POS, G, A).


% -- ISCO/ODBC type equivalences ----------------------------------------------

isco_odbc_type(int,      integer).
isco_odbc_type(text,     varchar).
isco_odbc_type(float,    float).
isco_odbc_type(double,   double).
isco_odbc_type(bool,     bool).
isco_odbc_type(date,     timestamp).
isco_odbc_type(dt,       timestamp).
isco_odbc_type(datetime, timestamp).
isco_odbc_type(term,     varchar).

isco_odbc_generated_type(text,      text).
isco_odbc_generated_type(float,     real).
isco_odbc_generated_type(double,    'double precision').
isco_odbc_generated_type(timestamp, date).
isco_odbc_generated_type(bool,      boolean).
isco_odbc_generated_type(int,       integer).
isco_odbc_generated_type(date,      timestamp).
isco_odbc_generated_type(dt,        timestamp).
isco_odbc_generated_type(datetime,  timestamp).
isco_odbc_generated_type(term,      text).

% -- ISCO/ODBC type conversions -----------------------------------------------
%
% isco_odbc_conv(+TYPE, +EXTERNAL_VALUE, -PROLOG_VALUE)
%

isco_odbc_conv(bool).
isco_odbc_conv(term).
isco_odbc_conv(text).

%%DBG isco_odbc_conv(T, XV, PV) :- format("[~w]~n", [isco_odbc_conv(T, XV, PV)]), fail.

isco_odbc_conv(bool, true, true) :- !.
isco_odbc_conv(bool, false, false) :- !.
isco_odbc_conv(bool, 1, true) :- !.
isco_odbc_conv(bool, 0, false) :- !.
isco_odbc_conv(bool, "t", true) :- !.
isco_odbc_conv(bool, "f", false) :- !.
isco_odbc_conv(bool, "1", true) :- !.
isco_odbc_conv(bool, "0", false) :- !.


isco_odbc_conv(term, [], []) :- !.
isco_odbc_conv(term, STRING, TERM) :-
	catch( read_term_from_atom(STRING, TERM,
				   [syntax_error(fail), end_of_term(eof)]),
	       _ERROR, TERM=STRING ).

isco_odbc_conv(text, V, V) :- !.
isco_odbc_conv(text, XV, PV) :- atom(XV), is_string(PV), !, name(XV, PV).
isco_odbc_conv(text, XV, PV) :- is_string(XV), atom(PV), !, name(PV, XV).



% -- ISCO/ODBC formats --------------------------------------------------------
%
% isco_odbc_format(TYPE, DBCONN, PL_REP, EXT_REP)
%

isco_odbc_format(TYPE, PL_REP, EXT_REP) :- % assume postgres by default
	isco_odbc_format(TYPE, pg(_), PL_REP, EXT_REP).



isco_odbc_format(date, _, dt(Y,M,D,HH,MM,SS), DT) :- !,
	format_to_codes(DT, "'~w-~w-~w ~w:~w:~w'::timestamp", [Y,M,D,HH,MM,SS]).

isco_odbc_format(date, _, dt(Y,M,D), DT) :- !,
	format_to_codes(DT, "'~w-~w-~w'::date", [Y,M,D]).

isco_odbc_format(text, _, S, SS) :-
	nonvar(S),
	is_string(S),
	!,
	SN = S,
	isco_odbc_text_format(SN, SX),		% mung quotes...
	format_to_codes(SS, "'~s'", [SX]).

isco_odbc_format(text, _, S, SS) :-
	atom(text),
	!,
	name(S, SN),
	isco_odbc_text_format(SN, SX),		% mung quotes...
	format_to_codes(SS, "'~s'", [SX]).

isco_odbc_format(text, _, S, _) :-
	throw(illegal(string_or_atom, S)).

isco_odbc_format(term, _, S, SS) :- !,
	format_to_codes(SN, "~k", [S]),
	isco_odbc_text_format(SN, SX),		% mung quotes...
	format_to_codes(SS, "'~s'", [SX]).

isco_odbc_format(int, _, nextval(SEQ), NVS) :- !,
	format_to_codes(NVS, "nextval ('~w')", [SEQ]).

isco_odbc_format(bool, DB, T, "true") :- has_bool(DB), memberchk(T, [t, true, 1]), !.
isco_odbc_format(bool, DB, F, "false") :- has_bool(DB), memberchk(F, [f, false, 0]), !.
isco_odbc_format(bool, _, T, "1") :- memberchk(T, [t, true, 1]), !.
isco_odbc_format(bool, _, F, "0") :- memberchk(F, [f, false, 0]), !.

isco_odbc_format(_TYPE, _, S, SS) :-
	format_to_codes(SS, "~w", [S]).



is_string([]) :- !.
is_string([C|Cs]) :- integer(C), C>0, C<256, !, is_string(Cs).


has_bool(_).					% they all accept booleans!


isco_odbc_text_format([], []).
isco_odbc_text_format([39|A], [92, 39|B]) :- !,	% ' -> \'
	isco_odbc_text_format(A, B).
isco_odbc_text_format([34|A], [92, 34|B]) :- !,	% " -> \"
	isco_odbc_text_format(A, B).
isco_odbc_text_format([X|A], [X|B]) :-		% default
	isco_odbc_text_format(A, B).

isco_odbc_text_format_no_percent([], []).
isco_odbc_text_format_no_percent([39, 37, 39|A], [37|B]) :- !, % '%' -> %
	isco_odbc_text_format_no_percent(A, B).
isco_odbc_text_format_no_percent([39|A], [92, 39|B]) :- !, % ' -> \'
	isco_odbc_text_format_no_percent(A, B).
isco_odbc_text_format_no_percent([34|A], [92, 34|B]) :- !, % " -> \"
	isco_odbc_text_format_no_percent(A, B).
isco_odbc_text_format_no_percent([X|A], [X|B]) :- % default
	isco_odbc_text_format_no_percent(A, B).


% -- Replace all variables by '%' (for SQL "like" matching...) ----------------

isco_percent_variables(V) :- var(V), !, V='%'.
isco_percent_variables(X) :- atomic(X), !.
isco_percent_variables([V|Vs]) :- !,
	isco_percent_variables(V),
	isco_percent_variables(Vs).
isco_percent_variables(V) :- V=..[_|Vs], isco_percent_variables(Vs).


% -- ISCO/ODBC formats for constrained variables ------------------------------

isco_odbc_fd_format(V, N, VF) :-
	fd_has_vector(V),
	!,
	fd_dom(V, VLIST),
	format_to_codes(VLISTn, "~w", [VLIST]),
	isco_odbc_list_to_tuple(VLISTn, VALUES),
	format_to_codes(VF, '"~w" in ~s', [N, VALUES]).
isco_odbc_fd_format(V, N, VF) :-
	fd_max_integer(MAX), fd_max(V, MAX),
	!,
	fd_min(V, MIN),
	format_to_codes(VF, '"~w" >= ~w', [N, MIN]).
isco_odbc_fd_format(V, N, VF) :-
	fd_max(V, MAX),
	fd_min(V, MIN),
	format_to_codes(VF, '"~w" >= ~w and o."~w" <= ~w', [N, MIN, N, MAX]).


isco_odbc_list_to_tuple([], []).
isco_odbc_list_to_tuple("]", ")") :- !.
isco_odbc_list_to_tuple([0'[|Lin], [0'(|Lout]) :- !,
	isco_odbc_list_to_tuple(Lin, Lout).
isco_odbc_list_to_tuple([C|Lin], [C|Lout]) :- !,
	isco_odbc_list_to_tuple(Lin, Lout).

% -- SQL WHERE generation for individual variables ----------------------------
%
% isco_where_var(CONN, VAR, NAME, TYPE, WPFXin, WPFXout, WHEREin, WHEREout)
% Where:
%   CONN     is the connection (pg(_), odbc(_)...)
%   VAR      is the variable being tested
%   NAME     is the field name
%   TYPE     is the field's type
%   WPFXin   is the WHERE input prefix (either 'where' or 'and')
%   WPFXout  is the WHERE output prefix (either same as PFXin or 'and')
%   WHEREin  in the input WHERE clause
%   WHEREout is the output WHERE clause
% Incrementally constructs a WHERE clause for a SELECT or other SQL query.

isco_where_var(_, V, _, _, WP, WP, WC, WC) :- var(V), !.
isco_where_var(_, V, N, T, WP, 'and', WC, WCo) :-
	fd_var(V), T=int,			% FD only for int type...
	!,
	isco_odbc_fd_format(V, N, VF),
	format_to_codes(WCo, '~s ~w o.~s', [WC, WP, VF]).
isco_where_var(_, V, N, term, WP, 'and', WC, WCo) :- % special for terms...
	nonvar(V), !,
	copy_term(V, VC),
	isco_percent_variables(VC),
	format_to_codes(VS, '~k', [VC]),
	isco_odbc_text_format_no_percent(VS, VF), % mung quotes...
	format_to_codes(WCo, '~s ~w o."~w" like \'~s\'', [WC, WP, N, VF]).
isco_where_var(C, V, N, T, WP, 'and', WC, WCo) :-
	isco_odbc_format(T, C, V, VF),
	(N = instanceOf ->
	    format_to_codes(WCo, '~s ~w c.relname=~s', [WC, WP, VF])
	;
	    format_to_codes(WCo, '~s ~w o."~w"=~s', [WC, WP, N, VF]) ).

% -- SQL ORDER BY generation for individual variables -------------------------
%
% isco_order_var(ORDER, NAME, OPFXin, OPFXout, ORDERin, ORDERout)
% Where:
%   ORDER    is either 'asc' or 'desc' or 'none'
%   NAME     is the field name
%   OPFXin   is the input prefix (either ' order by' or ',')
%   OPFXout  is the output prefix (either same as OPFXin or ',')
%   ORDERin  is the input ORDER BY clause
%   ORDERout is the output ORDER BY clause
% Is the same as the above except that an ORDER BY clause is being built.
%
% This is invoked by the compiler or the '@' runtime predicate when needed,
% i.e. it doesn't have to check for the variable itself.

isco_order_clause(ORDERs, CLAUSE) :-
	isco_order_clause(ORDERs, " order by", _, "", CLAUSE).

isco_order_clause([], _, _, O, O).
isco_order_clause([N=O|Os], OP, OPo, OC, OCo) :-
	isco_order_var(O, N, OP, OPi, OC, OCi),
	isco_order_clause(Os, OPi, OPo, OCi, OCo).

isco_order_var(none,  _, OP, OP,  OC, OC) :- !.
isco_order_var(ORDER, N, OP, ",", OC, OCo) :-
	format_to_codes(OCo, '~s~s o."~w" ~w', [OC, OP, N, ORDER]).

% -- Transmit default values --------------------------------------------------

isco_insert_arg_default(_,  _,  V, V) :- nonvar(V), !.
isco_insert_arg_default(CN, FN, _, D) :- isco_field_default(CN, FN, D), !.
isco_insert_arg_default(CN, FN, _, null) :- isco_field_not_null(CN, FN), !,
	format("isco: warning: field ~w.~w not_null but no value provided.~n",
		[CN, FN]).
isco_insert_arg_default(_,  _,  _, null).

% -- SET part of SQL UPDATE for individual variables --------------------------

isco_update_set_var(V, FN, T, PFi, PFo, SCi, SCo) :- % glue
	isco_update_set_var(_, V, FN, T, PFi, PFo, SCi, SCo).


isco_update_set_var(_, V, _,  _, PF, PF,  SC,   SC) :- var(V), !.
isco_update_set_var(C, V, FN, T, PF, ",", SCin, SCout) :-
	isco_odbc_format(T, C, V, VF),
	format_to_codes(SCout, "~s~s ~w=~s", [SCin, PF, FN, VF]).

% -- Create "ORDER BY" list for SELECT goal -----------------------------------

isco_order_tuple(ARG, ARG, []) :- var(ARG), !.
isco_order_tuple((A,As), (NA,NAs), [O|Os]) :-
	isco_order_by_item(A, NA, O), !,
	isco_order_tuple(As, NAs, Os).
isco_order_tuple((A,As), (A,NAs), Os) :- !,
	isco_order_tuple(As, NAs, Os).
isco_order_tuple(A, NA, [O]) :- isco_order_by_item(A, NA, O), !.
isco_order_tuple(A, A, []).


isco_order_by_item(X, _, _) :- var(X), !, fail.
isco_order_by_item(N=V <@, N=V, N=(desc)).
isco_order_by_item(N=V >@, N=V, N=(asc)).
isco_order_by_item(N=V desc, N=V, N=(desc)).	% allow alternate syntax
isco_order_by_item(N=V asc,  N=V, N=(asc)).

% -- Create masks for variables to use in SELECTs -----------------------------

% isco_mask_to_var_list(INDEX, MASK, CNAME, GOAL, VARLIST)

isco_mask_to_var_list(HEAD, NV, MASK, VARLIST) :-
	functor(HEAD, CNAME, ARITYp2),
	isco_mask_to_var_list(ARITYp2, NV, MASK, CNAME, HEAD, VARLIST, []).

isco_mask_to_var_list(1, 0, _, _, _, VL, VL) :- !.
isco_mask_to_var_list(N, NV, MASK, CNAME, HEAD, VL, VLo) :-
	N1 is N-1,
	isco_mask_to_var_list(N1, NV1, MASK, CNAME, HEAD, VLi, VLo),
	( 0 is MASK /\ (1<<N1) ->		% field unused
	    VL = VLi,
	    NV1 = NV
	;					% field used
	    isco_field(CNAME, FNAME, N1, NFAKES, FAKE, _TYPE),
	    ( FAKE = fake ->			% fake field
		VL = VLi,	
		NV = NV1
	    ;
		arg(N, HEAD, VAR),
		VL = [f(FNAME,NV,VAR)|VLi],
		NV is NV1+1,
		NVK is NV-NFAKES ) ).


isco_var_list_to_select(VARLIST, _, SELECT) :- % backward compatibility.
	isco_var_list_to_select(VARLIST, SELECT).

isco_var_list_to_select(VARLIST, SELECT) :-
	reverse(VARLIST, RVARLIST),
	isco_var_list_to_select(RVARLIST, "", "", SELECT).

isco_var_list_to_select([], _, SEL, SEL).
isco_var_list_to_select([f(FNAME,_,_)|Fs], SPF, SELECT, SELECTo) :-
	format_to_codes(SELECTi, '~s~so."~w"', [SELECT, SPF, FNAME]),
	isco_var_list_to_select(Fs, ", ", SELECTi, SELECTo).


% == Back-end interface predicates ============================================

% -- Get argument from correct position ---------------------------------------

%%DBG isco_be_get_arg(A, B, C, D, E, F, G, H, I, J) :- %%DBG
%%DBG 	write(isco_be_get_arg(A, B, C, D, E, F, G, H, I, J)), nl, fail.	%%DBG

isco_be_get_arg(MASK, _, P, CH, SH, OTn, CONV, V, T, _) :-
	MASK =< 0,				% includes 0 and -1
	!,
	isco_be_get_data(CH, SH, P, OTn, Vx),
	( CONV=yes -> isco_odbc_conv(T, Vx, V) ; V=Vx ).
isco_be_get_arg(_, N, _, CH, SH, OTn, CONV, V, T, VL) :-
	memberchk(f(N,PX,_), VL),
	!,
	isco_be_get_data(CH, SH, PX, OTn, Vx),
	( CONV=yes -> isco_odbc_conv(T, Vx, V) ; V=Vx ).
isco_be_get_arg(_, _, _, _, _, _, _, _, _, _).


% -- Other back-end predicates ------------------------------------------------

isco_be_exec(BE, Q, R) :- BE :> exec(Q, R).
isco_be_fetch(BE, R) :- BE :> fetch(R).
isco_be_ntuples(BE, R, N) :- BE :> ntuples(R, N).
isco_be_oid(BE, R, O) :- BE :> oid(R, O).
isco_be_get_data(BE, R, X, T, V) :- BE :> get_data(R, X, T, V).


% -- Check for adequate revision ----------------------------------------------

isco_check_revision(R) :- isco_revision(R), !.
isco_check_revision(R) :- throw(wrong_isco_version_needs(R)).


% -- Rewrite an argument LIST as an argument TUPLE ----------------------------

isco_arglist_to_args([X], X) :- !.
isco_arglist_to_args([X|Xs], (X,Ys)) :- isco_arglist_to_args(Xs, Ys).


% -- Class hierarchy utilities ------------------------------------------------

isco_subclass(CLASS, CLASS).
isco_subclass(SUPER, SUB) :-
	isco_has_subclass(SUPER, SUB).

isco_has_subclass(SUPER, SUB) :- isco_superclass(SUB, SUPER).
isco_has_subclass(SUPER, SUB) :-
	isco_superclass(SUB, MID),
	isco_has_subclass(SUPER, MID).


isco_tablename(CNAME, CNAME) :- isco_classtype(CNAME, regular), !.
isco_tablename(CNAME, TNAME) :- isco_classtype(CNAME, external(_, TNAME)), !.


% == Specialized "term_expansion/2" for ISCO predicates =======================

isco_term_expansion(X, Y) :- isco_term_expansion(X, Y, _).

isco_term_expansion(X, Y, yes) :- ite(X, Y), !.
isco_term_expansion(X, X, no).

% -- ISCO/Prolog update goal call ---------------------------------------------

ite((RELNAME @ ARGS :\), GOAL) :- !,
	ite((RELNAME @ ARGS), SGOAL),
	SGOAL =.. [ SFUNC | ARGLIST ],
	atom_concat('isco_delete__', SFUNC, FUNC),
	GOAL =.. [ FUNC | ARGLIST ].

ite((RELNAME := NEWARGS @ ARGS), GOAL) :- !,
	ite((RELNAME @ ARGS := NEWARGS), GOAL).

ite((RELNAME @ ARGS := NEWARGS / G), GOAL) :-
	!,
	isco_class(RELNAME, NUMARGS),
	ARITY is NUMARGS+1+(NUMARGS-2)+1,
	atom_concat('isco_update__', RELNAME, PREDNAME),
	functor(GOAL, PREDNAME, ARITY),
	arg(ARITY, GOAL, G),
	isco_order_tuple(ARGS, NARGS, ORDER),
	isco_arg_list(NARGS, GOAL, RELNAME, 0, 0, MASK),
	isco_order_clause(ORDER, ORDER_CLAUSE),
	NUMARGSp1 is NUMARGS+1,
	arg(NUMARGSp1, GOAL, ORDER_CLAUSE+MASK),
	NUMARGSp2 is NUMARGS+1-2, % -2: omit OID and IOF, +1: skip OC+M
	isco_arg_list(NEWARGS, GOAL, RELNAME, NUMARGSp2, 0, _).

ite((RELNAME @ ARGS := NEWARGS), GOAL) :-
	ite((RELNAME @ ARGS := NEWARGS / true), GOAL).


% -- ISCO/Prolog non-positional argument goal call ----------------------------

ite((RELNAME @ ARGS), GOAL) :-
	isco_classtype(RELNAME, computed),
	!,
	isco_class(RELNAME, ARITY),
	isco_order_tuple(ARGS, NARGS, _ORDER),	% ignore ORDER marks
	functor(GOAL, RELNAME, ARITY),
	isco_arg_list(NARGS, GOAL, RELNAME, 0, 0, _).

ite((RELNAME @ ARGS), GOAL) :-
	isco_class(RELNAME, ARITYm1), ARITY is ARITYm1+1,
	!,
	isco_order_tuple(ARGS, NARGS, ORDER),
	functor(GOAL, RELNAME, ARITY),
	isco_arg_list(NARGS, GOAL, RELNAME, 0, 0, MASK),
	isco_order_clause(ORDER, ORDER_CLAUSE),
	arg(ARITY, GOAL, ORDER_CLAUSE+MASK).

% -- ISCO/Prolog delete goal --------------------------------------------------

ite((RELNAME_ARGS :\), GOAL) :-
	ite(RELNAME_ARGS, SGOAL),
	SGOAL =.. [ SFUNC | ARGLIST ],
	atom_concat('isco_delete__', SFUNC, FUNC),
	GOAL =.. [ FUNC | ARGLIST ].

% -- ISCO/Prolog select goal call ---------------------------------------------

ite((RELNAME_ARGS), GOAL) :-			% with positional args
	functor(RELNAME_ARGS, RELNAME, NUMARGS), % what relation is this?
	isco_valid_pos_args(RELNAME, NUMARGS, 0), % is this directly OK?
	!,
	RELNAME_ARGS =.. [_ | ARGLIST],
	append([RELNAME | ARGLIST], [[]+0], GOAL_LIST),
	GOAL =.. GOAL_LIST.

ite((RELNAME_ARGS), GOAL) :-			% with positional args
	functor(RELNAME_ARGS, RELNAME, NUMARGS), % what relation is this?
	isco_valid_pos_args(RELNAME, NUMARGS, 2), % requires OID+IOF?
	!,
	RELNAME_ARGS =.. [_ | ARGLIST],
	append([RELNAME, _, _ | ARGLIST], [[]+0], GOAL_LIST),
	GOAL =.. GOAL_LIST.

ite((RELNAME_ARGS), GOAL) :-			% with named args
	functor(RELNAME_ARGS, RELNAME, NUMARGS), % what relation is this?
	isco_class(RELNAME, _),			% do we know about it?
	NUMARGS > 0,				% need at least one.
	RELNAME_ARGS =.. [_ | ARGLIST],		% look at argument list...
	isco_valid_npos_args(ARGLIST, RELNAME),	% ...do the args look OK?
	!,
	isco_arglist_to_args(ARGLIST, ARGS),
	ite((RELNAME @ ARGS), GOAL). % easy way around...

% -- ISCO/Prolog insert goal --------------------------------------------------

ite((RELNAME := NEWARGS), GOAL) :-
	atom(RELNAME),				% make sure it's bound
	isco_class(RELNAME, ARITY),
	!,
	atom_concat('isco_insert__', RELNAME, PREDNAME),
	functor(GOAL, PREDNAME, ARITY),
	isco_arg_list(NEWARGS, GOAL, RELNAME).

ite((RELNAME_ARGS :+), GOAL) :-	% alternate syntax
	RELNAME_ARGS =.. [RELNAME | ARGLIST],
	atom(RELNAME),
	isco_class(RELNAME, _ARITY),
	!,
	isco_arglist_to_args(ARGLIST, ARGS),
	ite((RELNAME := ARGS), GOAL).

% -- Update goal --------------------------------------------------------------

ite((RELNAME_ARGS := NEWARGS), GOAL) :-
	functor(RELNAME_ARGS, RELNAME, _NUMARGS), % what relation is this?
	isco_class(RELNAME, _),			% do we know about it?
	!,
	RELNAME_ARGS =.. [_ | ARGLIST],
	isco_arglist_to_args(ARGLIST, ARGS),
	ite((RELNAME@ARGS:=NEWARGS), GOAL).


% -- Compound goals -----------------------------------------------------------

ite((A,B), (GA,GB)) :-
	!,
	isco_term_expansion(A, GA),
	isco_term_expansion(B, GB).

ite((A->B;C), (GA->GB;GC)) :-
	!,
	isco_term_expansion(A, GA),
	isco_term_expansion(B, GB),
	isco_term_expansion(C, GC).

ite((A;B), (GA;GB)) :-
	!,
	isco_term_expansion(A, GA),
	isco_term_expansion(B, GB).

ite(setof(X,A,Y), setof(X,GOAL,Y)) :-
	!,
	isco_term_expansion_setof(A, GOAL).

ite(bagof(X,A,Y), bagof(X,GOAL,Y)) :-
	!,
	isco_term_expansion_setof(A, GOAL).

ite(findall(X,A,Y), findall(X,GOAL,Y)) :-
	!,
	isco_term_expansion(A, GOAL).

ite(call(A), call(GOAL)) :- !, isco_term_expansion(A, GOAL).
ite(\+(A), \+(GOAL)) :- !, isco_term_expansion(A, GOAL).
ite(once(A), once(GOAL)) :- !, isco_term_expansion(A, GOAL).

ite((U :> A), (U :> GOAL)) :- !, isco_term_expansion(A, GOAL).
ite((C :< A), (C :< GOAL)) :- !, isco_term_expansion(A, GOAL).
ite((:^ A), (:^ GOAL)) :- !, isco_term_expansion(A, GOAL).
ite((:# A), (:# GOAL)) :- !, isco_term_expansion(A, GOAL).


isco_term_expansion_setof(X^A, X^GOAL) :-
	!,
	isco_term_expansion_setof(A, GOAL).
isco_term_expansion_setof(A, GOAL) :-
	isco_term_expansion(A, GOAL).


isco_valid_pos_args(CNAME, NA, 0) :- isco_class(CNAME, NA), !.
isco_valid_pos_args(CNAME, NA, 2) :- isco_class(CNAME, NAp2), NA is NAp2-2.


isco_valid_npos_args([], _).
isco_valid_npos_args([A|As], C) :-
	nonvar(A), A = (_=_), !,
	isco_valid_npos_args(As, C).
isco_valid_npos_args([A|As], C) :-
	nonvar(A), A =.. [SORTOP, (_=_)], isco_sort_op(SORTOP), !,
	isco_valid_npos_args(As, C).


isco_sort_op(@>).
isco_sort_op(@<).
isco_sort_op(asc).
isco_sort_op(desc).


% -----------------------------------------------------------------------------

isco_schema_class(C) :-
	isco_tsort_level(X, _L),
	member(C, X),
	isco_classtype(C, regular).


isco_requires(REL1, REL2) :-
	isco_class(REL1, _),
	( isco_superclass(REL1, REL2)
	; setof(R, A^B^isco_field_domain(REL1, A, R, B), RS), member(REL2, RS)
	; fail ).


isco_tsort_level(X, N) :- isco_tsort_level(0, _, N, X).


isco_tsort_level(0, _, N, X) :- !,
	setof(R, A^(isco_class(R, A), \+ isco_requires(R, _)), Rs),
	( N=0, X=Rs
	; isco_tsort_level(1, Rs, N, X) ).

isco_tsort_level(M, PX, N, X) :-
	setof(R, A^(isco_class(R, A),
		    \+ member(R, PX),
		    \+ (isco_requires(R, S), \+ member(S, PX))), Rs),
	( X=Rs, N=M
	; M1 is M+1,
	  append(PX, Rs, PXRs),
	  isco_tsort_level(M1, PXRs, N, X) ).


% -----------------------------------------------------------------------------

% $Log$
% Revision 1.16  2003/04/15 15:07:03  spa
% - fix SQL construction of constrained parameters (quotes were wrong)
% - isco_term_expansion rebuilt.
% - recognize contextual calls.
% - deletes are now explicitly built from reworked selects.
% - recognize sorted arguments, etc, when doing non-positional args.
%
% Revision 1.15  2003/04/09 12:06:54  spa
% "Quote" field names in inserts, as they may be reserved words.
% Cast date+time to "timestamp", not "datetime".
%
% Revision 1.14  2003/03/22 11:47:55  spa
% *** empty log message ***
%
% Revision 1.13  2003/03/18 13:40:33  spa
% isco_odbc_format/4: booleans now always produce "true" or "false".
%
% Revision 1.12  2003/03/16 09:22:25  spa
% Transaction code fixed (patch by Gonçalo Marrafa <gjm@sc.uevora.pt>).
%
% Revision 1.11  2003/03/10 22:18:34  spa
% Don't forget about args in REL(ARGS):=NEWARGS.
%
% Revision 1.10  2003/03/09 01:54:26  spa
% New code for update!
%
% Revision 1.9  2003/03/07 23:01:21  spa
% boolean conversions...
%
% Revision 1.8  2003/03/07 09:59:58  spa
% term type.
% delete done as select(oid)+delete(oid).
%
% Revision 1.7  2003/03/05 01:12:41  spa
% support oid= and instanceOf= arguments.
% support redefinition of arguments, namely for default values.
%
% Revision 1.6  2003/02/26 19:21:05  spa
% isco_be_get_arg/10: offset by 1 to account for OID...
%
% Revision 1.5  2003/01/18 15:48:04  spa
% isco_update_set_var/7 helper predicate (it's now /8, really, but there are
% calls to the same predicate w/o the connection: provide a default value...)
%
% Revision 1.4  2003/01/17 22:17:33  spa
% Top-level wrappers for TUPLE :+ and TUPLE :\.
%
% Revision 1.3  2003/01/17 14:55:34  spa
% isco_odbc_format/4 defaults to an atom-to-string conversion.
%
% Revision 1.2  2003/01/17 14:26:41  spa
% re-insert isco_odbc_format/3, as it's used in generated code.
%
% Revision 1.1.1.1  2003/01/06 14:27:22  spa
% Imported into CVS
%
% Revision 1.34  2001/08/29 16:39:32  spa
% Tweaked transaction support.
%
% Revision 1.33  2001/08/27 15:52:21  spa
% Typos in isco_be_{exec,fetch} (missing arg.)
%
% Revision 1.32  2001/08/27 13:49:18  spa
% Adapted transaction code for new back-end independent interface.
%
% Revision 1.31  2001/08/24 19:39:49  spa
% Properly pass quotes on to the back-end (\' instead of ').
%
% Revision 1.30  2001/08/24 18:17:30  spa
% isco_requires/2 and isco_tsort_level/4 have been corrected.
%
% Revision 1.29  2001/08/23 23:48:17  spa
% Access to the database now performed via the ISCO back-end
% predicates (isco_be_*).
%
% Revision 1.28  2001/08/17 13:43:29  spa
% Partly undo edit 1.27: revert back to quotes instead of double-quotes
% because the SQL back-end wants it that way.  sigh.
%
% Revision 1.27  2001/08/07 18:28:18  spa
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
% Revision 1.26  2001/06/13 13:51:16  spa
% isco_odbc_list_to_tuple/2 should have been deterministic!
%
% Revision 1.25  2001/06/13 08:46:18  spa
% Special format for 'nextval(SEQNAME)'.
%
% Revision 1.24  2001/06/12 17:27:52  spa
% Generic clauses for isco_term_expansion/2.
%
% Revision 1.23  2001/06/12 09:16:51  spa
% New predicate isco_term_expansion/2 does all the rewrites.
% All user-visible interface predicates now use isco_term_expansion/2.
%
% Revision 1.22  2001/05/22 16:33:20  spa
% New predicate isco_subclass/2.
%
% Revision 1.21  2001/05/21 18:03:16  spa
% Buglet in isco_mask_to_var_list/4: was dropping one argument!
%
% Revision 1.20  2001/05/19 01:04:20  spa
% Again: new optional syntax for ORDER BY qualifiers: safer syntax.
%
% Revision 1.19  2001/05/19 00:51:43  spa
% New optional syntax for ORDER BY qualifiers: safer syntax.
%
% Revision 1.18  2001/05/15 09:49:24  spa
% Don't assign variable in f/3 subterm (isco_get_arg/9).
%
% Revision 1.17  2001/05/15 09:22:14  spa
% Support for SELECT queries w/o all variables (MASK).
%
% Revision 1.16  2001/05/10 23:56:33  spa
% @/2 now able to do computed classes.
% New predicate isco_get_connection/2.
% Deal with "ORDER BY" operators.
% More predicate documentation.
%
% Revision 1.15  2001/05/10 00:49:16  spa
% Release statement handles whenever appropriate.
% New isco_create_table/1 and isco_create_table/2 predicates; working!
% New isco_odbc_generated_type/2 predicate to build "create tables"...
%
% Revision 1.14  2001/05/09 17:06:06  spa
% Save connection as conn(C) instead of just C, because the new ODBC
% interface returns small integers and not addresses.
%
% Revision 1.13  2001/05/08 12:57:27  spa
% Skeleton for dynamic table creation.
% isco_arg_list/4 should not die with unbound arguments.
%
% Revision 1.12  2001/05/07 14:40:11  spa
% Reorder @ and := clauses.
% Embryonic isco_create_table/1 predicate.
% Some formats are now atoms instead of strings.
% Missing defaults now generate 'null' instead of giving an error message.
% New predicate isco_update_set_var/7 for use in update clauses.
%
% Revision 1.11  2001/04/30 17:28:38  spa
% Include 'ops' to enable compilation...
%
% Revision 1.10  2001/04/30 12:47:59  spa
% Operator for "interface" assignment is now ":=" instead of ":".
%
% Revision 1.9  2001/04/29 07:38:30  spa
% Get rid of singleton variables.
%
% Revision 1.8  2001/04/27 15:00:07  spa
% Initial code to perform updates.
%
% Revision 1.7  2001/04/24 23:04:07  spa
% Prepare for update (rewrite isco_arg_list/3).
%
% Revision 1.6  2001/04/22 21:55:30  spa
% Transaction predicates.
% Update and Insert wrapper predicates.
%
% Revision 1.5  2001/04/21 00:43:10  spa
% Order of arguments for isco_odbc_fd_format/3.
%
% Revision 1.4  2001/04/21 00:31:23  spa
% Typo in vector-domained constrained variables.
%
% Revision 1.3  2001/04/21 00:30:01  spa
% New operator '@'/2, for non-positional runtime calls.
% Do something about constrained variables: very simple for now, as it only
% checks for upper and lower bounds, or for vector-domains.
%
% Revision 1.2  2001/04/19 12:53:06  spa
% Copyright.
% Support for setting up connections.
%
% Revision 1.1  2001/04/19 11:06:46  spa
% Initial version.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% comment-column: 48
% End:
