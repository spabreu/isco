% $Id$

% =============================================================================
%
% Unit to specialize code generation for PostgreSQL versions 6.0 and below.

:- unit(sql).

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

isco_prolog_class_body(Vs, CNAME, RNAME, HEAD, GOAL, CH, OC_VAR+MASK) :-
	functor(HEAD,CNAME,_),
	GOAL = (
	  ( MASK = 0 -> NMASK = -1 ; NMASK=MASK ),
	  MASK1 is NMASK /\ \0, % 0 is: 0 forced args (2^N-1)<<1
	  MASK2 is NMASK \/ 0,
	  isco_mask_to_var_list(HEAD, _, MASK2, VL),
	  isco_mask_to_var_list(HEAD, _, MASK1, VL1),
	  reverse(VL1, VL1r),
	  isco_var_list_to_select(VL1r, "", "", SELf),
	  format_to_codes(SQLin, 'select ~s from ~w o', [SELf, RNAME]),
	  G1 ),
	isco_where_clause(Vs, CH, G1, G2, SQLin, SQLout),
	G2 = (append(SQLout, OC_VAR, SQLfinal),
	      ( g_read(isco_debug_sql, 1) ->
		  format('sql(~w): ~s~n', [CH, SQLfinal]) ; true ),
	      isco_be_exec(CH, SQLfinal, SH),
	      isco_be_ntuples(CH, SH, NT),
	      g_assign(isco_ntuples, NT),
	      isco_be_fetch(CH, SH),
	      BODY),
	isco_prolog_class_body_fields(Vs, BODY, CH, SH, VL, MASK2, CNAME).

isco_prolog_class_body_fields(EOV, true, _, _, _, _, _) :- var(EOV), !.
isco_prolog_class_body_fields([], true, _, _, _, _, _).
isco_prolog_class_body_fields([_=f(V,N,_)|VARs], GOAL, CH, SH, VL, MASK, CN) :-
	N = OID, !,
	GOAL = (V=0, Gs),
	isco_prolog_class_body_fields(VARs, Gs, CH, SH, VL, MASK, CN).
isco_prolog_class_body_fields([_=f(V,N,_)|VARs], GOAL, CH, SH, VL, MASK, CN) :-
	N = instanceOf, !,
	GOAL = (V=CN, Gs),
	isco_prolog_class_body_fields(VARs, Gs, CH, SH, VL, MASK, CN).
isco_prolog_class_body_fields([P=f(V,N,T)|VARs], GOAL, CH, SH, VL, MASK, CN) :-
	isco_odbc_type(T, OT), odbc_type(OT, OTn),
	( isco_odbc_conv(T) -> CONV=yes ; CONV=no ),
	GOAL = (isco_be_get_arg(MASK, N, P, CH, SH, OTn, CONV, V, T, VL), Gs),
	isco_prolog_class_body_fields(VARs, Gs, CH, SH, VL, MASK, CN).


% -- SQL WHERE clause generation ----------------------------------------------

isco_where_clause(Vs, C, Gin, Gout, SQLin, SQLout) :-
	prolog([]) :> isco_where_clause(Vs, C, 'where', _, Gin, Gout, SQLin, SQLout).


isco_auto_inheritance :- fail.

has(_) :- fail.

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% End:
