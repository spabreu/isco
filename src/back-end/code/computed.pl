% $Id$

% =============================================================================
%
% Unit to specialize code generation for computed (Prolog) classes.

:- unit(computed).

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
	throw(error(computed_class_body(CNAME))).


% -- SQL WHERE clause generation ----------------------------------------------

isco_where_clause(Vs, C, Gin, Gout, SQLin, SQLout) :-
	throw(error(computed_class_where_clause(C))).

isco_auto_inheritance :- fail.

has(_) :- fail.

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% End:
