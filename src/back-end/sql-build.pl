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

:- unit(isco_sql).

% == SQL code generation (schema) for ISCO ====================================

% note: isco_schema_class/1 returns classes after a topological sort, yielding
%       the least dependant classes first.

emit :- emit(_).

emit(SEQ) :- isco_sequence(SEQ, regular), isco_sql_code(SEQ), nl, fail.
emit(REL) :- nl, isco_schema_class(REL), isco_sql_code(REL), nl, fail.
emit(_).


build :- build(_).

build(REL) :-
	current_output(STREAM),			% to preserve
	build(STREAM, REL).
	

build(STREAM, REL) :-
	isco_connection(CH),			% back-end connection
	open_output_codes_stream(OUT), add_stream_alias(OUT, sql),
	set_output(sql),			% open codes stream alias
	( isco_sequence(REL, regular)
	; isco_schema_class(REL) ),
	isco_sql_code(REL),			% generate SQL code (nondet)
	flush_output(sql),
	close_output_codes_stream(sql, SQL),	% get output
	set_output(STREAM),
	(g_read(isco_debug_sql, 1) ->
	    format(STREAM, 'sql(~w): ~s ...', [CH, SQL]),
	    flush_output(STREAM)
%	    get0(_)
	;   true),
	catch((isco_be_exec(CH, SQL, _),
	       g_read(isco_debug_sql,1) -> format(STREAM, ' ok\n', []) ; true),
	      error(system_error(ERROR), _PRED),
	      format(STREAM, ' warning:\n~w', [ERROR])),
	open_output_codes_stream(OUT2), add_stream_alias(OUT2, sql),
	set_output(sql),			% reopen codes stream alias
	fail.
build(STREAM, _) :-
	( close_output_codes_stream(sql, _) -> true ; true ),
	set_output(STREAM).


isco_sql_code(REL) :- isco_sql_code(REL, o_rel).


%% -- Generate code for an ISCO relation --------------------------------------
%%
%% REL: ISCO relation name
%% DBTYPE: one of
%%   o_rel ("object relational") -> explicit inheritance in database
%%   rel   ("relational") -> simulate inheritance by duplicating superclass

isco_sql_code(SNAME, _) :-
	isco_sequence(SNAME, regular),
	format('create sequence "~w";', [SNAME]).

isco_sql_code(REL, DBTYPE) :-
	isco_class(REL, _),
	isco_sql_header(REL, COMMA),
	isco_sql_fields(REL, DBTYPE, COMMA),
	isco_sql_class_attributes(REL, DBTYPE),
	isco_sql_trail(REL, DBTYPE).

isco_sql_code(REL, DBTYPE) :-
	isco_class(REL, _),
	isco_sql_extra_stuff(REL, DBTYPE).


% -- Generate the SQL table creation ------------------------------------------

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_sql_header(CNAME, '\n') :-
	format('create table "~w" (', [CNAME]).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_sql_fields(REL, o_rel, COMMA) :- !,
	isco_class(REL, ARITY),
	( isco_superclass(REL, SC) -> isco_class(SC, SCARITY) ; SCARITY=0 ),
	isco_sql_fields(SCARITY, ARITY, REL, COMMA).

isco_sql_fields(REL, rel, COMMA) :- !,
	isco_class(REL, ARITY),
	isco_sql_fields(0, ARITY, REL, COMMA).


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_sql_fields(N, N, _, _) :- !.
isco_sql_fields(N, M, R, X) :-
	N1 is N+1,
	( isco_field(R, F, N1, TYPE) -> true ; fail ),
	( F=oid, N1=1 ->	% OID is built-in
	    NEXT_X=X
	;   F=instanceOf, N1=2 -> % instanceOf is built-in
	    NEXT_X=X
	;
	    isco_odbc_generated_type(TYPE, FTYPE),
	    format('~w  "~w" ~w', [X, F, FTYPE]),
	    (	isco_sql_field_attributes(R, F), fail ; true ),
	    NEXT_X=',\n' ),
	isco_sql_fields(N1, M, R, NEXT_X).

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_default(CLASS, FIELD, DEFAULT),
	isco_field(CLASS, FIELD, _, TYPE),
	isco_odbc_format(TYPE, DEFAULT, DEF),
	format(' default (~s)', [DEF]).

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_unique(CLASS, FIELD),
	format(' unique', []).

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_not_null(CLASS, FIELD),
	format(' not null', []).

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_key(CLASS, FIELD),
	format(' primary key', []).

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_domain(CLASS, FIELD, RCLASS, RFIELD),
	isco_has_subclass(RCLASS, _), !,
	% ---------------------------------------------------------------------
	% the domain is the root of an inheritance hierarchy: must use a
	% domain-verification function...
	% ---------------------------------------------------------------------
	format(' check (ok_~w_~w ("~w"))', [RCLASS, RFIELD, FIELD]).

isco_sql_field_attributes(CLASS, FIELD) :-
	isco_field_domain(CLASS, FIELD, RCLASS, RFIELD),
	format(' references "~w" ("~w") deferrable', [RCLASS, RFIELD]).



% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

isco_sql_class_attributes(REL, _) :-
	isco_subclass(SC, REL), isco_compound_key(SC, FIELDs), !,
	format(',\n    primary key (', []),
	isco_sql_class_keylist(FIELDs, ''),
	format(')', []).
isco_sql_class_attributes(_, _).


isco_sql_class_keylist([], _).
isco_sql_class_keylist([F|Fs], PFX) :-
	format('~w"~w"', [PFX, F]),
	isco_sql_class_keylist(Fs, ', ').


isco_sql_trail(REL, o_rel) :-
	isco_superclass(REL, SC), !,
	format(')\n    inherits ("~w");', [SC]).
isco_sql_trail(_, _) :-
	format(');', []).



% -----------------------------------------------------------------------------

isco_sql_extra_stuff(REL, _) :-
	isco_compound_index(REL, FIELDs),
	atom_concat(cic__, REL, RELX),
	g_read(RELX, RELXN), RELXN1 is RELXN+1, g_assign(RELX, RELXN1),
	format('create index "~w_~w" on "~w" (', [RELX, RELXN1, REL]),
	isco_sql_class_keylist(FIELDs, ''),
	format(');', []).

isco_sql_extra_stuff(REL, _) :-
	isco_field_index(REL, FNAME),
	atom_concat(ci__, REL, RELX),
	g_read(RELX, RELXN), RELXN1 is RELXN+1, g_assign(RELX, RELXN1),
	format('create index "~w__~w" on "~w" (', [RELX, RELXN1, REL]),
	format('"~w");', [FNAME]).

isco_sql_extra_stuff(REL, _) :-
	once((isco_superclass(REL, SC),
	      isco_field_key(REL, K),		% primary key shared w/ super
	      isco_field_key(SC, K))),
	format('alter table "~w" add constraint "~w_pkey" primary key ("~w");',
	       [REL, REL, K]).

isco_sql_extra_stuff(REL, _) :-
	isco_field_key(REL, FIELD),
	once(isco_has_subclass(REL, _)),
	isco_field(REL, FIELD, _POS, ITYPE),
	isco_odbc_generated_type(ITYPE, TYPE),
	format('create function ni_~w_~w (~w) returns bigint\n',
	       [REL, FIELD, TYPE]),
	format(' as \'select count(*) from "~w" where "~w" = $1\'\n',
	       [REL, FIELD]),
	format('  language \'sql\';', []).

isco_sql_extra_stuff(REL, _) :-
	isco_field_key(REL, FIELD),
	once(isco_has_subclass(REL, _)),
	isco_field(REL, FIELD, _POS, ITYPE),
	isco_odbc_generated_type(ITYPE, TYPE),
	format('create function ok_~w_~w (~w) returns bool\n',
	       [REL, FIELD, TYPE]),
	format(' as \'select ni_~w_~w($1) = 1\' language \'sql\';',
	       [REL, FIELD]).


% -----------------------------------------------------------------------------

% $Log$
% Revision 1.2  2003/04/10 16:46:19  spa
% predicates emit and build now come both in /0 and /1 forms.
% build/2 takes special care not to do anything silly...
% avoid spurious newlines at the end of SQL commands.
% resort to domain-checking functions when appropriate (eg. keys, "references").
% isco_sql_extra_stuff/2: don't gratuitously succeed at the end...
%
% Revision 1.1  2003/03/30 22:58:13  spa
% Rename sql.pl to sql-build.pl, to avoid collision with code/sql.pl.
%
% Revision 1.4  2003/03/12 19:05:25  spa
% - new predicates build/0, /1 and /2: like emit/0 but they actually call the
%   back-end to create the tables, etc.
%
% - isco_sql_inherited_key/1 now a special case of isco_sql_extra_stuff/2.
%
% - build/2 relies on isco_sql_code/2 returning exactly ONE SQL statement per
%   solution (eg, no index creation after a "create table")
%
% Revision 1.3  2003/03/10 23:18:03  spa
% Create indexes in subclasses, whenever needed.
%
% Revision 1.2  2003/03/07 15:30:27  spa
% *** empty log message ***
%
% Revision 1.1  2003/01/06 15:15:01  spa
% *** empty log message ***
%
% Revision 1.1.1.1  2003/01/06 14:27:18  spa
% Imported into CVS
%
% Revision 1.11  2001/10/14 07:31:27  spa
% properly include commas after "classe" field.
%
% Revision 1.10  2001/08/24 18:16:08  spa
% Generate what must be done for "index" attributes (class and field).
%
% Revision 1.9  2001/08/07 18:28:18  spa
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
% Revision 1.8  2001/06/23 23:24:46  spa
% isco_sequence/1 is now isco_sequence/2 again.
% new predicate isco_sql_code/0.
% "double quotes" around class and argument names, everywhere.
%
% Revision 1.7  2001/06/15 13:26:39  spa
% isco_sequence/2 should have been isco_sequence/1!
%
% Revision 1.6  2001/06/13 08:45:04  spa
% Initial sequence code generation.
%
% Revision 1.5  2001/05/10 00:47:30  spa
% Completely redone.  Does all the basic schema now (no constraints yet).
%
% Revision 1.4  2001/05/08 12:55:39  spa
% No pretty-printing as this will be used to actually execute a query.
%
% Revision 1.3  2001/04/29 07:38:56  spa
% Avoid some postgresisms.
%
% Revision 1.2  2001/04/27 18:44:46  spa
% First working version.
%
% Revision 1.1  2001/04/21 00:26:28  spa
% Initial version;
% In fact this might be superfluous as the database creation should be done from
% a running application; maybe via a predicate to create each regular relation.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% mode: outline-minor
% comment-column: 48
% End:
