% $Id$ -*-Prolog-*-

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

% -- Command line parser  -----------------------------------------------------

top_level :-
	open('/dev/fd/2', write, _, [alias(err), buffering(line)]),
	argument_list(ARGS),
	cmdline :> args(ARGS, FILEs, ACTIONs),
	cmdline :> action(FILEs, ACTIONs).
top_level.

% -- Act upon a list of files and a command -----------------------------------

:- unit(cmdline).

action([], _) :-
	format(err, "no files specified.  use --help for information.~n", []),
	halt.

action(_, []) :-
	format(err, "no action specified.  use --help for information.~n", []),
	halt.

action(FILEs, [compile]) :-
	!,
	( g_read(isco_default_database, DBNAME), % preload symbol table...
	  DBNAME \= 0 ->
	    insert(ST, external(isco)=postgres(DBNAME))
	; insert(ST, external(isco)=postgres(isco)) ),
	!,
	isco_load_files(FILEs, [], PROGRAM),
	parser :> parse(APT, PROGRAM, []),
	( isco_apt(APT, ST) -> true ; throw(internal_error(isco_apt)) ),
	!,
	directives(ST) :> emit,
	schema(ST) :> emit,
	prolog(ST) :> emit.

action(FILEs, [php]) :-
	!,
	g_read(isco_exe_filename, EXENAME),
	isco_file_list(FILEs, Fs),
	isco_php_libs(Ls),
	format_to_atom(CMD, 'gplc -o ~w ~w ~w', [EXENAME, Fs, Ls]),
	write(CMD), nl,
	system(CMD).

action(FILEs, [sql]) :-
	!,
	isco_load_files(FILEs, [], PROGRAM),
	parser :> parse(APT, PROGRAM, []),
	isco_apt(APT, ST),
	!,
	isco_sql(ST) :> emit.

action(_, ACTION) :-
	format(err, "illegal action(s): ~w.  use --help for information.~n", [ACTION]),
	halt.

% -- Pick up command line arguments -------------------------------------------

args([], [], []).
args(FLAGs, Fs, ACTION) :-
	flag(FLAGs, REST, ACTION, ACTIONs),
	!,
	args(REST, Fs, ACTIONs).
args([DIR|As], FILEs, Ps) :-
	file_property(DIR, type(directory)),
	!,
	directory_files(DIR, DFILEs),
	isco_files(DFILEs, DIR, FILEs, Fs),
	args(As, Fs, Ps).
args([FILE|As], [FILE|Fs], Ps) :-
	file_exists(FILE), !,
	args(As, Fs, Ps).


flag(['-d', DBNAME|REST], REST, A, A) :- !,
	g_assign(isco_default_database, DBNAME).
flag([DB_DBNAME|REST], REST, A, A) :-
	atom_concat('--db=', DBNAME, DB_DBNAME), !,
	g_assign(isco_default_database, DBNAME).
flag(['-p', EXENAME|REST], REST, [php|A], A) :- !,
	g_assign(isco_exe_filename, EXENAME).
flag([PHP_EXENAME|REST], REST, [php|A], A) :-
	atom_concat('--php=', EXENAME, PHP_EXENAME), !,
	g_assign(isco_exe_filename, EXENAME).
flag([FLAG|REST], REST, [COMMAND|A], A) :-
	single_flag(FLAG, COMMAND), !.
flag([FLAG|_], _, _, _) :-
	atom_concat('-', _, FLAG),
	!,
	format(err, "~w: unknown flag.  use --help for information.~n", [FLAG]),
	halt.

%single_flag('--verbose', _) :-
%	g_assign(isco_verbose, 1).
%single_flag('-v', _) :-
%	g_assign(isco_verbose, 1).
single_flag('--sql', sql).
single_flag('-s', sql).
single_flag('--compile', compile).
single_flag('-c', compile).
single_flag('-h', _) :- !, single_flag('--help', _).
single_flag('--help', _) :-
	format("usage: isco FLAGS FILES...~n", []),
	format("where FILES is one or more of:~n", []),
	format("  FILENAME.isco  Include source file~n", []),
	format("  DIRNAME        Include all source files in directory~n", []),
	format("and FLAGS:~n", []),
	format("  -s, --sql      Produce SQL output~n", []),
	format("  -p, --php=NAME Produce PHP-ready executable in file NAME~n", []),
	format("  -c, --compile  Produce Prolog output~n", []),
	format("  -h, --help     This information~n", []),
	format("  -d, --db=NAME  Specify ISCO default database~n", []),
%	format("  -v, --verbose  Verbose information~n", []),
	format("  --version      Display version information and exit~n", []),
	halt.
single_flag('--version', _) :-
	isco_revision(R),
	format(err, "ISCO ~w~n", [R]),
	halt.


isco_files([], _DIR, FILEs, FILEs).
isco_files([FILE|FILEs], DIR, [XFILE|XFILEs], YFILEs) :-
	atom_concat(_, '.isco', FILE),
	!,
	format_to_atom(XFILE, "~w/~w", [DIR, FILE]),
	isco_files(FILEs, DIR, XFILEs, YFILEs).
isco_files([_|FILEs], DIR, XFILEs, YFILEs) :-
	isco_files(FILEs, DIR, XFILEs, YFILEs).


isco_file_list(FILEs, Fs) :- isco_file_list(FILEs, "", "", Fs).

isco_file_list([], _, FX, FXa) :- !, name(FXa, FX).
isco_file_list([-F|Fs], PFX, FXi, FXo) :- !,
	isco_lib_directory(D),
	format_to_codes(FX, "~w~w~w/~w", [FXi, PFX, D, F]),
	isco_file_list(Fs, " ", FX, FXo).
isco_file_list([F|Fs], PFX, FXi, FXo) :-
	format_to_codes(FX, "~w~w~w", [FXi, PFX, F]),
	isco_file_list(Fs, " ", FX, FXo).


isco_php_libs(Ls) :-
	findall(L, isco_php_lib(L), Lx),
	isco_file_list(Lx, Ls).

isco_php_lib(-'php-top.o').
isco_php_lib(-'top-level.o').
isco_php_lib(-'ctype.o').
isco_php_lib(-'pillow.o').
isco_php_lib('--no-debugger').
isco_php_lib('--no-top-level').
isco_php_lib(-'unixODBC_c.o').
isco_php_lib(-'unixODBC.pl').
isco_php_lib('-L -lodbc').
isco_php_lib(-'pl-pq.o').
isco_php_lib(-'pl-pq-prolog.pl').
isco_php_lib('-L -lpq').


% $Log$
% Revision 1.4  2003/04/15 15:03:06  spa
% - error messages to stderr (new "err" stream alias...)
%
% Revision 1.3  2003/04/11 08:53:04  spa
% Correct computed classes...
%
% Revision 1.2  2003/03/12 19:06:49  spa
% Use simplified unit names...
%
% Revision 1.1.1.1  2003/01/06 14:27:15  spa
% Imported into CVS
%
% Revision 1.16  2001/08/24 18:44:14  spa
% typo.
%
% Revision 1.15  2001/08/24 18:42:05  spa
% Symbol table is now preloaded in cmdline.pl.
%
% Revision 1.14  2001/08/24 18:02:59  spa
% Typo in creating isco_setup_connection/2. (missing quotes!)
%
% Revision 1.13  2001/08/24 07:30:20  spa
% Don't forget all the postgres stuff!
%
% Revision 1.12  2001/08/23 23:47:10  spa
% Default database now defaults to native postgres connection.
%
% Revision 1.11  2001/06/21 18:32:05  spa
% Implement --php=EXENAME flag.
%
% Revision 1.10  2001/06/12 09:14:12  spa
% Now use isco_lib_directory/1 to find out about the library's location.
%
% Revision 1.9  2001/05/10 00:46:41  spa
% Predicate isco_setup_connection/2 is discontiguous.
% Clause for isco_setup_connection/2 to create default ISCO connection.
% New --db=DBNAME flag.
% Flag parser reorganization.
%
% Revision 1.8  2001/05/09 11:12:46  spa
% Include ops.pl in output.
%
% Revision 1.7  2001/05/07 14:48:38  spa
% Revision now given by isco_revision/1.
%
% Revision 1.6  2001/05/03 13:46:46  spa
% CVS revision in messagem
%
% Revision 1.5  2001/04/30 17:23:26  spa
% Missing '-h' flag.
%
% Revision 1.4  2001/04/29 22:15:13  spa
% Some more flags and information on them.
%
% Revision 1.3  2001/04/29 10:54:35  spa
% More command line validation.
%
% Revision 1.2  2001/04/29 10:50:17  spa
% --help flag.
% Handle unknown flags.
%
% Revision 1.1  2001/04/29 07:36:55  spa
% Initial version.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
