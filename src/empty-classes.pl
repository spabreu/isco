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

% -- Fake classes to build stand-alone ISCO compiler --------------------------

:- dynamic(isco_setup_connection/2).
:- dynamic(isco_class/2).
:- dynamic(isco_superclass/2).
:- dynamic(isco_classtype/2).
:- dynamic(isco_compound_key/2).
:- dynamic(isco_compound_index/2).
:- dynamic(isco_field/4).
:- dynamic(isco_field_default/3).
:- dynamic(isco_field_not_null/2).
:- dynamic(isco_field_unique/2).
:- dynamic(isco_field_key/2).
:- dynamic(isco_field_index/2).
:- dynamic(isco_field_domain/4).
:- dynamic(isco_sequence/2).

:- initialization(abolish(isco_setup_connection/2)).
:- initialization(abolish(isco_class/2)).
:- initialization(abolish(isco_superclass/2)).
:- initialization(abolish(isco_classtype/2)).
:- initialization(abolish(isco_compound_key/2)).
:- initialization(abolish(isco_compound_index/2)).
:- initialization(abolish(isco_field/4)).
:- initialization(abolish(isco_field_default/3)).
:- initialization(abolish(isco_field_not_null/2)).
:- initialization(abolish(isco_field_unique/2)).
:- initialization(abolish(isco_field_key/2)).
:- initialization(abolish(isco_field_index/2)).
:- initialization(abolish(isco_field_domain/4)).
:- initialization(abolish(isco_sequence/2)).

% $Log$
% Revision 1.1  2003/01/06 14:27:16  spa
% Initial revision
%
% Revision 1.10  2001/08/24 18:16:25  spa
% Take note of "index" attributes (class and field).
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
% Revision 1.8  2001/06/23 23:25:14  spa
% isco_sequence/1 is now isco_sequence/2 again.
% new empty predicate isco_field_domain/4.
%
% Revision 1.7  2001/06/15 13:26:25  spa
% isco_sequence/2 should have been isco_sequence/1!
%
% Revision 1.6  2001/06/13 08:45:17  spa
% Added isco_sequence/2.
%
% Revision 1.5  2001/05/10 00:47:56  spa
% Bunch of new ones.
%
% Revision 1.4  2001/05/08 12:56:16  spa
% New empty predicate isco_field_not_null/2.
%
% Revision 1.3  2001/05/07 14:38:05  spa
% New empty predicate isco_classtype/2.
%
% Revision 1.2  2001/04/30 22:25:32  spa
% Ensure these predicates don't cause problems when linking, but also
% that they may be defined later on.
%
% Revision 1.1  2001/04/28 13:14:10  spa
% Predicates from pseudo-runtime.
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
