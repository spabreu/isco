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

% -- Operators  ---------------------------------------------------------------

:- op(600, xfx, '@').				% greater than ':='.
:- op(590, xfx, ':=').
:- op(590, xf,  ':+').				% now unary suffix
:- op(590, xf,  ':\\').				% now unary suffix

:- op(1100, xfx, 'class').			% greater than ','
:- op(1100,  fx, 'class').

:- op(1100, xfx, 'sequence').			% greater than ','
:- op(1100,  fx, 'sequence').

:- op(580,  fx, 'field').
:- op(580,  fx, 'domain').
:- op(580,  fx, 'key').
:- op(580,  fx, 'index').

:- op(550, xfy, '.').

:- op(150, xfx, '$').				% for PiLLoW
:- op(150, fx,  '$').				% for PiLLoW

% -- All of these must have lower precedence than ',' -------------------------

:- op(999,  yf, '<@').				% for sort order.
:- op(999,  yf, '>@').				% for sort order.

:- op(999,  yf, 'desc').			% for sort order.
:- op(999,  yf, 'asc').				% for sort order.

:- op(999,  yf, '!@').				% for sort order.
:- op(999,  yf, 'distinct').			% for sort order.


% $Log$
% Revision 1.1  2003/01/06 14:27:16  spa
% Initial revision
%
% Revision 1.16  2001/08/24 18:16:37  spa
% Take note of "index" attributes (class and field).
%
% Revision 1.15  2001/08/07 18:28:18  spa
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
% Revision 1.14  2001/06/12 12:52:49  spa
% Add '$' operators, to conform to PiLLoW!
%
% Revision 1.13  2001/06/12 09:15:08  spa
% Added delete (:\) operator.
%
% Revision 1.12  2001/05/21 18:04:08  spa
% New sort operator: '!@'.
%
% Revision 1.11  2001/05/19 01:03:20  spa
% 'sequence' may now be a binary operator.
%
% Revision 1.10  2001/05/19 00:49:43  spa
% New optional syntax for ORDER BY qualifiers.
%
% Revision 1.9  2001/05/10 23:55:05  spa
% Added "ORDER BY" operators.
%
% Revision 1.8  2001/05/03 13:47:19  spa
% Precedence of "class" operator greater than ','.
%
% Revision 1.7  2001/04/30 12:47:30  spa
% Operator for "interface" assignment is now ":=" instead of ":".
%
% Revision 1.6  2001/04/22 13:36:06  spa
% '@' operator now has higher precedence than ':', to allow for UPDATE and
% INSERT syntaxes.
%
% Revision 1.5  2001/04/21 00:28:24  spa
% New operator '@'/2, for non-positional runtime calls.
%
% Revision 1.4  2001/04/19 12:52:25  spa
% Copyright.
%
% Revision 1.3  2001/04/08 13:47:53  spa
% Many unused operators are now gone: follow a more demand-driven approach :)
%
% Revision 1.2  2000/12/28 08:39:23  spa
% *** empty log message ***
%
% Revision 1.1.1.1  2000/11/15 22:59:31  spa

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
