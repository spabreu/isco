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

% == Cache unit generation ====================================================

:- include('ops').
:- unit(cache).

% -----------------------------------------------------------------------------

one(CLASS) :- isco_class(CLASS, _).

one(CLASS, (HEAD :- BODY)) :-
	isco_class(CLASS, ARITY),
	functor(GOAL, CLASS, ARITY),
	atom_concat(CLASS, '_cache', CLASS_CACHE),
	atom_concat(cached_, CLASS, CLASS_IS_CACHED),
	GOAL =.. [_ | ARGS],
	HEAD =.. [CLASS_CACHE | ARGS],
	BODY = (g_read(CLASS_IS_CACHED, 1)),
	!,
	GOAL.


% -----------------------------------------------------------------------------

% $Log$
% Revision 1.1  2003/04/08 13:46:40  spa
% *** empty log message ***
%

% Local Variables:
% mode: prolog
% mode: font-lock
% comment-column: 48
% End:
