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

:- initialization(				% Copyright notice
	(_DATE = "$Date$",
	 isco_revision(R),
	 format("ISCO version ~w~n", [R]), 
	 format("Copyright (C) 1998-2008 Salvador Abreu~n~n", []))).

% -----------------------------------------------------------------------------

% $Log$
% Revision 1.3  2008/09/10 16:53:02  spa
% *** empty log message ***
%
% Revision 1.2  2006/07/18 07:46:51  spa
% *** empty log message ***
%
% Revision 1.1.1.1  2003/01/06 14:27:16  spa
% Imported into CVS
%
% Revision 1.96  2001/05/07 14:48:50  spa
% isco_revision/1 now elsewhere.
%
% Revision 1.95  2001/05/07 14:45:48  spa
% new predicate isco_revision/1.
%
% Revision 1.94  2001/05/04 12:25:10  spa
% Improved SQL parser.
%
% Revision 1.93  2001/05/03 13:46:58  spa
% CVS revision in message.
%
% Revision 1.92  2001/05/02 17:32:14  spa
% Direct revision output.
%
% Revision 1.91  2001/05/02 17:30:37  spa
% Inspect versions from CVS directly.
%
% Revision 1.90  2001/05/02 17:29:13  spa
% Initial release.
%
% Revision 1.1  2001/05/02 17:29:12  spa
% intermediate
%

% Local Variables:
% mode: prolog
% mode: font-lock
% End:
