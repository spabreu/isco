* CVS $Id$

* BUGS / PROBLEMS
** reference to unknown fields
in other classes: rules and integrity constraints.
currently causes false fields to be added to original class!!!
** Queries within setof/bagof
unused variables show up in rewritten goal: use another approach,
such as having a special term to indicate an unused column.
** oid attributes as foreign keys
it forgets to create the ok_CLASS_oid () function
** oids can't be referenced as foreign keys, for postgresql anyway...
** cyclic foreign keys

* ISCO language
** Generate strings instead of atoms, whenever possible
Partly done.  Still need to define new datatype.
** new types:
*** atom (same as text)
*** codes (same as text, just use strings instead of atoms)
*** term (rep. as text; use write_canonical/read_term)

* ISCO compiler and runtime
** REMOVE SQL DEPENDENCIES from general code
** Split back-ends
** Incorporate OIDs into all calls.
*** instance OID
All selects, updates, deletes (and inserts?) are to be of the form:
 ... SELECT oid, ... FROM ...;
-> done for selects
** class (table) OID / classOf operator
Use: SELECT relname from pg_class, CL where CL.tableoid=pg_class.oid;
ISCO syntax?
*** Use something like this:
SELECT p.oid, relname, p.* from pix p,w pg_class where p.tableoid=pg_class.oid;
(this is a slow query)
or
SELECT relname from pg_class c, pix p where p.oid=16568 and p.tableoid=c.oid;
(on-demand, after a regular oid-toting query)
(after that, use isco_classtype(ISCOCLASS, external(DB, PGCLASS)).)
*** ISCO Syntax:
extra argument: instanceOf=CLASSNAME
*** treated special, value is obtained as argument 2:
select o.oid, c.relname as instanceOf, o.* from CLASS o, pg_class c where c.oid=o.tableoid

** Do ACR thing

* Big things
** ISCO/ISTO multi-agentes: info parcial, etc.
** snapshots?
** documentos...
** RDF/XML
** EoI 6th Framework
** Projectos ALFA c/ parceiros internos ao CENTRIA
*** jmp: sistema de aconselhamento pedagógico :)
*** 

* Code generation
** Make code generation dependent upon back-end
Use CxLP!
*** ODBC/psql 6
*** ODBC/psql 7
*** ODBC/oracle
*** native psql 7
*** native psql 8
*** LDAP
*** SNMP
*** DNS
*** native MySQL
** ORDER BY clause
*** Syntax
*** Implementation
** Include files for use in PHP (backward compatibility)
** SQL code to generate/populate inferior databases (eg PostgreSQL)
** Java code
Figure out just what this will be...

# Local Variables:
# mode: outline
# mode: font-lock
# End:
