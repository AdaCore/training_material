=====================
Object Declarations
=====================

---------------------
Object Declarations
---------------------

**Syntax**

  .. code:: bnf

    object_declaration ::= 
       identifier_list : [constant] typemark [:= expression];
    identifier_list ::= identifier {, identifier}

* An *object* is either :dfn:`variable` or :dfn:`constant`

  * **<identifier>** is the defining name for the object
  * **<typemark>** is the name describing the type of the object

* Simple objects

  .. code:: Ada

     An_Object : Some_Type;
     Max       : constant Some_Type := 200;
     Count     : Some_Type range 0 .. Max;
