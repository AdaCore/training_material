==============
Introduction
==============

---------------------
Syntax and Examples
---------------------

**Syntax**

.. code:: bnf

  record_definition ::= 
     type identifier is record
        component_declaration
     end record;
  
  component_declaration ::= 
     defining_identifier_list : subtype_indication
        [:= default_expression];

**Example**

.. code:: Ada

   type Record1_T is record
      Is_Valid : Boolean := False;
      Content  : Integer;
   end record;

Records can be **discriminated** as well

.. code:: Ada

    type Varying_Length_String (Size : Natural := 0) is record
       Text : String (1 .. Size);
    end record;
