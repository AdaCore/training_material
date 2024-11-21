============================
Referencing Other Packages
============================

----------------
 `with` Clause
----------------

* When package :ada:`Client` needs access to package :ada:`Server`, it uses a :ada:`with` clause

   - Specify the library units that :ada:`Client` depends upon
   - The "context" in which the unit is compiled
   - :ada:`Client`'s code gets **visibility** over :ada:`Server`'s specification


* Syntax (simplified)

   .. code:: Ada

      context_clause ::= { context_item }
      context_item ::= with_clause | use_clause
      with_clause ::= with library_unit_name
                      { , library_unit_name };

.. code:: Ada

   with Server; -- dependency
   procedure Client is

----------------------------
Referencing Exported Items
----------------------------

* Achieved via "dot notation"
* Package Specification

   .. code:: Ada

      package Float_Stack is
        procedure Push (X : in Float);
        procedure Pop (X : out Float);
      end Float_Stack;

* Package Reference

   .. code:: Ada

      with Float_Stack;
      procedure Test is
         X : Float;
      begin
         Float_Stack.Pop (X);
         Float_Stack.Push (12.0);
         ...

----------------------
`with` Clause Syntax
----------------------

* A library unit is a package or subprogram that is not nested within another unit

   - Typically in its own file(s)

     - e.g. for package :ada:`Test`, GNAT defaults to expect the spec in :filename:`test.ads` and body in :filename:`test.adb`)

* Only library units may appear in a :ada:`with` statement

   * Can be a package or a standalone subprogram

* Due to the :ada:`with` syntax, library units cannot be overloaded

   - If overloading allowed, which `P` would :ada:`with P;` refer to?

----------------
What To Import
----------------

* Need only name direct dependencies

   - Those actually referenced in the corresponding unit

* Will not cause compilation of referenced units

   - Unlike "include directives" of some languages

.. code:: Ada

   package A is
     type Something is ...
   end A;

   with A;
   package B is
     type Something is record
       Field : A.Something;
     end record;
   end B;

   with B; -- no "with" of A
   procedure Foo is
     X : B.Something;
   begin
     X.Field := ...

