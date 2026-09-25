========================
Building a Simple Rule
========================

-------------------------------
Example of a Very Simple Rule
-------------------------------

* We want a rule to find all objects of :ada:`Integer` types

  * Because we know :ada:`Integer` may not be completely portable

* We start by creating our rule file :filename:`integer_objects.lkql`

  * First determine if the node is an object

    .. code:: lkql

      @check
      fun integer_objects(node) =
          node is ObjectDecl

    * Returns :lkql:`true` for any object declaration

----------------------
Checking Object Type
----------------------

* Object declaration has field :lkql:`f_type_expr`

  * Returns :lkql:`TypeExpr`
  * We want to know the base type of the object

* Type expression has field :lkql:`p_designated_type_decl()`

  * Returns :lkql:`BaseTypeDecl`
  * This is just the type declaration - we need the base type

* Type declaration has field :lkql:`p_base_type()`

  * Returns :lkql:`BaseTypeDecl`
  * This will be the base type of the object

* Base type declaration has field :lkql:`f_name`

  * Returns :lkql:`DefiningName`
  * We want the name as a string

* Defining Name has field :lkql:`p_fully_qualified_name()`

  * Returns :lkql:`str`
  * We want to see if this is :ada:`Integer`

-------------------------
Putting It All Together
-------------------------

:filename:`integer_objects.lkql`

.. code:: lkql

  @check
  fun integer_objects(node) =
      node is ObjectDecl and
      node.f_type_expr
          .p_designated_type_decl()
          .p_base_type()
          .f_name
          .p_fully_qualified_name() == "Standard.Integer"

:filename:`main.adb`

.. code:: ada
  :number-lines: 1
  :font-size: scriptsize

  procedure Main is
     type Integer_T is new Integer range -1_000 .. 1_000;
     type Number_T is range -1_000 .. 1_000;
     type Float_T is digits 6 range -1_000.0 .. 1_000.0;
     One   : Integer_T := 11;
     Two   : Number_T  := 222;
     Three : Float_T   := 3.3e-3;
  begin
     One   := One + 1;
     Two   := Two + 1;
     Three := Three * 1.1;
  end Main;

:command:`gnatcheck -P default.gpr --rules-dir=. --rule integer_objects`

.. code:: output
  :font-size: footnotesize

  main.adb:5:04: rule violation: integer_objects
  main.adb:6:04: error: internal issue at integer_objects.lkql:18:07:
      Null receiver in dot access [integer_objects]
  main.adb:7:04: error: internal issue at integer_objects.lkql:18:07:
      Null receiver in dot access [integer_objects]

----------------------
Cleaning Up the Rule
----------------------

.. code:: error

  Null receiver in dot access [integer_objects]

* Many of the fields downstream from :lkql:`ObjectDecl` can be null

  * But our rule assumes we can always dereference a field

* We want to make sure we only dereference **non-null** fields

:filename:`integer_objects.lkql`

.. code:: lkql
  :font-size: tiny
  
  @check
  fun integer_objects(node) =
  {
      val is_object = node is ObjectDecl;
      val has_type = is_object and node.f_type_expr != null;
      val has_designated = has_type and
          node.f_type_expr.p_designated_type_decl() != null;
      val has_base = has_designated and
          node.f_type_expr.p_designated_type_decl().p_base_type() != null;
      val has_name = has_base and
          node.f_type_expr.p_designated_type_decl().p_base_type().f_name != null;
      has_name and node.f_type_expr
                   .p_designated_type_decl()
                   .p_base_type()
                   .f_name
                   .p_fully_qualified_name() == "Standard.Integer"
  }

.. code:: output

  main.adb:5:04: rule violation: integer_objects

.. note::

  An *if expression* would be cleaner - we'll see that later
