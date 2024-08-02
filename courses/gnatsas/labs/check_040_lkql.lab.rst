--------------------
GNATcheck LKQL Lab
--------------------

This lab is a hands-on walkthrough of creating your own LKQL rule for use with :toolname:`GNATcheck`.
You can use any text editor to create this rule file.

We want to create a rule that will flag all :ada:`integer` types that could be replaced by an enumeration type.
To flag those type declarations we must define a criteria list:

  * No use of any arithmetic or bitwise operator on the type
  * No type conversion from or to the type
  * No subtype definition
  * No type derivation
  * No reference to the type in generic instantiations

We're going to see how to express those criteria using LKQL.

---------------------------
Source Code Specification
---------------------------

.. code:: Ada

   with Ada.Text_IO;
   package Test_Pkg is

      type Good_Candidate is range 0 .. 100;
      function Supplier1 (X : Good_Candidate) return Good_Candidate;

      type Operator_T is range 0 .. 100;
      function Supplier2 (X : Operator_T) return Operator_T;

      type Conversion_Tgt_T is range 0 .. 100;
      function Supplier3 (X : Integer) return Conversion_Tgt_T;

      type Conversion_Source_T is range 0 .. 100;
      function Supplier4 (X : Conversion_Source_T) return Integer;

      type Subtype_Parent_T is range 0 .. 100;
      subtype Subtype_T is Subtype_Parent_T range 1 .. Subtype_Parent_T'Last;
      function Supplier5 (X : Subtype_T) return Subtype_T;

      type Derived_Parent_T is range 0 .. 100;
      type Derived_T is new Derived_Parent_T range 1 .. Derived_Parent_T'Last;
      function Supplier6 (X : Derived_T) return Derived_T;
   
      type Generic_Instantiaion_T is range 0 .. 100;
      package IO is new Ada.Text_IO.Integer_IO (Generic_Instantiaion_T);

   end Test_Pkg;

------------------
Source Code Body
------------------

.. code:: Ada

   package body Test_Pkg is

      function Supplier1 (X : Good_Candidate) return Good_Candidate is
        (X);

      function Supplier2 (X : Operator_T) return Operator_T is
        (X + 1);

      function Supplier3 (X : Integer) return Conversion_Tgt_T is
        (Conversion_Tgt_T (X));

      function Supplier4 (X : Conversion_Source_T) return Integer is
        (Integer (X));

      function Supplier5 (X : Subtype_T) return Subtype_T is
        (X);

      function Supplier6 (X : Derived_T) return Derived_T is
        (X);

   end Test_Pkg;

----------------------------
Step 1 - Flag All Integers
----------------------------

1. Create rule **enum_for_integer**

   a. In file :filename:`enum_for_integer.lkql`
   b. Use :lkql:`@check` annotation

2. Flag all integers

   a. Look for **p_is_int_type** LAL property using a *node kind* pattern

      .. code:: lkql

         @check
         fun enum_for_integer(node) = node is TypeDecl(p_is_int_type() is true)

3. Test it out - see what happens when you run the rule::

      gnatcheck -P prj.gpr --rules-dir=. -rules +Renum_for_integer

   * This gives us the output::

      test_pkg.ads:4:09: enum_for_integer
      test_pkg.ads:9:09: enum_for_integer
      test_pkg.ads:14:09: enum_for_integer
      test_pkg.ads:19:09: enum_for_integer
      test_pkg.ads:24:09: enum_for_integer
      test_pkg.ads:30:09: enum_for_integer
      test_pkg.ads:36:09: enum_for_integer

* All integer types are reported - we need to add filters


--------------------------
Step 2 - Improve Message
--------------------------

* Default message for boolean rules is just the name of the rule::

      test_pkg.ads:4:09: enum_for_integer

* To improve message, add **message** attribute to :lkql:`@check` token

   .. code:: lkql

      @check(message="Integer type could be replaced by an enumeration")
      fun enum_for_integer(node) =
         node is TypeDecl(p_is_int_type() is true)

* Gives much more information::

   test_pkg.ads:4:09: Integer type could be replaced by an enumeration
   test_pkg.ads:9:09: Integer type could be replaced by an enumeration
   test_pkg.ads:14:09: Integer type could be replaced by an enumeration
   test_pkg.ads:19:09: Integer type could be replaced by an enumeration
   test_pkg.ads:24:09: Integer type could be replaced by an enumeration
   test_pkg.ads:30:09: Integer type could be replaced by an enumeration
   test_pkg.ads:31:09: Integer type could be replaced by an enumeration
   test_pkg.ads:36:09: Integer type could be replaced by an enumeration

-----------------------------------
Step 3 - Implement First Criteria
-----------------------------------

1. Implement the first criteria:  **No use of any arithmetic or bitwise operator on the type**.

   a. Need to fetch all operators - use global :lkql:`select` with :lkql:`BinOp` and :lkql:`UnOp` node kind patterns. (Field :lkql:`f_op` contains the kind of the operator.)

      .. code:: lkql

         select BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                              OpPow or OpRem or OpXor or OpAnd or OpOr) or
                UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)

   b. :lkql:`select` returns list of :lkql:`BinOp` and :lkql:`UnOp`

      * Both inherit from the **Expr** node - so we use **p_expression_type** property to retrieve **TypeDecl** node associated with expression's actual type.

2. Implement function named :lkql:`arithmetic_ops` to return the list of **TypeDecl** used in arithmetic and logical operations

   .. code:: lkql

      fun arithmetic_ops() =
         [op.p_expression_type()
          for op in select
             BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

-------------------------------------
Step 4 - Use First Criteria in Rule
-------------------------------------

1. Update :lkql:`enum_for_integer` function to filter integer type declarations by excluding all **TypeDecl** used in operators

   .. code:: lkql

      @check
      fun enum_for_integer(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node]

2. Test it out - see what happens when you run the rule::

      gnatcheck -P prj.gpr --rules-dir=. -rules +Renum_for_integer

   * This gives us the output::

      test_pkg.ads:4:09: Integer type could be replaced by an enumeration
      test_pkg.ads:14:09: Integer type could be replaced by an enumeration
      test_pkg.ads:19:09: Integer type could be replaced by an enumeration
      test_pkg.ads:24:09: Integer type could be replaced by an enumeration
      test_pkg.ads:30:09: Integer type could be replaced by an enumeration
      test_pkg.ads:31:09: Integer type could be replaced by an enumeration
      test_pkg.ads:36:09: Integer type could be replaced by an enumeration

   *Note we are no longer reporting on the type at line 9*

------------------------------------
Step 5 - Implement Second Criteria
------------------------------------

* Criteria: **No type conversion from or to the type**

   * In the LAL tree type conversions appear as **CallExpr** whose referenced declaration is a **TypeDecl**

1. Implement new function :lkql:`types` to return list of **TypeDecl** used as target type in a conversion

   .. code:: lkql

      fun types() =
          [c.p_referenced_decl()
           for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list

   * **to_list** member is necessary if we want to combine lists later

2. Add our new filtering function in the rule body.

   .. code:: lkql

      @check
      fun enum_for_integer(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node] and
              not [t for t in types() if t == node]

* This version of :lkql:`types` only returns **TypeDecl** used as target in conversions - we also want to filter out source of conversions

-------------------------------
Step 6 - Improve Types Filter
-------------------------------

1. Update the :lkql:`types` function to also return types used as source type in conversions

   * LAL field **f_suffix**

      * Returns **ParamAssocList** with a single element - source expression
      * Use on type conversion nodes to get source of conversions

   .. code:: lkql

      fun types() =
         concat ([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                  for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list)

   * :lkql:`concat` function takes a list of lists and returns the one-dimensional result of concatenation of all lists.

2. Test it out - see what happens when you run the rule::

      gnatcheck -P prj.gpr --rules-dir=. -rules +Renum_for_integer

   * This gives us the output::

      test_pkg.ads:4:09: Integer type could be replaced by an enumeration
      test_pkg.ads:24:09: Integer type could be replaced by an enumeration
      test_pkg.ads:30:09: Integer type could be replaced by an enumeration
      test_pkg.ads:31:09: Integer type could be replaced by an enumeration
      test_pkg.ads:36:09: Integer type could be replaced by an enumeration

   * List of integers that meet our criteria is shrinking!

-----------------------------------
Step 7 - Implement Third Criteria
-----------------------------------

* Criteria: **No subtype definition**

1. We can use global :lkql:`select` with list comprehension filtering

   .. code:: lkql

      [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl]

   * Expression gives list of subtype **TypeDecl**. We can now add it to the result of the :lkql:`types` function.

   .. code:: lkql

      fun types() =
         concat ([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl].to_list

2. And once again test it out

      gnatcheck -P prj.gpr --rules-dir=. -rules +Renum_for_integer

   * This gives us the output::

      test_pkg.ads:4:09: Integer type could be replaced by an enumeration
      test_pkg.ads:30:09: Integer type could be replaced by an enumeration
      test_pkg.ads:31:09: Integer type could be replaced by an enumeration
      test_pkg.ads:36:09: Integer type could be replaced by an enumeration

   * Even fewer integers meet our criteria

------------------------------------
Step 8 - Implement Fourth Criteria
------------------------------------

* Criteria: **No type derivation**

1. We can implement this similar to the subtype check using

.. container:: latex_environment scriptsize

   .. code:: lkql

      [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
       for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

2. Add this expression to the :lkql:`types` function

.. container:: latex_environment scriptsize

   .. code:: lkql

      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl] &
         [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
          for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

-----------------------------------
Step 9 - Implement Final Criteria
-----------------------------------

* Criteria: **No reference to the type in generic instantiations**

1. Look in every each generic instantiation for identifiers referring to the type

   .. code:: lkql

      from (select GenericInstantiation) select Identifier

   * Gives list of each **Identifier** used in **GenericInstantiation**
   * Use **p_referenced_decl** property we to get associated declaration (that may be a **TypeDecl**

2. Express our query as a function

   .. code:: lkql

      fun instantiations() =
          [id.p_referenced_decl()
           for id in from select GenericInstantiation select Identifier].to_list

3. Add to :lkql:`enum_for_integer` function to finalize filtering

   .. code:: lkql

      @check
      fun enum_for_integer(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node] and
              not [t for t in types() if t == node] and
              not [t for t in instantiations() if t == node]

---------------------
Complete Rules File
---------------------

Here is the final view of our :filename:`enum_for_integer.lkql` file.

.. code:: lkql

   fun arithmetic_ops() =
      [op.p_expression_type()
       for op in select
          BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                        OpPow or OpRem or OpXor or OpAnd or OpOr) or
          UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

   fun instantiations() =
       [id.p_referenced_decl()
               for id in from select GenericInstantiation select Identifier].to_list

   fun types() =
       concat ([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
               for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
               [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl].to_list &
               [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
                for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

   @check(message="Integer type could be replaced by an enumeration")
   fun enum_for_integer(node) =
      node is TypeDecl(p_is_int_type() is true)
      when not [t for t in arithmetic_ops() if t == node]
       and not [t for t in types() if t == node]
       and not [t for t in instantiations() if t == node]

--------------
Final Result
--------------

* One more run to get the "correct" result

   gnatcheck -P prj.gpr --rules-dir=. -rules +Renum_for_integer

* This gives us the output

.. container:: latex_environment scriptsize

   ::

      test_pkg.ads:4:09: Integer type could be replaced by an enumeration
      test_pkg.ads:31:09: Integer type could be replaced by an enumeration

-------------------------------
Improving the Behavior Part 1
-------------------------------

* Speed of the rule as written is slow

   * Repeated calls to global :lkql:`select` query in :lkql:`arithmentic_ops`, :lkql:`types`, :lkql:`instantiations`

* Query functions can be instructed to cached their results

   * Use :lqkl:`@memoized` attribute

.. container:: latex_environment scriptsize

   .. code:: lkql

      @memoized
      fun arithmetic_ops() =
         [op.p_expression_type()
          for op in select
             BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

-------------------------------
Improving the Behavior Part 2
-------------------------------

* Take advantage of conditional short circuiting

   * Typically more arithmentic/logical operations than conversions, subtypes, instantiations
   * Swap filtering order to check for those last

.. container:: latex_environment small

   .. code:: lkql

      @check(message="integer type may be replaced by an enumeration")
      fun enum_for_integer(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in types() if t == node] and
              not [t for t in instantiations() if t == node] and
              not [t for t in arithmetic_ops() if t == node]
