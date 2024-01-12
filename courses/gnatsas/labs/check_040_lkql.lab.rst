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

----------------------------
Step 1 - Flag All Integers
----------------------------

We start by creating an LKQL rule with the :lkql:`@check` annotation. We call it :lkql:`integer_as_enum` and make it flag all integer type declarations with the **p_is_int_type** LAL property combined with a node kind pattern.

.. container:: latex_environment footnotesize

   .. code:: lkql

     @check
     fun integer_as_enum(node) = node is TypeDecl(p_is_int_type() is true)

At this stage this rule will flag all integer type declarations - we need to add our filters

-----------------------------------
Step 2 - Implement First Criteria
-----------------------------------

Now we're going to implement the first criteria of our list:  **No use of any arithmetic or bitwise operator on the type**.

To fetch all operators of the project, we can use a global :lkql:`select` with :lkql:`BinOp` and :lkql:`UnOp` node kind patterns. The field :lkql:`f_op` contains the kind of the operator.

.. container:: latex_environment scriptsize

   .. code:: lkql

      select BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)

+ This expression returns a list of :lkql:`BinOp` and :lkql:`UnOp` , both inherit from the **Expr** node so we can use the **p_expression_type** property to retrieve the **TypeDecl** node associated with the expression actual type.

---------------------------------
Step 3 - Create Helper Function
---------------------------------

We implement a function named :lkql:`arithmetic_ops` which returns the list of **TypeDecl** used in arithmetic and logical operations

.. container:: latex_environment small

   .. code:: lkql

      fun arithmetic_ops() =
         [op.p_expression_type()
          for op in select
             BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

Then we update the :lkql:`integer_as_enum` function to filter integer type declarations by excluding all **TypeDecl** used in operators

.. container:: latex_environment small

   .. code:: lkql

      @check
      fun integer_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node]

------------------------------------
Step 4 - Implement Second Criteria
------------------------------------

The next step of the rule development process is to implement the **No type conversion from or to the type** criteria. In the LAL tree the type conversions appear as **CallExpr** whose referenced declaration is a **TypeDecl**.

We implement a new function called :lkql:`types` which returns a list of **TypeDecl** used as target type in conversions in the Ada sources.

.. container:: latex_environment footnotesize

   .. code:: lkql

      fun types() =
         [c.p_referenced_decl()
          for c in select CallExpr(p_referenced_decl() is TypeDecl)]. to_list

Then we add our new filtering function in the rule body.

.. container:: latex_environment footnotesize

   .. code:: lkql

      @check
      fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node] and
              not [t for t in types() if t == node]

However this version of :lkql:`types` only returns **TypeDecl** used as target in type conversions and we want to filter both *source* and *target*.

-------------------------------
Step 5 - Improve Types Filter
-------------------------------

We update the :lkql:`types` function to also return types used as source type in conversions. To achieve this we can use the **f_suffix** LAL field on type conversion nodes, it returns a **ParamAssocList** with a single element - the source expression.

.. container:: latex_environment scriptsize

   .. code:: lkql

      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list)

As you can see we're using the :lkql:`concat` function which takes a list of lists and returns the mono-dimensional result of the concatenation of all those lists.

-----------------------------------
Step 6 - Implement Third Criteria
-----------------------------------

We can now move forward to the next step of the rule creation: implementing the **No subtype definition** criteria. To realize this we can use again a global :lkql:`select` with a list comprehension filtering.

.. container:: latex_environment scriptsize

   .. code:: lkql

      [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl]

This expression gives the list of subtyped **TypeDecl**. We can now add it to the result of the :lkql:`types` function.

.. container:: latex_environment scriptsize

   .. code:: lkql

      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl]

------------------------------------
Step 7 - Implement Fourth Criteria
------------------------------------

By now, we have implemented these criterias:

  + No use of any arithmetic or bitwise operator on the type
  + No type conversion from or to the type
  + No subtype definition

So the next criteria to add is **No type derivation**. It can be done the same way as for the subtypes, with the following expression.

.. container:: latex_environment scriptsize

   .. code:: lkql

      [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
       for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

+ We can now add this expression to the result of the :lkql:`types` function.

.. container:: latex_environment scriptsize

   .. code:: lkql

      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl] &
         [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
          for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

-----------------------------------
Step 8 - Implement Final Criteria
-----------------------------------

Finally we have to implement the last criteria **No reference to the type in generic instantiations**. We can achieve this by looking into each generic instantiation for identifiers referring to the type.

.. container:: latex_environment scriptsize

   .. code:: lkql

      from (select GenericInstantiation) select Identifier

This expression gives us the list of each **Identifier** which is used in a **GenericInstantiation** so with the **p_referenced_decl** property we can get their associated declaration that may be a **TypeDecl**. We may express this as a function.

.. container:: latex_environment scriptsize

   .. code:: lkql

      fun instantiations() =
         [id.p_referenced_decl()
          for id in from (select GenericInstantiation) select Identifier].to_list

Then we use it in the :lkql:`integer_types_as_enum` function to finalize integer type declaration filtering.

.. container:: latex_environment scriptsize

   .. code:: lkql

      @check
      fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node] and
              not [t for t in types() if t == node] and
              not [t for t in instantiations() if t == node]

---------------------
Complete Rules File
---------------------

Here is the final result of our :filename:`integer_types_as_enum.lkql` file.

.. container:: latex_environment tiny

   .. code:: lkql

      fun arithmetic_ops() =
         [op.p_expression_type()
          for op in select
             BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl] &
         [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
          for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

      fun instantiations() =
         [id.p_referenced_decl()
          for id in from (select GenericInstantiation) select Identifier].to_list

      @check(message=" integer type may be replaced by an enumeration ")
      fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in arithmetic_ops() if t == node] and
              not [t for t in types() if t == node] and
              not [t for t in instantiations() if t == node]

-------------------------------
Improving The Behavior Part 1
-------------------------------

If you test the rule by now you may find that it is really slow. This is mainly because of repeated calls to the very costly global :lkql:`select` in :lkql:`arithmetic_ops`, :lkql:`types` and :lkql:`instantiations`. To avoid this we can use the :lkql:`@memoized` annotation on those functions to cache their result when they are first called.

.. container:: latex_environment tiny

   .. code:: lkql

      @memoized
      fun arithmetic_ops() =
         [op.p_expression_type()
          for op in select
             BinOp(f_op is OpDiv or OpMinus or OpMod or OpMult or OpPlus or
                           OpPow or OpRem or OpXor or OpAnd or OpOr) or
             UnOp(f_op is OpAbs or OpMinus or OpPlus or OpNot)].to_list

      @memoized
      fun types() =
         concat([[c.p_referenced_decl(), c.f_suffix[1].f_r_expr.p_expression_type()]
                 for c in select CallExpr(p_referenced_decl() is TypeDecl)].to_list) &
         [s.f_subtype.f_name.p_referenced_decl() for s in select SubtypeDecl] &
         [c.f_type_def.f_subtype_indication.f_name.p_referenced_decl()
          for c in select TypeDecl(f_type_def is DerivedTypeDef)].to_list

      @memoized
      fun instantiations() =
         [id.p_referenced_decl()
          for id in from (select GenericInstantiation) select Identifier].to_list

-------------------------------
Improving The Behavior Part 2
-------------------------------

Finally we notice that there are many more arithmetic and logical operations than type conversions, subtype declarations or generic instantiations, so we can swap the filtering order in the :lkql:`integer_types_as_enum` function.

.. container:: latex_environment tiny

   .. code:: lkql

      @check(message="integer type may be replaced by an enumeration")
      fun integer_types_as_enum(node) =
         node is TypeDecl(p_is_int_type() is true)
         when not [t for t in types() if t == node] and
              not [t for t in instantiations() if t == node] and
              not [t for t in arithmetic_ops() if t == node]

-------------
We're Done!
-------------

That's it! We successfully implemented the rule which flags all integer type declarations that may be replaced by an enumeration type. It is ready to be used by :toolname:`GNATcheck`.

.. container:: latex_environment footnotesize

   :command:`gnatcheck -P prj.gpr -rules-dir=. -rules +Rinteger_types_as_enum`
