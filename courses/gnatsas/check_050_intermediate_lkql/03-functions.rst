===========
Functions
===========

-------------------
Review: Functions
-------------------

* Previous chapter covered basic functions

  * Building block of LKQL

* Simple function to add two numbers

  .. code:: lkql

    fun add(left, right) = left + right

  * Name of function: :lkql:`add`
  * Parameters: :lkql:`left` and :lkql:`right`

    * As there is no global data, most functions will have parameters

------------------
Decorators (1/2)
------------------

* Functions can be prefaced with :dfn:`decorators` describing their behavior

* :lkql:`@check` indicates rule returning a boolean value

  * Operates on nodes within the source code
  * Checks if node breaks the rule

* :lkql:`@unit_check` indicates rule returning list of objects

  * Each return object is a message and a location
  * Message and location indicate code that breaks the rule

.. note::

  Files can have **either** :lkql:`@check` **or** :lkql:`@unit_check` but not both

  *Files containing support functions could have neither*

------------------
Decorators (2/2)
------------------

* :lkql:`@memoized` is a function that 'remembers' its result

  * Function result is cached
  * Function called with same arguments returns cached value

    * Functions cannot have side effects
    * So result must be the same

* Decorators can have annotations (for documentation purposes)

  .. code:: lkql

    @check(message="Return true if node is a renaming")
    fun is_renaming(node) = node is RenamingClause

---------
Nesting
---------

* LKQL allows nested functions

  * Closures (returning reference to local environment) allowed

.. code:: lkql

  fun make_closure(closure_var) = {
      fun use_closure() = closure_var + 1;
      use_closure
  }

  # This will display the functional value "use_closure"
  print(make_closure(12))

---------------------
Function "Pointers"
---------------------

**Objects can be function calls**

  .. code:: lkql

    fun div(a, b) = a / b

    fun safe_div(a, b) = {
        # fn is null if b == 0, otherwise references "div"
        val fn = if b == 0 then null else div
        fn? (a, b) # Return () if b = 0, else a/b
