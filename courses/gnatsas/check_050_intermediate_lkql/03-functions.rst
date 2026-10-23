===========
Functions
===========

-------------------
Review: Functions
-------------------

* The previous module introduced basic functions

  * Building block of LKQL

* Simple function to add two numbers

  .. code:: lkql

    fun add(left, right) = left + right

  * **Function name:** :lkql:`add`
  * **Parameters:** :lkql:`left`, :lkql:`right`

    * As there is no global data, most functions will have parameters

------------------
Decorators (1/2)
------------------

* Preface functions with :dfn:`decorators` to describe their behavior

* :lkql:`@check` indicates rule returning a boolean value

  * Operates on every syntax tree node within the source code
  * Returns true if node violates the rule

* :lkql:`@unit_check` indicates a unit rule

  * Operates on every library unit within the source code
  * Returns a *list* of failures within the unit

    * Each element of the list is a message and a location
    * Message and location indicate code that violates the rule

.. note::

  Files can have **either** :lkql:`@check` **or** :lkql:`@unit_check` but not both

  *Files containing support functions might have neither*

------------------
Decorators (2/2)
------------------

* :lkql:`@memoized` is a function that caches its result

  * Calls with same arguments return cached value

    * Functions cannot have side effects
    * So result must be the same

* Decorators can have annotations

  * **message** - violation message to be displayed
  * **help** - text displayed in a help message
  * Others beyond the scope of this course

  .. code:: lkql

    @check(message="renaming declaration")
    fun renamings(node) = ...

    @unit_check(help="usage of specified aspects")
    fun forbidden_aspects(unit) = ...

---------
Nesting
---------

* LKQL allows nested functions

  * Closures allowed (return reference to local environment)

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

* Local values can be used to store a function call

  * Can be set conditionally based on other values

  .. code:: lkql

    fun div(a, b) = a / b

    fun safe_div(a, b) = {
        # fn is null if b == 0, otherwise references "div"
        val fn = if b == 0 then null else div;
        fn? (a, b) # Return () if b = 0, else a/b
    }

* Use "safe access" to call function (:lkql:`fn?`)

  * If value is null, return value is :lkql:`()`
  * Otherwise, returns function result
