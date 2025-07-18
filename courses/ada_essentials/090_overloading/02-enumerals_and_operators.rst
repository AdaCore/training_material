=========================
Enumerals and Operators
=========================

-----------------------
Overloading Enumerals
-----------------------

* Each is treated as if a function identifier
* Thus same rules as for function identifier overloading

.. code:: Ada

   type Stop_Light is (Red, Yellow, Green);
   type Colors is (Red, Blue, Green);
   Shade : Colors := Red;
   Current_Value : Stop_Light := Red;

.. container:: speakernote

   Red and Green are overloaded (but context helps to resolve)

-------------------------------
Overloadable Operator Symbols
-------------------------------

* Only those defined by the language already

   - Users cannot introduce new operator symbols

* Note that assignment (:=) is not an operator
* Operators (in precedence order)

  :Logicals: and, or, xor
  :Relationals: ``<``, ``<=``, ``=``, ``>=``, ``>``
  :Unary: ``+``, ``-``
  :Binary: ``+``, ``-``, ``&``
  :Multiplying: ``*``, ``/``, ``mod``, ``rem``
  :Highest precedence : ``**``, ``abs``, ``not``

-------------------------------------
Parameters for Overloaded Operators
-------------------------------------

* Must not change syntax of calls

   - Number of parameters must remain same (unary, binary...)
   - No default expressions allowed for operators

* Infix calls use positional parameter associations

   - Left actual goes to first formal, right actual goes to second formal
   - Definition

      .. code:: Ada

         function "*" (Left, Right : Integer) return Integer;

   - Usage

      .. code:: Ada

         X := 2 * 3;

* Named parameter associations allowed but ugly

   - Requires prefix notation for call

   .. code:: Ada

      X := "*" (Left => 2, Right => 3);

