==================================
Preconditions and Postconditions
==================================

-----------------------------
Subprogram-based Assertions
-----------------------------

* **Explicit** part of a subprogram's **specification**

    - Unlike defensive code

* :dfn:`Precondition`

   - Assertion expected to hold **prior to** subprogram call

* :dfn:`Postcondition`

   - Assertion expected to hold **after** subprogram return

* Requirements and guarantees on both supplier and client
* Syntax uses **aspects**

  .. code:: Ada

     procedure Push (This : in out Stack_T;
                     Value : Content_T)
       with Pre  => not Full (This),
            Post => not Empty (This)
                    and Top (This) = Value;

---------------
Preconditions
---------------

* Define obligations on client for successful call

   - Precondition specifies required conditions
   - Clients must meet precondition for supplier to succeed

* Boolean expressions

   - Arbitrary complexity
   - Specified via aspect name :ada:`Pre`

* Checked prior to call by client

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre => not Full (This);

----------------
Postconditions
----------------

* Define obligations on supplier

   - Specify guaranteed conditions after call

* Boolean expressions (same as preconditions)

   - Specified via aspect name :ada:`Post`

* Content as for preconditions, plus some extras
* Checked after corresponding subprogram call

   - :ada:`Assertion_Error` raised if false

.. code:: Ada

   procedure Push (This : in out Stack;  Value : Content)
     with Pre  => not Full (This),
          Post => not Empty (This) and Top (This) = Value;
   ...
   function Top (This : Stack) return Content
     with Pre => not Empty (This);

------------------------------
Postcondition 'Old Attribute
------------------------------

* Values as they were just before the call
* Uses language-defined attribute :ada:`'Old`

   - Can be applied to most any visible object

      + May be expensive

   - Expression can be **arbitrary**

        + Typically :ada:`out`, :ada:`in out` parameters and globals

          .. code:: Ada

             procedure Increment (This : in out Integer) with
                 Pre  => This < Integer'Last,
                 Post => This = This'Old + 1;

------------------------------------------
Function Postcondition 'Result Attribute
------------------------------------------

Function result can be referenced by :ada:`'Result` on the function name

.. code:: Ada

      function Greatest_Common_Denominator (Num1, Num2 : Integer)
        return Integer with
          Pre  => Num1 > 0 and Num2 > 0,
          Post => Is_GCD (Num1, Num2,
                          Greatest_Common_Denominator'Result);
