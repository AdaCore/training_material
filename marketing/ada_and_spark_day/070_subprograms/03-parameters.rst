============
Parameters
============

---------------------------------
Parameter Associations in Calls
---------------------------------

* Associate formal parameters with actuals
* Both positional and named association allowed

.. code:: Ada

   Something (ActualX, Formal2 => ActualY);
   Something (Formal2 => ActualY, Formal1 => ActualX);

* Having named **then** positional is forbidden

.. code:: Ada

   -- Compilation Error
   Something (Formal1 => ActualX, ActualY);

-----------------
Parameter Modes
-----------------

* Mode :ada:`in`

  - Formal parameter is :ada:`constant`

    * So actual is not modified either

  - Can have **default**, used when **no value** is provided

    .. code:: Ada

      procedure P (N : in Integer := 1; M : in Positive);
      [...]
      P (M => 2);

* Mode :ada:`out`

   - Writing is **expected**
   - Reading is **allowed**
   - Actual **must** be a writable object

* Mode :ada:`in out`

   - Actual is expected to be **both** read and written
   - Actual **must** be a writable object

-----------------
Function Return
-----------------

* Function :ada:`return` **must** always be handled

* Return type is **not** an object

  * Type does not have to be constrained

.. code:: Ada

   function From_String (Value : String) return Integer;
   function To_String (Value : Integer) return String;
