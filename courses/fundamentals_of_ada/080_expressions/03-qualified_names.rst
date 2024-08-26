=================
Qualified Names
=================

---------------
Qualification
---------------

* Explicitly indicates the subtype of the value
* Syntax

   .. code:: Ada

      qualified_expression ::= subtype_mark'(expression) |
                               subtype_mark'aggregate

* Similar to conversion syntax

   - Mnemonic - "qualification uses quote"

* Various uses shown in course

   - Testing constraints
   - Removing ambiguity of overloading
   - Enhancing readability via explicitness

---------------------------------------
Testing Constraints Via Qualification
---------------------------------------

* Asserts value is compatible with subtype

   - Raises exception :ada:`Constraint_Error` if not true

.. code:: Ada

   subtype Weekdays is Days range Mon .. Fri;
   This_Day : Days;
   ...
   case Weekdays'(This_Day) is -- run-time error if out of range
     when Mon =>
       Arrive_Late;
       Leave_Early;
     when Tue .. Thur =>
       Arrive_Early;
       Leave_Late;
     when Fri =>
       Arrive_Early;
       Leave_Early;
   end case; -- no 'others' because all subtype values covered

