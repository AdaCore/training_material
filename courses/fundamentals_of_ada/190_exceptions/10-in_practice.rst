=============
In Practice
=============

---------------------------------------
Fulfill Interface Promises to Clients
---------------------------------------

* If handled and not re-raised, normal processing continues at point of client's call
* Hence caller expectations must be satisfied

.. code:: Ada

   procedure Get (Reading : out Sensor_Reading) is
   begin
     ...
     Reading := New_Value;
     ...
   exceptions
     when Some_Error =>
       Reading := Default_Value;
   end Get;

   function Foo return Some_Type is
   begin
     ...
     return Determined_Value;
     ...
   exception
     when Some_Error =>
       return Default_Value; -- error if this isn't here
   end Foo;

-----------------------------------
Allow Clients to Avoid Exceptions
-----------------------------------

* Callee

   .. code:: Ada

      package Stack is
        Overflow : exception;
        Underflow : exception;
        function Full return Boolean;
        function Empty return Boolean;
        procedure Push (Item : in Some_Type);
        procedure Pop (Item : out Some_Type);
      end Stack;

* Caller

   .. code:: Ada

      if not Stack.Empty then
        Stack.Pop (...);  -- will not raise Underflow

----------------------------------
You Can Suppress Run-Time Checks
----------------------------------

* Syntax (could use a compiler switch instead)

   .. code:: Ada

      pragma Suppress (check-name [, [On =>] name]);

* Language-defined checks emitted by compiler
* Compiler may ignore request if unable to comply
* Behavior will be unpredictable if exceptions occur

   - Raised within the region of suppression
   - Propagated into region of suppression

.. code:: Ada

   pragma Suppress (Range_Check);
   pragma Suppress (Index_Check, On => Table);

-----------------------
Error Classifications
-----------------------

* Some errors must be detected at run-time

   - Corresponding to the predefined exceptions

* **Bounded Errors**

   - Need not be detected prior to/during execution if too hard
   - If not detected, range of possible effects is bounded

      + Possible effects are specified per error

   - Example: evaluating an un-initialized scalar variable
   - It might "work"!

* **Erroneous Execution**

   - Need not be detected prior to/during execution if too hard
   - If not detected, range of possible effects is not bounded
   - Example: Occurrence of a suppressed check

.. container:: speakernote

   Evaluation of an uninitialized scalar variable is a bounded error; evaluation of non-scalars is erroneous. See 13.9.1 Data Validity

