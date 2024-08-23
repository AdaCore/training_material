=========
Summary
=========

---------------------------------------
Exceptions Are Not Always Appropriate
---------------------------------------

.. container:: columns

 .. container:: column

    * What does it mean to have an unexpected error in a safety-critical application?

       - Maybe there's no reasonable response

 .. container:: column

    .. image:: airbag_exception_handler.png

---------------------------------------
Relying on Exception Raising Is Risky
---------------------------------------

* They may be **suppressed**
   
   * By runtime environment
   * By build switches

* Not recommended

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        return Days'Succ (Today);
      exception
        when Constraint_Error =>
          return Days'First;
      end Tomorrow;

* Recommended

   .. code:: Ada

      function Tomorrow (Today : Days) return Days is
      begin
        if Today = Days'Last then
          return Days'First;
        else
          return Days'Succ (Today);
        end if;
      end Tomorrow;

---------
Summary
---------

* Should be for unexpected errors
* Give clients the ability to avoid them
* If handled, caller should see normal effect

   - Mode :ada:`out` parameters assigned
   - Function return values provided

* Package `Ada.Exceptions` provides views as objects

   - For both raising and special handling
   - Especially useful for debugging

* Checks may be suppressed
