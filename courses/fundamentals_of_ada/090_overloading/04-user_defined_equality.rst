=======================
User-Defined Equality
=======================

--------------
Introduction
--------------

* Overloading equality is tricky

    - Is it bit-wise comparison or semantic comparison?
    - Result allowed to be non-boolean

* Complexity with user-defined composed types

    - Composition of equality


-----------------------
User-Defined Equality
-----------------------

* Allowed like any other operator

   - Must remain a binary operator

* May have any parameter and result types

   - Typically declared to return type Boolean

* Non-Boolean result example:

   .. code:: Ada

      type Fuzzy_Result is (Unknown, False, True);
      function "=" (Left : Foo;  Right : Bar)
          return Fuzzy_Result;

------------------------------------
User-Defined `=` Returning Boolean
------------------------------------

* Implicitly declares ``/=``
* Thus negation has consistent meaning

   .. code:: Ada

      if X /= Y then
      if not (X = Y) then
      if X not = Y then

* No explicit declaration of ``/=`` returning Boolean

   - Returning values of other types is allowed

      .. code:: Ada

         function "/=" (Left : Foo;  Right : Bar)
             return Fuzzy_Result;

-------------------------------
User-Defined Equality Example
-------------------------------

* Especially useful for composite types
* Predefined ``=`` is bit-wise comparison over entire structure so may be inappropriate semantics
* Given the following types:

   .. code:: Ada

      Max : constant := 100;
      type Index is range 0 .. Max;
      type Vector is array (Index range 1 .. Max) of Integer;
      type Stack is record
        Values : Vector;
        Top : Index := 0;
      end record;

* Equality function might look like:

   .. code:: Ada

      function "=" (Left, Right : Stack) return Boolean is
      begin
        if Left.Top /= Right.Top then -- not same size
          return False;
        else -- compare values
          for K in 1 .. Left.Top loop
            if Left.Values (K) /= Right.Values (K) then
              return False;
            end if;
          end loop;
        end if;
        return True;
      end "=";

