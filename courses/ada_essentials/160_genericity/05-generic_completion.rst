====================
Generic Completion
====================

-----------------------------
Generic and Freezing Points
-----------------------------

* A generic type **freezes** the type and needs the **full view**
* May force separation between its declaration (in spec) and instantiations (in private or body)

.. code:: Ada

  generic
     type Formal_T is private;
  package Generic_Package is
     Pointer : access Formal_T;
  end Generic_Package;

.. code:: Ada
  :number-lines: 1

  with Generic_Package;
  package Example is
     type Actual_T is private;
     package Instance is new Generic_Package (Actual_T);
  private
     type Actual_T is null record;
  end Example;

:error:`example.ads:4:45: error: premature use of private type`

-------------------------------
Generic Incomplete Parameters
-------------------------------

* A generic type can be incomplete
* Allows generic instantiations before full type definition
* Restricts the possible usages (only :ada:`access`)

.. code:: Ada

   generic
      type Formal_T; -- incomplete
   package Generic_Package is
      Pointer : access Formal_T;
   end Generic_Package;

   package Example is
      type Actual_T is private;
      package Instance is new Generic_Package (Actual_T);
   private
      type Actual_T is null record;
   end Example;

------
Quiz
------

.. code:: Ada

    generic
       type T1;
       A1 : access T1;
       type T2 is private;
       A2, B2 : T2;
    procedure G_P;
    procedure G_P is
       Flag : Boolean;
    begin
       -- Complete here
    end G_P;

Which of the following statement(s) is (are) legal for ``G_P``'s body?

A. :answermono:`Flag := A1 /= null`
B. ``Flag := A1.all'Size > 32``
C. :answermono:`Flag := A2 = B2`
D. ``Flag := A2 - B2 /= 0``

.. container:: animate

    A. Can always check an access for :ada:`null`
    B. :ada:`T1` is incomplete, so we don't know its size
    C. Comparison of private types is allowed
    D. We do not know if :ada:`T2` allows math
