===========================
Private Part Construction
===========================

--------------------------------
Private Part and Recompilation
--------------------------------

* **Clients** can compile their code before the package body is compiled or even written
* Private part is part of the specification

   - Compiler needs info from private part for **clients'** code, e.g., storage layouts for private-typed objects

* Thus changes to private part require **client** recompilation

---------------------
Declarative Regions
---------------------

* Declarative region of the spec extends to the body

   - Anything declared there is visible from that point down
   - Thus anything declared in specification is visible in body

.. code:: Ada

   package Foo is
      type Private_T is private;
      procedure Visible (B : in out Private_T);
   private
      -- Hidden and Hidden_T are not visible to clients
      procedure Hidden (Param : in out Private_T);
      type Hidden_T is ...;
      type Private_T is array (1 .. 3) of Hidden_T;
   end Foo;

   package body Foo is
      -- Local is not visible to clients
      procedure Local (Param : in out Private_T) is ...
      procedure Hidden (Param : in out Private_T) is ...
      procedure Visible (Param : in out Private_T) is ...
    end Foo;

-----------------------
Full Type Declaration
-----------------------

.. container:: columns

 .. container:: column

    * May be any type

       - Predefined or user-defined
       - Including references to imported types

    * Contents of private part are unrestricted

       - Anything a package specification may contain
       - Types, subprograms, variables, etc.

 .. container:: column

   .. container:: latex_environment small

     .. code:: Ada

        package Designer is
          type Item_T is private;
          ...
        private
          type Vector is array (1.. 10)
             of Integer;
          function Initial
             return Vector;
          type Item_T record
            X, Y : Vector := Initial;
          end record;
        end Designer;

.. container:: speakernote

   Vector and Initial are not visible to callers

--------------------
Deferred Constants
--------------------

* Visible constants of a hidden representation

   - Value is "deferred" to private part
   - Value must be provided in private part

* Not just for private types, but usually so

.. code:: Ada

   package Example is
     type Set is private;
     Null_Set : constant Set; -- exported name
     ...
   private
     type Index is range ...
     type Set is array (Index) of Boolean;
     Null_Set : constant Set :=  -- definition
        (others => False);
   end Example;

------
Quiz
------

.. code:: Ada

   package Example is
      type Private_T is private;
      Object_A : Private_T;
      procedure Proc (Param : in out Private_T);
   private
      type Private_T is new Integer;
      Object_B : Private_T;
   end package Example;

   package body Example is
      Object_C : Private_T;
      procedure Proc (Param : in out Private_T) is null;
   end Example;

Which object definition(s) is (are) legal?

   A. ``Object_A``
   B. :answermono:`Object_B`
   C. :answermono:`Object_C`
   D. None of the above

.. container:: animate

   An object cannot be declared until its type is fully declared.
   :ada:`Object_A` could be declared constant, but then it would
   have to be finalized in the :ada:`private` section.

