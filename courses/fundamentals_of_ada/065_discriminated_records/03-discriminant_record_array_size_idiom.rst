======================================
Discriminant Record Array Size Idiom
======================================

----------------------------
Vectors of Varying Lengths
----------------------------

* In Ada, array objects must be fixed length

   .. code:: Ada

      S : String (1 .. 80);
      A : array (M .. K*L) of Integer;

* We would like an object with a maximum length and a variable current length

   + Like a queue or a stack
   + Need two pieces of data

      * Array contents
      * Location of last valid element

* For common usage, we want this to be a type (probably a record)

   + Maximum size array for contents
   + Index for last valid element

---------------------------------
Simple Vector of Varying Length
---------------------------------

* Not unconstrained - we have to define a maximum length to make it a :dfn:`definite type`

.. code:: Ada

   type Simple_Vstring is
      record
         Last : Natural range 0 .. Max_Length := 0;
         Data : String (1 .. Max_Length) := (others => ' ');
      end record;

   Obj1 : Simple_Vstring := (0, (others => '-'));
   Obj2 : Simple_Vstring := (0, (others => '+'));
   Obj3 : Simple_Vstring;

* Issue - Operations need to consider :ada:`Last` component

   * :ada:`Obj1 = Obj2` will be false
   * Can redefine :ada:`=` to be something like

      .. code:: Ada

         if Obj1.Data (1 .. Obj1.Last) = Obj2.Data (1 .. Obj2.Last)

   * Same thing with concatentation

      .. code:: Ada

         Obj3.Last := Obj1.Last + Obj2.Last;
         Obj3.Data (1 .. Obj3.Last) := Obj1.Data (1 .. Obj1.Last) &
                                       Obj2.Data (1 .. Obj2.Last)
* Other Issues

   + Every object has same maximum length
   + ``Last`` needs to be maintained by program logic

----------------------------------------------------
Vector of Varying Length via Discriminated Records
----------------------------------------------------

* Discriminant can serve as bound of array component

   .. code:: Ada

      type Vstring (Last : Natural := 0) is
        record
          Data   : String (1 .. Last) := (others => ' ');
        end record;

* Mutable objects vs immutable objects

   + With default discriminant value (mutable), objects can be copied even if lengths are different
   + With no default discriminant value (immutable), objects of different lengths cannot be copied (and we can't change the length)

-----------------
Object Creation
-----------------

* When a mutable object is created, runtime assumes largest possible value

   + So this example is a problem

      .. code:: Ada

         type Vstring (Last : Natural := 0) is record
            Data   : String (1 .. Last) := (others => ' ');
         end record;

         Good : Vstring (10);
         Bad  : Vstring;

      + Compiler warning

         ``warning: creation of "Vstring" object may raise Storage_Error``

      + Run-time error

         ``raised STORAGE_ERROR : EXCEPTION_STACK_OVERFLOW`` 

* Better implementation

   .. code:: Ada

      subtype Length_T is natural range 0 .. 1_000;
      type Vstring (Last : Length_T := 0) is record
         Data   : String (1 .. Last) := (others => ' ');
      end record;

      Good      : Vstring (10);
      Also_Good : Vstring;

------------------------
Simplifying Operations
------------------------

* With mutable discriminated records, operations are simpler

   .. code:: Ada

      Obj : Simple_Vstring;
      Obj1 : Simple_Vstring := (6, " World");

   * Creation

      .. code:: Ada

         function Make (S : String)
           return Vstring is (S'length, S);
         Obj2 : Simple_Vstring := Make ("Hello");

   * Equality: :ada:`Obj1 = Obj2`

      * :ada:`Data` is exactly the correct length
      * if :ada:`Data` or :ada:`Last` is different, equality fails

   * Concatentation

      .. code:: Ada

         Obj := (Obj1.Last + Obj2.Last,
                 Obj1.Data & Obj2.Data);

------
Quiz
------

.. include:: ../quiz/mutable_with_array/quiz.rst
