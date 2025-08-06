=======================
Containers Operations
=======================

-------------------
Common Operations
-------------------

* Lots of common operations

    - What is available depends greatly on the exact container type
    - ... so does syntax

* Insertion
* Iteration
* Comparison
* Sort
* Search
* Aggregates

-----------
Insertion
-----------

* May be in order :ada:`Append` or :ada:`Prepend`
* May be :ada:`Insert` (at random or at given index)
* May :ada:`Replace` an existing component

.. code:: Ada

   Student_Per_Day.Append (10);
   Student_Per_Day.Append (8);
   Student_Per_Day.Append (9);

   Received_Parcels.Insert ("FEDEX AX431661VD");
   Received_Parcels.Insert ("UPS ZZ-44-I12");

   Math_Constants.Insert
     (To_Unbounded_String ("Pi"), 3.141_59);
   Math_Constants.Insert (To_Unbounded_String ("e"), 2.718);

-----------
Iteration
-----------

* Container has a :ada:`Cursor` type

    - Points to a component in a container
    - Can be used for advanced iterations

.. code:: Ada

   for Student_Count of Student_Per_Day loop
      Put_Line (Integer'Image (Student_Count));
   end loop;

   for Parcel_Id of Received_Parcels loop
      Put_Line (Parcel_Id);
   end loop;

   -- We use the cursor to have both key and value
   for C in Math_Constants.Iterate loop
      Put_Line
        (To_String (Key (C)) & " = " &
         Float'Image (Element (C)));
   end loop;

------------
Comparison
------------

.. code:: Ada

   -- xxx2 are objects with the exact same content
   pragma Assert (Student_Per_Day = Student_Per_Day2);
   pragma Assert (Received_Parcels = Received_Parcels2);
   pragma Assert (Math_Constants = Math_Constants2);

   -- After changing the content, equality does not hold
   Student_Per_Day.Append (10);
   Received_Parcels.Insert ("Chronopost 13214GUU-035");
   Math_Constants.Insert (To_Unbounded_String ("G"), 9.8);

   pragma Assert (Student_Per_Day /= Student_Per_Day2);
   pragma Assert (Received_Parcels /= Received_Parcels2);
   pragma Assert (Math_Constants /= Math_Constants2);

------
Sort
------

* Arrays

   - `Ada.Containers.Generic_Array_Sort`
   - `Ada.Containers.Generic_Constrained_Array_Sort`

* Any type that supports indexing

   - `Ada.Containers.Generic_Sort`

.. code:: Ada

   procedure Sort
     (V    : in out Pkg_Vectors.Vector; First : Index_Type;
      Last :        Index_Type)
   is
      procedure Swap_Object (A, B : Index_Type) is
         Temp : Integer := V (A);
      begin
         V (A) := V (B);
         V (B) := Temp;
      end Swap_Object;

      procedure Sort_Object is new Ada.Containers
        .Generic_Sort
        (Index_Type => Index_Type, Before => "<",
         Swap       => Swap_Object);
   begin
      Sort_Object (First, Last);
   end Sort;

--------
Search
--------

* Use :ada:`Find` for a :ada:`Cursor`

    - :ada:`<Pkg>.No_Element` returned if unsuccesful

        + :ada:`Has_Element (No_Element) = False`

* Use :ada:`Find_Index` for an :ada:`Index_Type` (vectors)

.. code:: Ada

   C : constant Pkg_Vectors.Cursor :=
     Student_Per_Day.Find (10);
   C2 : constant Pkg_Sets.Cursor :=
     Received_Parcels.Find ("UPS ZZ-44-I12");
   C3 : constant Pkg_Maps.Cursor :=
     Math_Constants.Find
       (To_Unbounded_String
          ("Pi")); -- Finds by the key!

------------
Aggregates
------------

.. admonition:: Language Variant

    Ada 2022

* In Ada 2022, containers can be initialized with aggregates

   * Similar to arrays and records
   * Container aggregates use square brackets **[..]**

* Maps required named notation, all other containers use positional notation

.. code:: Ada

   package Int_Vectors is new Ada.Containers.Vectors
     (Positive, Integer);

   X : constant Int_Vectors.Vector := [1, 2, 3];

   type Key_T is (Height, Width, Depth);
   package Float_Maps is new Ada.Containers.Ordered_Maps
     (Key_T, Float);

   Y : constant Float_Maps.Map := [Height => 1.0,
                                   Width => 2.5,
                                   Depth => 5.51];

*Note that if you create your own container types, you will need to use the aspect* :ada:`Aggregate` *to enable this functionality.*

