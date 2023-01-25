********************************
Interfacing to Other Languages
********************************

..
    Coding language

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

..
    Math symbols

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

==============
Introduction
==============

--------------
Introduction
--------------

* Lots of code out there already

   - Maybe even a lot of reusable code in your own repositories
   - C/C++ or non-SPARK Ada

* Need a way to interface SPARK code with existing libraries

-------------------------------------
Interfacing With a Foreign Language
-------------------------------------

* Example languages are:

   - Non-SPARK Ada
   - SPARK 83/95/2005
   - C and C++
   - Assembler

* A SPARK wrapper is usually needed to interface to a foreign programming language component

   - A specification is written in SPARK with contracts modelling the behavior of the foreign component
   - Sometimes the SPARK specification and component can be linked
   - More often a body written in Ada (with `SPARK_Mode => Off`) is required to link to the component

=================
SPARK Wrappers
=================

--------------------------------
Interfacing with non-SPARK Ada
--------------------------------

* A SPARK wrapper is not always necessary

   - Ada may be compatible with SPARK - just add `SPARK_Mode => On`
   - The declarations required from the component are SPARK compatible even if not the whole Ada component specification
   - It may be possible (permissible?) to add SPARK contracts to the Ada component to prevent exceptions occurring in the component

* A SPARK wrapper will be required

   - If the required declarations from the component are not in SPARK

      + For example a component from the Ada container library

   - The Ada component has more than the simplest hidden state
   - The Ada component needs SPARK contracts but it is not permitted to alter the code of the component

---------------------------------
Example: Ada.Containers.Vectors
---------------------------------

* Type Ada.Containers.Vectors is not SPARK compatible

   .. code:: Ada

      package Ada.Containers.Vectors is
         -- ... Declarations ...
         type Vector is tagged private
         ...
      private
         type Vector is new Controlled with record ...

* We will show a simple interface to Ada.Containers.Vectors

   - Facilitates extending vector using `&`
   - Reading and updating an element of the vector
   - The same principles can be used for a more extensive interface

---------------------------------
Example: Ada.Containers.Vectors
---------------------------------

* Initial declarations for `My_Vectors`

.. code:: Ada

   -- Legal even though Ada.Containers.Vectors is not in SPARK
   with Ada.Containers.Vectors;
   package My_Vectors with SPARK_Mode is
      subtype Index_Type is Positive;
      subtype Extended_Index is Index_Type'Base
            range Index_Type'First - 1 ..
                  Index_Type'Min (Index_Type'Base'Last - 1,
                                  Index_Type'Last) + 1;
      subtype Element_Type is Integer;
      -- Declaration of our Vector type
      type Vector is private;
      Empty_Vector : constant Vector;

---------------------------------
Example: Ada.Containers.Vectors
---------------------------------

* Operations on `My_Vectors.Vector`

.. code:: Ada

   function "&" (Left : Vector; Right : Element_Type)
         return Vector
      with Global => null;
   function First_Index (V : Vector) return Index_Type
      with Global => null;
   function Last_Index (V : Vector) return Extended_Index
      with Global => null;
   function Element (V : Vector; I : Index_Type)
         return Element_Type
      with Global => null,
           Pre => I in First_Index (V) .. Last_Index (V);
   procedure Replace_Element (V : in out Vector;
                              I : Index_Type;
                              It : Element_Type)
      with Global => null,
           -- Preconditions added to prevent potential
           -- exceptions in calls to Ada.Containers.Vectors
           Pre => I in First_Index (V) .. Last_Index (V);

---------------------------------
Example: Ada.Containers.Vectors
---------------------------------

* The private part of `My_Vectors`

.. code:: Ada

   private
      -- The private part is not in SPARK
      pragma SPARK_Mode (Off);
      -- Instantiation of Ada.Containers.Vectors
      package Vectors is new Ada.Containers.Vectors
         (Index_Type => Index_Type,
          Element_Type => Element_Type,
          "=" => "=");
      -- Declaration of My_Vectors.Vector
      type Vector is record
         Vec : Vectors.Vector;
      end record;
      Empty_Vector : constant Vector :=
                     (Vec => Vectors.Empty_Vector);
   end My_Vectors;

---------------------------------
Example: Ada.Containers.Vectors
---------------------------------

* The body of `My_Vectors`

.. code:: Ada

   -- Body not in SPARK like private part
   package body My_Vectors with SPARK_Mode => Off is
      ...
      function Element (V : Vector; I : Index_Type)
            return Element_Type is
         (Vectors.Element (V.Vec, I));
      procedure Replace_Element (V : in out Vector;
         I : Index_Type;
         It : Element_Type) is
      begin
         -- Implementation in terms of Ada.Containers.Vectors
         Vectors.Replace_Element (V.Vec, I, It);
      end Replace_Element;
      function First_Index (V : Vector) return Index_Type
         is (Vectors.First_Index (V.Vec));
      function Last_Index (V : Vector) return Extended_Index
         is (Vectors.Last_Index (V.Vec));
      ...
   end My_Vectors;

-----------------------------------------
Interfacing with older SPARK components
-----------------------------------------

* Possible approaches:

   - Convert component to SPARK 2014 using conversion tool to translate most of constructs to SPARK 2014
   - Add SPARK 2014 contracts to the component

      + The SPARK 83/95/2005 Examiner will ignore aspects and pragmas used for specifying contracts in SPARK 2014
      + The annotations used to specify contracts in SPARK 83/95/2005 are formal comments and are ignored in SPARK 2014

   - Write a SPARK 2014 wrapper for the component

===================
Foreign Languages
===================

--------------------------------------
Interfacing with a non-Ada component
--------------------------------------

* Interface to C or C++ for instance

   - When faced with having a call to a C API, apply the following steps:

      + Read the API documentation!
      + Does the C API have persistent (possibly hidden) state?
      + Consider how the subprogram will be specified in SPARK
      + If possible bind the SPARK subprogram specification to the C API using Annex B of the Ada RM
      + If the SPARK subprogram is incompatible with the C API create a SPARK wrapper in which an Ada binding to the C API is used

------------------
Interfacing to C
------------------

* Example - C implementation of the SHA-1 Secure Hash Algorithm
* We are given:

   .. code:: C++

      typedef unsigned char BYTE;
      int sha_1 (BYTE *data, BYTE *result);

* What does this tell us? Not much compared with an Ada or SPARK specification!

-------------------------------------
Interfacing to C - Examine C Source
-------------------------------------

* Step 1 - read the documentation

   .. code:: C++

      typedef unsigned char BYTE;
      /* data is a pointer to 1024 bytes of data to be hashed.
      result is a pointer to 20 bytes where the result is
      placed.  Function returns an integer in the range 0 to 3.
      0 indicates no error has occurred. */
      int sha_1 (BYTE *data, BYTE *result);

   - Ok - this tells us enough to make progress

* Step 2 - Is there any persistent state?

   - No, none that is apparent at the interface

------------------------------------
Interfacing to C - Study Interface
------------------------------------

* Step 3a - Investigate how to construct a SPARK subprogram specification

   - Big decision - is this a procedure or a function in SPARK?
   - A C function that returns a value and has a side-effect via a parameter cannot be a function in SPARK - so it has to be a procedure
   - Has the C subprogram any global variables? If so add them to a SPARK Global contract

      + `sha_1` appears not to have any global variables

   - Determine the mode of each parameter and global variable

-------------------------------------
Interfacing to C - Build Foundation
-------------------------------------

* Step 3b - Create artifacts needed to construct a SPARK subprogram specification

   - Some type declarations are required for the SPARK specification

      .. code:: Ada

         subtype Byte is Interfaces.C.unsigned_char;
         subtype Int is Interfaces.C.int;
         subtype Buffer_Index is Int range 1 .. 1024;
         subtype Result_Index is Int range 1 .. 20;
         -- Assume here obvious C-style data layout. Could force with rep
         -- clauses if necessary.
         type Buffer_Type is array (Buffer_Index) of Byte;
         type Result_Type is array (Result_Index) of Byte;
         subtype Error_Code is Int range 0 .. 3;

   - If proof of properties is required a ghost function would be useful

      .. code:: Ada

         function Is_SHA_1 (Data   : Buffer_Type;
                            Result : Result_Type)
                            return Boolean
            with Ghost;

----------------------------------
Interfacing to C - Build Wrapper
----------------------------------

* Step 3 - Construct a SPARK subprogram specification

   - Now for the SPARK specification of the `SHA_1` procedure

      .. code:: Ada

         procedure SHA_1 (Data   : in Buffer_Type;
                          Result : out Result_Type;
                          Error  : out Error_Code)
            with Global => null,
                 Post => (if Error = 0 then Is_SHA_1(Data,Result));

   - This gives us a clean SPARK-compatible interface to the C API

      + **NOTE:** If error is not equal to zero the postcondition says nothing about the result of calling `SHA_1`
      + This is consistent with the supplied documentation

--------------------------------------
Interfacing to C - Implement Binding
--------------------------------------

* Step 4 - Bind SPARK procedure to C API

   - In this example directly binding the SPARK procedure is not feasible as the C API is a function with side-effects
   - The `SHA_1` procedure will have to be a SPARK wrapper calling an Ada function equivalent to the C function
   - Consult the Ada RM Annex B.1 through B.3
   - Understand what **Convention C** means
   - Check that your compiler follows the Ada RM implementation advice and read your compiler's Annex M

---------------------------
Interfacing to C - Result
---------------------------

* Step 4 - Bind SPARK procedure to C API (The SPARK wrapper body)

.. code:: Ada

   -- Internal_SHA_1 has a side-effect - an out parameter -
   -- which corresponds to the C function sha_1
   procedure SHA_1 (Data   : in Buffer_Type;
                    Result : out Result_Type;
                    Error  : out Error_Code)
      -- The body of procedure SHA_1 is not in SPARK
      with SPARK_Mode => Off is
      function Internal_SHA_1 (Data   : in Buffer_Type;
                               Result : out Result_Type)
            -- Result type of Int rather than Error_Code to
            -- avoid possibility of invalid values
            return Int
         -- Data and Result are both passed by reference
         -- by Convention C
         with Import,
              Convention => C,
              External_Name => "sha_1";
      Status_Code : Int;
   begin
      Status_Code := Internal_SHA_1 (Data, Result);
      -- Ensure that the value returned by Internal_SHA_1
      -- is in Error_Code, i.e., a valid value
      Error := (if Status_Code in Error_Code
                then Status_Code
                else 3);
   end SHA_1;

==========
Summary
==========

---------
Summary
---------

* Interfacing to non-Ada is relatively simple

   + Some Ada code is already SPARK-compliant
   + Non-compliant code requires wrappers

      - Interface behavior might change, as in a function with an `out` parameter

* Possible to interface with other languages (typically C/C++)

   + Ada provides some built-in support to make interfacing simpler
   + Crossing languages can be made safer

      - But it still increases complexity of design / implementation
