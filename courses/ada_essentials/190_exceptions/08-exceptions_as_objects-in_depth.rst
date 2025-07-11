=======================
Exceptions As Objects
=======================

----------------------------
Exceptions Are Not Objects
----------------------------

* May not be manipulated

   - May not be components of composite types
   - May not be passed as parameters

* Some differences for scope and visibility

   - May be propagated out of scope

----------------------------------
Example Propagation Beyond Scope
----------------------------------

.. container:: columns

 .. container:: column

    .. code:: Ada

       package P is
         procedure Q;
       end P;
       package body P is
         Error : exception;
         procedure Q is
         begin
           ...
           raise Error;
         end Q;
       end P;

 .. container:: column

    .. code:: Ada

       with P;
       procedure Client is
       begin
         P.Q;
       exception
         -- not visible
         when P.Error =>
            ...
         -- captured here
         when others =>
            ...
       end Client;

------------------------------------------
Mechanism to Treat Exceptions As Objects
------------------------------------------

* For raising and handling, and more
* Standard Library

.. code:: Ada

   package Ada.Exceptions is
     type Exception_Id is private;
     procedure Raise_Exception (E : Exception_Id;
                                Message : String := "");
     ...
     type Exception_Occurrence is limited private;
     function Exception_Name (X : Exception_Occurrence)
         return String;
     function Exception_Message (X : Exception_Occurrence)
         return String;
     function Exception_Information (X : Exception_Occurrence)
         return String;
     procedure Reraise_Occurrence (X : Exception_Occurrence);
     procedure Save_Occurrence (
       Target : out Exception_Occurrence;
       Source : Exception_Occurrence);
     ...
   end Ada.Exceptions;

---------------------
Exception Occurrence
---------------------

* Syntax associates an object with active exception

   .. code:: Ada

      when <identifier> : exception_name ... =>

* A constant view representing active exception
* Used with operations defined for the type

   .. code:: Ada

      exception
        when Caught_Exception : others =>
          Put (Exception_Name (Caught_Exception));

----------------------------------------
`Exception_Occurrence` Query Functions
----------------------------------------

* `Exception_Name`

   - Returns full expanded name of the exception in string form

      + Simple short name if space-constrained

   - Predefined exceptions appear as just simple short name

* `Exception_Message`

   - Returns string value specified when raised, if any

* `Exception_Information`

   - Returns implementation-defined string content
   - Should include both exception name and message content
   - Presumably includes debugging information

      + Location where exception occurred
      + Language-defined check that failed (if such)

-----------------------------------
User Subprogram Parameter Example
-----------------------------------

.. code:: Ada

   with Ada.Exceptions; use Ada.Exceptions;
   procedure Display_Exception
       (Error : in Exception_Occurrence)
   is
     Msg : constant String := Exception_Message (Error);
     Info : constant String := Exception_Information (Error);
   begin
     New_Line;
     if Info /= "" then
       Put ("Exception information => ");
       Put_Line (Info);
     elsif Msg /= "" then
       Put ("Exception message => ");
       Put_Line (Msg);
     else
       Put ("Exception name => ");
       Put_Line (Exception_Name (Error));
     end if;
   end Display_Exception;

--------------------
Exception Identity
--------------------

* Attribute 'Identity converts exceptions to the type

   .. code:: Ada

      package Ada.Exceptions is
        ...
        type Exception_Id is private;
        ...
        procedure Raise_Exception (E : in Exception_Id;
                                   Message : in String := "");
        ...
      end Ada.Exceptions;

* Primary use is raising exceptions procedurally

   .. code:: Ada

      Foo : exception;
      ...
      Ada.Exceptions.Raise_Exception (Foo'Identity,
                                      Message => "FUBAR!");


------------------------------------
Re-Raising Exceptions Procedurally
------------------------------------

* Typical :ada:`raise` mechanism

   .. code:: Ada

      begin
        ...
      exception
        when others =>
          Cleanup;
          raise;
      end;

* Procedural :ada:`raise` mechanism

   .. code:: Ada

      begin
        ...
      exception
        when X : others =>
          Cleanup;
          Ada.Exceptions.Reraise_Occurrence (X);
      end;

----------------------------------------
Copying `Exception_Occurrence` Objects
----------------------------------------

* Via procedure `Save_Occurrence`

   - No assignment operation since is a :ada:`limited` type

.. code:: Ada

   Error : Exception_Occurrence;

   begin
     ...
   exception
     when X : others =>
       Cleanup;
       Ada.Exceptions.Save_Occurrence (X, Target => Error);
   end;

---------------------------------------
Re-Raising Outside Dynamic Call Chain
---------------------------------------

.. code:: Ada

   procedure Demo is
     package Exceptions is new
         Limited_Ended_Lists (Exception_Occurrence,
                              Save_Occurrence);
     Errors : Exceptions.List;
     Iteration : Exceptions.Iterator;
     procedure Normal_Processing
         (Troubles : in out Exceptions.List) is ...
   begin
     Normal_Processing (Errors);
     Iteration.Initialize (Errors);
     while Iteration.More loop
       declare
         Next_Error : Exception_Occurrence;
       begin
         Iteration.Read (Next_Error);
         Put_Line (Exception_Information (Next_Error));
         if Exception_Identity (Next_Error) =
            Trouble.Fatal_Error'Identity
         then
           Reraise_Occurrence (Next_Error);
         end if;
       end;
     end loop;
     Put_Line ("Done");
   end Demo;

