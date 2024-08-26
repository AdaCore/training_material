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

-----------------------------------
But You Can Treat Them As Objects
-----------------------------------

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

      when defining_identifier : exception_name ... =>

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

--------------
Exception ID
--------------

* For an exception identifier, the :dfn:`identity` of the exception is :ada:`<name>'Identity`

.. code:: Ada

   Mine : exception
   use Ada.Exceptions;
   ...
   exception
      when Occurrence : others =>
         if Exception_Identity (Occurrence) = Mine'Identity
         then
            ...

