=========
Summary
=========

--------------------------------------
 Benefits of Strongly Typed Numerics
--------------------------------------

* **Prevent** subtle bugs
* Cannot mix :ada:`Apples` and :ada:`Oranges`
* Force to clarify **representation** needs

    - eg. constant with or with fractional part

   .. code:: Ada

      type Yen is range 0 .. 1_000_000;
      type Ruble is range 0 .. 1_000_000;
      Mine : Yen := 1;
      Yours : Ruble := 1;
      Mine := Yours; -- illegal

------------------------------------
User-Defined Numeric Type Benefits
------------------------------------

* Close to **requirements**

   - Types with **explicit** requirements (range, precision, etc.)
   - Best case: Incorrect state **not possible**

* Either implemented/respected or rejected

   - No run-time (bad) suprise

* **Portability** enhanced

   - Reduced hardware dependencies

---------
Summary
---------

* User-defined types and strong typing is **good**

   - Programs written in application's terms
   - Computer in charge of checking constraints
   - Security, reliability requirements have a price
   - Performance **identical**, given **same requirements**

* User definitions from existing types *can* be good
* Right **trade-off** depends on **use-case**

   - More types |rightarrow| more precision |rightarrow| less bugs
   - Storing **both** feet and meters in :ada:`Float` has caused bugs
   - More types |rightarrow| more complexity |rightarrow| more bugs
   - A :ada:`Green_Round_Object_Altitude` type is probably **never needed**

* Default initialization is **possible**

   - Use **sparingly**
