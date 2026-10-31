===================
Enumeration Types
===================

-------------------
Enumeration Types
-------------------

**Syntax**

.. code:: bnf

  enumeration_type_definition ::= 
    type identifier is (enumeration_literal_specification
                          {, enumeration_literal_specification});
  enumeration_literal_specification ::= 
     defining_identifier | defining_character_literal

* Enumeration of **logical** values

    - Integer value is an implementation detail

* Literals

   - Distinct, ordered
   - Can be in **multiple** enumerations

**Examples**

.. code:: Ada

   type Colors is (Red, Orange, Yellow, Green, Blue, Violet);
   type Stop_Light is (Red, Yellow, Green);
   ...
   -- Red both a member of Colors and Stop_Light
   Shade : Colors := Red;
   Light : Stop_Light := Red;

-----------------------------
Enumeration Type Operations
-----------------------------

* Assignment, relationals
* **Not** numeric quantities

.. code:: Ada

   type Directions is (North, South, East, West);
   type Days is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);
   Heading : Directions;
   Today, Tomorrow : Days;
   ...
   Today := Mon;
   Today := North; -- compile error
   Heading := South;
   Heading := East + 1; -- compile error
   if Today < Tomorrow then ...
