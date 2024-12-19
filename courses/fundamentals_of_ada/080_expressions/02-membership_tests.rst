==================
Membership Tests
==================

-------------------------
 "Membership" Operation
-------------------------

* Syntax

   .. code:: Ada

      simple_expression [not] in membership_choice_list
      membership_choice_list ::= membership_choice
                                 { | membership_choice}
      membership_choice ::= expression | range | subtype_mark

* Acts like a boolean function
* Usable anywhere a boolean value is allowed

.. code:: Ada

   X : Integer := ...
   B : Boolean := X in 0..5;
   C : Boolean := X not in 0..5; -- also "not (X in 0..5)"

------------------------------------
Testing Constraints Via Membership
------------------------------------

.. code:: Ada

   type Calendar_Days  is
       (Mon, Tues, Wed, Thur, Fri, Sat, Sun);
   subtype Weekdays is Calendar_Days range Mon .. Fri;
   Day : Calendar_Days := Today;
   ...
   if Day in Mon .. Fri then ...
   if Day in Weekdays then ... -- same as above

-----------------------------------
Testing Non-Contiguous Membership
-----------------------------------

* We use :ada:`in` to indicate membership in a range of values

   .. code:: Ada

      if Color in Red .. Green then
      if Index in List'Range then

* But what if the values are not contiguous?

   * We could use a Boolean conjunction

      .. code:: Ada

         if Index = 1 or Index = 3 or Index = 5 then

   * Or we could simplify it by specifying a collection (or set)

      .. code:: Ada

         if Index in 1 | 3 | 5 then

      * **|** is used to separate members
      * So :ada:`1 | 3 | 5` is the set for which we are verifying membership

..
  language_version 2012

------
Quiz
------

.. code:: Ada

   type Days_T is (Sun, Mon, Tue, Wed, Thu, Fri, Sat);
   subtype Weekdays_T is Days_T range Mon .. Fri;
   Today : Days_T;

Which condition(s) is (are) legal?

   A. ``if Today = Mon or Wed or Fri then``
   B. :answermono:`if Today in Days_T then`
   C. :answermono:`if Today not in Weekdays_T then`
   D. :answermono:`if Today in Tue | Thu then`

.. container:: animate

   Explanations

   A. :ada:`Wed` and :ada:`Fri` are not Boolean expressions - need to compare each of them to :ada:`Today`
   B. Legal - should always return :ada:`True`
   C. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Sat` or :ada:`Sun`
   D. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Tue` or :ada:`Thu`

