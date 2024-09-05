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

* Uses vertical bar "choice" syntax

.. code:: Ada

   declare
    M : Month_Number := Month (Clock);
   begin
     if M in 9 | 4 | 6 | 11 then
       Put_Line ("31 days in this month");
     elsif M = 2 then
       Put_Line ("It's February, who knows?");
     else
       Put_Line ("30 days in this month");
     end if;

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

   A. To use :ada:`or`, both sides of the comparison must be duplicated (e.g. :ada:`Today = Mon or Today = Wed`)
   B. Legal - should always return :ada:`True`
   C. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Sat` or :ada:`Sun`
   D. Legal - returns :ada:`True` if :ada:`Today` is :ada:`Tue` or :ada:`Thu`

