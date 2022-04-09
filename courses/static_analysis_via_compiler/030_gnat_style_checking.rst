*********************
GNAT Style Checking
*********************

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

------------------
"Style" Checking
------------------

+ Style rules we use within AdaCore

  + Not a general coding standards checker (see :toolname:`GNATcheck`)
  + Some are arbitrary
  + Main thing is to be consistent

+ Categories of checks

  + Layout/presentation
  + Sound Engineering

+ Note that you don't have to use any/all of these!

===================
Compiler Switches
===================

---------------------------------
GNAT Style Enforcement Switches
---------------------------------

+ Activated with option :command:`-gnatyxx`

  + Where **xx** is replaced with list of style check parameters

+ Deactivated after minus (-):

  + :command:`-gnatyc` activates, :command:`-gnaty-c` deactivates

+ :command:`-gnaty` activates most style warnings (also :command:`-gnatyY`)

  + Equivalent to :command:`-gnaty3abcefhiklmnprst`
  + (Descriptions on following pages)

+ :command:`-gnatyN` suppresses all style warnings
+ See *GNAT User's Guide* section 3.2.5 for all the options available

--------------------------------
Layout and Presentation Checks
--------------------------------

.. list-table::
   :header-rows: 1

  * - Style check

    - Behavior

  * - 1-9

    - check indentation

  * - a

    - check attribute casing

  * - b

    - check no blanks at end of lines

  * - c

    - check comment format (two spaces)

  * - C

    - check comment format (one space)

  * - d

    - check no DOS line terminators

  * - f

    - check no form feeds/vertical tabs in source

  * - h

    - check no horizontal tabs in source

  * - i

    - check if-then layout

  * - k

    - check casing rules for keywords

  * - l

    - check reference manual layout

  * - m

    - check line length <= 79 characters

  * - Mnn

    - check line length <= nn characters

  * - n

    - check casing of package Standard identifiers

  * - o

    - check subprogram bodies in alphabetical order

  * - p

    - check pragma casing

  * - r

    - check casing for identifier references

  * - S

    - check separate lines after THEN or ELSE

  * - t

    - check token separation rules

  * - u

    - check no unnecessary blank lines

---------------------------------
Layout and Presentation Example
---------------------------------

.. code:: Ada
   :number-lines: 79

  -- Procedure to find the defining name for the node
  procedure Find_Defining_Name (Node : Lal.Ada_Node'Class) is
     Parent : Lal.Ada_Node := node.Parent;
  begin
     --  Go up the tree until we find what we are looking for
     Search_Loop:
     While not Parent.Is_Null loop
        exit Search_Loop when Names.Map_Size = Natural'last;
        if Parent.Kind = Lalco.Ada_Defining_Name then
           if Valid_Length (Qualified_Name) then
             Names.Add_Name (Qualified_Name);
           end if;
        end if;
        Parent := Parent.Parent;
     end loop Search_Loop;
  end Find_Defining_Name;

.. list-table::

  * - **Message**

    - **Caused by**

  * - obfuscate.adb:79:07: (style) space required

    - *-gnatyc*

  * - obfuscate.adb:81:32: (style) bad casing of "Node" declared at line 80

    - *-gnatyr*

  * - obfuscate.adb:84:18: (style) space required

    - *-gnatyt*

  * - obfuscate.adb:85:07: (style) reserved words must be all lower case

    - *-gnatyk*

  * - obfuscate.adb:86:57: (style) bad capitalization, mixed case required

    - *-gnatya*

  * - obfuscate.adb:89:15: (style) bad indentation

    - *-gnaty3*

--------------------------
Sound Engineering Checks
--------------------------

.. list-table::
   :header-rows: 1

  * - Style check

    - Behavior

  * - A

    - check array attribute indexes

  * - B

    - check no use of AND/OR for boolean expressions

  * - e

    - check end/exit labels present

  * - I

    - check mode in

  * - Lnn

    - check max nest level < nn

  * - O

    - check overriding indicators

  * - s

    - check separate subprogram specs present

  * - x

    - check extra parentheses around conditionals

---------------------------
Sound Engineering Example
---------------------------

.. code:: Ada
   :number-lines: 4

   package Example is
      Count : Natural;
      type Tagged_T is tagged null record;
      procedure Primitive (R : in Tagged_T);
      type Child_T is new Tagged_T with record
         Field : Natural;
      end record;
      procedure Primitive (R : in Child_T);
   end Example;

   package body Example is
      procedure Primitive (R : in Tagged_T) is
      begin
         if (Count > 0) then Count := 0; end if;
      end Primitive;
      procedure Primitive (R : in Child_T) is
      begin
         Lup :
         while (Count > 0) and (Count < 100) loop
            Count := Count + R.Field;
            exit when Count = 50;
         end loop Lup;
      end Primitive;
   end Example;

.. list-table::

  * - **Message**

    - **Caused by**

  * - examples.adb:7:32: (style) "in" should be omitted

    - *-gnatyI*

  * - examples.adb:11:07: (style) missing "overriding" indicator in declaration of "Primitive"

    - *-gnatyO*

  * - examples.adb:17:13: (style) redundant parentheses

    - *-gnatyx*

  * - examples.adb:17:30: (style) no statements may follow "then" on same line

    - *-gnatyS*

  * - examples.adb:19:07: (style) missing "overriding" indicator in body of "Primitive"

    - *-gnatyO*

  * - examples.adb:22:28: (style) "and then" required

    - *-gnatyB*

  * - examples.adb:24:13: (style) "exit Lup" required

    - *-gnatye*

==========================
Controlling The Behavior
==========================

------------------------
Warnings Versus Errors
------------------------

+ If you must ensure issues are caught, failing to compile is the most rigorous enforcement
+ Compiler can be told to treat warnings as errors

  + Thus code rejected at compile-time

+ Use switch :command:`-gnatwe`

  + Warnings become errors
  + Style violations become errors too
  + Warning messages still appear but no code generation

----------------------------------------------
IDE Integration (Project Properties Editor)
----------------------------------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada.jpg

-----------------
Warnings Dialog
-----------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-warnings.jpg

---------------------
Style Checks Dialog
---------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-style.jpg

--------------------------------------
Dialog Pop-Ups Explain Style Options
--------------------------------------

.. image:: gnat_studio/menu-edit/project_properties/build-switches-ada-style-tooltip.jpg
