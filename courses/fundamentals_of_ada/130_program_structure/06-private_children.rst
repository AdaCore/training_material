===================
Private Children
===================

------------------
Private Children
------------------

* Intended as implementation artifacts
* Only available within subsystem

   - Rules prevent :ada:`with` clauses by clients
   - Thus cannot export anything outside subsystem
   - Thus have no parent visibility restrictions

      + Public part of child also has visibility to ancestors' private parts

.. code:: Ada

  private package Maze.Debug is
     procedure Dump_State;
     ...
  end Maze.Debug;

-------------------------------------------
Rules Preventing Private Child Visibility
-------------------------------------------

* Only available within immediate family

   - Rest of subsystem cannot import them

* Public unit declarations have import restrictions

   - To prevent re-exporting private information

* Public unit bodies have no import restrictions

   - Since can't re-export any imported info

* Private units can import anything

   - Declarations and bodies can import public and private units
   - Cannot be imported outside subsystem so no restrictions

--------------
Import Rules
--------------

* Only parent of private unit and its descendants can import a private child
* Public unit declarations import restrictions

   - Not allowed to have :ada:`with` clauses for private units

      + Exception explained in a moment

   - Precludes re-exporting private information

* Private units can import anything

   - Declarations and bodies can import private children

--------------------------------------
Some Public Children Are Trustworthy
--------------------------------------

* Would only use a private sibling's exports privately
* But rules disallow :ada:`with` clause

.. code:: Ada

   private package OS.UART is
    type Device is limited private;
    procedure Open (This : out Device; ...);
    ...
   end OS.UART;

   -- illegal - private child
   with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       -- but I only need it here!
       COM : OS.UART.Device;
     ...
     end record;
   end OS.Serial;

-----------------------------------------
Solution 1: Move Type to Parent Package
-----------------------------------------

.. code:: Ada

   package OS is
     ...
   private
     -- no longer an ADT!
     type Device is limited private;
   ...
   end OS;
   private package OS.UART is
     procedure Open (This : out Device;
      ...);
     ...
   end OS.UART;

.. code:: Ada

   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : Device; -- now visible
       ...
     end record;
   end OS.Serial;

-------------------------------------------
Solution 2: Partially Import Private Unit
-------------------------------------------

* Via :ada:`private with` clause
* Syntax

   .. code:: Ada

      private with package_name {, package_name} ;

* Public declarations can then access private siblings

   - But only in their private part
   - Still prevents exporting contents of private unit

* The specified package need not be a private unit

   - But why bother otherwise

..
  language_version 2005

------------------------
`private with` Example
------------------------

.. code:: Ada

   private package OS.UART is
     type Device is limited private;
     procedure Open (This : out Device;
        ...);
     ...
   end OS.UART;

.. code:: Ada

   private with OS.UART;
   package OS.Serial is
     type COM_Port is limited private;
     ...
   private
     type COM_Port is limited record
       COM : OS.UART.Device;
       ...
     end record;
   end OS.Serial;

..
  language_version 2005

-------------------------------------
Combining Private and Limited Withs
-------------------------------------

* Cyclic :ada:`limited with` clauses allowed
* A public unit can :ada:`with` a private unit
* With-ed unit only visible in the private part

.. code:: Ada

   limited with Parent.Public_Child;
   private package Parent.Private_Child is
     type T is ...
   end Parent.Private_Child;

   limited private with Parent.Private_Child;
   package Parent.Public_Child is
     ...
   private
     X : access Parent.Private_Child.T;
   end Parent.Public_Child;

..
  language_version 2005

-------------------
Child Subprograms
-------------------

* Child units can be subprograms

   - Recall syntax
   - Both public and private child subprograms

* Separate declaration required if private

   - Syntax doesn't allow :ada:`private` on subprogram bodies

* Only library packages can be parents

   - Only they have necessary scoping

.. code:: Ada

   private procedure Parent.Child;

