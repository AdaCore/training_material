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
Solution: Move Type to Parent Package
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

.. note::

   Ada 2005 and beyond allows a special :ada:`with` to "partially"
   import a unit - :ada:`private with OS.UART;`

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

