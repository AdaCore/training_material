===================
Memory Corruption
===================

---------------------------
Dealing with Access Types
---------------------------

* Access types introduce many issues

* Access types point to a location in memory

  * Modifying the pointer can point to bad locations
  * Clearing the pointer can lead to excessive memory issues
  * And lots more

* These issues are not language-specific!

------------------------
Uninitialized Pointers
------------------------

.. code:: Ada

  declare
     type An_Access is access Integer;
     Object : An_Access;
  begin
     Object.all := 5; -- constraint error

* In Ada, this is a problem because access type objects are initialized to null

* In other languages, there's no guarantee that the pointer is null, so you
  might write to a random memory location

------------------------------
Freeing Already-Freed Memory
------------------------------

.. code:: Ada

   declare
      type An_Access is access Integer;
      procedure Free is new
         Ada.Unchecked_Deallocation (Integer, An_Access);
      Object_1 : An_Access := new Integer;
      Object_2 : An_Access := Object_1;
   begin
      Free (Object_1);
      delay 1.0;
      Free (Object_2);

* May raise :ada:`Storage_Error` if memory is still protected (unallocated)

* May deallocate a different object if memory has been reallocated

  * Putting that object in an inconsistent state

----------------------------------
Referencing Already-Freed Memory
----------------------------------

.. code:: Ada

   declare
      type An_Access is access Integer;
      procedure Free is new
         Ada.Unchecked_Deallocation (Integer, An_Access);
      Object_1 : An_Access := new Integer;
      Object_2 : An_Access := Object_1;
   begin
      Free (Object_1);
      Object_2.all := 5;

* May raise :ada:`Storage_Error` if memory is still protected (unallocated)
* May modify a different object if memory has been reallocated

  * Putting that object in an inconsistent state

-------------
Memory Leak
-------------

.. code:: Ada

   declare
      type An_Access is access Integer;
      procedure Free is new
         Ada.Unchecked_Deallocation (Integer, An_Access);
      Object : An_Access := new Integer;
   begin
      Object := null;

* Silent problem

  + Might raise :ada:`Storage_Error` if too many leaks
  + Might slow down the program if too many page faults

-----------------------------
How to Fix Memory Problems?
-----------------------------

* There is no language-defined solution
* Use the debugger!
* Use additional tools

   - :command:`gnatmem`  monitor memory leaks
   - :command:`valgrind`  monitor all the dynamic memory
   - `GNAT.Debug_Pools` gives a pool for an access type, raising explicit exception in case of invalid access
   - Others...
