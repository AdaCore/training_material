===================
Memory Corruption
===================

------------------------------
Common Memory Problems (1/3)
------------------------------

* Uninitialized pointers

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         V : An_Access;
      begin
         V.all := 5; -- constraint error

* Double deallocation

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V1 : An_Access := new Integer;
         V2 : An_Access := V1;
      begin
         Free (V1);
         ...
         Free (V2);

   - May raise :ada:`Storage_Error` if memory is still protected (unallocated)
   - May deallocate a different object if memory has been reallocated

      + Putting that object in an inconsistent state

------------------------------
Common Memory Problems (2/3)
------------------------------

* Accessing deallocated memory

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V1 : An_Access := new Integer;
         V2 : An_Access := V1;
      begin
         Free (V1);
         ...
         V2.all := 5;

   - May raise :ada:`Storage_Error` if memory is still protected (unallocated)
   - May modify a different object if memory has been reallocated (putting that object in an inconsistent state)

------------------------------
Common Memory Problems (3/3)
------------------------------

* Memory leaks

   .. code:: Ada

      declare
         type An_Access is access all Integer;
         procedure Free is new
            Ada.Unchecked_Deallocation (Integer, An_Access);
         V : An_Access := new Integer;
      begin
         V := null;

   - Silent problem

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

