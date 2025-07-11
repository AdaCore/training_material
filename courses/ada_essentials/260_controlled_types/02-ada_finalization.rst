==================
Ada.Finalization
==================

---------------
Package Spec
---------------

.. code:: Ada

   package Ada.Finalization is

     type Controlled is abstract tagged private;
     procedure Initialize (Object : in out Controlled)
        is null;
     procedure Adjust    (Object : in out Controlled)
        is null;
     procedure Finalize  (Object : in out Controlled)
        is null;

     type Limited_Controlled is abstract tagged limited private;
     procedure Initialize (Object : in out Limited_Controlled)
        is null;
     procedure Finalize  (Object : in out Limited_Controlled)
        is null;

   private
      -- implementation defined
   end Ada.Finalization;

-------
Uses
-------

* Prevent "resource leak"

   - Logic centralized in service rather than distributed across clients

* Examples: heap reclamation, "mutex" unlocking
* User-defined assignment

----------------
Initialization
----------------

* Subprogram `Initialize` invoked after object created

   - Either by object declaration or allocator
   - Only if no explicit initialization expression

* Often default initialization expressions on record components are sufficient

   - No need for an explicit call to `Initialize`

* Similar to C++ constructor

----------------
Finalization
----------------

* Subprogram `Finalize` invoked just before object is destroyed

   - Leaving the scope of a declared object
   - Unchecked deallocation of an allocated object

* Similar to C++ destructor

------------
Assignment
------------

* Subprogram `Adjust` invoked as part of an assignment operation
* Assignment statement `Target := Source;` is basically:

   - `Finalize (Target)`
   - Copy Source to Target
   - `Adjust (Target)`
   - *Actual rules are more complicated, e.g. to allow cases where Target and Source are the same object*

* Typical situations where objects are access values

   - `Finalize` does unchecked deallocation or decrements a reference count
   - The copy step copies the access value
   - `Adjust` either clones a "deep copy" of the referenced object or increments a reference count

