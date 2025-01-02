===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Multitask-safe** accessors to get and set state
* **No** direct state manipulation
* **No** concurrent modifications
* :ada:`limited` types (No copies allowed)

.. container:: columns

 .. container:: column

  .. code:: Ada

   protected type
     Protected_Value is
      procedure Set (Some_Value : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Protected_Value;

 .. container:: column

  .. code:: Ada

   protected body Protected_Value is
      procedure Set (Some_Value : Integer) is
      begin
         Value := Some_Value;
      end Set;

      function Get return Integer is
      begin
         return Value;
      end Get;
   end Protected_Value;

.

--------------------------
Misc: Single Declaration
--------------------------

 * Instantiate an **anonymous** task (or protected) type
 * Declares an object of that type

    - Body declaration is then using the **object** name

 .. code:: Ada

   task Printer;

.. code:: Ada

   task body Printer is
   begin
      loop
        Put_Line ("loops");
      end loop;
   end Printer;

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

* In case of concurrency, other callers get **blocked**

    - Until call finishes

-------------------
Protected Entries
-------------------

* A :ada:`entry` is equivalent to a procedure but

   - It can have a **guard condition**

       + Must be a **Boolean variable**
       + Declared as :ada:`private` member of the type

   - Calling task **blocks** on the guard until it is lifted

       + At most one task blocked (in Ravenscar)
   
   - At most one entry per protected type (in Ravenscar)

.. code:: Ada

    protected Blocker is
        entry Wait when Ready;
        procedure Mark_Ready; --  sets Ready to True
    private
        Ready : Boolean := False;
    end protected;

