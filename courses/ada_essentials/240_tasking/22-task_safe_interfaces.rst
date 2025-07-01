====================
Task Safe Interfaces
====================

-----------------
Problem Statement
-----------------

* Designing task-safe code requires using dedicated constructs
* How to reuse the components?
* How to refactor task-unsafe code into task-safe version?

----------------
Access Protected
----------------

* Access to :ada:`protected` objects' subprograms
* :ada:`type P is access protected procedure (args...)`
* :ada:`type F is access protected function (args...) return ...`

.. code:: Ada

   type Work_Id is tagged limited private;

   type Work_Handler is
      access protected procedure (T : Work_Id);

----------------------
Synchronized Interface
----------------------

* :ada:`synchronized interface` can be inherited by :ada:`task`/:ada:`protected` types

.. code:: Ada

   type Counter_I is synchronized interface;
   procedure Increment (Counter : in out Counter_I) is abstract;

   task type Counter_Task_T is new Counter_I with
      -- Always implemented as an entry for tasks
      entry Increment;
   end task;

   protected type Counter_Prot_T is new Counter_I with
      procedure Increment;
   end Counter_Prot_T;

* Also present:

  - :ada:`task interface` meant for tasks only
  - :ada:`protected interface` meant for protected types only

.. warning::

    Only available in **full-tasking** runtimes

---------------------------------
Standard Library Queues Interface
---------------------------------

* In :ada:`Ada.Containers`
* :ada:`Synchronized_Queue_Interfaces` interface

.. tip::

    Provides a portable interface

.. code:: Ada

    generic
       type Element_Type is private;
    package Ada.Containers.Synchronized_Queue_Interfaces is
       type Queue is synchronized interface;

---------------------------------------
Standard Library Queues Implementations
---------------------------------------

* Four implementations

.. tip::

    Recommended over rolling-out one's own queue implementation

* Synchronized implementations

    - :ada:`Unbounded_Synchronized_Queues`
    - :ada:`Bounded_Synchronized_Queues`
    - As :ada:`protected` types
    - With priority ceiling

* Priority implementations

    - :ada:`Unbounded_Priority_Queues`
    - :ada:`Bounded_Priority_Queues`
    - As :ada:`protected` types
    - Elements provide :ada:`Get_Priority`

        + Used for sorting elements

----------------------------
Example: Scheduler Interface
----------------------------

.. code:: Ada

   type Scheduler_I;
   type Maybe_Work_Item_I is access protected procedure;
   type Work_Item_I is not null access protected procedure;

   type Scheduler_I is synchronized interface;
   procedure Queue (S : in out Scheduler_I; W : Work_Item_I) is abstract;
   procedure Execute_Next (S : in out Scheduler_I) is abstract;

   type Work_Items_Array is array (Positive range <>)
     of Maybe_Work_Item_I;

   protected type Scheduler_T (Size : Positive) is new Scheduler_I with
      procedure Queue (W : Work_Item_I);
      entry Execute_Next;
   private
      Number_Of_Items : Natural := 0;
      Items : Work_Items_Array (1 .. Size);
   end Scheduler_T;

-------------------------
Example: Scheduler (Body)
-------------------------

.. code:: Ada

   protected body Scheduler_T is
      procedure Queue (W : Work_Item_I) is
      begin
         Number_Of_Items := Number_Of_Items + 1;
         Items (Number_Of_Items) := Maybe_Work_Item_I (W);
      end Queue;

      entry Execute_Next
         when Number_Of_Items > 0
      is
         W : Work_Item_I := Work_Item_I (Items (Number_Of_Items));
      begin
         Number_Of_Items := Number_Of_Items - 1;
         W.all;
      end Execute_Next;
   end Scheduler_T;
