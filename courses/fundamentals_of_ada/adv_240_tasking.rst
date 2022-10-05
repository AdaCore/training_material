******************
Advanced Tasking
******************

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

---------------
A Simple Task
---------------

* Parallel code execution via **task**
* :ada:`limited` types (No copies allowed)

   .. code:: Ada

      procedure Main is
         task type T;
         task body T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end T;
      begin
         loop
            delay 1.0;
            Put_Line ("Main");
         end loop;
      end;

* A task is started when its declaration scope is **elaborated**
* Its enclosing scope exits when **all tasks** have finished

--------------------------
Two Synchronization Models
--------------------------

* Active

   - Rendezvous
   - **Client / Server** model
   - Server **entries**
   - Client **entry calls**

* Passive

   - **Protected objects** model
   - Concurrency-safe **semantics**

=======
Tasks
=======

------------------------
Rendezvous Definitions
------------------------

* **Server** declares several :ada:`entry`
* Client calls entries like subprograms
* Server :ada:`accept` the client calls
* At each standalone :ada:`accept`, server task **blocks**

    - **Until** a client calls the related :ada:`entry`

   .. code:: Ada

      task type Msg_Box_T is
         entry Start;
         entry Receive_Message (S : String);
      end Msg_Box_T;

      task body Msg_Box_T is
      begin
         loop
            accept Start;
            Put_Line ("start");

            accept Receive_Message (S : String) do
               Put_Line (S);
            end Receive_Message;
         end loop;
      end Msg_Box_T;

------------------------
Rendezvous Entry Calls
------------------------

* Upon calling an :ada:`entry`, client **blocks**

     - **Until** server reaches :ada:`end` of its :ada:`accept` block

   .. code:: Ada

      Put_Line ("calling start");
      T.Start;
      Put_Line ("calling receive 1");
      T.Receive_Message ("1");
      Put_Line ("calling receive 2");
      T.Receive_Message ("2");

* May be executed as follows:

   .. code:: Ada

      calling start
      start             -- May switch place with line below
      calling receive 1 -- May switch place with line above
      Receive 1
      calling receive 2
      -- Blocked until another task calls Start

------------------------
Accepting a Rendezvous
------------------------

* :ada:`accept` statement

   - Wait on single entry
   - If entry call waiting: Server handles it
   - Else: Server **waits** for an entry call

* :ada:`select` statement

   - **Several** entries accepted at the **same time**
   - Can **time-out** on the wait
   - Can be **not blocking** if no entry call waiting
   - Can **terminate** if no clients can **possibly** make entry call
   - Can **conditionally** accept a rendezvous based on a **guard expression**

------------------------
Rendezvous Calls
------------------------

* When calling an **entry**, the caller waits until the task is ready to be called

   .. code:: Ada

      Put_Line ("calling start");
      T.Start;
      Put_Line ("calling receive 1");
      T.Receive_Message ("1");
      Put_Line ("calling receive 2");
      --  Locks until somebody calls Start
      T.Receive_Message ("2");

* Results in an output like:

   .. code:: Ada

      calling start
      start
      calling receive 1
      Receive 1
      calling receive 2

.. container:: speakernote

   The loop requires Accept and ReceiveMessage to be called one followed by other

------------------------
Accepting a Rendezvous
------------------------

* Simple :ada:`accept` statement

   - Used by a server task to indicate a willingness to provide the service at a given point

* Selective :ada:`accept` statement (later in these slides)

   - Wait for more than one rendezvous at any time
   - Time-out if no rendezvous within a period of time
   - Withdraw its offer if no rendezvous is immediately available
   - Terminate if no clients can possibly call its entries
   - Conditionally accept a rendezvous based on a guard expression

---------------------------
Example: Task - Declaration
---------------------------

.. include:: examples/task_very_simple/src/tasks.ads
    :code: Ada

--------------------
Example: Task - Body
--------------------

.. include:: examples/task_very_simple/src/tasks.adb
    :code: Ada

---------------
Example: Main
---------------

.. include:: examples/task_very_simple/src/main.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    task type T is
        entry Go;
    end T;

    task body T is
    begin
        accept Go do
            loop
                null;
            end loop;
        end Go;
    end T;

    My_Task : T;

What happens when :ada:`My_Task.Go` is called?

A. Compilation error
B. Runtime error
C. :answer:`The calling task hangs`
D. :answer:`My_Task hangs`

------
Quiz
------

.. code:: Ada

    task type T is
        entry Go;
    end T;

    task body T is
    begin
        accept Go;
        loop
            null;
        end loop;
    end T;

    My_Task : T;

What happens when :ada:`My_Task.Go` is called?

A. Compilation error
B. Runtime error
C. The calling task hangs
D. :answer:`My_Task hangs`

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      with Ada.Text_IO; use Ada.Text_IO;
      procedure Main is
         task type T is
            entry Hello;
            entry Goodbye;
         end T;
         task body T is
         begin
            loop
               accept Hello do
                  Put_Line ("Hello");
               end Hello;
               accept Goodbye do
                  Put_Line ("Goodbye");
               end Goodbye;
            end loop;
            Put_Line ("Finished");
         end T;
      begin
         T.Hello;
         T.Goodbye;
         Put_Line ("Done");
      end Main;

 .. container:: column

   What is the output of this program?

      A. Hello, Goodbye, Finished, Done
      B. Hello, Goodbye, Finished
      C. :answer:`Hello, Goodbye, Done`
      D. Hello, Goodbye

   .. container:: animate

      |

      - Entries :ada:`Hello` and :ada:`Goodbye` are reached (so "Hello" and
      "Goodbye" are printed).

      - After :ada:`Goodbye`, task returns to :ada:`Main`
      (so "Done" is printed) but the loop in the task never finishes (so
      "Finished" is never printed).

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

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - **Multiple-Readers**
   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **Single-Writer**
   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

* In case of concurrency, other callers get **blocked**

    - Until call finishes

* Support for read-only locks **depends on OS**

    - Windows has **no** support for those
    - In that case, :ada:`function` are **blocking** as well

------------------------
Protected: Limitations
------------------------

* **No** potentially blocking action

   - :ada:`select`, :ada:`accept`, :ada:`entry` call, :ada:`delay`, :ada:`abort`
   - :ada:`task` creation or activation
   - Some standard lib operations, eg. IO

      + Depends on implementation

* May raise :ada:`Program_Error` or deadlocks
* **Will** cause performance and portability issues
* :ada:`pragma Detect_Blocking` forces a proactive runtime detection
* Solve by deferring blocking operations

   - Using eg. a FIFO

-------------------------------------
Protected: Lock-Free Implementation
-------------------------------------

* GNAT-Specific
* Generates code without any locks
* Best performance
* No deadlock possible
* Very constrained

   - No reference to entities **outside** the scope
   - No direct or indirect :ada:`entry`, :ada:`goto`, :ada:`loop`, :ada:`procedure` call
   - No :ada:`access` dereference
   - No composite parameters
   - See GNAT RM 2.100

.. include:: examples/protected_objects_lock_free/extracts/protected_objects.lock_free_declare.ads
    :code: Ada

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: examples/protected_objects/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: examples/protected_objects/src/protected_objects.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    protected O is
       function Get return Integer;
       procedure Set (V : Integer);
    private
       Val, Access_Count : Integer := 0;
    end O;

    protected body O is
       function Get return Integer is
       begin
          Access_count := Access_Count + 1;
          return Val;
       end Get;

       procedure Set (V : Integer) is
       begin
          Access_count := Access_Count + 1;
          Val := V;
       end Set;
    end O;

What is the result of compiling and running this code?

A. No error
B. :answer:`Compilation error`
C. Runtime error

.. container:: animate

    Cannot set :ada:`Access_Count` from a :ada:`function`

------
Quiz
------

.. container:: latex_environment footnotesize

 .. code:: Ada

   protected P is
      procedure Initialize (V : Integer);
      procedure Increment;
      function Decrement return Integer;
      function Query return Integer;
   private
      Object : Integer := 0;
   end P;

Which completion(s) of :ada:`P` is(are) illegal?

 .. container:: latex_environment footnotesize

   A. |  ``procedure Initialize (V : Integer) is``
      |  ``begin``
      |     ``Object := V;``
      |  ``end Initialize;``
   B. |  ``procedure Increment is``
      |  ``begin``
      |     ``Object := Object + 1;``
      |  ``end Increment;``
   C. |  :answermono:`function Decrement return Integer is`
      |  :answermono:`begin`
      |     :answermono:`Object := Object - 1;`
      |     :answermono:`return Object;`
      |  :answermono:`end Decrement;`
   D. |  ``function Query return Integer is begin``
      |     ``return Object;``
      |  ``end Query;``

.. container:: animate

   A. Legal
   B. Legal - subprograms do not need parameters
   C. Functions in a protected object cannot modify global objects
   D. Legal

======
Delays
======

-------------
Delay keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Relative: Blocks for at least :ada:`Duration`
- Absolute: Blocks until no earlier than :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. include:: examples/delays/src/main.adb
   :code: Ada

==========================
Task and Protected Types
==========================

---------------
Task Activation
---------------

* Instantiated tasks start running when **activated**
* On the **stack**

   - When **enclosing** declarative part finishes **elaborating**

* On the **heap**

   - **Immediately** at instantiation

.. code:: Ada

   task type First_T is ...
   type First_T_A is access all First_T;

   task body First_T is ...
   ...
   declare
      V1 : First_T;
      V2 : First_T_A;
   begin  -- V1 is activated
      V2 := new First_T;  -- V2 is activated immediately

--------------------
Single Declaration
--------------------

 * Instantiate an **anonymous** task (or protected) type
 * Declares an object of that type

.. code:: Ada

   task type Task_T is
      entry Start;
   end Task_T;

   type Task_Ptr_T is access all Task_T;

   task body Task_T is
   begin
      accept Start;
   end Task_T;
   ...
      V1 : Task_T;
      V2 : Task_Ptr_T;
   begin
      V1.Start;
      V2 := new Task_T;
      V2.all.Start;

-----------
Task Scope
-----------

* Nesting is possible in **any** declarative block
* Scope has to **wait** for tasks to finish before ending
* At library level: program ends only when **all tasks** finish

   .. code:: Ada

      package P is
         task type T;
      end P;

      package body P is
         task body T is
            loop
               delay 1.0;
               Put_Line ("tick");
            end loop;
         end T;

         Task_Instance : T;
      end P;

------------------------------
Waiting On Different Entries
------------------------------

* It is convenient to be able to accept several entries
* The :ada:`select` statements can wait simultaneously on a list of entries

    - For :ada:`task` only
    - It accepts the **first** one that is requested

.. code:: Ada

   select
     accept Receive_Message (V : String)
     do
       Put_Line ("Message : " & String);
     end Receive_Message;
   or
     accept Stop;
       exit;
     end select;

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: examples/protected_objects_2/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: examples/protected_objects_2/src/protected_objects.adb
    :code: Ada

------------------------------
Example: Tasks - Declaration
------------------------------

.. include:: examples/protected_objects_2/src/tasks.ads
    :code: Ada

-----------------------
Example: Tasks - Body
-----------------------

.. include:: examples/protected_objects_2/extracts/tasks.body.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: examples/protected_objects_2/src/test_protected_objects.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    procedure Main is
        protected type O is
           entry P;
        end O;

        protected body O is
           entry P when True is
           begin
              Put_Line ("OK");
           end P;
        end O;
    begin
        O.P;
    end Main;

What is the result of compiling and running this code?

A. "OK"
B. Nothing
C. :answer:`Compilation error`
D. Runtime error

.. container:: animate

    :ada:`O` is a :ada:`protected type`, needs instantiation

========================
Some Advanced Concepts
========================

----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement can wait with a :ada:`delay`

    - If that delay is exceeded with no entry call, block is executed

* The :ada:`delay until` statement can be used as well
* There can be multiple :ada:`delay` statements

   - (useful when the value is not hard-coded)

.. code:: Ada

   select
     accept Receive_Message (V:String) do
       Put_Line ("Message : " & String);
     end Receive_Message;
   or
     delay 50.0;
     Put_Line ("Don't wait any longer");
     exit;
   end select;

------------------------------------------
Calling an Entry With a Delay Protection
------------------------------------------

* A call to :ada:`entry` **blocks** the task until the entry is :ada:`accept` 'ed
* Wait for a **given amount of time** with :ada:`select ... delay`
* Only **one** entry call is allowed
* No :ada:`accept` statement is allowed

.. code:: Ada

   task Msg_Box is
      entry Receive_Message (V : String);
   end Msg_Box;

   procedure Main is
   begin
      select
         Msg_Box.Receive_Message ("A");
      or
         delay 50.0;
      end select;
   end Main;

----------------------------
The Delay Is Not A Timeout
----------------------------

* The time spent by the client is actually **not bounded**

    - Delay's timer **stops** on :ada:`accept`
    - The call blocks **until** :ada:`end` of server-side statements

* In this example, the total delay is up to **1010 s**

.. code:: Ada

    task body Msg_Box is
       accept Receive_Message (S : String) do
          delay 1000.0;
       end Receive_Message;
    ...
    procedure Client is
    begin
       select
          Msg_Box.Receive_Message ("My_Message")
       or
          delay 10.0;
       end select;

----------------------------
Non-blocking Accept or Entry
----------------------------

* Using :ada:`else`

    - Task **skips** the :ada:`accept` or :ada:`entry` call if they are **not ready** to be entered

* On an :ada:`accept`

.. include:: examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* As caller on an :ada:`entry`

.. include:: examples/select_non_blocking_entry_and_call/extracts/main_no_stop.adb
   :code: Ada

* :ada:`delay` is **not** allowed in this case

-----------------------------------
Issues With "Double Non-Blocking"
-----------------------------------

* For :ada:`accept ... else` the server **peeks** into the queue

   - Server **does not** wait

* For :ada:`<entry-call> ... else` the caller looks for a **waiting** server
* If both use it, the entry will **never** be called
* Server

.. include:: examples/select_non_blocking_entry_and_call/extracts/tasks.adb
   :code: Ada

* Caller

.. include:: examples/select_non_blocking_entry_and_call/extracts/main_no_call.adb
   :code: Ada

-----------------------
Terminate Alternative
-----------------------

* An entry can't be called anymore if all tasks calling it are over
* Handled through :ada:`or terminate` alternative

   - Terminates the task if **all others** are terminated
   - Or are **blocked** on :ada:`or terminate` themselves

* Task is terminated **immediately**

    - No additional code executed

.. code:: Ada

   select
      accept Entry_Point
   or
      terminate;
   end select;

-------------------
Guard Expressions
-------------------

* :ada:`accept` may depend on a **guard condition** with :ada:`when`

    - Evaluated when entering :ada:`select`

.. code:: Ada

   task body T is
      Val : Integer;
      Initialized : Boolean := False;
   begin
      loop
         select
            accept Put (V : Integer) do
               Val := V;
               Initialized := True;
            end Put;
         or
            when Initialized =>
               accept Get (V : out Integer) do
                  V := Val;
               end Get;
         end select;
      end loop;
   end T;

------------------------
Protected Object Entries
------------------------

* **Special** kind of protected :ada:`procedure`
* May use a **barrier**, that **only** allows call on a **boolean** condition
* Barrier is **evaluated** and may be **relieved** when

   - A task calls :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

* Several tasks can be waiting on the same :ada:`entry`

    - Only **one** will be re-activated when the barrier is relieved

.. code:: Ada

   protected body Stack is
      entry Push (V : Integer) when Size < Buffer'Length is
      ...
      entry Pop  (V : out Integer) when Size > 0 is
      ...
   end Object;

-------------------------------------
Select On Protected Objects Entries
-------------------------------------

* Same as :ada:`select` but on task entries

   - With a :ada:`delay` part

   .. code:: Ada

      select
         O.Push (5);
      or
         delay 10.0;
         Put_Line ("Delayed overflow");
      end select;

  - or with an :ada:`else` part

   .. code:: Ada

      select
         O.Push (5);
      else
         Put_Line ("Overflow");
      end select;

------
Queue
------

* Protected :ada:`entry`, :ada:`procedure`, and tasks :ada:`entry` are activated by **one** task at a time
* **Mutual exclusion** section
* Other tasks trying to enter are **queued**

    - In **First-In First-Out** (FIFO) by default

* When the server task **terminates**, tasks still queued receive :ada:`Tasking_Error`

-----------------
Queuing Policy
-----------------

* Queuing policy can be set using

.. code:: Ada

   pragma Queuing_Policy (<policy_identifier>);

* The following policy_identifier are available

   - FIFO_Queuing (default)
   - Priority_Queuing

* FIFO_Queuing

   - First-in First-out, classical queue

* Priority_Queuing

   - Takes into account priority
   - Priority of the calling task **at time of call**

-----------------------
Setting Task Priority
-----------------------

* GNAT available priorities are :ada:`0 .. 30`, see :file:`gnat/system.ads`
* Tasks with the highest priority are prioritized more
* Priority can be set **statically**

.. code:: Ada

  task type T
     with Priority => <priority_level>
     is ...

* Priority can be set **dynamically**

.. code:: Ada

   with Ada.Dynamic_Priorities;

   task body T is
   begin
      Ada.Dynamic_Priorities.Set_Priority (10);
   end T;

--------------------------
:ada:`requeue` Instruction
--------------------------

* :ada:`requeue` can be called in any :ada:`entry` (task or protected)
* Puts the requesting task back into the queue

   - May be handled by another :ada:`entry`
   - Or the same one...

* Reschedule the processing for later

   .. code:: Ada

      entry Extract (Qty : Integer) when True is
      begin
         if not Try_Extract (Qty) then
            requeue Extract;
         end if;
      end Extract;

* Same parameter values will be used on the queue

-----------------------
:ada:`requeue` Tricks
-----------------------

* Only an accepted call can be requeued
* Accepted entries are waiting for :ada:`end`

    - Not in a :ada:`select ... or delay ... else` anymore

* So the following means the client blocks for **2 seconds**

.. include:: examples/select_requeue_issue/extracts/tasks.bodies.adb
   :code: Ada

------------------
Abort Statements
------------------

* :ada:`abort` stops the tasks **immediately**

    - From an external caller
    - No cleanup possible
    - Highly unsafe - should be used only as **last resort**

   .. code:: Ada

      procedure Main is
         task type T;

         task body T is
         begin
            loop
               delay 1.0;
               Put_Line ("A");
            end loop;
         end T;
      begin
         delay 10.0;
         abort T;
      end;

-----------------------------------
:ada:`select` ... :ada:`then abort`
-----------------------------------

* :ada:`select` can call :ada:`abort`
* Can abort anywhere in the processing
* **Highly** unsafe

-------------------------
Multiple Select Example
-------------------------

.. include:: examples/task_select_multiple_or/extracts/task_select.select_or.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: examples/task_select_multiple_or/src/main.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    task T is
       entry E1;
       entry E2;
    end T;
    ...
    task body Other_Task is
    begin
       select
          T.E1;
       or
          T.E2;
       end select;
    end Other_Task;

What is the result of compiling and running this code?

A. :ada:`T.E1` is called
B. Nothing
C. :answer:`Compilation error`
D. Runtime error

.. container:: animate

    A :ada:`select` entry call can only call one :ada:`entry` at a time.

------
Quiz
------

.. container:: columns

 .. container:: column

  .. code:: Ada

      procedure Main is
         task T is
            entry A;
         end T;

         task body T is
         begin
            select
               accept A;
               Put ("A");
            else
               delay 1.0;
            end select;
         end T;
      begin
         select
            T.A;
         else
            delay 1.0;
         end select;
      end Main;

 .. container:: column

  What is the output of this code?

  A. "AAAAA..."
  B. :answer:`Nothing`
  C. Compilation error
  D. Runtime error

  .. container:: animate

      Common mistake: :ada:`Main` and :ada:`T` won't wait on each other and
      will both execute their :ada:`delay` statement only.

.

------
Quiz
------

.. code:: Ada

    procedure Main is
       task type T is
          entry A;
       end T;

       task body T is
       begin
          select
             accept A;
          or
             terminate;
          end select;

          Put_Line ("Terminated");
       end T;

       My_Task : T;
    begin
       null;
    end Main;

What is the output of this code?

A. "Terminated"
B. :answer:`Nothing`
C. Compilation error
D. Runtime error

.. container:: animate

    :ada:`T` is terminated at the end of :ada:`Main`

------
Quiz
------

.. code:: Ada

    procedure Main is
    begin
       select
          delay 2.0;
       then abort
          loop
             delay 1.5;
             Put ("A");
          end loop;
       end select;

       Put ("B");
    end Main;

What is the output of this code?

A. "A"
B. "AAAA..."
C. :answer:`"AB"`
D. Compilation error
E. Runtime error

.. container:: animate

    :ada:`then abort` aborts the select only, not :ada:`Main`.

------
Quiz
------

.. code:: Ada

    procedure Main is
        Ok : Boolean := False

        protected O is
           entry P;
        end O;

        protected body O is
        begin
           entry P when Ok is
              Put_Line ("OK");
           end P;
        end O;
    begin
        O.P;
    end Main;

What is the result of compiling and running this code?

A. "OK"
B. :answer:`Nothing`
C. Compilation error
D. Runtime error

.. container:: animate

    Stuck on waiting for :ada:`Ok` to be set, :ada:`Main` will never terminate.

--------------------------------------
Standard "Embedded" Tasking Profiles
--------------------------------------

* Better performances but more constrained
* Ravenscar profile

  - Ada 2005
  - No :ada:`select`
  - No :ada:`entry` for tasks
  - Single :ada:`entry` for :ada:`protected` types
  - No entry queues

* Jorvik profile

  - Ada 2022
  - Less constrained, still performant
  - Any number of :ada:`entry` for :ada:`protected` types
  - Entry queues

* See RM D.13

=========
Summary
=========

---------
Summary
---------

* Tasks are language-based multithreading mechanisms

   - Not necessarily designed to be operated in parallel
   - Original design assumed task-switching / time-slicing

* Multiple mechanisms to synchronize tasks

   - Delay
   - Rendezvous
   - Protected Objects
