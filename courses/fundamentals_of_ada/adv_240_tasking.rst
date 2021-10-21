
*********
Tasking
*********

.. role:: ada(code)
    :language: Ada

==============
Introduction
==============

---------------
A Simple Task
---------------

* Ada implements the notion of a **thread** via the task entity

   .. code:: Ada

      procedure Main is
         task T;
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

* A task is started when its declaration scope is elaborated
* Its enclosing scope exits when all tasks have finished

=======
Tasks
=======

------------------------
Interacting With Tasks
------------------------

* Active synchronization

   - Client/server model of interaction ("asymmetric rendezvous")
   - Server task declares **entries** for interacting

      + Services it offers to other tasks
      + Can wait for a client task to request its service

   - Client task makes an **entry call**

      + Request for a service offered by another task
      + Will wait for the server task to **accept** and handle entry call

* Passive synchronization

   - Uses data objects with concurrency-safe access semantics
   - **Protected objects** in Ada - more about them later

------------------------
Rendezvous Definitions
------------------------

* A task can declare entries for interacting and wait for an entry call to arrive

   .. code:: Ada

      task T is
         entry Start;
         entry Receive_Message (V : String);
      end T;

      task body T is
      begin
         loop
            accept Start do
               Put_Line ("start");
            end Start;
            accept Receive_Message(V : String) do
               Put_Line ("Receive " & V);
            end Receive_Message;
         end loop;
      end T;

* When reaching :ada:`accept` statement, the task waits until **entry** is called

.. container:: speakernote

   The loop requires Accept and ReceiveMessage to be called one followed by other

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

------
Quiz
------

.. container:: columns

 .. container:: column

  .. container:: latex_environment tiny

   .. code:: Ada

      with Ada.Text_IO; use Ada.Text_IO;
      procedure Main is
         task T is
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

* Tasks are **active** objects
* Synchronization can be achieved through **passive** objects that hold and manage values
* A protected object is an object with an interface

   - No concurrent modifications allowed

* It is a natural replacement for a lot of cases where a semaphore is needed

.. code:: Ada

   protected Object is
      --  Only subprograms are allowed here
      procedure Set (V : Integer);
      function Get return Integer;
   private
      --  Data declaration
      Local : Integer;
   end Object;

   protected body Object is
      procedure Set (V : Integer) is
      begin
         Local := V;
      end Set;

      function Get return Integer is
      begin
         return Local;
      end Get;
   end Object;

-------------------------------------
Protected: Functions Vs. Procedures
-------------------------------------

* Procedures can modify the state of the protected data

   - No concurrent access to procedures can be done
   - No procedure can be called when functions are called

* Functions are just ways to retrieve values, the protected data is read-only

   - Concurrent access to functions can be done

* No function can be called while a procedure is executing

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

What of the following completions for :ada:`P`'s members is illegal?

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

==========================
Task and Protected Types
==========================

------------
Task Types
------------

* It is possible to create task types

   - Objects can be instantiated on the stack or on the heap

* Tasks instantiated on the stack are activated at the end of the elaboration of their enclosing declarative part

   - As if they were declared there

* Tasks instantiated on the heap are activated right away
* Tasks are limited objects (no copies allowed)

.. code:: Ada

   task type T is
      entry Start;
   end T;

   type T_A is access all T;

   task body T is
   begin
      accept Start;
   end T;
   ...
      V1 : T;
      V2 : A_T;
   begin
      V1.Start;
      V2 := new T;
      V2.all.Start;

------------------------
Protected Object Types
------------------------

* Like tasks, protected objects can be defined through types
* Instantiation can then be done on the heap or the stack
* Protected object types are :ada:`limited` types

.. code:: Ada

   protected type Register_T is
      function Read return Integer;
      procedure Write (Value : Integer);
   private
      Register : Integer;
   end Register_T;

   protected body Register_T is
      function Read return Integer is
      begin
         return Register;
      end Read;
      procedure Write (Value : Integer) is
      begin
         Register := Value;
      end Write;
   end Register_T;

-----------------
Scope Of a Task
-----------------

.. container:: columns

 .. container:: column

    * Tasks can be nested in any declarative block
    * When nested in a subprogram, for example, the task and the subprogram body have to finish before the subprogram ends
    * Tasks declared at library level all have to finish before the program terminates

 .. container:: column

    .. code:: Ada

       package P is
          task T;
       end P;

       package body P is
          task body T is
             loop
                delay 1.0;
                Put_Line ("tick");
             end loop;
          end T;
       end P;

========================
Some Advanced Concepts
========================

------------------------------
Waiting On Different Entries
------------------------------

* It is convenient to be able to accept several entries
* The :ada:`select` statements can wait simultaneously on a list of entries, and accept the first one that is requested

.. code:: Ada

   task T is
     entry Start;
     entry Receive_Message (V : String);
     entry Stop;
   end T;

   task body T is
   begin
     accept Start;
     loop
       select
         accept Receive_Message (V : String)
         do
           Put_Line ("Message : " & String);
         end Receive_Message;
       or
         accept Stop;
           exit;
         end select;
     end loop;
   end T;

----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement can wait for only a given amount of time, and then do something when that delay is exceeded
* The :ada:`delay until` statement can be used as well
* There can be multiple :ada:`delay` statements

   - (useful when the value is not hard-coded)

.. code:: Ada

   task body T is
   begin
     loop
       select
         accept Receive_Message (V:String) do
           Put_Line ("Message : " & String);
         end Receive_Message;
       or
         delay 50.0;
         Put_Line ("Don't wait any longer");
         exit;
       end select;
     end loop;
   end T;

.. container:: speakernote

   Task will wait up to 50 seconds for "Receive_Message", print a message, and then enter the loop
   Without the "exit" it will print the message and wait another 50 seconds, and so on

------------------------------------------
Calling an Entry With a Delay Protection
------------------------------------------

* A call to an entry normally blocks the thread until the entry can be accepted by the task
* It is possible to wait for a given amount of time using a :ada:`select` ... :ada:`delay` statement
* Only one entry call is allowed
* No :ada:`accept` statement is allowed

.. code:: Ada

   task T is
      entry Receive_Message (V:String);
   end T;

   procedure Main is
   begin
      select
         T.Receive_Message ("A");
      or
         delay 50.0;
      end select;
   end Main;

.. container:: speakernote

   Procedure will wait up to 50 seconds for "Receive_Message" to be accepted before it gives up

-------------------------------------------
Avoid Waiting If No Entry Or Accept Ready
-------------------------------------------

* The :ada:`else` part allows task to avoid waiting if the accept statements or entries are not ready to be entered
* No delay statement is allowed in this case

.. code:: Ada

   task body T is
   begin
      select
         accept Receive_Message (V : String) do
            Put_Line ("Received : " & V);
         end Receive_Message;
      else
         Put_Line ("Nothing to receive");
      end select;
   end T;

   procedure Main is
   begin
      select
         T.Receive_Message ("A");
      else
         Put_Line ("Receive message not called");
      end select;
   end Main;

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
      [...]

      entry Pop  (V : out Integer) when Size > 0 is
      [...]
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

------------------
Abort Statements
------------------

* :ada:`abort` stops the tasks **immediately**

    - From an external caller
    - No cleanup possible
    - Highly unsafe - should be used only as **last resort**

   .. code:: Ada

      procedure Main is
         task T;

         task T is
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
* Can abort anywhere in the processing, highly unsafe

=========
Summary
=========

---------
Summary
---------

* Tasks are language-based multiprocessing mechanisms

   - Not necessarily designed to be operated in parallel
   - Original design assumed task-switching / time-slicing

* Multiple mechanisms to synchronize tasks

   - Delay
   - Rendezvous
   - Protected Objects
