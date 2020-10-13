
*********
Tasking
*********

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

* When reaching `accept` statement, the task waits until **entry** is called

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
      --  Locks until somebody calls Start
      Put_Line ("calling receive 2");
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

* Simple `accept` statement

   - Used by a server task to indicate a willingness to provide the service at a given point

* Selective `accept` statement (later in these slides)

   - Wait for more than one rendezvous at any time
   - Time-out if no rendezvous within a period of time
   - Withdraw its offer if no rendezvous is immediately available
   - Terminate if no clients can possibly call its entries
   - Conditionally accept a rendezvous based on a guard expression

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

* No function can be called when a procedure is called

========================
Task / Protected Types
========================

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
* Protected object types are `limited` types

.. code:: Ada
    
   protected type Object is
      entry Push (V : Integer);
      entry Pop  (V : out Integer);
   private
      Buffer : Integer_Array (1 .. 10);
      Size : Integer := 0;
   end Object;
       
   protected body Object is
      entry Push (V : Integer) when Size < Buffer'Length is
      begin
         Buffer (Size + 1) := V;
         Size := Size + 1;
      end Push;
          
      entry Pop  (V : out Integer) when Size > 0 is
      begin
         V := Buffer (Size);
         Size := Size - 1;
      end Pop;
   end Object;
     
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
     
===========================
Some Advanced Concepts...
===========================

------------------------------
Waiting On Different Entries
------------------------------

* It is convenient to be able to accept several entries
* The `select` statements can wait simultaneously on a list of entries, and accept the first one that is requested

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

* A `select` statement can wait for only a given amount of time, and then do something when that delay is exceeded
* The `delay until` statement can be used as well
* There can be multiple `delay` statements 

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
* It is possible to wait for a given amount of time using a `select` ... `delay` statement
* Only one entry call is allowed
* No `accept` statement is allowed

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

* The `else` part allows task to avoid waiting if the accept statements or entries are not ready to be entered
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

* When waiting for an entry, if all other task dependent on the same master task (including the master task) are terminated, the entry can't be called anymore
* This can be detected by the `or terminate` alternative, which terminates the tasks if all other tasks are terminated

   - Or themselves waiting on `or terminate` select statements

* Once reached, the task is terminated right away, no additional code is called

.. code:: Ada
    
   select
      accept Entry_Point;
   or
      terminate;
   end select;
     
-------------------
Guard Expressions
-------------------

* The `accept` statement can be activated according to a guard condition
* This condition is evaluated when entering select

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
     
--------------------------------
Protected Object Entries (1/2)
--------------------------------

* Protected entries are a special kind of protected procedures
* They can be defined using a barrier, a conditional expression allowing the entry to be called or not
* The barriers are evaluated...

   - Every time a task requests to call an entry
   - Every time a protected entry or procedure is exited

.. code:: Ada
    
   protected Object is
      entry Push (V : Integer);
      entry Pop  (V : out Integer);
   private
      Buffer : Integer_Array (1 .. 10);
      Size : Integer := 0;
   end Object;
       
   protected body Object is
      entry Push (V : Integer) when Size < Buffer'Length is
      begin
         Buffer (Size + 1) := V;
         Size := Size + 1;
      end Push;
          
      entry Pop  (V : out Integer) when Size > 0 is
      begin
         V := Buffer (Size);
         Size := Size - 1;
      end Pop;
   end Object;
     
--------------------------------
Protected Object Entries (2/2)
--------------------------------

* Several tasks can be waiting on entries
* Only one task is reactivated when the barrier is relieved, depending on the activation policy

.. code:: Ada
    
   task body T1 is
      V : Integer;
   begin
      Object.Pop (V);
   end T1;
       
   task body T2 is
      V : Integer;
   begin
      Object.Pop (V);
   end T2;
       
   task body T3 is
   begin
      delay 1.0;
      Object.Push (42);
   end T3;
     
-------------------------------------
Select On Protected Objects Entries
-------------------------------------

* Works the same way as select on task entries

   - With a `delay` part

   .. code:: Ada

      select
         O.Push (5);
      or
         delay 10.0;
         Put_Line ("Delayed overflow");
      end select;
      
   - With an `else` part

   .. code:: Ada

      select
         O.Push (5);
      else
         Put_Line ("Overflow");
      end select;
 
-------------------
Notion Of a Queue
-------------------

* Protected entries, protected procedures and task entries can only be activated by one task at a time
* If several tasks are trying to enter a mutually exclusion section, they are put in a queue
* By default, tasks are entering the queue in FIFO
* If several tasks are in a queue when the server task is terminated, `Tasking_Error` is sent to the waiting tasks

-----------------------
`requeue` Instruction
-----------------------

* The `requeue` instruction can be called in an entry (task or protected)
* It places the queued task back to another entry with the same profile

   - Or the same entry...

* Useful if the treatment couldn't be done and need to be re-considered later 

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

* All tasks can be abruptly aborted

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
 
* Abortion may stop the task almost anywhere in the assembly code
* Highly unsafe - should be used only as last resort

---------------------------
`select` ... `then abort`
---------------------------

* A sequence of statements can be planned to be aborted as a result of an incoming event (entry call or delay expiration)
* Again, abortion can occur anywhere in the processing, avoid it if other options exist

   .. code:: Ada

      select
         delay 10.0;
      then abort
         Some_Long_Processing;
      end select;
      -- statements to execute after select
 
   .. code:: Ada

      select
         T.Wait_For_Interruption;
      then abort
         Some_Long_Processing;
      end select;
      -- statements to execute after select
 
===========
Ravenscar
===========

--------------------------------
What Is the Ravenscar Profile?
--------------------------------

* Use concurrency in embedded real-time systems

   - Verifiable
   - Simple (Implemented reliably and efficiently)

* Scheduling theory for accurate analysis of real-time behavior
* A subset of the Ada tasking model
* Defined to help meeting safety-critical real-time requirements

   - Determinism
   - Schedulability analysis
   - Memory-boundedness
   - Execution efficiency and small footprint
   - Suitability for certification

* State-of-the-art concurrency constructs

   - Adequate for most types of real-time software

-------------------------------
Ravenscar Tasking Limitations
-------------------------------

* Active synchronization not supported

   - Asymmetric rendezvous
   - Entries

* Tasks can only be declared at library level

   - All have to finish before the program terminates

* Protected object entries

   - Only one entry per protected object
   - Barriers can only be simple boolean values

---------------------------
Task Types with Ravenscar
---------------------------

* It is possible to create task types

   - Task objects can only be instantiated statically in Ravenscar
   - Can be instantiated on stack or heap in full Ada tasking model

* By default tasks are activated at the end of the elaboration of their library unit's declarative part

   - As if they were declared there
   - Activation can be deferred to the end of all elaboration

--------------------------------
Protected Types With Ravenscar
--------------------------------

* Like tasks, protected objects can be defined through types
* Instantiation can then be done on library level
* Protected object types are `limited` types

--------------------
Ravenscar Patterns
--------------------

* Periodic tasks (cyclic tasks / time triggered)

   - Sensor data acquisition
   - System monitoring
   - Control loops
   - Display update

* Event driven tasks

   - Alarm, Timeout
   - Interrupt
   - Data from another task

* Protected objects to synchronize tasks

----------------------------------
How Tasks Look Like in Ravenscar
----------------------------------

* Time-triggered task
    
   .. code:: Ada
    
      task body Cyclic is
        Period : constant Time_Span : Milliseconds (10);
        Activation : Time := Clock;
      begin
        loop
          delay until Activation;   
          Do_Something;
          --  Compute next activation time
          Activation := Activation + Period;
         end loop;
      end Cyclic;
     
* Event-triggered task
    
   .. code:: Ada
    
      task body Sporadic is
      begin
         loop
           -- Protected entry        
           Monitor.Wait_Event;
           Do_Something;
         end loop;
      end Sporadic;
     
-----------------
Ravenscar Tasks
-----------------

.. container:: columns

 .. container:: column
  
    * Fixed set of tasks

       - Only at library level
       - No dynamic allocation
       - No nested task declarations
       - Fixed priority
       - Statically created

          + Task descriptors, stacks, ...

 .. container:: column
  
    * Each task is an infinite loop

       - Single "triggering" action (delay or event)
       - Tasks never terminate
       - No entries, no rendezvous
       - No abort

    * Task creation and activation is very simple

       - All tasks are created at initialization
       - Then all are executed according to their priorities

------------------------------------------
Protected Objects and Interrupt Handling
------------------------------------------

* Simple protected operations

   - At most one entry
   - No queuing

      + Only one task can be blocked on the entry

   - Ceiling locking on monoprocessor

      + Bounded priority inversion
      + Efficient locking/unlocking by increasing/decreasing priority

   - "Proxy model" for protected entries

      + Avoid unneeded context switches

* Interrupt handling

   - Simple and efficient

      + Protected procedures as low level interrupt handlers
      + Masking hardware interrupts according to active priority

------------
Priorities
------------

.. container:: columns

 .. container:: column
  
    * Priorities are defined in package System

       - Lower values mean lower priority
       - Two non-overlapping ranges:

          + `Priority`
          + `Interrupt_Priority`

    * Priority is set by a `pragma Priority` or `pragma Interrupt_Priority`

       - Ignored for non-main subprograms

          + Set the priority of the environment task

    * `Interrupt_Priority` for priorities in the interrupt range

 .. container:: column
  
    .. code:: Ada
    
       procedure Main is
         pragma Priority (2);
       
       task T is
         pragma Priority (4);
       
       protected Buffer is
          ...
       private
          pragma Priority (3);
       end Buffer;
     
------------
Scheduling
------------

* Priority based
* No time slicing (quantum)
* A task executes until ...

   - The task is blocked (on delays or on protected object entry)
   - A higher priority task is woken up or unblocked

-----------------
Ceiling Locking
-----------------

.. container:: columns

 .. container:: column

   * Task priority is increased within a protected object

      - Priorities of task must be lower or equal than the priorities of protected objects used
      - Blocks other tasks
      - Performs locks without using locks

|

.. image:: ../../images/ravenscar_ceiling_locking.png
   :width: 45%

 .. container:: column

   .. code:: Ada

      task T is
        pragma Priority(4);
        ...
   
      task body T is
        ...
        P.Set (1);
        ...
 
   .. code:: Ada

      protected P is
         pragma Priority(5);
         procedure Set 
            V : Integer);
 
--------------------------
Synchronous Task Control
--------------------------

* Provides primitives to construct synchronization mechanisms and two-stage suspend operations
* Package exports a `Suspension_Object` type

   - Values are "True" and "False", initially "False"
   - Such objects are awaited by one task but set by other tasks

.. code:: Ada

   package Ada.Synchronous_Task_Control is
      type Suspension_Object is limited private;
      procedure Set_True (S : in out Suspension_Object);
      procedure Set_False (S : in out Suspension_Object);
      procedure Suspend_Until_True (S : in out Suspension_Object);
      function Current_State (S : Suspension_Object) return Boolean;
   private
      ...
   end Ada.Synchronous_Task_Control;
 
---------------
Timing Events
---------------

* User-defined actions executed at a specified wall-clock time

   - Implemented as protected procedures

* Do not require a task or a delay statement

* Controlled via procedural interface 

   - Links the protected procedure
   - Sets the time 

* Ravenscar Run-time Interface
    
   .. code:: Ada
          
      package Ada.Real_Time.Timing_Events is
         type Timing_Event is tagged limited private;
         type Timing_Event_Handler is access protected procedure (
             Event : in out Timing_Event);
         procedure Set_Handler (Event   : in out Timing_Event;
                                At_Time : Time;
                                Handler : Timing_Event_Handler);
         function Current_Handler (Event : Timing_Event)
                                   return Timing_Event_Handler;
         procedure Cancel_Handler (Event     : in out Timing_Event;
                                   Cancelled : out Boolean);
         function Time_Of_Event (Event : Timing_Event) 
                                 return Time;
      private
         ...
      end Ada.Real_Time.Timing_Events;
     
-----------------------
Execution Time Clocks
-----------------------

* Each task has an associated CPU time clock

   - Accessible via function call

* Clocks start after creation but before activation
* Whenever the task executes the clock increments
* Total time includes run-time library and O.S. services executed on its behalf
* System and run-time library execution not specific to a given task may be assigned to some task(s)

   - Implementation-defined whether it does
   - Implementation-defined which task if it does

-------------------------------
Partition Elaboration Control
-------------------------------

* Library units are elaborated in an undefined order

   - They can declare tasks and interrupt handlers
   - Once elaborated tasks start executing
   - Interrupts occur as soon as hardware is enabled

* These are unacceptable race conditions

   - Especially for certification!

* `pragma Partition_Elaboration_Policy`

   - Controls when activation and attachment happens relative to library unit elaboration completion
   - Defined in High Integrity Systems Annex
   - **Concurrent policy**

      + Normal semantics: tasks and interrupts are concurrent with remaining library units' elaboration

   - **Sequential policy**

      + Task activation and interrupt handler attachment are deferred until library unit elaboration completes

-------------------------------
Task Termination Notification
-------------------------------

* Tasks silently terminate

   - Without notification by default

* User-defined handlers for termination

   - Essentially a task's "last wishes"
   - Handlers are protected procedures called by the run-time library

* States differentiated

   - Normal termination
   - Termination due to an unhandled exception 
   - Termination due to task abort

* Ravenscar Run-time Interface
    
   .. code:: Ada
    
      package Ada.Task_Termination is
        type Termination_Handler is access protected procedure (
            T : Ada.Task_Identification.Task_Id);
        procedure Set_Dependents_Fallback_Handler (Handler : Termination_Handler);
        function Current_Task_Fallback_Handler return Termination_Handler;
      end Ada.Task_Termination;
     
---------------------------
Ravenscar Small FootPrint
---------------------------

.. container:: columns

 .. container:: column
  
    * Everything is done by the Ada run-time library

       - No OS underneath

    * Simple

       - Less than 2800 Logical SLOCs
       - Footprint for simple tasking program is 10KB

    * Static tasking model

       - Tasks descriptors and stacks are statically created at compile time
       - Task creation and activation is very simple
       - All tasks are created at initialization
       - Then all activated and executed according to their priority

 .. container:: column
  
    * Simple protected operations

       - No queuing
       - Locking/unlocking by increasing/decreasing priority

    * Complex features removed

       - Such as exception handling and propagation 

    * ECSS (E-ST-40C and Q-ST-80C) qualification material

========
Lab
========

.. include:: labs/240_tasking.lab.rst

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
