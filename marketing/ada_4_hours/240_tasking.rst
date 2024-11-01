*********
Tasking
*********

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
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

================
Introduction
================

---------------
A Simple Task
---------------

* Concurrent code execution via :ada:`task`

   .. code:: Ada

      procedure Main is
         task type Put_T;
         task body Put_T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end Put_T;

         T : Put_T;
      begin -- Main task body
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

      T : Msg_Box_T;

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

===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Multitask-safe** accessors to get and set state
* **No** direct state manipulation
* **No** concurrent modifications

.. container:: columns

 .. container:: column

  .. code:: Ada

   protected type Some_Value is
      procedure Set
         (V : Integer);
      function Get
         return Integer;
   private
      Value : Integer;
   end Some_Value;

 .. container:: column

  .. code:: Ada

   protected body Some_Value is
      procedure Set
         (V : Integer) is
      begin
         Value := V;
      end Set;

      function Get
         return Integer is
      begin
         return Value;
      end Get;
   end Some_Value;

.

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **No** concurrent call to either :ada:`procedure` or :ada:`function`

   - In case of concurrency, other callers get **blocked**

      - Until call finishes

======
Delays
======

-------------
Delay keyword
-------------

- :ada:`delay` keyword part of tasking
- Blocks for a time
- Relative: Blocks for at least :ada:`Duration`
- Absolute: Blocks until a given :ada:`Calendar.Time` or :ada:`Real_Time.Time`

.. code:: Ada

   procedure Main is
      Relative : Duration := 1.0;
      Absolute : Calendar.Time
        := Calendar.Time_Of (2030, 10, 01);
   begin
      delay Relative;
      delay until Absolute;
   end Main;
