==============
Introduction
==============

-----------------------
Concurrency Mechanisms
-----------------------

* Task

   - **Active**
   - Rendezvous: **Client / Server** model
   - Server **entries**
   - Client **entry calls**
   - Typically maps to OS threads

* Protected object

   - **Passive**
   - *Monitors* protected data
   - **Restricted** set of operations
   - Concurrency-safe **semantics**
   - No thread overhead
   - Very portable

* Object-Oriented

   - :ada:`Synchronized` interfaces
   - Protected objects inheritance

---------------
A Simple Task
---------------

* Concurrent code execution via **task**
* :ada:`limited` types (No copies allowed)

   .. code:: Ada

      procedure Main is
         task type Simple_Task_T;
         task body Simple_Task_T is
         begin
            loop
               delay 1.0;
               Put_Line ("T");
            end loop;
         end Simple_Task_T;
         Simple_Task : Simple_Task_T;
         -- This task starts when Simple_Task is elaborated
      begin
         loop
            delay 1.0;
            Put_Line ("Main");
         end loop;
      end;

* A task is started when its declaration scope is **elaborated**
* Its enclosing scope exits when **all tasks** have finished
