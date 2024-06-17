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

.. include:: 240_tasking/22_tasks_with_rendezvous_details.rst
.. include:: 240_tasking/32_protected_objects_in_depth.rst
.. include:: 240_tasking/41_delays.rst
.. include:: 240_tasking/52_task_types_and_entries.rst
.. include:: 240_tasking/62_advanced_concepts_for_advanced.rst
.. include:: 240_tasking/91_summary.rst
