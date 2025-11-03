========================
Some Advanced Concepts
========================

---------------
Task Activation
---------------

* Instantiated tasks start running when **activated**
* On the **stack**

   - When **enclosing** declarative part finishes **elaborating**

* On the **heap**

   - **Immediately** at instantiation

.. code:: Ada

   task type Some_Task_T is ...
   type Some_Task_Ptr_T is access all Some_Task_T;

   task body Some_Task_T is ...
   ...
   declare
      Task_Object    : Some_Task_T;   -- Task_Object starts
      Access_To_Task : Some_Task_Ptr_T;
   begin
      Access_To_Task := new Some_Task_T;
      -- Task pointed to by Access_To_Task starts

-----------
Task Scope
-----------

* Nesting is possible in **any** declarative block
* Scope has to **wait** for tasks to finish before ending
* At library level: program ends only when **all tasks** finish

  .. code:: Ada

     package Task_Definition is
        task type One_Second_Timer;
     end Task_Definition;

     package body Task_Definition is
        task body One_Second_Timer is
           loop
              delay 1.0;
              Put_Line ("tick");
           end loop;
        end One_Second_Timer;

        Task_Instance : One_Second_Timer;
     end Task_Definition;
