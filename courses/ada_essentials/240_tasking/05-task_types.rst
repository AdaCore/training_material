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
