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
Waiting on Different Entries
------------------------------

* It is convenient to be able to accept several entries
* The :ada:`select` statements can wait simultaneously on a list of entries

    - For :ada:`task` only
    - It accepts the **first** one that is requested

.. code:: Ada

   select
     accept Receive_Message (V : String)
     do
       Put_Line ("Message : " & V);
     end Receive_Message;
   or
     accept Stop;
       exit;
     end select;

------------------
Guard Conditions
------------------

* :ada:`accept` may depend on a **guard condition** with :ada:`when`

    - Evaluated when entering :ada:`select`

* May use a :dfn:`guard condition`, that **only** accepts entries on a **boolean** condition

    - Condition is evaluated when the task reaches it

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
* May use a :dfn:`barrier` which is evaluated when

   - A task calls an :ada:`entry`
   - A protected :ada:`entry` or :ada:`procedure` is **exited**

* Several tasks can be waiting on the same :ada:`entry`

    - Only **one** may be re-activated when the barrier is **relieved**

.. code:: Ada

   protected body Stack is
      entry Push (V : Integer) when Size < Buffer'Length is
      ...
      entry Pop  (V : out Integer) when Size > 0 is
      ...
   end Object;

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: ../examples/protected_objects_2/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: ../examples/protected_objects_2/src/protected_objects.adb
    :code: Ada

------------------------------
Example: Tasks - Declaration
------------------------------

.. include:: ../examples/protected_objects_2/src/tasks.ads
    :code: Ada

-----------------------
Example: Tasks - Body
-----------------------

.. include:: ../examples/protected_objects_2/extracts/tasks.body.adb
    :code: Ada

------------------
Example: Main
------------------

.. include:: ../examples/protected_objects_2/src/test_protected_objects.adb
    :code: Ada

------
Quiz
------

.. code:: Ada

    procedure Main is
        protected type O is
           entry P;
        private
            Ok : Boolean := False;
        end O;

        protected body O is
           entry P when not Ok is
           begin
              Ok := True;
           end P;
        end O;
    begin
        O.P;
    end Main;

What is the result of compiling and running this code?

A. :ada:`Ok = True`
B. Nothing
C. :answer:`Compilation error`
D. Run-time error

.. container:: animate

    :ada:`O` is a :ada:`protected type`, needs instantiation
