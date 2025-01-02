=======
Tasks
=======

------------------
Task Declaration
------------------

* Each instance of a task type is executing **concurrently**
* The **whole** tasking setup must be **static**

    - Compiler "compiles-in" the scheduling

* Task instances must be declared at the **library level**

    - Reminder: :ada:`main` declarative part is **not** at library level

* Body of a task must **never stop**
* Tasks should probably **yield**

    - For example with :ada:`delay until`
    - Or also a **protected entry guard** (see later)
    - Because of **Ravenscar scheduling** (see later)

-------------------------------------
Ravenscar Tasks Declaration Example
-------------------------------------

:filename:`my_tasks.ads`

.. code:: Ada

    package My_Tasks is
        task type Printer;

        Printer_Task_1 : Printer;
        Printer_Task_2 : Printer;
    end My_Tasks;

:filename:`my_tasks.adb`

.. code:: Ada

    with Ada.Text_IO; use Ada.Text_IO;
    with Ada.Real_Time; use Ada.Real_Time;

    package body My_Tasks is
        Printer_Task_3 : Printer; --  correct

        task body Printer is
            Period : Time_Span := Milliseconds (100);
            Next : Time := Clock + Period;
            -- P : Printer -- /!\ Would be incorrect: not at library level
        begin
            loop
                Put_Line ("loops");

                --  Yielding
                delay until Next;
                Next := Next + Period;
            end loop;
        end Printer;
    end My_Tasks;

