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
               Put_Line ("receive " & S);
            end Receive_Message;
         end loop;
      end Msg_Box_T;

      T : Msg_Box_T;

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
      receive 1
      calling receive 2
      -- Blocked until another task calls Start

------------------------
Rendezvous with a Task
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

.. include:: ../examples/task_very_simple/src/tasks.ads
    :code: Ada

--------------------
Example: Task - Body
--------------------

.. include:: ../examples/task_very_simple/src/tasks.adb
    :code: Ada

---------------
Example: Main
---------------

.. include:: ../examples/task_very_simple/src/main.adb
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
B. Run-time error
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
B. Run-time error
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
         Task_Instance : T;
      begin
         Task_Instance.Hello;
         Task_Instance.Goodbye;
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
