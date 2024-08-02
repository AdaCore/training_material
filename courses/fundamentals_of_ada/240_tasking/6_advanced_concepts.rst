========================
Some Advanced Concepts
========================

---------------------------
Waiting On Multiple Entries
---------------------------

* :ada:`select` can wait on multiple entries

    - With **equal** priority, regardless of declaration order

.. code:: Ada

  loop
    select
      accept Receive_Message (V : String)
      do
        Put_Line ("Message : " & V);
      end Receive_Message;
    or
      accept Stop;
      exit;
    end select;
  end loop;
  ...
  T.Receive_Message ("A");
  T.Receive_Message ("B");
  T.Stop;

----------------------
Waiting With a Delay
----------------------

* A :ada:`select` statement may **time-out** using :ada:`delay` or :ada:`delay until`

    - Resume execution at next statement

* Multiple :ada:`delay` allowed

   - Useful when the value is not hard-coded

.. code:: Ada

  loop
    select
      accept Receive_Message (V : String) do
        Put_Line ("Message : " & V);
      end Receive_Message;
    or
      delay 50.0;
      Put_Line ("Don't wait any longer");
      exit;
    end select;
  end loop;

*Task will wait up to 50 seconds for* :ada:`Receive_Message`, *print a message, and then enter the loop. Without the* :ada:`exit` *it will print the message and wait another 50 seconds, and so on*

------------------------------------------
Calling an Entry With a Delay Protection
------------------------------------------

* A call to :ada:`entry` **blocks** the task until the entry is :ada:`accept` 'ed
* Wait for a **given amount of time** with :ada:`select ... delay`
* Only **one** entry call is allowed
* No :ada:`accept` statement is allowed

.. code:: Ada

   task Msg_Box is
      entry Receive_Message (V : String);
   end Msg_Box;

   procedure Main is
   begin
      select
         Msg_Box.Receive_Message ("A");
      or
         delay 50.0;
      end select;
   end Main;

*Procedure will wait up to 50 seconds for* :ada:`Receive_Message` *to be accepted before it gives up*

----------------------------
Non-blocking Accept or Entry
----------------------------

* Using :ada:`else`

    - Task **skips** the :ada:`accept` or :ada:`entry` call if they are **not ready** to be entered

* :ada:`delay` is **not** allowed in this case

.. code:: Ada

   select
      accept Receive_Message (V : String) do
         Put_Line ("Received : " & V);
      end Receive_Message;
   else
      Put_Line ("Nothing to receive");
   end select;

   [...]

   select
      T.Receive_Message ("A");
   else
      Put_Line ("Receive message not called");
   end select;

------
Queue
------

* Protected :ada:`entry` or :ada:`procedure` and tasks :ada:`entry` are activated by **one** task at a time
* **Mutual exclusion** section
* Other tasks trying to enter are **queued**

    - In **First-In First-Out** (FIFO) by default

* When the server task **terminates**, tasks still queued receive :ada:`Tasking_Error`

----------------
Advanced Tasking
----------------

Other constructions are available

* **Guard condition** on :ada:`accept`
* :ada:`requeue` to **defer** handling of an :ada:`entry` call
* :ada:`terminate` the task when no :ada:`entry` call can happen anymore
* :ada:`abort` to stop a task immediately
* :ada:`select ... then abort` some other task
