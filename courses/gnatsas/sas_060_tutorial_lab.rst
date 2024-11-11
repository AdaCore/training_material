**********************************
GNAT SAS Tutorial - Step by Step
**********************************

.. PRELUDE:: BEGIN

.. PRELUDE:: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE:: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE:: REQUIRES

.. PRELUDE:: PROVIDES

.. PRELUDE:: END

==============
Introduction
==============

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

-----------------
Getting Started
-----------------

* This module is a lab-based version of the *GNAT SAS Tutorial* found `here <https://docs.adacore.com/live/wave/gnatsas/html/tutorial/index.html>`_

* Copy the :filename:`tutorial` folder from the course materials location

* Contents of the tutorial folder:

  * :filename:`sdc.gpr` - project file
  * :filename:`common` - source directory
  * :filename:`struct` - source directory
  * :filename:`obj` - object file (and metrics results) directory

----------------------
Starting GNAT Studio
----------------------

* From a command prompt, type :command:`gnatsas --help` to verify your path is set correctly

  * If not, add the appropriate :filename:`bin` directory to your path
  * Typically (for Windows), this is located in ``C:\GNATSAS\<version>\bin``

* Start :toolname:`GNAT Studio` and open the :filename:`sdc.gpr` project file by one of these methods:

  * From the application library, select :toolname:`GNAT Studio` and use :menu:`File` |rightarrow| :menu:`Open Project` to navigate to and open :filename:`sdc.gpr`
  * From the command prompt navigate to the :filename:`tutorial` directory and enter :command:`gnatstudio sdc.gpr` to open the project

    * You don't actually need :filename:`sdc.gpr` - :toolname:`GNAT Studio` will automatically open a GPR file if it is the only GPR file in the folder

==================
Running GNAT SAS
==================

----------------
First Analysis
----------------

.. container:: animate 1-

   Perform a deep static analysis on the project

.. container:: animate 2-

      * :menu:`GNATSAS` |rightarrow| :menu:`Analyze`
      * Set **Analysis mode** to *deep*
      * Press :menu:`Execute`

-------------------------
Filter Messages by Rank
-------------------------

.. container:: animate 1-

   * In the *GNATSAS Report*, note the count of *High*, *Medium*, and *Low* messages

     * In the **Locations** window, note the actual messages displayed

.. container:: animate 2-

   * Check/uncheck the *Medium* and *Low* items in **Message ranking**

     * Note the **Locations** window content changes based on which messages are displayed

================
Check Messages
================

-------------------------
Finding a Check Message
-------------------------

.. container:: columns

  .. container:: column

    In the **Locations** window, click on the *medium* message for line 26 of :filename:`tokens.adb`

    .. image:: gnatsas/sas_lab_report.png
      :width: 80%

  .. container:: column

    .. container:: animate 2-

      * Click the triangle next to :filename:`tokens.adb` to show all the messages
      * Select the *medium* message for line 26

      .. image:: gnatsas/sas_lab_tokens_line_26.png
        :width: 50%
        :align: right

      *Note that the file appears and the line is highlighted*

-------------------------------
Understanding a Check Message
-------------------------------

.. container:: latex_environment small

  .. code:: Ada
     :number-lines: 17

     Read_A_Valid_Token : declare
        Word : String := Input.Next_Word;

     begin
        --  Figure out which kind of token we have from the first
        --  character and delegate the full token recognition to
        --  the Read routine in the appropriate Instruction, Values
        --  or Values.Operations package.

        case Word (Word'First) is

.. container:: latex_environment small

   .. list-table::
     :header-rows: 1

     * - Message Part
       - Description

     * - ``tokens.adb:26:18``
       - Source location

     * - ``medium``
       - Message ranking

     * - ``array index check [CWE 120] (Inspector)``
       - Short description of message

     * - ``requires (Input.Next_Word'First) <= (Input.Next_Word'Last)``
       - Explanation / possible remediation

* :toolname:`GNATsas` is warning that line 26 indexes into array* :ada:`Word` without ever checking if the array is not empty, possibly raising a :ada:`Constraint_Error`

  * So we need to investigate how :ada:`Word` is initialized, so we will look at :ada:`Input.Next_Word`

------------------------------
Determining Cause of Message
------------------------------

* To investigate the behavior of :ada:`Input.Next_Word`, right-click on it and select :menu:`Go to Body or Full Declaration`

  * This brings us to the implementation, including the :toolname:`GNATsas` annotations

  .. code:: Ada
    :number-lines: 180

     ---------------
     -- Next_Word --
     ---------------

  .. code:: Ada

     --
     --  Subprogram: input.next_word
     --
     --  Post:
     --    possibly_updated(input.next_word'Result(1..2_147_483_647))
     --    possibly_updated(Line(1..1_024))
     --    input.next_word'Result'Last in 0..1_023
     --    input.next_word'Result'First <= 1_024
     --    Line_Num'Initialized
     --    Last_Char /= 0
     --    First_Char <= 1_024
     --    First_Char - input.next_word'Result'First in 0..1_023
     --

  .. code:: Ada
    :number-lines: 184

     function Next_Word return String is

--------------------------
Interpreting Annotations
--------------------------

* Our interest here is in the result of the call, so we're looking at the postconditions as determined by :toolname:`GNATsas`

  .. code:: Ada

     --    input.next_word'Result'Last in 0..1_023
     --    input.next_word'Result'First <= 1_024

* This is indicating that for the result (return value) of :ada:`Input.Next_Word`, :ada:`'Last` can be 0 to 1023, and :ada:`'First` just has to be less than 1024

  * This means the last index can be less than the first index, which, in Ada, is an indication of a 0-length array

--------------------
Fixing Our Problem
--------------------

* So we need to add a check in :ada:`Tokens.Next` to deal with this issue

  * On line 25, add the following code:

    .. code:: Ada

      if Word = "" then
         declare
            Temp : Token := (Kind => Val,
                             Val  => Values.Read (""));
         begin
            return Temp;
         end;
      end if;

* Rerun the analysis, and see that the totals changed, and the *check* message is no longer there

==========
Warnings
==========

------------------------
Potential Logic Errors
------------------------

* In the **Locations** window, click on the message for line 41 of :filename:`stack.adb`

  .. container:: latex_environment tiny

    ``stack.adb:41:4: medium warning: suspicious precondition (Inspector): precondition for Last does not have a contiguous range of values``

  .. code:: Ada
    :number-lines: 1

    --  Subprogram: stack.push
    --
    --  Post:
    --    Tab(1..198 | 200) = One-of{V, Tab(1..198 | 200)'Old}
    --    Last in (1..198 | 200)
    --    Last = Last'Old - 1
    --
    --  Pre:
    --    V.E'Initialized
    --    V /= null
    --    Last in (2..199 | 201)
    --
    --  Global_outputs:
    --    Last, Tab(1..198 | 200)

* The non-contiguous values on line 4, 5, 11, and 14 indicate a possible issue

------------------------------
Determining Cause of Message
------------------------------

* Precondition of :ada:`-- Last in (2..199 | 201)` indicates that 199 and 201 are legal, but 200 is not

  * 200 is an interesting number - it happens to be the length of :ada:`Tab`
  * What happens in the code when Last is 199, 200, or 201?

.. code:: Ada
  :number-lines: 41

   procedure Push (V : Value) is
   begin
      if Last = Tab'Last then
         raise Overflow;
      end if;

      Screen_Output.Debug_Msg ("Pushing -> " & Values.To_String (V));

      Last := Last - 1;
      Tab (Last) := V;
   end Push;

.. container:: animate

  * If :ada:`Last` is 199, the :ada:`if` statement is False, and we assign :ada:`Tab(198)` to :ada:`V`
  * If :ada:`Last` is 201, the :ada:`if` statement is False, and we assign :ada:`Tab(200)` to :ada:`V`
  * If :ada:`Last` is 200, the :ada:`if` statement is True, and we raise an overflow exception

  If this is a :ada:`Push` routine, why are we *decrementing* :ada:`Last`?

  Fix the issue, and re-run the analysis.

================
False Positive
================

----------------------------------------
Messages for Something That Is Correct
----------------------------------------

* Not all messages reported by :toolname:`GNAT SAS` are actual errors

  * :dfn:`False positive` - result of performing static analysis on complex code

* In the **Locations** window, click on the message for line 191 of :filename:`input.adb`

    ``input.adb:191:13: low: array index check [CWE 120] (Inspector): requires First_Char <= 1_024``

Why is this a false positive?

.. container:: animate 2-

    * :ada:`Skip_Spaces` uses :ada:`Get_Char` to get the next printable character
    * :ada:`Get_Char` increments :ada:`First_Char` to a maximum of :ada:`Line'Last + 1`
    * :ada:`Skip_Spaces` calls :ada:`Unread_Char` to decrement :ada:`First_Char`
    * So :ada:`First_Char` will never be greater than :ada:`Line'Last`

----------------
Review Message
----------------

* In the **Locations** window, click on the message for line 191 of :filename:`input.adb`

* Click then pencil icon next to the message and select :menu:`Manual Review`

* Set the status to **False positive** and press :menu:`OK`

* Rerun the analysis

  * Note the number of messages decreased
  * To include the message in the report, select **False positive** from the *Message review status* filter

========================
Running GNAT SAS Again
========================

-----------------------
Comparing to Baseline
-----------------------

* Note that each of the previous runs have new timestamps (upper right corner of *GNATSAS Report* tab), but our baseline hasn't changed (upper left corner)

  * Messages removed by fixing code are still in the history
  * Select **removed** in *Message history* filter to see old messages

    * Old messages appear in *Locations* window in italics

    .. image:: gnatsas/sas_lab_removed_messages.png
      :width: 50%

  * **added** displays messages added since baseline run
  * **unchanged** displays messages in baseline and also in current run

--------------------
Resetting Baseline
--------------------

* To set current state to be baseline 

  * :menu:`GNATSAS` |rightarrow| :menu:`Baseline` |rightarrow| :menu:`Bump Baseline to Current Run`
  * History is lost
  * All future runs will be compared to this new baseline

*Note: You can also use the* :command:`timeline` *switch when comparing runs. See the* **Timelines** *chapter in the* **GNAT SAS User's Guide**
