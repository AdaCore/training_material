**********************
Lab - Message Review
**********************

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
Lab Setup
==============

----------------------
Perform Baseline Run
----------------------

* Use :toolname:`GNAT Studio` to open the :filename:`default.gpr` project in the :filename:`130_lab_message_review` folder
* Start a baseline run:

  * Select the menu :menu:`CodePeer` |rightarrow| :menu:`Analyze` in the menu bar at the top.
  * Change :menu:`Analysis Level` to **4**
  * Select the :menu:`Baseline run` switch
  * Press :menu:`Execute`

.. image:: codepeer/radar_analyze_level_4_baseline.png

* Review the error message :file:`radar.adb:10`

  * Select :menu:`Locations` tab in Message window
  * Expand :file:`radar.adb` hierarchy (if not already)
  * Select a message on line 10

* The subprogram has two bugs:

  * Overflow check on addition
  * Array index check on array access

===================
Reviewing changes
===================

-------------------
Marking a Message
-------------------

* Mark a bug in the review interface, with a proper comment.

  * Click the pencil icon to the left of the message and select :menu:`Manual Review`
  * In the **CodePeer message review** window, change :menu:`New Status` to *Intentional* and add a comment
  * Click :menu:`OK`

* Filter this message out

  * Switch to the **CodePeer report** tab
  * Find the **Message review status** switches and uncheck everything except *Uncategorized* and *Intentional*
  * Observe the messages displayed in **Locations**
  * Now uncheck *Intentional* and note the number of messages decreases

.. image:: codepeer/radar_report_filter_status.png

------------------
Non-Baseline Run
------------------

* Reapply the default status filter (all boxes checked)

  * Note the reviewed message appears but grayed-out

* Fix the bug
* Run a new analysis but without making it baseline

  * Same analysis command, but uncheck :menu:`Baseline run`

* Message no longer appears
* Notice that the baseline run is displayed on the top left and the current run on the top right.

.. image:: codepeer/radar_report_current_runs.png

==============
Runs History
==============

----------------
Comparing Runs
----------------

* At this point you have 2 runs

  1. Baseline
  2. Current run #2 (bug fixed)

* Set filter to display only the fix

  * In **Message History** uncheck *Added* and *Unchanged*, but check *Removed*
  * Only the message that was for the fixed code appears

* Reset the filter back

  * In **Message History** check *Added* and *Unchanged*, uncheck *Removed*

--------------------------
Repaired Bug Gets Broken
--------------------------

* What will happen if the bug is reintroduced?
* Try it out with a new run

  * Remove the fix
  * Perform another non-baseline run

* The bug is displayed back, and it kept its review history!

* We now have 3 runs

  1. Baseline
  2. Current run #2 (bug fixed)
  3. Current run #2 (code re-broken)

---------------------
Abbreviated History
---------------------

We want to compare our currently broken code to review #2

* Launch a new run with a cutoff to #2

  * Same analysis command, but uncheck :menu:`Baseline run` and set :menu:`Cutoff` to **2**

* In **Message History** check *Added*, uncheck *Unchanged* and *Removed*

  .. image:: codepeer/radar_report_filter_added_only.png

  * Only the broken piece of code should appear

* Fix back the code and launch a new baseline run
