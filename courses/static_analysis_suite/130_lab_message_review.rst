****************
Message Review
****************

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

==================
Reviewing changes
==================

----------------------
Perform Baseline Run
----------------------

* Select the menu :menuselection:`CodePeer` |rightarrow| :menu:`Analyze` in the menu bar at the top.
* Run a **level 4** analysis as a **baseline run**

.. image:: codepeer/radar_analyze_level_4_baseline.png

* Review the error message :file:`radar.adb:10`

The code indeed has a bug

-------------------
Marking a Message
-------------------

* Mark it as a bug in the review interface, with a proper comment.

To do this you have to click on the little pencil to the left of the message in the
*Location* tab.

* Filter this message out

In :menu:`Message Review Status`, select only :menu:`Uncategorized`

.. image:: codepeer/radar_report_filter_status.png

The bug should no longer be visible

------------------
Non-Baseline Run
------------------

* Reapply the default status filter: only :menu:`Intentional` are hidden
* Fix the bug
* Run a new analysis but without making it baseline

The error should entirely disappear.

Notice that the baseline run is displayed on the top left and the current run on the top right.

.. image:: codepeer/radar_report_current_runs.png

==============
Runs History
==============

----------------
Comparing Runs
----------------

At this point you have 2 runs

| ``#1 baseline``
| ``|``
| ``v``
| ``#2 bug fixed``

* Set filter to display only the fix

In :menu:`Message History` select only :menu:`Removed`

* Reset the filter back

--------------------------
Repaired Bug Gets Broken
--------------------------

* Quick quiz: What will happen if the bug is reintroduced?
* Try it out with a new run

| ``#1 baseline``
| ``|``
| ``v``
| ``#2 bug fixed``
| ``|``
| ``v``
| ``#3 bug reintroduced``

The bug is displayed back, and it kept its review history!

---------------------
Abbreviated History
---------------------

We want to compare our currently broken code to review #2

* Launch a new run with a cutoff to #2
* In message history, filter to display only added bugs

.. image:: codepeer/radar_report_filter_added_only.png

Only the broken piece of code should appear

* Fix back the code and launch a new baseline run

| ``#1 baseline``
| ``|``
| ``v``
| ``#2 fixed``
| ``|``
| ``v``
| ``#3 bug reintroduced``
| ``|``
| ``v``
| ``#4``
| ``|``
| ``v``
| ``#5 fixed and baseline``

=======================
Using a Review Server
=======================

--------------------------
Starting a Review Server
--------------------------

Review server allows for sharing reviews over a baseline run.

This server needs to be run from a console.

* Copy the project directory content to a new directory
* Start a review server from this new directory

.. code:: bash

    $ codepeer --ide-server -Pradar --verbose

The review server starts listening to ``localhost:8080``
This command will serve forever, it is necessary to keep it in the background.

.. image:: codepeer/radar_ide_server_start.png

**Warning**: you may encounter ``SQLite DB Error`` messages. In that case you will need to reset the database, by removing the :file:`codepeer/` directory, then running a new baseline analysis from :toolname:`GNAT Studio`.

------------------------------
Connect to the Review Server
------------------------------

* Open the :file:`radar.gpr` file

This can be done by right-clicking the name of the project on the Project tab, then clicking Project > Edit Source File 

.. image:: codepeer/radar_open_gpr.png

* Uncomment the following line in the project file

.. code::

    for Server_URL use "http://localhost:8080";

* Refresh :toolname:`GNAT Studio` project view to connect to the server

.. image:: codepeer/radar_refresh_project.png

-----------------------------
Verifying the Review Server
-----------------------------

You can check that the Review Server is indeed being used

* Open the CodePeer Menu, only the "Display Code Review" option is displayed.

.. image:: codepeer/radar_only_display_code_review.png

* Quick quiz: Why aren't the other options available?

The Review Server is running on a distant database that is not available to the local CodePeer.
As a matter of fact, in that setup you are not expected to run your local analysis, but rather to connect to the distant server to perform review onto analysis that have been already performed.

* Add a review
* Check on the server log that the review has been added

You should see a call to ``appendAudit``.

.. image:: codepeer/radar_ide_server_add_review.png
