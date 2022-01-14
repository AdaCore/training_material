:title: :toolname:`CodePeer` - Reviews
:subtitle: Reviewing with :toolname:`GNAT Studio`

.. highlight:: ada

******************
Reviewing changes
******************

* Select the menu :menuselection:`CodePeer -> Analyze` in the menu bar at the top.
* Run a **level 4** analysis as a **baseline run**
* Review the error message :file:`radar.adb:10`

The code is indeed a bug

* Mark it as a bug in the review interface, with a proper comment.
* Filter this message out

In `Message Review Status`, select only `Uncategorized`
The bug should no longer be visible

* Reapply the default status filter: only `Intentional` are hidden
* Fix the bug
* Run a new analysis but without making it baseline

The error should entirely disappear.

Notice that the baseline run is displayed on the top left and the current run on the top right.

**************
Runs History
**************

At this point you have 2 runs

| ``#1 baseline``
| ``|``
| ``v``
| ``#2 bug fixed``

* Set filter to display only the fix

In `Message History` select only `removed`

* Reset the filter back
* Question: What will happen if the bug is reintroduced?
* Try it out with a new run

| ``#1 baseline``
| ``|``
| ``v``
| ``#2 bug fixed``
| ``|``
| ``v``
| ``#3 bug reintroduced``

The bug is displayed back, and it kept its review history!

We want to compare our currently broken code to review #2

* Launch a new run with a cutoff to #2
* In message history, filter to display only added bugs

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

***********************
Using a Review Server
***********************

Review server allows for sharing reviews over a baseline run.

This server needs to be run from a console.

* Copy the project directory content to a new directory
* Start a review server from this directory

.. code:: bash

    $ codepeer --ide-server -Pradar

The review server starts listening to `localhost:8080`

Warning: in case you encounter `SQLite DB Error` messages, you may need to reset the database first, by removing the :file:`codepeer/` directory, then running a new baseline analysis from :toolname:`GNAT Studio`

* Add the following to the radar.gpr file

.. code::

    package CodePeer is
       for Server_URL use "http://localhost:8080";
    end CodePeer;

* Refresh :toolname:`GNAT Studio` project view to connect to the server
* Add reviews, comments...
* You can check that the server is indeed used by closing the report, removing the :file:`codepeer/` directory and check that the reviews are still available through the report
