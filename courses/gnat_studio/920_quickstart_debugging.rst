.. role:: ada(code)
   :language: ada

============================
Debugging with GNAT Studio
============================

-----------------------
1. Start the Debugger
-----------------------

.. columns::

   .. column::

      Click on the Build and Debug icon

         .. image:: gnat_studio/debug_icon.jpg

   .. column::

         :toolname:`GNAT Studio` will go into debugging perspective, which you can see because a few more views have popped up.

         .. image:: gnat_studio/debug_active_perspective.jpg

Eight new buttons are added to the toolbar, allowing for precise debugging actions.

.. image:: gnat_studio/debug_toolbar.jpg
   :width: 30%

---------------------
2. Set a Breakpoint
---------------------

a. Open source file :file:`main.adb` and select an executable line
b. Right-click and select :menu:`Debug` :math:`\rightarrow` :menu:`Continue until line <>`

   * Temporary breakpoint will be set, but *Debugger Console* states "The program is not being run"

c. In toolbar, click :menu:`Debug Continue` icon

   * **Run/Start** dialog will appear - make sure *Stop at beginning of main subprogram* is selected, then press :menu:`OK`
   * Execution halts at first line of executable code

d. Click :menu:`Debug Continue` button again

   * Execution halts at line you previously selected

e. Practice clicking the remaining navigation buttons

-------------------
3. Examining Data
-------------------

.. columns::

   .. column::

      a. While execution is stopped, hover over any variable to see it's value and other information

         .. image:: gnat_studio/quickstart/debug-data_hover.jpg

   .. column::

      b. To monitor a data object during execution, you can add it to the **Variables** window

         * Right-click the object and select :menu:`Debug` :math:`\rightarrow` :menu:`Display <> in Variables view`

         .. image:: gnat_studio/quickstart/debug-data_view.jpg
