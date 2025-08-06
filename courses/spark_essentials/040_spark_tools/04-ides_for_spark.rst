================
IDEs for SPARK
================

---------------------------------
Available IDEs Supporting SPARK
---------------------------------

* :toolname:`GNAT Studio`

  - The AdaCore flagship IDE
  - **Best** integration overall

    + Most interaction capabilities
    + Specialized display of rich messages
    + Display of traces and counterexamples

* Ada/SPARK extension for Visual Studio Code

   - If you are already using VS Code

---------------------------------------------
Basic :toolname:`GNAT Studio` Look and Feel
---------------------------------------------

.. image:: gnatstudio-look_and_feel.png

-----------------------------------------------
:toolname:`GNATprove` :menu:`SPARK` Main Menu
-----------------------------------------------

.. image:: spark_menu-explanations.png

------------------------------
Project Tree Contextual Menu
------------------------------

.. image:: spark_rightclick-source_tree.jpeg
   :width: 100%

-----------------------------
Source Code Contextual Menu
-----------------------------

.. image:: spark_rightclick-code.jpeg

.. container:: speakernote

   Prove Line - The current line **under the cursor** when the contextual menu was invoked.

----------------------------
"Basic" Proof Dialog Panel
----------------------------

.. image:: prove_dialog-basic.png

-----------------------------------------------------
Example Analysis Results in :toolname:`GNAT Studio`
-----------------------------------------------------

.. image:: gnatprove-output-ide.jpeg

----------------------------------
Preference for Selecting Profile
----------------------------------

.. container:: columns

 .. container:: column

    * Controlled by SPARK preference "User profile"

       - Basic
       - Advanced

    * Allow more control and options

       - Prover timeout (seconds)
       - Prover steps (effort)
       - Etc.

 .. container:: column

    .. image:: gnatstudio-preferences-spark.jpeg

-------------------------------
"Advanced" Proof Dialog Panel
-------------------------------

.. image:: prove_dialog-advanced.png

