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

-------------------------------------
Basic **GNAT Studio** Look and Feel
-------------------------------------

.. image:: spark_essentials/spark_with_gnatstudio.png

---------------------------------
**GNATprove** "SPARK" Main Menu
---------------------------------

.. image:: spark_menu-explanations.png

------------------------------
Project Tree Contextual Menu
------------------------------

.. image:: spark_essentials/spark_rightclick-source_tree.png
   :width: 100%

-----------------------------
Source Code Contextual Menu
-----------------------------

.. image:: spark_essentials/spark_rightclick-code.png

.. container:: speakernote

   Prove Line - The current line **under the cursor** when the contextual menu was invoked.

----------------------------
"Basic" Proof Dialog Panel
----------------------------

.. image:: spark_essentials/prove_dialog-basic.png

---------------------------------------------
Example Analysis Results in **GNAT Studio**
---------------------------------------------

.. image:: spark_essentials/gnatprove-output-ide.png

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

    .. image:: spark_essentials/gnatstudio-preferences-spark.png

-------------------------------
"Advanced" Proof Dialog Panel
-------------------------------

.. image:: spark_essentials/prove_dialog-advanced.png

