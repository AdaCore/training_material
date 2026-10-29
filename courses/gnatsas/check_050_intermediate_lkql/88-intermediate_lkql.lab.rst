=====
Lab
=====

--------------------
A Common Unit Rule
--------------------

In this lab we are going to build a "unit rule" to validate that
the number of lines in a subprogram do not exceed some value.

We are using a "unit rule" because we only want to search for
subprograms within a unit - no need to run the rule for every node.

.. note::

  This rule already exists in :toolname:`GNATcheck` as :lkql:`maximum_subprogram_lines`.
  Because rule names cannot conflict, we're using a different name here as we
  learn how the delivered rule was created.

-----------------
Example Project
-----------------

* Copy the :filename:`intermediate_lkql` folder from the course materials location

* Contents of the folder:

  * :filename:`default.gpr` - project file
  * :filename:`src` - source directory

    * Contains similar source code to the previous chapter lab
    * Package body now has some (mostly harmless) subprogram implementations

---------------------------
Step 1 - Create Rule File
---------------------------

.. container:: animate 1-

  * Create rule :lkql:`flag_large_subprograms`

    * In file :filename:`flag_large_subprograms.lkql`
    * Use :lkql:`@unit_check` annotation
    * This rule will require a parameter :lkql:`Lines`

      * So we only flag subprograms longer than :lkql:`Lines`

  * For now, just return a message indicating a violation

.. container:: animate 2-

  .. code:: lkql

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 0) = [
       {message: "unit flagged", loc: unit.root}
    ]

  **Note**

    .. raw:: latex

      \vspace{2mm}

    *This rule uses "list comprehension" to build a list of values to return.
    Right now, the list contains just one element (for the selected unit)*

----------------------------------
Step 2 - Dealing With Parameters
----------------------------------

* We need a mechanism to pass a value to **Lines**

  * We cannot do it from the command line

* Instead, we will need to build a rules file and update the :toolname:`GNATcheck` call

* Rules file looks like LKQL with a list of rules to apply

  :filename:`rules_file.lkql`

  .. code:: lkql

    val rules = @{
      Flag_Large_Subprograms: {Lines: 30}
    }

  *where* :lkql:`Lines` *will be the parameter name and* :lkql:`30` *the value*

* To run this, we now pass in the rules file to :toolname:`GNATcheck`

  :command:`gnatcheck -P default.gpr --rules-dir . --rule-file=rules_file.lkql`

.. code:: error

  loggers.adb:1:01: rule violation: unit flagged
  loggers.ads:1:01: rule violation: unit flagged

-----------------------------------
Step 3 - Checking Each Subprogram
-----------------------------------

.. container:: animate 1-

  * As written, it is the unit itself that is being flagged

    * We want to flag subprograms

  * Update the rule to select subprograms from the unit

    * Update the message to include the subprogram name in the message

.. container:: animate 2-

  .. code:: lkql

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 1000) =
        [
            {message: "subprogram body flagged: " & subp.p_defining_name().text,
             loc: subp}
            for subp in from unit.root select SubpBody
        ]

  * Run :toolname:`GNATcheck` to see the new results

.. container:: animate 3-

  .. code:: error
    :font-size: scriptsize

    loggers.adb:9:04: rule violation: subprogram body flagged: Severity_To_String
    loggers.adb:31:04: rule violation: subprogram body flagged: Log_Message
    loggers.adb:93:04: rule violation: subprogram body flagged: Enable_Color_Output
    loggers.adb:135:04: rule violation: subprogram body flagged: Should_Log
    loggers.adb:158:04: rule violation: subprogram body flagged: Create_Filter

  **Note**

    .. raw:: latex

      \vspace{2mm}

    *List comprehension means we now get an element for every subprogram body we
    find in the unit.*

--------------------------------------------
Step 4 - Finding the Subprogram Statements
--------------------------------------------

.. container:: animate 1-

  * We now need to get the statements in the subprogram

  * Add a selector to iterate over all statements (:lkql:`HandledStmts`) whose parent is :lkql:`SubpBody`

  **Note**

    .. raw:: latex

      \vspace{2mm}

    *There is no easy way for the message to get the subprogram name from the statement list,
    so just remove that part of the* :lkql:`message` *argument*

.. container:: animate 2-

  .. code:: lkql

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 1000) =
        [
            {message: "subprogram body flagged: ",
             loc: subp}
            for subp in from unit.root
                select HandledStmts(parent: SubpBody)
    ]

  * Run :toolname:`GNATcheck` to see the new results (they're different!)

.. container:: animate 3-

  .. code:: error
    :font-size: small

    loggers.adb:12:09: rule violation: subprogram body flagged:
    loggers.adb:35:09: rule violation: subprogram body flagged:
    loggers.adb:96:09: rule violation: subprogram body flagged:
    loggers.adb:139:09: rule violation: subprogram body flagged:
    loggers.adb:161:09: rule violation: subprogram body flagged:

  * The line numbers are now tracking to the :ada:`begin` rather than subprogram declaration

--------------------------------------------
Step 5 - Counting the Number of Statements
--------------------------------------------

.. container:: animate 1-

  * We need a way to count how many lines are in the subprogram

    * Given we have a list of statements

  * Simplest calculation

    * Difference between line of first token on first line and last token on last line

  * Hints:

    * Need to give a name to the object returned by the :lkql:`select`
    * :lqkl:`AdaNode` has properties :lkql:`token_start` and :ada:`token_end`
    * :lkql:`Token` has fields :lkql:`end_line` and :lkql:`start_line`

.. container:: animate 2-

  .. code:: lkql

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 1000) =
        [
            {message: "subprogram body flagged: ",
             loc: subp}
            for subp in from unit.root
                select stmts@HandledStmts(parent: SubpBody)
                    when lines < stmts.token_end().end_line -
                                 stmts.token_start().start_line + 1
        ]

  * Run :toolname:`GNATcheck` to see the "correct" results

.. container:: animate 3-

  .. code:: error
    :font-size: small

    loggers.adb:35:09: rule violation: subprogram body flagged:
    loggers.adb:161:09: rule violation: subprogram body flagged:

-----------------------------------------
Step 6 - Using Functions for Modularity
-----------------------------------------

.. container:: animate 1-

  * Rather than doing the calculation in-line, create a function

    * Hint: function parameter will be :lkql:`HandledStmts`

.. container:: animate 2-

  .. code:: lkql

    fun count_lines(stmts) =
        stmts.token_end().end_line - stmts.token_start().start_line + 1

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 1000) =
        [
            {message: "subprogram body flagged: ",
             loc: subp}
            for subp in from unit.root
                select stmts@HandledStmts(parent: SubpBody)
                    when lines < count_lines(stmts)
        ]

  * Run :toolname:`GNATcheck` and verify messages haven't changed

--------------------------------------------
Step 6 - Updating the Message for Clarity
--------------------------------------------

.. container:: animate 1-

  * Now update the message to be more descriptive

    * Show number of lines in failing subprogram
    * Use start of statement list for error message

  * Hints

    * The name of the object in the message is the iterator (:lkql:`subp`)
    * But the **type** of the operator is specified by :lkql:`select`

.. container:: animate 2-

  .. code:: lkql

    fun count_lines(stmts) =
        stmts.token_end().end_line - stmts.token_start().start_line + 1

    @unit_check
    fun flag_large_subprograms(unit, lines: int = 1000) =
        [
            {message: "too many lines in subprogram body: " & img(count_lines(subp)),
             loc: subp.token_start()}
            for subp in from unit.root
                select stmts@HandledStmts(parent: SubpBody)
                    when lines < count_lines(stmts)
        ]

  *With the* :lkql:`count_lines` *function, we have access to the calculation*
  *in multiple places.*

  * Run :toolname:`GNATcheck` one last time!

.. container:: animate 3-

  .. code:: error
    :font-size: scriptsize

    loggers.adb:36:07: rule violation: too many lines in subprogram body: 51
    loggers.adb:162:07: rule violation: too many lines in subprogram body: 32
>>>>>>> 1761e6ce (First cut at intermediate LKQL lab)
