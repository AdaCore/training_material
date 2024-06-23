**************************************
Reviewing Results and Improving Code
**************************************

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

====================
Reviewing Messages
====================

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

-----------------------------
Documenting Review Comments
-----------------------------

* :toolname:`GNAT SAS` generates many messages

  * Sometimes the code is OK as-is
  * Sometimes we might want to say we'll worry about it later

* Two methods of documenting human response to the message

  * Interactive review via

    * :toolname:`GNAT Studio`
    * CSV import

  * Code-based review via :ada:`pragma Annotate`

* Benefits of interactive review

  * No source code modification
  * Can be performed by non-Ada reviewers
  * Additional review statuses available

* Benefits of code-based review

  * Review appears with source code
  * Review less likely to be affected by other source changes
  * Editing/Source code control can be used to manage review

----------------
Review Actions
----------------

* Left-click pencil icon in *Locations* window to get review choices

  .. image:: gnatsas/sas_review_actions.png
    :width: 100%

---------------
Manual Review
---------------

* **Manual review** brings up dialog to add review comments

  .. image:: gnatsas/sas_manual_review.png
    :width: 40%

* **Annotate** inserts :ada:`pragma Annotate` after source code

  * Reviewer updates *<insert review>* text

    .. code:: Ada

      pragma Annotate
        (CodePeer, False_Positive, "array index check", "<insert review>");

-------------------------
Default Review Statuses
-------------------------

* :toolname:`GNAT SAS` groups statuses into three categories

  * **Pending**
  * **Not a bug**
  * **Bug**
  * *By default,* :toolname:`GNAT Studio` *does not show messages in category* **Not a bug**

* :toolname:`GNAT SAS` predefines the following review statuses

  * ``Uncategorized``
  * ``Pending``
  * ``Not a bug``
  * ``Bug``
  * ``False positive``
  * ``Intentional``
  * *Note that* ``False positive`` *and* ``Intentional`` *fall into the* **Not a bug** *category*

* For :ada:`pragma Annotate`, only ``False_Positive`` and ``Intentional`` are allowed

------------------------
Custom Review Statuses
------------------------

It is possible to create your own statuses for the **Manual review** dialog

.. container:: columns

  .. container:: column

    .. container:: latex_environment tiny

      :menu:`Edit` |rightarrow| :menu:`Edit Project Properties` |rightarrow| :menu:`GNATSAS`

    .. image:: gnatsas/sas_custom_statuses.png
      :width: 80%

  .. container:: column

    .. container:: latex_environment tiny

      .. code:: Ada

        project Sdc is
          package Analyzer is
            for Pending_Status use ("Don't Know",
                                    "To do");
            for Not_A_Bug_Status use ("Don't care",
                                      "To be dealt with later");
            for Bug_Status use ("Problem",
                                "To be fixed ASAP");
          end Analyzer;

Resulting in an updated **Manual review** dialog

  .. image:: gnatsas/sas_manual_review_custom.png
    :width: 20%

==================================
Code Annotations via GNAT Studio
==================================

--------------------------------
Understanding Code Annotations
--------------------------------

* The *Inspector* engine generates documentation for each analyzed subprogram

  * Appears as virtual comments in :toolname:`GNAT Studio` source editor
  * General reasoning behind analysis that caused message to appear

.. container:: latex_environment small

  .. list-table::

    * - ``Pre``
      - Requirements subprogram imposes on inputs

    * - ``Presumption``
      - Presumptions about results of external subprogram

    * -
      - (when code is unavailable or in separate partition)

    * - ``Post``
      - Behavior of subprogram in terms of outputs

    * - ``Unanalyzed``
      - External subprograms that are unanalyzed

    * -
      - (Participate in determination of presumptions)

    * - ``Global inputs``
      - All global objects referenced by subprogram

    * - ``Global outputs``
      - All global objects and components modified by subprogram

    * - ``New Objects``
      - List of heap-allocated objects created but not reclaimed

--------------------
Annotation Example
--------------------

  .. image:: gnatsas/sas_annotation_example.png
    :width: 100%

--------------------------------
Annotation Syntax Explanations
--------------------------------

.. container:: latex_environment tiny

  .. list-table::

    * - :ada:`--  Post:`

      - *On completion of the subprogram*

    * - :ada:`--    stack.pop'Result = Tab(Last'Old)`

      - *The return value will be the value in* :ada:`Tab` *at the location*

    * -

      - *specified by* :ada:`Last` *on entry into the subprogram*

    * - :ada:`--    stack.pop'Result /= null`

      - *The return value will not be* :ada:`null`

    * - :ada:`--    Last = Last'Old - 1`

      - :ada:`Last` *will be its value on entry minus 1*

    * - :ada:`--    Last <= 199`

      - :ada:`Last` *will be less than 200*

    * - 

    * - :ada:`--  Pre:`

      - *On entry into the subprogram*

    * - :ada:`--    V.E'Initialized`

      - :ada:`V.E` *has been initialized*

    * - :ada:`--    Tab(Last) /= null`

      - :ada:`Tab(Last)` *is not null*

    * - :ada:`--    Last in 1..200`

      - :ada:`Last` *is in range 1 .. 200*

    * - 

    * - :ada:`--  Global_outputs:`

      - *List of global objects modified*

    * - :ada:`--    Last`

    * - 

    * - :ada:`--  Global_inputs:`

      - *List of global objects read*

    * - :ada:`--    Last, Tab, Tab(1..200)`

    * - 

    * - :ada:`--  Presumption:`

      - *Presumptions about* :ada:`Image` *call in* :ada:`To_String`

    * - :ada:`--    'Image'Result@44'Last in 1..1_234`

    * - :ada:`--    'Image'Result@44'First = 1`

*For more information about annotation syntax, refer to* Inspector Annotations *chapter in* **GNAT SAS User's Guide**




