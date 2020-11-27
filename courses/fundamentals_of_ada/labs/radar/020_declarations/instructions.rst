:title: Ada Fundamentals - Lab 1 - Declarations

.. include:: support_files/docs_common.rst

The purpose of this lab is to discover the basics of the GNAT toolchain on Windows
and to put that knowledge to use by declaring some variables.

.. include:: courses/gnatstudio/getting_started_lab_1.rst
.. raw:: latex

    \clearpage

---------
Questions
---------

* Follow the instruction in the source file
* Locate and click on the "Compile & Run" button in GNAT Studio.

.. image:: GNAT_Studio/UI/Run_Button.png

---------
Labs Tips
---------

* Comment your code by using :ada:`-- Line comments`
* Ada identifiers are written in :code:`Mixed_Case`.
* Use thousand separators when possible :ada:`1_000`
* Float and integer literals are not identical
* Ada types helps expressivity
    
    - :ada:`Natural` for counting things
    - Named numbers :ada:`constant` for perfect precision
    - Indexing is mostly used through `Positive`
    - Strict typing is a **pillar** of the language

* More resource on recommended Ada style can be found at https://www.adaic.org/resources/add_content/docs/95style/html/sec_3/3-5.html
