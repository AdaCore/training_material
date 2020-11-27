:title: Ada Fundamentals - Lab 1 - Declarations
:author: AdaCore
:titlepage: true
:titlepage-text-color: FFFFFF
:titlepage-rule-color: 360049
:titlepage-background: "images/titleBackground.pdf"
:logo: images/logo.png

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
* The code should compile after question 2
* Locate and click on the "Compile & Run" button in GNAT Studio.

.. image:: GS_Compile_And_Run_Button.png

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
