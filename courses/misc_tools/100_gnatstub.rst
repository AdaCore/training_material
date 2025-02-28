**********************
:toolname:`GNATstub`
**********************

.. container:: PRELUDE BEGIN

.. container:: PRELUDE ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. role:: rust(code)
    :language: Rust

.. container:: PRELUDE SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. container:: PRELUDE REQUIRES

.. container:: PRELUDE PROVIDES

.. container:: PRELUDE END

==============
Introduction
==============

---------------------
Body Stub Generator
---------------------

* Creates empty (but compilable) package/subprogram bodies
* Can use GNAT Project file

  * Configuration in package :ada:`gnatstub`

* Default behavior is to raise exception if stub is called

  * It means you did not create a "real" body

==============================
Running :toolname:`GNATstub`
==============================

------------------------------
Running :toolname:`GNATstub`
------------------------------

:command:`gnatstub [switches] {filename}`

where :filename:`{filename}` can be a package spec or body

* Package spec

  * :toolname:`GNATstub` will generate a package body containing "dummy" bodies for subprograms defined not completed in the spec

* Package body

  * For any subprogram defined as :ada:`separate` in the package body, a file will be created containing a body for the subprogram

    * Need to specify :command:`--subunits` switch

----------------------
Example Package Spec
----------------------

* Filename :filename:`example.ads` contains

   .. code:: Ada

      package Example is
         procedure Null_Procedure is null;
         procedure Needs_A_Stub;
         function Expression_Function return Integer is (1);
      end Example;

* :command:`gnatstub example.ads` will generate :filename:`example.adb`

   .. code:: Ada

      pragma Ada_2012;
      package body Example is

         ------------------
         -- Needs_A_Stub --
         ------------------

         procedure Needs_A_Stub is
         begin
            pragma Compile_Time_Warning
              (Standard.True, "Needs_A_Stub unimplemented");
            raise Program_Error with "Unimplemented procedure Needs_A_Stub";
         end Needs_A_Stub;

      end Example;

----------------------
Example Package Body
----------------------

* Filename :filename:`example.adb` contains

   .. code:: Ada

      package body Example is
         procedure Do_Something_Else;
         procedure Do_Something is separate;
         procedure Do_Something_Else is
         begin
            Do_Something;
         end Do_Something_Else;
      end Example;

* :command:`gnatstub --subunits example.adb` will generate :filename:`example-do_something.adb`

   .. code:: Ada

      pragma Ada_2012;
      separate (Example)
      procedure Do_Something is
      begin
         pragma Compile_Time_Warning (Standard.True, "Do_Something unimplemented");
         raise Program_Error with "Unimplemented procedure Do_Something";
      end Do_Something;

===============================
:toolname:`GNATstub` Switches
===============================

----------------------------------
Controlling Behavior When Called
----------------------------------

* By default, a stubbed subprogram will raise :ada:`Program_Error` when called

   * Procedures use a :ada:`raise` statement
   * Functions use a :ada:`raise` expression in a :ada:`return`

      * To prevent warnings about no return in a function

* You can disable the exception in procedures

   * Switch :command:`--no-exception`
   * Functions still need a return statement, so :ada:`raise` expression is still present

---------------------------
Formatting Comment Blocks
---------------------------

* Sometimes you use :toolname:`GNATstub` to create a shell for your implementation

   * Having the tool populate the shell with comments can be helpful

* Comment switches:

   :command:`--comment-header-sample`

      Create a file header comment block

   :command:`--comment-header-spec`

      Copy file header from spec into body

   :command:`--header-file=<filename>`

      Insert the contents of :filename:`<filename>` at the beginning of the stub body

* Default behavior is to add a comment block for each subprogram

  * Use :command:`--no-local-header` to disable this

-----------------------
Other Common Switches
-----------------------

:command:`files=<filename>`

   :filename:`<filename>` contains a list of files for which stubs will be generated

:command:`--force`

   Overwrite any existing file (without this, :toolname:`GNATstub` will flag as an error

:command:`--output-dir=<directory>`

   Put generated files in :filename:`<directory>`

:command:`max-line-length=<nnn>`

   Maximum length of line in generated body. Default is 79, maximum is 32767

.. include:: labs/100_gnatstub/lab.rst

