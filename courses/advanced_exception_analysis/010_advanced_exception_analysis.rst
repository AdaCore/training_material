*****************************
Advanced Exception Analysis
*****************************

.. PRELUDE:: BEGIN

.. PRELUDE:: ROLES

.. role:: ada(code)
    :language: Ada

.. role:: C(code)
    :language: C

.. role:: cpp(code)
    :language: C++

.. PRELUDE:: SYMBOLS

.. |rightarrow| replace:: :math:`\rightarrow`
.. |forall| replace:: :math:`\forall`
.. |exists| replace:: :math:`\exists`
.. |equivalent| replace:: :math:`\iff`
.. |le| replace:: :math:`\le`
.. |ge| replace:: :math:`\ge`
.. |lt| replace:: :math:`<`
.. |gt| replace:: :math:`>`
.. |checkmark| replace:: :math:`\checkmark`

.. PRELUDE:: REQUIRES

.. PRELUDE:: PROVIDES

.. PRELUDE:: END

==============
Introduction
==============

---------------------
Diagnosing Problems
---------------------

* In Ada, a problem (usually) results in an exception being raised

* Exception handlers allow us to recover from an exception

  * But not necessarily diagnose the problem

* :ada:`Ada.Exceptions` can give us some information about the exception

  * But only if the runtime or application provides it

* GNAT provides some extra capabilities

-------------
Sample Code
-------------

* Simple code example that doesn't do anything intelligent

  * Recursive calls - any one of them can cause a failure

.. code:: Ada
  :number-lines: 1

  with Ada.Command_Line; use Ada.Command_Line;
  with Ada.Text_IO;      use Ada.Text_IO;
  procedure Main is
     type Short_T is range -1_000 .. 1_000;
     Input : Short_T := Short_T'Value (Ada.Command_Line.Argument (1));
     function One (Num : Short_T) return Short_T;

     function Three (Num : Short_T) return Short_T is
       (if Num < Short_T'Last then One (Num + 300) else Num);
     function Two (Num : Short_T) return Short_T is
       (if Num < Short_T'Last then Three (Num + 200) else Num);
     function One (Num : Short_T) return Short_T is
       (if Num < Short_T'Last then Two (Num + 100) else Num);

  begin
     Put_Line (Input'Image & " => " & Short_T'Image (Three (Input)));
  end Main;

=============================
Exceptions Without Handlers
=============================

------------------------------
Typical Exception Occurrence
------------------------------

* What happens when an exception is propagated out of :ada:`main`?

.. container:: latex_environment footnotesize

  .. code:: Ada
    :number-lines: 3

    procedure Main is
       type Short_T is range -1_000 .. 1_000;
       Input : Short_T := Short_T'Value (Ada.Command_Line.Argument (1));

::

  obj\main.exe

.. container:: animate

    raised CONSTRAINT_ERROR : a-comlin.adb:61 explicit raise

  No argument on the command line caused the exception

------------
Call Chain
------------

* Sequence of addresses corresponding to code locations

  * also known as stack tracebacks

* GNAT provides access to call chains

  * Associated with exceptions
  * No runtime performance penalty when enabled

* Subject to platform and runtime limitations

  * Some features may not be available...

--------------------------------------
Call Chains on Exception Occurrences
--------------------------------------

+ Must build the program with :command:`-E` binder switch

  + Causes tracebacks to be stored in exception occurrences
  + Using :command:`-Es` instead will output symbolic backtraces

+ Will be printed on an unhandled exception

.. image:: advanced_exception_analysis/properties_dialog.jpg

--------------------------------------
Exception Occurrence with Call Chain
--------------------------------------

Using :command:`-E`

.. container:: latex_environment tiny

  ::

    obj\main.exe

    Execution of obj\main.exe terminated by unhandled exception
    raised CONSTRAINT_ERROR : a-comlin.adb:61 explicit raise
    Load address: 0x7ff76a030000
    Call stack traceback locations:
    0x7ff76a032223 0x7ff76a031737 0x7ff76a032076 0x7ff76a031423
        0x7ff76a03113b 0x7ffedfa37032 0x7ffedffc264f

Using :command:`-Es`

.. container:: latex_environment tiny

  ::

    obj\main.exe

    raised CONSTRAINT_ERROR : a-comlin.adb:61 explicit raise
    [C:\temp\advanced_exception_analysis\obj\main.exe]
    0x7ff7ece72233 ada__command_line__argument at ???
    0x7ff7ece71737 _ada_main at ???
    0x7ff7ece72082 main at ???
    0x7ff7ece71423 __tmainCRTStartup at ???
    0x7ff7ece7113b mainCRTStartup at ???
    [C:\Windows\System32\KERNEL32.DLL]
    0x7ffedfa37032
    [C:\Windows\SYSTEM32\ntdll.dll]
    0x7ffedffc264f

=======================================
Information Within Exception Handlers
=======================================

----------------
Ada.Exceptions
----------------

* :ada:`Ada.Exceptions` provides information about exception occurrence

  * :ada:`Exception_Information` provides whatever runtime has available
  * What is available depends on binder switches

.. container:: latex_environment footnotesize

  .. code:: Ada
    :number-lines: 1

    with Ada.Command_Line; use Ada.Command_Line;
    with Ada.Exceptions;   use Ada.Exceptions;
    with Ada.Text_IO;      use Ada.Text_IO;
    procedure Main_Exceptions is
    ...

  .. code:: Ada
    :number-lines: 17

    ...
      Put_Line (Input'Image & " => " & Short_T'Image (Three (Input)));
    exception
       when The_Err : others =>
          Put_Line ("FAILED: " & Exception_Information (The_Err));
    end Main_Exceptions;

---------------------------------
Available Exception Information
---------------------------------

* No binder switches

  ::

    obj\main_exceptions.exe foo
    FAILED: raised CONSTRAINT_ERROR : bad input for 'Value: "foo"

* Using :command:`-E`

  ::

    obj\main.exe foo

    FAILED: raised CONSTRAINT_ERROR : bad input for 'Value: "foo"
    Load address: 0x7ff7ad110000
    Call stack traceback locations:
    0x7ff7ad1325ad 0x7ff7ad131689 0x7ff7ad131744 0x7ff7ad11175a
        0x7ff7ad112236 0x7ff7ad111423 0x7ff7ad11113b 0x7ffedfa37032 0x7ffedffc264f

* Using :command:`-Es`

  ::

    obj\main.exe foo

    FAILED: raised CONSTRAINT_ERROR : bad input for 'Value: "foo"
    [C:\temp\advanced_exception_analysis\obj\main_exceptions.exe]
    0x7ff657b525bd ada__finalization___assign__3 at ???
    0x7ff657b51699 ada__finalization___assign__3 at ???
    0x7ff657b51754 ada__finalization___assign__3 at ???
    0x7ff657b3175a _ada_main_exceptions at ???
    0x7ff657b32242 main at ???
    0x7ff657b31423 __tmainCRTStartup at ???
    0x7ff657b3113b mainCRTStartup at ???
    [C:\Windows\System32\KERNEL32.DLL]
    0x7ffedfa37032
    [C:\Windows\SYSTEM32\ntdll.dll]
    0x7ffedffc264f

============
Tracebacks
============

--------------------------
Examining the Call Stack
--------------------------

* Need :command:`-E` (or :command:`-Es`) in binder switches for traceback information
* Need :command:`-g` in binder *and compiler* switches for symbol information

* Package :ada:`Ada.Exceptions.Traceback`

  * Subprogram :ada:`Tracebacks` takes :ada:`Exception_Occurrence` as input and returns call stack (as :ada:`Tracebacks_Array`)

* Package :ada:`GNAT.Traceback`

  * Subprogram :ada:`Call_Chain` returns :ada:`Tracebacks_Array` from current location

    * Function or procedure call

  * Does not need to be called from exception handler

* Package :ada:`GNAT.Traceback.Symbolic`

  * Subprogram :ada:`Symbolic_Traceback` returns call stack using source references (rather than addresses)
  * Overloaded functions to accept either :ada:`Exception_Occurrence` or :ada:`Tracebacks_Array`

--------------------------
Ada.Exceptions.Traceback
--------------------------

.. code:: Ada
  :number-lines: 2

    with Ada.Exceptions;           use Ada.Exceptions;
    with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
    with GNAT.Debug_Utilities;     use GNAT.Debug_Utilities;
    with Ada.Text_IO;              use Ada.Text_IO;
    procedure Main_Tracebacks is
    ...

.. code:: Ada
  :number-lines: 19

  ...
     Put_Line (Input'Image & " => " & Short_T'Image (Three (Input)));
  exception
     when The_Err : others =>
        declare
           A : Tracebacks_Array := Tracebacks (The_Err);
        begin
           Put_Line ("FAILED: " & Exception_Name (The_Err) & " at: ");
           for Addr of A
           loop
              Put_Line ("   " & GNAT.Debug_Utilities.Image (Addr) & " ");
           end loop;
        end;

* Results

  ::

    obj\main_tracebacks 30
    FAILED: CONSTRAINT_ERROR at:
       16#0000_7FF7_B576_1F47#
       16#0000_7FF7_B576_1FAD#
       16#0000_7FF7_B576_2009#
       16#0000_7FF7_B576_1F52#
       16#0000_7FF7_B576_1FAD#
       16#0000_7FF7_B576_1814#
       16#0000_7FF7_B576_259E#
       16#0000_7FF7_B576_1423#
       16#0000_7FF7_B576_113B#
       16#0000_7FFE_DFA3_7032#
       16#0000_7FFE_DFFC_264F#

----------------
GNAT.Traceback
----------------

.. code:: Ada
  :number-lines: 3

  with GNAT.Traceback;          use GNAT.Traceback;
  with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
  with Ada.Text_IO;             use Ada.Text_IO;
  procedure Main_Symbolic is
  ...

.. code:: Ada
  :number-lines: 20

  ...
  function One (Num : Short_T) return Short_T is
  begin
    Put_Line ("ONE: " &
              GNAT.Traceback.Symbolic.Symbolic_Traceback
               (GNAT.Traceback.Call_Chain (10)));
    if Num < Short_T'Last then
      return Two (Num + 100);
    else
      return Num;
    end if;
  end One;
  ...

.. code:: Ada
  :number-lines: 34

  ...
  exception
     when The_Err : others =>
        Put_Line ("FAILED: " & Exception_Name (The_Err) & " at: ");
        Put_Line (GNAT.Traceback.Symbolic.Symbolic_Traceback (The_Err));

=================
Advanced Topics
=================

-------------------------------
Controlling Exception Tracing
-------------------------------

* :ada:`GNAT.Exception_Traces`

  * Automatic output of exception information to stdout
  * Can be turned on and off

    * For every raise
    * For unhandled raise

  * Can be customized with an arbitrary decorator (callback)
  * Occurs at raise

    * In the runtime, not in user code

.. container:: latex_environment scriptsize

  .. code:: Ada

    -- Set the decorator (function that returns a string from a
    -- traceback array) that will get called when an exception occurs
    GNAT.Exception_Traces.Set_Trace_Decorator (
        GNAT.Traceback.Symbolic.Symbolic_Traceback);
    -- Turn on automatic exception information for
    -- any unhandled exception
    GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Unhandled_Raise);

* For more information, see documentation in :ada:`GNAT.Exception_Traces`

-------------------------------
Controlling Exception Actions
-------------------------------

* :ada:`GNAT.Exception_Actions`

  * Even more flexible
  * Register arbitrary callbacks

    * For all (global) exceptions
    * For a specific exception

  * Callback is called before any unwinding
  * Also contains routine to dump core

.. container:: latex_environment scriptsize

  .. code:: Ada

    -- On Constraint Error, call Core_Dump
    GNAT.Exception_Actions.Register_Id_Action (
      Constraint_Error'Identity,
      GNAT.Exception_Actions.Core_Dump'Access );

* For more information, see documentation in :ada:`GNAT.Exception_Actions`

=====
Lab
=====

.. include:: labs/010_advanced_exception_analysis.lab.rst

=========
Summary
=========

---------
Summary
---------

* Exception packages typically used during development, not deployment

  * Useful information during testing
  * Expensive overhead in practice

* Symbolic information most useful

  * But increases size of application
