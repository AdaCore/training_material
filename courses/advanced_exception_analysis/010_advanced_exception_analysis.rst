.. role:: ada(code)
   :language: ada

*****************************
Advanced Exception Analysis
*****************************

=============
Call Chains
=============

------------
Call Chain
------------

+ A sequence of addresses corresponding to places in the code, also known as stack tracebacks
+ GNAT provides access to call chains

  + Associated with exceptions
  + At arbitrary point in code
  + No runtime performance penalty when enabled

+ Can be symbolized (converted to SLOC positions)

  + At runtime
  + Off-line

+ Subject to platform and runtime limitations

  + Some features may not be available...

--------------------------------------
Call Chains on Exception Occurrences
--------------------------------------

+ Must build the program with :command:`-E` binder switch

  + Causes tracebacks to be stored in exception occurrences
  + Using :command:`-Es` instead will output symbolic backtraces

+ Will be printed on an unhandled exception
+ Package :ada:`Ada.Exceptions.Traceback` can be used to see information within Ada program
+ Function :ada:`Ada.Exceptions.Exception_Information` returns a string that includes traceback

--------------------------------------
Call Chains on Exceptions: Example 1
--------------------------------------

.. columns:: 

  .. column::

    .. container:: latex_environment tiny

      *Example Code*

      .. code:: Ada

         with Ada.Exceptions.Traceback;
         use  Ada.Exceptions.Traceback;
         with GNAT.Debug_Utilities;
         with Text_IO; use Text_IO;

         procedure Raise_CE is
            procedure Inner_Raise is
            begin
               raise Constraint_Error;
            end;
         begin
            Inner_Raise;
         exception
            when E : others =>
               declare
                  A : Tracebacks_Array := Tracebacks (E);
               begin
                  for Addr of A loop
                     Put (GNAT.Debug_Utilities.Image
                           (Addr) & " ");
                  end loop;
                  raise;
               end;
         end;

  .. column::

    .. container:: latex_environment tiny

      *Sample Execution*

      :command:`gnatmake raise_ce -bargs -E`

      ``gcc -c raise_ce.adb``

      ``gnatbind -E -x raise_ce.ali``

      ``gnatlink raise_ce.ali``

      :command:`./raise_ce`

      ``16#0040_179C# 16#0040_17B2# 16#0040_176B# 16#0040_10B9# 16#0040_12A6# 16#7625_3398# 16#76F5_9EF0# 16#76F5_9EC3# ``

      ``Execution terminated by unhandled exception``

      ``Exception name: CONSTRAINT_ERROR``

      ``Message: raise_ce.adb:8 explicit raise``

      ``Call stack traceback locations:``

      ``0x40179c 0x4017b2 0x40176b 0x4010b9 0x4012a6 0x76253398 0x76f59ef0 0x76f59ec3``

.. container:: speakernote

   Note the reraise causing the unhandled exception to ultimately output a second message.

--------------------------------------
Call Chains on Exceptions: Example 2
--------------------------------------

.. columns:: 

  .. column::

    .. container:: latex_environment tiny

      *Example Code*

      .. code:: Ada

         with Ada.Exceptions; use Ada.Exceptions;
         with Ada.Text_IO;    use Ada.Text_IO;

         procedure Exceptions is

            procedure Recurse (Count : Integer) is
            begin
               if Count <= 0 then
                  raise Constraint_Error;
               else
                  Recurse (Count - 1);
               end if;
            end Recurse;

         begin
            Recurse (10);
         exception
            when E : others =>
               Put_Line (Exception_Information (E));
            raise;
         end Exceptions;

  .. column::

    .. container:: latex_environment tiny

      *Sample Execution*

      :command:`gnatmake exceptions.adb -bargs -E`

      ``gcc -c exceptions.adb``

      ``gnatbind -E -x exceptions.ali``

      ``gnatlink exceptions.ali``

      :command:`./exceptions`

      ``raised CONSTRAINT_ERROR : exceptions.adb:9 explicit raise``

      ``Call stack traceback locations:``

      ``0x402e47 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e9d 0x402ddf 0x7f9504003b43 0x402945 0xfffffffffffffffe``

      ``Execution of ./exceptions terminated by unhandled exception``

      ``raised CONSTRAINT_ERROR : exceptions.adb:9 explicit raise``

      ``Call stack traceback locations:``

      ``0x402e47 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e6f 0x402e9b 0x402ddf 0x7fbe5ada6b43 0x402945 0xfffffffffffffffe``

--------------------------------------
Call Chains on Exceptions: Example 3
--------------------------------------

.. columns:: 

  .. column::

    .. container:: latex_environment tiny

      *Example Code*

      .. code:: Ada

         with Ada.Exceptions; use Ada.Exceptions;
         with Ada.Text_IO;    use Ada.Text_IO;

         procedure Exceptions is

            procedure Recurse (Count : Integer) is
            begin
               if Count <= 0 then
                  raise Constraint_Error;
               else
                  Recurse (Count - 1);
               end if;
            end Recurse;

         begin
            Recurse (10);
         exception
            when E : others =>
               Put_Line (Exception_Information (E));
            raise;
         end Exceptions;

  .. column::

    .. container:: latex_environment tiny

      *Sample Execution*

      :command:`gnatmake -f exceptions.adb -bargs -Es`

      ``gcc -c exceptions.adb``

      ``gnatbind -Es -x exceptions.ali``

      ``gnatlink exceptions.ali``

      :command:`./exceptions`

      ``raised CONSTRAINT_ERROR : exceptions.adb:9 explicit raise``

      ``[./exceptions]``

      ``0x402e51 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402ea5 _ada_exceptions at ???``

      ``0x402de9 main at ???``

      ``[/lib/x86_64-linux-gnu/libc.so.6]``

      ``0x7f13579b9b43``

      ``[./exceptions]``

      ``0x402945 at ???``

      ``0xfffffffffffffffe``

      ``raised CONSTRAINT_ERROR : exceptions.adb:9 explicit raise``

      ``[./exceptions]``

      ``0x402e51 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402e79 exceptions__recurse.4201 at ???``

      ``0x402ea5 _ada_exceptions at ???``

      ``0x402de9 main at ???``

      ``[/lib/x86_64-linux-gnu/libc.so.6]``

      ``0x7f13579b9b43``

      ``[./exceptions]``

      ``0x402945 at ???``

      ``0xfffffffffffffffe``

============
Tracebacks
============

-------------------------------------------------
Call Chains on Arbitrary Locations in a Program
-------------------------------------------------

+ Package :ada:`GNAT.Traceback`

  + Returns call chain rooted at the point of call

.. container:: latex_environment scriptsize

   .. code:: Ada

      package GNAT.Traceback is
         ...
         procedure Call_Chain (Traceback : out Tracebacks_Array;
                               Len : out Natural);
         ...
      end GNAT.Traceback;

----------------------------
Symbolizing Tracebacks (1)
----------------------------

+ Package :ada:`GNAT.Traceback.Symbolic`

  + Returns symbolic representation of a given call chain

.. container:: latex_environment scriptsize

   .. code:: Ada

      package GNAT.Traceback.Symbolic is
         ...
         function Symbolic_Traceback (T : Tracebacks_Array) return String;
         function Symbolic_Traceback (E : Exception_Occurrence) return String;
         ...
      end GNAT.Traceback;

+ Some important limitations

  + Native only
  + No shared libraries support
  + Application must be compiled *and deployed* with :command:`-g`

----------------------------
Symbolizing Tracebacks (2)
----------------------------

+ Alternative method: save non-symbolic tracebacks and symbolize later
+ Deployed application doesn't need to have debug info

  + Compiled with :command:`-g` for analysis and without :command:`-g` for deployment (or, stripped of debug info for deployment)
  + Limitation: analyzed executable must match deployed executable

+ Tracebacks displayed with :toolname:`addr2line` tool

   :command:`addr2line -e ./raise_ce.exe 0x40179c 0x4017b2 0x40176b` *(as reported at run time)*

   ``.../raise_ce.adb:8``

   ``.../raise_ce.adb:11``

   ``.../b~raise_ce.adb:256``

----------------------------
Symbolizing Tracebacks (3)
----------------------------

+ :toolname:`vxaddr2line` :math:`\rightarrow` for VxWorks ports

  + On :toolname:`VxWorks`, the addresses used in the module are different from the ones used within the target memory address space
  + Need to also provide address of :ada:`adainit` to compute displacements

  + Execution

    :command:`-> ld < ce`

    ``Loading /ce |``

    ``value = 591824 = 0x907d0``

    :command:`-> sp ce`

    ``task spawned: id = 1b8aae0, name = s2u0``

    ``value = 28879584 = 0x1b8aae0``

    ``->``

    ``Execution terminated by unhandled exception``

    ``Exception name: CONSTRAINT_ERROR Message: ce.adb:6``

    ``Call stack traceback locations: 0x3b8394 0x3b83e0 0x3b8420 0x3b8458 0x3b82f0 0x19a184``

    :command:`-> lkup "adainit"`

    ``adainit 0x003b81d0 text (ce.exe)``

    ``value = 0 = 0x0``

  + Check result

    :command:`powerpc-wrs-vxworks-vxaddr2line ce 0x003b81d0 0x3b8394 ...`

    ``000001C0 at .../ce.adb:6``

    ``0000020C at .../ce.adb:12``

    ``0000024C at .../ce.adb:18``

    ``00000284 at .../ce.adb:21``

    ``0000011C at .../b~ce.adb:88``

---------------------------------------------
Instrumenting Exceptions : Exception Traces
---------------------------------------------

+ :ada:`GNAT.Exception_Traces`

  + Automatic output of exception information to stdout
  + Can be turned on and off

    + For every raise
    + For unhandled raise

  + Can be customized with an arbitrary decorator (callback)

.. container:: latex_environment scriptsize

   .. code:: Ada

      -- Careful, this call is resource-intensive!
      Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback);
      Trace_On (Unhandled_Raise);

----------------------------------------------
Instrumenting Exceptions : Exception Actions
----------------------------------------------

+ :ada:`GNAT.Exception_Actions`

  + Even more flexible
  + Register arbitrary callbacks

    + For all exceptions
    + For a specific exception

  + Callback is called before any unwinding
  + Also contains routine to dump core

.. container:: latex_environment scriptsize

   .. code:: Ada

      Register_Id_Action (Constraint_Error'Identity, Core_Dump'Access);

