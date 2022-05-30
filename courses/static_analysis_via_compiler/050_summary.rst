*********
Summary
*********

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

======================
GNAT versus CodePeer
======================

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

..
    Miscellaneous symbols

.. |checkmark| replace:: :math:`\checkmark`

----------
CodePeer
----------

+ A static analyzer

  + Provides deep analysis prior to execution and test

+ Helps identify vulnerabilities and bugs

  + Better than the compiler
  + Better than a human!

+ Is modular and scalable

  + Can be used on an entire project or a single file
  + Can be configured to be more or less strict

+ Is flexible

  + Usable with all Ada language variants
  + Usable with other vendors' compilers

--------------------------------
Why Not Just Use the Compiler?
--------------------------------

+ The compiler does generate useful warnings

  + But :toolname:`CodePeer` far exceeds the compiler's analyses

+ :toolname:`CodePeer`

  + Does much more thorough job
  + Finds problems compiler doesn't look for

------------------------------
How Does GNAT Analysis Work?
------------------------------

+ Intraprocedural

  + Ignores interactions between caller and called subprograms

+ Flow-sensitive but path- and context-insensitive

  + Recognizes order of statements
  + Ignores effects of conditional statements
  + Ignores calling context

+ Low-noise
+ Very useful, but not complete

--------------
Flow Tracing
--------------

.. code:: Ada
   :number-lines: 1

   function Example (K : Integer) return Integer is 
      A, B, C, D : Integer;
   begin
      C := A;
      if K > 4 then
         B := 3;
      end if;
      D := B;
      return D;
   end Example;

+ Compiler results:

  ::

    example.adb:2:04: warning: variable "A" is read but never assigned [-gnatwv]

+ :toolname:`CodePeer` results

  ::

    example.adb:4:9: high: validity check: A is uninitialized here
    example.adb:8:9: medium: validity check: B might be uninitialized

---------------
Value Tracing
---------------

.. code:: Ada
   :number-lines: 1

   function Example (K : Integer) return Integer is
      A : Integer;
   begin
      A := 4;
      if A > 3 then
         A := A + 1;
      end if;
      if A > 4 then
         A := A + 1;
      end if;
      return A + K;
   end Example;

+ GNAT does only rudimentary value tracing

  + Traces constant values assigned in straight-line code with no conditions

  ::

    example.adb:5:14: warning: condition is always True

+ :toolname:`CodePeer` does full value tracing

  ::

    example.adb:5:09: warning: condition is always True 
    example.adb:8:9: medium warning: test always true because A = 5

------------------------------------------------
"Intra"procedural vs. "Inter"procedural Analysis
------------------------------------------------

.. code:: Ada
   :number-lines: 1

  function Example (K : Integer) return Integer is
     A, B, C : Integer;
     function Zero return Integer is (0);
  begin
     A := 0;
     B := K / A;
     C := B / Zero;
     return C;
  end Example;

+ GNAT only analyzes one routine at a time

  .. container:: latex_environment scriptsize

    ::

      example.adb:6:13: warning: division by zero [enabled by default]

+ :toolname:`CodePeer` does whole-program analysis

  .. container:: latex_environment scriptsize

    ::

      example.adb:6:11: high: divide by zero fails here
      example.adb:7:11: high: divide by zero fails here: requires (zero'Result) /= 0

-----------------------------------------------
CodePeer's Capabilities Beyond the Compiler's
-----------------------------------------------

+ Detecting race conditions in tasking code
+ Incremental analysis

  + Historical database preserves results of every run
  + Allows user to focus on new problems or compare against baseline
  + Only the changes need be analyzed

+ Contract-based Programming support

  + Can generate contracts automatically from the code
  + Can detect incorrect contracts (statically)
  + Can use existing contracts in further analysis

+ Others...

=========
Summary
=========

---------
Summary
---------

+ Compiler can generate a large number of useful warnings
+ Multiple warning categories supported

  + Layout and presentation
  + Sound engineering coding practices
  + Language subset definitions

+ See the docs: we did not examine every possibility
+ :toolname:`CodePeer` can do much better, and much more

  + And analysis is sound

+ You can use these facilities directly but you can also apply them via :toolname:`GNATcheck`

