************************
GNATfuzz Demonstration
************************

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

========================
Typical Usage Scenario
========================

----------------------
Four Operating Modes
----------------------

analyze
   Identify fuzzable subprograms within system under test

generate
   Create test harness including all execution scripts

corpus-gen
   Automatically generate a starting corpus

fuzz
   Build system under test including support for :toolname:`GNATcover`

Only :toolname:`generate` and :toolname:`fuzz` are required

---------------
Typical Usage
---------------

1. :command:`gnatfuzz analyze` passing in a GPR file

   * Generates list of all subprograms capable of fuzz testing
   * Generates list of all subprograms not capable of fuzz testing and reasons why

2. :command:`gnatfuzz generate` to generate harness

   * Typical input is ``analyze`` output file and subprogram id
   * Can also pass path of Ada spec and subprogram line number

3. :command:`gnatfuzz corpus-gen` generates "interesting" test values

   * If not run, next step generates its own corpus using random data

4. :command:`gnatfuzz fuzz` to execute harness with fuzzed data

-------------------
Command Line Help
-------------------

.. container:: latex_environment Large

   :command:`gnatfuzz --help` - list all actions available

::

   gnatfuzz ACTION [OPTIONS ...]

   ACTION is one of:

      * analyze
          (analyze a GPR project file and find all fuzzable closures)
      * generate
          (generate a fuzz test harness for a package subprogram)
      * corpus-gen
          (creates the starting corpus used to seed the fuzzer mutator)
      * fuzz
          (build, run, monitor and stop a fuzzing campaign)
      --version
          (display GNATfuzz version information)
      --help
          (display this help message)

.. container:: latex_environment Large

   :command:`gnatfuzz <action> --help` - get help for **action**

::

   usage: gnatfuzz corpus-gen [--help|-h] [-P <PROJECT>] [-o <OUTPUT-DIRECTORY>]
                             [-X <NAME=VALUE>] [--disable-styled-output]

   GNATfuzz create_starting_corpus [options]

   positional arguments:

   optional arguments:
      --help, -h               Show this help message
      -P                       Use project file "PROJECT"
      -o                       Path to store the generated starting corpus
      -X                       Define a scenario variable for project files
      --disable-styled-output  Prevent GNATfuzz from outputing any text styled
                               using ANSI escape sequences

==============================
Step 1 - Analyze Source Code
==============================

---------------------------------------
Source Code Example - Mach Calculator
---------------------------------------

* Simple package that converts air speed and air temperature into speed relative to speed of sound 

* Code will

   1. Take input in form of a record containing values (:ada:`Air_Temperature` and :ada:`Air_Speed`) and value units (:ada:`Temperature_Unit` and :ada:`Speed_Unit`)
   2. Calculate speed of sound
   3. Return ratio of air speed to speed of sound (*Mach value*)

**NOTE** *There are bugs in this code to help show benefits of fuzz testing!*

   * Bounds checking
   * Divide by zero

---------------------
Source Code Example
---------------------

.. code:: Ada

   package Mach_Calculator is
      subtype Temperature_Type is Float range -2_000.0 .. 2_000.0;
      type Temperature_Unit_Type is (Faherheit, Celsius, Kelvin, Rankine);
      subtype Speed_Type is Float range 0.0 .. 3_000.0;
      type Speed_Unit_Type is
        (Miles_Per_Hour, Kilometers_Per_Hour, Knots, Meters_Per_Second,
         Feet_Per_Second);
      type Sensor_Data_Type is record
         Air_Temperature  : Temperature_Type;
         Temperature_Unit : Temperature_Unit_Type;
         Air_Speed        : Speed_Type;
         Speed_Unit       : Speed_Unit_Type;
      end record;
      subtype Mach_Type is Float range 0.0 .. 20.0;
      function Get_Mach_Value (Sensor_Data : Sensor_Data_Type) return Mach_Type;
   private
      function Faherheit_To_Kelvin
        (Faherheit_Value : Temperature_Type) return Temperature_Type;
      function Celsius_To_Kelvin
        (Celsius_Value : Temperature_Type) return Temperature_Type;
      function Rankin_To_Kelvin
        (Rankin_Value : Temperature_Type) return Temperature_Type;
      function Knots_To_Miles_Per_Hour
        (Knots_Value : Speed_Type) return Speed_Type;
      function Knots_To_Meters_Per_Second
        (Knots_Value : Speed_Type) return Speed_Type;
      function Knots_To_Feet_Per_Second
        (Knots_Value : Speed_Type) return Speed_Type;
      function Knots_To_Kilometers_Per_Hour
        (Knots_Value : Speed_Type) return Speed_Type;
   end Mach_Calculator;

------------------
Perform Analysis
------------------

``gnatfuzz analyze -P mach_calculator.gpr``

* Command response

   ``INFO: analyze results:<path-to>/analyze.json``

* JSON file (partial)

   .. code:: json

      {
        "fuzzable_subprograms": [
          {
            "corpus_gen_supported": true,
            "id": 1,
            "label": "Get_Mach_Value",
            "source_filename": "/demo/mach_calculator.ads",
            "start_line": 56
          },
          {
            "corpus_gen_supported": true,
            "id": 2,
            "label": "Faherheit_To_Kelvin",
            "source_filename": "/demo/mach_calculator.ads",
            "start_line": 64
          },

------------------------
Analysis File Contents
------------------------

   * All subprograms in spec (public and private) can be fuzzed

      * Cannot fuzz subprograms declared in body

   * Fields

      **corpus_gen_supported**

         True if corpus can be auto-generated

      **id**

         Subprogram ID for passing to other operations

      **label**

         Subprogram name

      **source_filename**

         File containing subprogram definition

      **start_line**

         Starting line of subprogram definition

===========================
Step 2 - Generate Harness
===========================

---------------------
Select a Subprogram
---------------------

* In our example, we want to test :ada:`Get_Mach_Value`

   * Our main concern
   * It calls the others anyways

::

   gnatfuzz generate -P mach_calculator.gpr
      --analysis <path-to>/analyze.json
      --subprogram-id 1
      -o generated_output

* Generates the response

::

   INFO: Building GNATfuzz run-time.....INFO: Done
   INFO: The generated files are placed under
      <path-to>/generated_output/

* :filename:`generated_output/fuzz_testing` folder contains GPR file for running fuzz testing

===============================================
Step 3 - Run Fuzzer Without Generating Corpus
===============================================

-------------------
Start the Testing
-------------------

* To just start testing without generating the "interesting data"

:: 

   gnatfuzz fuzz -P generated_output/fuzz_testing/fuzz_test.gpr

**BOOM**

