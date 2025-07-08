==============
Introduction
==============

---------------------------------------------
Subprogram Contracts and Information Hiding
---------------------------------------------

* Subprogram contracts expose variables and types

  - In preconditions with aspect :ada:`Pre`
  - In postconditions with aspect :ada:`Post`

* Variables and types mentioned directly need to be visible

* Information hiding forbids exposing variables and types

  - Global variables in the private part or body
  - Use of private types for parameters

* Solution is to use (ghost) query functions

  .. code:: ada

       type T is private;
       function Get_Int (X : T) return Integer;
       function Get_Glob return Integer;

       procedure Proc (X : in out T)
       with
         Pre  => Get_Int (X) /= Get_Glob;
         Post => Get_Int (X) = Get_Glob;
     private
       type T is ...   -- returned by Get_Int
       Glob : Integer; -- returned by Get_Glob

---------------------------------------------
Dependency Contracts and Information Hiding
---------------------------------------------

* Dependency contracts expose variables

  - In data dependencies with aspect :ada:`Global`
  - In flow dependencies with aspect :ada:`Depends`

|

* These variables need to be visible

|

* Information hiding forbids exposing variables

|

* Solution is to use :dfn:`state abstraction`

  - Names that denote one or more global variables
  - They represent all the :dfn:`hidden state` of the package

