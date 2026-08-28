=====
Lab
=====

-----------------
SPARK Tools Lab
-----------------

- Find the :filename:`spark_tools` directory in  the :filename:`labs` folder

   + You can copy it locally, or work with it in-place

- In :filename:`prompt` folder, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

   + Or, on the command-line, do :command:`gnatstudio -P default.gpr`

- Unfold the source code directory (.) in the project pane

-------------------------------
Upgrading to SPARK Mode (1/2)
-------------------------------

.. container:: animate 1-

  * Examine the linear search algorithm in :filename:`basics.adb`
  * Compile the project

    :command:`gprbuild -P default.gpr`

    **OR**

    :menu:`Build` |rightarrow| :menu:`Compile File`

    * Legal Ada, so the project compiles fine

  * Add :ada:`with SPARK_Mode` to the package spec and body

.. container:: animate 2-

  .. code:: Ada

    package Basics with
       SPARK_Mode
    is

    package body Basics with
       SPARK_Mode
    is

  * Compile the updated project

.. container:: animate 3-

  .. code:: error

    basics.ads:11:7: error: function cannot have parameter of
       mode "out" or "in out" in SPARK [E0015]
    basics.ads:11:7: error: launch "gnatprove --explain=E0015"
       for more information

-------------------------------
Upgrading to SPARK Mode (2/2)
-------------------------------

* We have two outputs (:ada:`At_Index` and a success flag)

  * We choose to convert our function to a procedure

* Replace :ada:`function Search` with :ada:`procedure Search`

  * Update the code accordingly (and make sure it compiles!)

.. container:: animate 2-

  .. code:: Ada
    :font-size: scriptsize

    procedure Search
      (The_Array :     Arr;
       Val       :     Element;
       At_Index  : out Index;
       Status    : out Boolean) is
       Pos : Index := A'First;
    begin
       while Pos < The_Array'Last loop
          if The_Array (Pos) = Val then
             At_Index := Pos;
             Status   := True;
             return;
          end if;
          Pos := Pos + 1;
       end loop;
       Status := False;
   end Search;

.. container:: animate 3-

  *To see how to keep* :ada:`Search` *as a function*, *refer to the* **SPARK Tutorial**

  :url:`https://docs.adacore.com/live/wave/spark2014/html/spark2014_ug/en/tutorial.html`

--------------------------
Performing Flow Analysis
--------------------------

.. container:: animate 1-

  * Before trying to prove the code, check data flow

    :command:`gnatprove -Pdefault.gpr --mode=flow`

    **OR**

    :menu:`SPARK` |rightarrow| :menu:`Examine All`

    Was that successful?

.. container:: animate 2-

  .. code:: error

    medium: "At_Index" might not be initialized in "Search"
   --> basics.ads:13:07
      13 |          At_Index : out Index;
         |          ^~~~~~~~
         + reason for check: OUT parameter should be initialized on return
         + possible fix: initialize "At_Index" on all paths or make
              "At_Index" an IN OUT parameter

  * Cannot require caller to check :ada:`Status`

    * :ada:`At_Index` must always be initialized

  * Update the code and check the data flow again

------------------------------
Proving the Code Works (1/2)
------------------------------

.. container:: animate 1-

  * Try proving if the code works

    :command:`gnatprove -Pdefault.gpr`

    **OR**

    :menu:`SPARK` |rightarrow| :menu:`Prove All`

    Was that successful?

.. container:: animate 2-

  * Yes - but that's because there are no defined postconditions

    * If a procedure has no postconditions, implied postcondition is **True**

------------------------------
Proving the Code Works (2/2)
------------------------------

.. container:: animate 1-

  * Add postconditions such that

    * If :ada:`Val` is in :ada:`The_Array` then

      * :ada:`At_Index` should be the index of :ada:`Val` in :ada:`The_Array`
      * :ada:`Status` should be :ada:`True`

    * Otherwise

      * :ada:`Status` should be :ada:`False`

.. container:: animate 2-

  .. code:: Ada

    procedure Search
      (The_Array :     Arr;
       Val       :     Element;
       At_Index  : out Index;
       Status    : out Boolean) with
      Post => (not Status)
              or else (The_Array (At_Index) = Val);

--------------
Extra Credit
--------------

.. container:: latex_environment LARGE

  Now that the code proves correctly, use :ada:`Test_Program` to confirm it!

.. container:: animate 2-

  .. code:: Ada
    :font-size: footnotesize

    with Basics;      use Basics;
    with Ada.Text_IO; use Ada.Text_IO;
    procedure Test_Program is
       The_Array : constant Arr := (10, 1, 9, 2, 8, 3, 7, 4, 6, 5);

       procedure Run_One (Val : Element) is
          At_Index : Index;
          Status   : Boolean;
       begin
          Search (The_Array, Val, At_Index, Status);
          if Status then
             Put_Line ("Found" & Val'Image & " at" & At_Index'Image);
          else
             Put_Line ("Did not find" & Val'Image);
          end if;
       end Run_One;

    begin

       Run_One (1);
       Run_One (3);
       Run_One (10);
       Run_One (-1);
       Run_One (11);

    end Test_Program;
