=====
Lab
=====

--------------------------
Subprogram Contracts Lab
--------------------------

- Find the :filename:`subprogram_contracts` directory in the :filename:`labs` folder

  * You can copy it locally, or work with it in-place

* In the :filename:`prompt` folder, open the project :filename:`default.gpr` in :toolname:`GNAT Studio`

  * Or, on the command-line, do :command:`gnatstudio -P default.gpr`

* Unfold the source code directory (.) in the project pane

----------------------------
Simple Subprogram Contract
----------------------------

.. container:: animate 1-

  Add a postcondition to :ada:`Add` to verify :ada:`Z`

.. container:: animate 2-

  *Hint: with a saturated add, the result is either the addition of*
  *the two values,* :ada:`Integer`First`, *or* :ada:`Integer`Last`

.. container:: animate 3-

  .. code::
    :number-lines: 12

    procedure Add
      (X, Y :     Integer;
       Z    : out Integer) with
      Post => Z in Integer'First | Integer'Last | X + Y;

  Prove the subprogram

.. container:: animate -

  .. code:: error

    math.ads:15:14: medium: postcondition might fail

  This is OK for now - the proof fails because we cannot prove
  :ada:`Saturated_Add`

--------------------------------------
More Complicated Subprogram Contract
--------------------------------------

.. container:: animate 1-

  :ada:`Saturated_Add` has three possible inputs

    * :ada:`X + Y` is an out-of-range negative number
    * :ada:`X + Y` is an out-of-range positive number
    * :ada:`X + Y` is some number

  Use :ada:`Contract_Cases` to define results for those three conditions

.. container:: animate 2-

  .. code:: ada

   function Saturated_Add
     (X, Y : Integer)
      return Integer with
     Contract_Cases =>
      ((X + Y in Integer)    => Saturated_Add'Result = X + Y,
       X + Y < Integer'First => Saturated_Add'Result = Integer'First,
       X + Y > Integer'Last  => Saturated_Add'Result = Integer'Last);

-------------------------------------
Dealing With Frame Conditions (1/3)
-------------------------------------

.. container:: animate 1-

  Add a postcondition to prove :ada:`Navigate` changed :ada:`Coord` correctly

.. container:: animate 2-

  .. code:: ada

    procedure Navigate
      (Coord    : in out Coordinate_T;
       X_Change :        Integer;
       Y_Change :        Integer) with
      Post =>
       Coord =
       (Saturated_Add (Coord.X'Old, X_Change),
        Saturated_Add (Coord.Y'Old, Y_Change));

  Prove the subprogram

.. container:: animate 3-

  .. code:: error

    math.ads:45:7: medium: postcondition might fail
    math.adb:60:1: possible fix: call at math.adb:60 should
       mention Coord (for argument Coord) in a postcondition

  :ada:`Move_Along_X` and :ada:`Move_Along_Y` do not describe the output

-------------------------------------
Dealing With Frame Conditions (2/3)
-------------------------------------

.. container:: animate 1-

  Add postconditions to the :ada:`Move_*` subprograms


.. container:: animate 2-

  .. code:: ada

    procedure Move_Along_X
      (Coord  : in out Coordinate_T;
       Change :        Integer) with
      Post => Coord.X = Saturated_Add (Coord.X'Old, Change);

    procedure Move_Along_Y
      (Coord  : in out Coordinate_T;
       Change :        Integer) with
      Post => Coord.Y = Saturated_Add (Coord.Y'Old, Change);

  Prove :ada:`Navigate` now

.. container:: animate 3-

  If you did not consider the "frame conditions" (as in the above example) you might see

  .. code:: error

    math.ads:47:7: medium: postcondition might fail

  The prover does know what happened to the other field in the :ada:`Move_*` subprograms

-------------------------------------
Dealing With Frame Conditions (3/3)
-------------------------------------

.. container:: animate 1-

  Update :ada:`Move_*` postconditions to indicate non-modified field stays constant

.. container:: animate 2-

  .. code:: ada

    procedure Move_Along_X
      (Coord  : in out Coordinate_T;
       Change :        Integer) with
      Post =>
       Coord.X = Saturated_Add (Coord.X'Old, Change) and Coord.Y = Coord.Y'Old;

    procedure Move_Along_Y
      (Coord  : in out Coordinate_T;
       Change :        Integer) with
      Post => Coord = (Coord.X'Old, Saturated_Add (Coord.Y'Old, Change));

  Note two different ways of validating the record

--------------------
Proving Exceptions
--------------------

.. container:: animate 1-

  Prove :ada:`Convert`

.. container:: animate 2-

  .. code:: error

    math.adb:74:10: medium: unexpected exception might be raised
    math.adb:80:10: medium: unexpected exception might be raised

  Exceptions are being raised by the code - need to tell it to the prover

  Add :ada:`Exceptional_Cases` aspect to the Subprogram

.. container:: animate 3-

  .. code:: ada

    procedure Convert
      (S     :     String;
       Value : out Integer) with
      Exceptional_Cases => (Invalid_String | Constraint_Error => True);
