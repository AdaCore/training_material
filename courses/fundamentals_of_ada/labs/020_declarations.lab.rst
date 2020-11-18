===
Lab
===

-----------------
Declarations Labs
-----------------

Declare 

* `Target_Name`, a `String` equal to "John Connor"
* `Target_Acquired` a `Boolean` set to True
* `Active_Processors`, a `Positive` with the value 1
* `Altitude_Meters` a `Float` equal to 2
* `Body_Weigth_Kg`, a constant equal to 2.5 x 10^3
* `Magazines_Left`, equal to 10
* `State_Register`, with the binary value 1 0101 1010

In the main procedure:

* Set `Active_Processors` to 0

    - See how it warns at compilation and fails at runtime. Remove it.

* Add a `declare` block
    
    - declare `Target_Name` set to "Sarah Conor"
    - declare `All_Targets` with the concatenation of both `Target_Name`

        - `&` is the Array concatenation operator

-----------------------
Declarations Lab Hints
-----------------------

* Comment your code
* Use `Mixed_Casing`
* Use thousands separators
* Explicit is better than implicit
* Float and integer literals are not the same
* Well used typing helps expressivity

    - `Natural` for counting things
    - Named numbers `constant` for perfect precision
    - Indexing is mostly using `Positive`
    - Strict typing is a pillar of the language

----------------------------------------
Declarations Lab Solution (Definitions)
----------------------------------------

.. code:: Ada

    procedure Main is
       Target_Name : String := "John Connor";
       Target_Acquired : Boolean := True;
       Active_Processors : Positive := 1;
       Altitude_Meters : Float := 2.0;
       Body_Weigth_Kg : constant := 2.5E+3;
       Magazines_Left : Natural := 10;
       State_Register : Natural := 2#1_0101_1010#;
    begin
       New_Target : declare
          Target_Name : String := "Sarah Connor";
          All_Targets : String
            := New_Target.Target_Name & " " & Main.Target_Name;
       begin null;
       end New_Target;
    end Main;
