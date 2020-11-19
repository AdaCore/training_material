===
Lab
===

----------------------------
Labs Setup (GNAT Studio) 1/2
----------------------------

For the labs it is recommended to use GNAT Studio as an IDE.

* Start GNAT Studio
* Chose "New Project" > "Default Ada project"
* Chose a new Deploy path
* Chose a name: in this case Declaration_Lab

.. container:: columns

    .. image:: ../../images/020_Declaration_Lab_GS_New_Project_1.png

    .. image:: ../../images/020_Declaration_Lab_GS_New_Project_2.png

----------------------------
Labs Setup (GNAT Studio) 2/2
----------------------------

* On the right pane, open `src/main.adb` by clicking on it
* Notice the `begin null;`: Ada procedures can not be empty

    - `null` is the No-op statement

    .. image:: ../../images/020_Declaration_Lab_GS_src_main.png

-----------------
Declarations Labs
-----------------

Declare 

* `Target_Name`, a `String` equal to "John Connor"
* `Target_Acquired` a `Boolean` set to True
* `Active_Processors`, a `Positive` with the value 1
* `Altitude_Meters` a `Float` equal to 2
* `Body_Weigth_Kg`, a named number equal to 2.5 x 10^3
* `Magazines_Left`, equal to 10
* `State_Register`, with the binary value 1 0101 1010

In the main procedure:

* Set `Active_Processors` to 0

    - See how it warns at compilation and fails at runtime. Remove it.

* Add a `declare` block named Find_John_Connor
    
    - declare a new `Active_Processors` set to 10
    - declare `All_Active_Processors` as the sum of active processors in both main and Find_John_Connor

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
       Find_John_Connor : declare
          Active_Processors : Positive := 10;
          All_Active_Processors : Positive
            := Find_John_Connor.Active_Processors + Main.Active_Processors;
       begin null;
       end New_Target;
    end Main;
