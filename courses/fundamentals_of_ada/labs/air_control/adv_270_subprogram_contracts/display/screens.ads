with Display.Basic;
with Planes;

package Screens is
    type Screen is tagged private;

    function Init return Screen;
    procedure Update (S : Screen; P : Planes.Plane);

private
    type Screen is record
        Window : Display.Basic.Window_ID;
    end record;
end package;
