with Radar_Internals; use Radar_Internals;
with PragmARC.Randomness.KISS;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

package body Devices.Radars is

    protected body Radar is
        entry Wait_Event (Event : out Event_T)
           -- TODO: Fill guard
           when True is
        begin
            null;
        end Wait_Event;

        procedure Ping (Object : Object_Type_T) is
        begin
            null;
        end Ping;

        procedure Mark (Object : Object_Type_T) is
        begin
            null;
        end Mark;

        procedure Lost (Object : Object_Type_T) is
        begin
            null;
        end Lost;
        
        procedure Rotate is
        begin
            null;
        end Rotate;

        function Angle return Radar_Internals.Angle_Degrees_T is
            (0);
    end Radar;

    type Detect_Op is (Ping, Mark, Lost);
    package Pkg_Rng_Op is new PragmARC.Randomness.KISS (Detect_Op);
    package Pkg_Rng_Obj is new PragmARC.Randomness.KISS (Object_Type_T);

    task body Radar_Detect is
        Rng_Op : Pkg_Rng_Op.Generator;
        -- TODO : Instantiate an Rng for Object_Type_T
    begin
        Pkg_Rng_Op.Set_Seed (Rng_Op, 1);
        while True loop
            -- TODO: Send random events to the radar
            null;

            -- TODO: Add timing or otherwise blocking behaviour
        end loop;
    end Radar_Detect;

    task body Radar_Rotate is
        Wait_Time : Time := Clock;
    begin
        while True loop
            Radar.Rotate;
            Wait_Time := Wait_Time + Milliseconds (1000 / 25);
            delay until Wait_Time;
        end loop;
    end Radar_Rotate;

end Devices.Radars;
