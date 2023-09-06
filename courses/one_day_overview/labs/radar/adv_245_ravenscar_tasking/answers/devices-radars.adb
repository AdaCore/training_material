with Radar_Internals; use Radar_Internals;
with PragmARC.Randomness.KISS;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

package body Devices.Radars is

    protected body Radar is
        entry Wait_Event (Event : out Event_T)
           when New_Event is
        begin
            Event := Radar.Event;
            New_Event := False;
        end Wait_Event;

        procedure Ping (Object : Object_Type_T) is
        begin
            Event := (Object, Tracked);
            New_Event := True;
        end Ping;

        procedure Mark (Object : Object_Type_T) is
        begin
            if Event.Object = Object and Event.Status = Tracked then
                Event := (Object, Selected);
                New_Event := True;
            end if;
        end Mark;

        procedure Lost (Object : Object_Type_T) is
        begin
            if Event.Object = Object and Event.Status /= Out_Of_Range then
                Event := (Object, Out_Of_Range);
                New_Event := True;
            end if;
        end Lost;
        
        procedure Rotate is
        begin
            Rotation := Rotation + 10;
        end Rotate;

        function Angle return Radar_Internals.Angle_Degrees_T is
            (Rotation);
    end Radar;

    type Detect_Op is (Ping, Mark, Lost);
    package Pkg_Rng_Op is new PragmARC.Randomness.KISS (Detect_Op);
    package Pkg_Rng_Obj is new PragmARC.Randomness.KISS (Object_Type_T);

    task body Radar_Detect is
        Rng_Op : Pkg_Rng_Op.Generator;
        Rng_Obj : Pkg_Rng_Obj.Generator;

        Wait_Time : Time := Clock;
    begin
        Pkg_Rng_Op.Set_Seed (Rng_Op, 1);
        while True loop
            -- TODO: Send random events to the radar
            declare
                Obj : Object_Type_T := Pkg_Rng_Obj.Random (Rng_Obj);
            begin
                case Pkg_Rng_Op.Random (Rng_Op) is
                when Ping => Radar.Ping (Obj);
                when Mark => Radar.Mark (Obj);
                when Lost => Radar.Lost (Obj);
                end case;
            end;

            -- TODO: Add timing or otherwise blocking behaviour
            Wait_Time := Wait_Time + Milliseconds (1000 / 10);
            delay until Wait_Time;
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
