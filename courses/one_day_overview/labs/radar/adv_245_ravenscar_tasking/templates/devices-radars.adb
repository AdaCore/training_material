with Radar_Internals; use Radar_Internals;
with PragmARC.Randomness.KISS;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO;

package body Devices.Radars is

    protected body Radar is
        entry Wait_Event (Event : out Event_T)
           --$ begin question
           -- TODO: Fill guard
           when True is
           --$ end question
           --$ line answer
           when New_Event is
        begin
            --$ line question
            null;
            --$ begin answer
            Event := Radar.Event;
            New_Event := False;
            --$ end answer
        end Wait_Event;

        procedure Ping (Object : Object_Type_T) is
        begin
            --$ line question
            null;
            --$ begin answer
            Event := (Object, Tracked);
            New_Event := True;
            --$ end answer
        end Ping;

        procedure Mark (Object : Object_Type_T) is
        begin
            --$ line question
            null;
            --$ begin answer
            if Event.Object = Object and Event.Status = Tracked then
                Event := (Object, Selected);
                New_Event := True;
            end if;
            --$ end answer
        end Mark;

        procedure Lost (Object : Object_Type_T) is
        begin
            --$ line question
            null;
            --$ begin answer
            if Event.Object = Object and Event.Status /= Out_Of_Range then
                Event := (Object, Out_Of_Range);
                New_Event := True;
            end if;
            --$ end answer
        end Lost;
        
        procedure Rotate is
        begin
            --$ line question
            null;
            --$ line answer
            Rotation := Rotation + 10;
        end Rotate;

        function Angle return Radar_Internals.Angle_Degrees_T is
            --$ line question
            (0);
            --$ line answer
            (Rotation);
    end Radar;

    type Detect_Op is (Ping, Mark, Lost);
    package Pkg_Rng_Op is new PragmARC.Randomness.KISS (Detect_Op);
    package Pkg_Rng_Obj is new PragmARC.Randomness.KISS (Object_Type_T);

    task body Radar_Detect is
        Rng_Op : Pkg_Rng_Op.Generator;
        --$ line question
        -- TODO : Instantiate an Rng for Object_Type_T
        --$ begin answer
        Rng_Obj : Pkg_Rng_Obj.Generator;

        Wait_Time : Time := Clock;
        --$ end answer
    begin
        Pkg_Rng_Op.Set_Seed (Rng_Op, 1);
        while True loop
            -- TODO: Send random events to the radar
            --$ line question
            null;
            --$ begin answer
            declare
                Obj : Object_Type_T := Pkg_Rng_Obj.Random (Rng_Obj);
            begin
                case Pkg_Rng_Op.Random (Rng_Op) is
                when Ping => Radar.Ping (Obj);
                when Mark => Radar.Mark (Obj);
                when Lost => Radar.Lost (Obj);
                end case;
            end;
            --$ end answer

            -- TODO: Add timing or otherwise blocking behaviour
            --$ begin answer
            Wait_Time := Wait_Time + Milliseconds (1000 / 10);
            delay until Wait_Time;
            --$ end answer
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
