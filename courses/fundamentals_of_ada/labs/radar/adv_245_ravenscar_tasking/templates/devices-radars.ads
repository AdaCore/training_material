with Radar_Internals;

package Devices.Radars is

    type Event_T is record
        Object : Radar_Internals.Object_Type_T;
        Status : Radar_Internals.Object_Status_T;
    end record;

    protected Radar is
        entry Wait_Event (Event : out Event_T);
        -- Wait for the next radar event

        procedure Ping (Object : Radar_Internals.Object_Type_T);
        -- Ping an object, indicating it has been detected

        procedure Mark (Object : Radar_Internals.Object_Type_T);
        -- Mark an object, following it more closely. Only detected objects can be marked.

        procedure Lost (Object : Radar_Internals.Object_Type_T);
        -- Indicate that the object is out of range of the radar

        procedure Rotate;
        -- Rotate the radar by an arbitrary amount

        function Angle return Radar_Internals.Angle_Degrees_T;
        -- Current radar angle
    private
        --$ line question
        -- TODO: Fill in any needed objects
        --$ begin answer
        New_Event : Boolean := False;
        Event : Event_T;
        Rotation : Radar_Internals.Angle_Degrees_T;
        --$ end answer
    end Radar;

    task Radar_Detect is
        -- This task is in charge of detecting the objects and updating the radar accordingly
    end Radar_Detect;

    task Radar_Rotate is
        -- This task is in charge of cyclically rotating the radar
    end Radar_Rotate;

end Devices.Radars;
