with Radar_Internals;

package Devices.Radars is

    type Event_T is record
        Object : Radar_Internals.Object_Type_T;
        Status : Radar_Internals.Object_Status_T;
    end record;

    protected Radar is
        entry Wait_Event (Event : out Event_T);
        procedure Ping (Object : Radar_Internals.Object_Type_T);
        procedure Mark (Object : Radar_Internals.Object_Type_T);
        procedure Lost (Object : Radar_Internals.Object_Type_T);
        procedure Rotate;
        function Angle return Radar_Internals.Angle_Degrees_T;
    private
        New_Event : Boolean := False;
        Event : Event_T;
        Rotation : Radar_Internals.Angle_Degrees_T;
    end Radar;

    task Radar_Detect is
    end Radar_Detect;

    task Radar_Rotate is
    end Radar_Rotate;

end Devices.Radars;
