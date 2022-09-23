package Counter is

    type Count_T is record
        A, B : Natural;
        Latest_Is_A : Boolean;
    end record;

    Count : Count_T;

    procedure Count_A with Global => (in out Count.A; out Count.Latest_Is_A);
    procedure Count_B with Global => (in out Count.B; out Count.Latest_Is_A);
    function Total return Natural with Global => (in Count.A; in Count.B);
    procedure Reset_Count with Global => (out Count.A; out Count.B);

end Counter;
