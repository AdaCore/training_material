package body Counter is

    procedure Count_A is
    begin
        Count.Count_A := @ + 1;
        Count.Latest_Is_A := True;
    end Count_A;
    
    procedure Count_B is
    begin
        Count.Count_B := @ + 1;
        Count.Latest_Is_B := True;
    end Count_B;

    function Total return Natural is
    begin
        return Count.Count_A + Count_B;
    end Total;

    procedure Reset_Count is
    begin
        Count.Count_A := 0;
        Count.Count_B := 0;
    end Reset_Count;

end Counter;
