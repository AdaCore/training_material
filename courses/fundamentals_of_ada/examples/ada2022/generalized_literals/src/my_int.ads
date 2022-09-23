package My_Int is
    type My_Int_T is private
        with Integer_Literal => Make_0;

    function Make_0 (S : String) return My_Int_T;
    function Image (M : My_Int_T) return String;

private

    type My_Int_T is record
        I : Integer;
    end record;

    function Make_0 (S : String) return My_Int_T is ((I => 0));
    function Image (M : My_Int_T) return String is
        (Integer'Image (M.I));

end My_Int;

