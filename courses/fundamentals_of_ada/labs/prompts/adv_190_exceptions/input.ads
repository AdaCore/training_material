package Input is
   Input_Canceled : exception;
   function Get_String
     (Prompt : String)
      return String;
   function Get_Integer
     (Prompt : String)
      return Integer;
   function Get_Natural
     (Prompt : String)
      return Natural;
   function Get_Positive
     (Prompt : String)
      return Positive;
   generic
      type Enum_T is (<>);
   function Get_Enum
     (Prompt : String)
      return Enum_T;
   generic
      type Integer_T is range <>;
   function Get_Number
     (Prompt : String)
      return Integer_T;
   generic
      type Float_T is digits <>;
   function Get_Float
     (Prompt : String)
      return Float_T;
end Input;
