package Aliasing
  with SPARK_Mode => On
is
   
  type Rec is record
     F, G : Integer;
  end Record;

  procedure Multiply (X, Y : in     Rec;
                      Z    :    out Rec);

  procedure Test;
  
end Aliasing;

  
