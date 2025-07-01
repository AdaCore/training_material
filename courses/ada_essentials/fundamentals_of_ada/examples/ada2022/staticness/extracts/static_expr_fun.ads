subtype T is Integer range 0 .. 2;
function In_T (A : Integer)
   return Boolean is
   (A in T) with Static;
