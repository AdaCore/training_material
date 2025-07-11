type A_F is access function (I : Integer) return Integer
   with Post => A_F'Result > I;
