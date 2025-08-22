package Random_Numbers is

   function Random (From, To : Integer) return Integer
     with Post => Random'Result in From .. To;

end Random_Numbers;
