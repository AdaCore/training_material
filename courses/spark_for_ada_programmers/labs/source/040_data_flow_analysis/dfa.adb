package body DFA
  with SPARK_Mode => On
is
   
   function F1 (X : in Integer) return Integer
   is
      Tmp : Integer;
   begin
      -- The most basic form of DFA bug - Tmp is definitely
      -- not initialized. We call this an Unconditional
      -- Data Flow Error
      return X + Tmp;
   end F1;

   function F2 (X : in Integer) return Integer
   is
      Tmp : Integer;
   begin
      case X is
	 when Integer'First .. -1 =>
	    Tmp := -1;
	 when 0 =>
	    null;
	 when 1 .. Integer'Last =>
	    Tmp := 1;
      end case;
      
      -- Slightly more subtle - Tmp _might_ not be
      -- initialized, depending on the initial value of X.
      -- We call this a Conditional Data Flow Error.
      -- Note that the error message issued here is different
      -- from that issued in F1 above.
      return X + Tmp;
   end F2;
   
end DFA;

