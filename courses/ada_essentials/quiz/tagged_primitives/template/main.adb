-- Which declaration(s) will make ``P`` a primitive of ``T1``?
procedure Main is
   package Pkg is
      --$ begin cut
      type T1 is tagged null record;
      procedure P (O : T1) is null;
      --$ end cut

      --$ begin cut
      type T0 is tagged null record;
      type T1 is new T0 with null record;
      type T2 is new T0 with null record;
      procedure P (O : T1) is null;
      --$ end cut

      --$ begin cut
      type T1 is tagged null record;
      Object : T1;
      procedure P (O : T1) is null;
      --$ end cut

      --$ begin cut
      package Nested is
         type T1 is tagged null record;
      end Nested;
      use Nested;
      procedure P (O : T1) is null;
      --$ end cut

      --$ begin answer
      -- A. Primitive (same scope)
      -- B. Primitive (T1 is not yet frozen)
      -- C. T1 is frozen by the object declaration
      -- D. Primitive must be declared in same scope as type
      --$ end answer

      type T_Child is new T1 with null record;
      O : T_Child; 
   end Pkg;
   use Pkg;
begin
   O.P;
end Main;
