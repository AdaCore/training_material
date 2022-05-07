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
      generic
         type T is tagged private;
      package G_Pkg is
         type T2 is new T with null record;
      end G_Pkg;
      package Pkg is new G_Pkg (T1);
      procedure P (O : T1) is null;
      --$ end cut

      --$ begin cut
      type T1 is tagged null record;
      generic
         type T;
      procedure G_P (O : T);
      procedure G_P (O : T) is null;
      procedure P is new G_P (T1);
      --$ end cut

      type T_Child is new T1 with null record;
      O : T_Child; 
   end Pkg;
   use Pkg;
begin
   O.P;
end Main;
