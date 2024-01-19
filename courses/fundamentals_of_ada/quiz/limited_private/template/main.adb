-- Which of the following piece(s) of code is(are) legal?
procedure Main is
   --$ begin question
   package P is
      type Priv is private;
   private
      type Lim is limited null record;
      -- Complete Here
   --$ end question
      
      --$ begin cut
      type Priv is record
         E : Lim;
      end record;
      -- ``E`` has limited type, partial view of ``Priv`` must be :ada:`limited private`
      --$ end cut
      
      --$ begin cut
      type Priv is record
         E : Float;
      end record;
      --$ end cut
      
      --$ begin cut
      type A is array (1 .. 10) of Lim;
      type Priv is record
         F : A;
      end record;
      -- ``F`` has limited type, partial view of ``Priv`` must be :ada:`limited private`
      --$ end cut
      
      --$ begin cut
      type Priv is record
         Field : Integer := Lim'Size;
      end record;
      --$ end cut

   --$ line question
   end P;
begin
   null;
end Main;
