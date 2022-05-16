-- Which of the following piece(s) of code allow for calling :ada:`Proc (V)`?

procedure Main is
   package Pkg is
      --$ begin question
      type T1 is tagged null record;
      type T2 is new T1 with null record;
      -- Declarations
      --$ end question

      --$ begin cut
      procedure Proc (V : T1) is null;
      --  Not compiling: declared after T1 is frozen
      --$ end cut

      --$ begin cut
      procedure Proc (V : T1'Class) is null;
      -- Correct, but not a primitive
      --$ end cut

      --$ begin cut
      procedure Proc (V : T1'Class) is null;
      procedure Proc (V : T2'Class) is null;
      -- :ada:`T1'Class` contains :ada:`T2'Class`
      --$ end cut

      --$ begin cut
      procedure Proc (V : T2) is null;
      -- Proc is a primitive of T2 **only**
      --$ end cut

      --$ begin question
      V : T2;
      --$ end question
   end Pkg;
   use Pkg;
begin
   Proc (V);
end Main;
