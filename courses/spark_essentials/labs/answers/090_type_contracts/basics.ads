pragma Unevaluated_Use_Of_Old (Allow);

package Basics is

   type Base_Pair is record
      X, Y : Integer;
   end record;

   subtype Pair is Base_Pair
     with Predicate => Pair.X /= Pair.Y;

   procedure Swap_Pair (P : in out Pair)
     with Post => P.X = P.Y'Old and P.Y = P.X'Old;

   procedure Bump_And_Swap_Pair (P : in out Pair)
   with
     Pre  => P.X < Integer'Last and P.Y < Integer'Last;

   type Triplet is private;

   procedure Swap_Triplet (T : in out Triplet);

   procedure Bump_And_Swap_Triplet (T : in out Triplet)
   with
     Pre => T.Get_A < Integer'Last and T.Get_B < Integer'Last and T.Get_C < Integer'Last;

   function Get_A (T : Triplet) return Integer;
   function Get_B (T : Triplet) return Integer;
   function Get_C (T : Triplet) return Integer;

private

   procedure Bump_Pair (P : in out Pair)
   with
     Pre  => P.X < Integer'Last and P.Y < Integer'Last;

   procedure Bump_Triplet (T : in out Triplet)
   with
     Pre  => T.A < Integer'Last and T.B < Integer'Last and T.C < Integer'Last,
     Post => T.A = T.A'Old + 1 and T.B = T.B'Old + 1 and T.C = T.C'Old + 1;

   type Triplet is record
      A : Integer := 0;
      B : Integer := 1;
      C : Integer := 2;
   end record
     with Invariant => All_Different (Triplet);

   function All_Different (T : Triplet) return Boolean is
      (T.A /= T.B and T.B /= T.C and T.A /= T.C);

   function Get_A (T : Triplet) return Integer is (T.A);
   function Get_B (T : Triplet) return Integer is (T.B);
   function Get_C (T : Triplet) return Integer is (T.C);

end Basics;
