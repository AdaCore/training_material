package body Pixels is

   --$ begin answer
   type Pixel_Component_Product_T is
     range Pixel_Component_T'First**2 .. Pixel_Component_T'Last**2;

   function "*" (A, B : Pixel_Component_T) return Pixel_Component_Product_T is
   begin
      return Pixel_Component_Product_T (A) * Pixel_Component_Product_T (B);
   end "*";

   function "+"
     (A, B : Pixel_Component_Product_T) return Pixel_Component_Product_T
   is
   begin
      if A > Pixel_Component_Product_T'Last - B then
         return Pixel_Component_Product_T'Last;
      else
         return Pixel_Component_Product_T (Natural (A) + Natural (B));
      end if;
   end "+";

   type Pixel_Product_T is record
      R, G, B : Pixel_Component_Product_T;
      A       : Pixel_Component_T;
   end record;

   function To_Pixel_Component
     (A : Pixel_Component_Product_T) return Pixel_Component_T
   is
   begin
      return
        Pixel_Component_T
          (A / Pixel_Component_Product_T (Pixel_Component_T'Last));
   end To_Pixel_Component;

   function To_Pixel (P : Pixel_Product_T) return Pixel_T is
   begin
      return
        (To_Pixel_Component (P.R), To_Pixel_Component (P.G),
         To_Pixel_Component (P.B), P.A);
   end To_Pixel;

   --$ end answer
   function "+" (A, B : Pixel_T) return Pixel_T is
      --$ begin answer
      function Add_Alpha (A1, A2 : Pixel_Component_T) return Pixel_Component_T
      is
      begin
         return
           Pixel_Component_T
             (Natural'Min
                (Natural (Pixel_Component_T'Last),
                 Natural (A1) + Natural (A2)));
      end Add_Alpha;
      --$ end answer
   begin
      --$ line question
      return (others => 0); -- TODO
      --$ begin answer
      return
        To_Pixel
          ((R => A.R * A.A + B.R * B.A, G => A.G * A.A + B.G * B.A,
            B => A.B * A.A + B.B * B.A, A => Add_Alpha (A.A, B.A)));
      --$ end answer
   end "+";

   function Luminosity (P : Pixel_T) return Pixel_Component_T is
   begin
      --$ line question
      return 0; -- TODO
      --$ begin answer
      return
        Pixel_Component_T
          (((Natural (P.R) + Natural (P.G) + Natural (P.B)) * Natural (P.A)) /
           (3 * Natural (Pixel_Component_T'Last)));
      --$ end answer
   end Luminosity;

end Pixels;
