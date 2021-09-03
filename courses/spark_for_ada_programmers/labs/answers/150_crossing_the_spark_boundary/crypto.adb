pragma Ada_2012;
with Interfaces.C;
use type Interfaces.C.int;
package body Crypto with
   SPARK_Mode,
   Refined_State => (key_store => null)
is

   -------------
   -- loadkey --
   -------------

   procedure loadkey
     (keyslot : in Key_Slot; the_key : in Key; result : out Boolean)
   is
      function c_loadkey
        (keyslot : Key_Slot; the_key : Key) return Interfaces.C.int with
         Import,
         Convention    => c,
         External_Name => "loadkey",
         Global        => null;
   begin
      result := c_loadkey (keyslot, the_key) = 0;
   end loadkey;

   -------------
   -- encrypt --
   -------------

   procedure encrypt
     (keyslot : in Key_Slot; the_data : in out Data; result : out Boolean) with
      SPARK_Mode => off
   is
      function c_encrypt
        (keyslot : Key_Slot; the_data : in out Data) return Interfaces.C.int with
         Import,
         Convention    => c,
         External_Name => "encrypt";
   begin
      result := c_encrypt (keyslot, the_data) = 0;
   end encrypt;

end Crypto;
