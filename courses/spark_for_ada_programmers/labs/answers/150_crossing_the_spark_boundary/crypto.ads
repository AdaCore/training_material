with Interfaces.C;

package Crypto with
   SPARK_Mode,
   Abstract_State => key_store,
   Initializes    => key_store
is

   subtype Byte is Interfaces.C.unsigned_char;
   subtype Key_Slot is Interfaces.C.int range 0 .. 3;
   type Key_Index is range 1 .. 128;
   type Key is array (Key_Index) of Byte;
   type Data_Index is range 1 .. 1_024;
   type Data is array (Data_Index) of Byte;

   procedure load_key
     (keyslot : in Key_Slot; the_key : in Key; result : out Boolean) with
      Global => (in_out => key_store);

   procedure encrypt
     (keyslot : in Key_Slot; the_data : in out Data; result : out Boolean) with
      Global => (in_out => key_store);

end Crypto;
