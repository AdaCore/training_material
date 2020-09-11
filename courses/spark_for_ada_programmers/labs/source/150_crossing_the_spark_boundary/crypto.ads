with Interfaces.C;

package Crypto
  with SPARK_Mode
is

   subtype Byte is Interfaces.C.unsigned_char;
   subtype Key_Slot is Interfaces.C.int range 0 .. 3;
   type Key_Index is range 1 .. 128;
   type Key is array (Key_Index) of Byte;
   type Data_Index is range 1 .. 1024;
   type Data is array (Data_Index) of Byte;

end Crypto;
