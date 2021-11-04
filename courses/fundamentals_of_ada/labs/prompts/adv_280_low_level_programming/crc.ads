--Crc
with System;
package Crc is
   type Crc_T is mod 2**32;
   for Crc_T'size use 32;
   function Generate
     (Address : System.Address;
      Size    : Natural)
      return Crc_T;
end Crc;
