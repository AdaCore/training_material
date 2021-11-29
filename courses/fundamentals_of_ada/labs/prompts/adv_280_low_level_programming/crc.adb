package body Crc is
   type Array_T is array (Positive range <>) of Crc_T;
   function Generate
     (Address : System.Address;
      Size    : Natural)
      return Crc_T is
      Retval : Crc_T := 0;
   begin
      -- Create an object of the appropriate size at Address
      -- and then sum the contents of the object
      return Retval;
   end Generate;
end Crc;
