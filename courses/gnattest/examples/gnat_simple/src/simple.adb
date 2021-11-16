--
--  Copyright (C) 2011-2012, AdaCore
--

--$ begin cut
package body Simple is

   function Inc (X : Integer) return Integer
   is
   begin
      return X + 1;
   end Inc;

--$ end cut
--     function Dec (X : Integer) return Integer
--     is
--     begin
--        return X - 1;
--     end Dec;

--$ begin cut
end Simple;
--$ end cut
