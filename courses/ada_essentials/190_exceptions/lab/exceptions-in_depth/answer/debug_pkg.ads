--Calculator_Spec
--Debug
with Ada.Exceptions;
package Debug_Pkg is
   procedure Save_Occurrence (X : Ada.Exceptions.Exception_Occurrence);
   procedure Print_Exceptions;
end Debug_Pkg;
