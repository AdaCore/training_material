   procedure Free_String is new Ada.Unchecked_Deallocation
     (String, String_Ref);

   procedure Finalize (Object : in out Ustring_T) is
   begin
      Free_String (Object.Ref);
   end Finalize;

   procedure Adjust (Object : in out Ustring_T) is
   begin
      Object.Ref := new String'(Object.Ref.all);
   end Adjust;

end Unbounded_String_Pkg;
