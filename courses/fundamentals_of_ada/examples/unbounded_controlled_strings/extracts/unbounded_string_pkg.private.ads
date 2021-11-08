private

   type String_Ref is access String;
   type Ustring_T is new Controlled with record
      Ref : String_Ref := new String (1 .. 0);
   end record;

   procedure Finalize (Object : in out Ustring_T);
   procedure Adjust (Object : in out Ustring_T);
end Unbounded_String_Pkg;
