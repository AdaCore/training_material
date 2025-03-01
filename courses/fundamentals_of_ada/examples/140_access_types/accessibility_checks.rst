.. code:: ada
    :class: ada-syntax-only

   package Accessibility_Checks is
      procedure Proc_Access;
      procedure Proc_Unchecked_Access;
   end Accessibility_Checks;

   with Ada.Text_IO; use Ada.Text_IO;
   package body Accessibility_Checks is
   
      type Recursive_Record_T;
      type Global_Access_T is access all Recursive_Record_T;
      type Recursive_Record_T is record
         Component : Integer;
         Next      : Global_Access_T := null;
      end record;
      Global_Pointer : Global_Access_T;
      Global_Object  : aliased Recursive_Record_T;
      procedure Proc_Access is
         type Local_Access_T is access all Recursive_Record_T;
         Local_Pointer : Local_Access_T;
         Local_Object  : aliased Recursive_Record_T;
      begin
         Global_Pointer := Global_Object'Access;
         Put_Line (Integer'Image (Global_Pointer.Component));
         -- Global_Pointer := Local_Object'Access; -- illegal
         Global_Pointer := Local_Object'Unchecked_Access;
         Put_Line (Integer'Image (Global_Pointer.Component));
         Local_Pointer := Global_Object'Access;
         Put_Line (Integer'Image (Local_Pointer.Component));
         Local_Pointer := Local_Object'Access;
         Put_Line (Integer'Image (Local_Pointer.Component));
         Local_Pointer := Local_Access_T (Global_Pointer);
         Put_Line (Integer'Image (Local_Pointer.Component));
         -- Global_Pointer := Global_Access_T (Local_Pointer); -- illegal
      end Proc_Access;
   
      procedure Proc_Unchecked_Access is
         Local_Object : aliased Recursive_Record_T;
      begin
         -- Global_Pointer := Local_Object'Access; -- illegal
         Global_Pointer := Local_Object'Unchecked_Access;
      end Proc_Unchecked_Access;
   
   end Accessibility_Checks;
