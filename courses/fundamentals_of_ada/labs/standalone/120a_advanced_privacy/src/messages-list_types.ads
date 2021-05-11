
package Messages.List_Types is

   type List_T is private;

private

   type List_Content_T;
   type List_T is access List_Content_T;

   type Id_Type is range 1_000 .. 9_999;
   type List_Content_T is record
      Id      : Id_Type;
      Content : Message_T;
      Next    : List_T;
   end record;

end Messages.List_Types;
