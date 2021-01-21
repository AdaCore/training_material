with Ada.Text_IO; use Ada.Text_IO;
with Datastore;   use Datastore;
procedure Main is

  function Get
   (Prompt : String)
    return String is
  begin
    Put ("   " & Prompt & "> ");
    return Get_Line;
  end Get;

  procedure Add
   (History : in out Datastore.History_T;
    Text    : in     String) is
  begin
    for Event of History loop
      if Event = null then
        Event := new String'(Text);
        exit;
      end if;
    end loop;
  end Add;

  Index  : Integer;
  Object : Datastore.Constant_Reference_T;

begin

  loop
    Index := Integer'Value (Get ("Enter index"));
    exit when Index not in Datastore.Index_T'Range;
    Add (Datastore.Object (Index).History, Get ("Text"));

  end loop;

  for I in Index_T'Range loop
    Object := Datastore.View (I);
    if Object.History (1) /= null then
      Put_Line (Integer'Image (I) & ">");
      for Item of Object.History loop
        exit when Item = null;
        Put_Line ("  " & Item.all);
      end loop;
    end if;
  end loop;

end Main;
