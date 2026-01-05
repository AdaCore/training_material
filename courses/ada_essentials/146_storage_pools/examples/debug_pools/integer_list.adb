with Ada.Unchecked_Deallocation;
with GNAT.Debug_Pools;
with Memory_Mgmt;

package body Integer_List is

   function Is_Empty
     (List : List_T)
      return Boolean is
   begin
      return List.Head = null;
   end Is_Empty;

   procedure First (List : in out List_T) is
   begin
      List.Current := List.Head;
   end First;

   procedure Next (List : in out List_T) is
   begin
      if not Is_Empty (List) then
         if List.Current /= null then
            List.Current := List.Current.Next;
         end if;
      end if;
   end Next;

   function End_Of_List
     (List : List_T)
      return Boolean is
   begin
      return List.Current = null;
   end End_Of_List;

   function Current
     (List : List_T)
      return Integer is
   begin
      return List.Current.Content;
   end Current;

   --|insert_begin
   procedure Insert
     (List      : in out List_T;
      Component :        Integer) is
      New_Component : constant Linked_List_Ptr_T := new Linked_List_T'
          (Next    => null,
           Content => Component);
   begin
      if Is_Empty (List) then
         List.Current := New_Component;
         List.Head    := New_Component;
      elsif Component < List.Head.Content then
         New_Component.Next := List.Head;
         List.Current       := New_Component;
         List.Head          := New_Component;
      else
         declare
            Current : Linked_List_Ptr_T := List.Head;
         begin
            while Current.Next /= null
              and then Current.Next.Content < Component
            loop
               Current := Current.Next;
            end loop;
            New_Component.Next := Current.Next;
            Current.Next       := New_Component;
         end;
      end if;
      --  Print information for every insertion
      GNAT.Debug_Pools.Print_Info_Stdout (Memory_Mgmt.Storage_Pool);
   end Insert;
   --|insert_end

   procedure Free is new Ada.Unchecked_Deallocation
     (Linked_List_T, Linked_List_Ptr_T);
   procedure Delete
     (List      : in out List_T;
      Component :        Integer) is
      To_Delete : Linked_List_Ptr_T := null;
   begin
      if not Is_Empty (List) then
         if List.Head.Content = Component then
            To_Delete    := List.Head;
            List.Head    := List.Head.Next;
            List.Current := List.Head;
         else
            declare
               Previous : constant Linked_List_Ptr_T := List.Head;
               Current  : Linked_List_Ptr_T          := List.Head.Next;
            begin
               while Current /= null loop
                  if Current.Content = Component then
                     To_Delete     := Current;
                     Previous.Next := Current.Next;
                  end if;
                  Current := Current.Next;
               end loop;
            end;
            List.Current := List.Head;
         end if;
         if To_Delete /= null then
            Free (To_Delete);
         end if;
      end if;
   end Delete;
end Integer_List;
