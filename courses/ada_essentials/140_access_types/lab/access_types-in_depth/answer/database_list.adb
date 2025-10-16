--  Turn off warnings about Sorted vs Unsorted
pragma Warnings (Off, "is not referenced");

with Ada.Unchecked_Deallocation;
package body Database_List is

   Extra_Credit : constant Boolean := True;

   --|helpers_begin
   function Is_Empty (List : List_T) return Boolean is
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

   function End_Of_List (List : List_T) return Boolean is
   begin
      return List.Current = null;
   end End_Of_List;

   function Current (List : List_T) return Database_T is
   begin
      return List.Current.Content;
   end Current;
   --|helpers_end

   --|insertion_routines_begin
   procedure Sorted_Insert (List      : in out List_T;
                            Component :        Database_T) is
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
   end Sorted_Insert;

   procedure Unsorted_Insert (List      : in out List_T;
                              Component :        Database_T) is
      New_Component : constant Linked_List_Ptr_T := new Linked_List_T'
          (Next    => null,
           Content => Component);
   begin
      if Is_Empty (List) then
         List.Head    := New_Component;
         List.Current := List.Head;
      else
         New_Component.Next := List.Head;
         List.Head          := New_Component;
         List.Current       := List.Head;
      end if;
   end Unsorted_Insert;

   procedure Insert (List      : in out List_T;
                     Component :        Database_T) is
   begin
      if Extra_Credit then
         Sorted_Insert (List, Component);
      else
         Unsorted_Insert (List, Component);
      end if;

      -- Uncomment next line when using debug/storage pools
      -- Memory_Mgmt.Print_Info;
   end Insert;
   --|insertion_routines_end

   --|deletion_routine_begin
   procedure Free is new Ada.Unchecked_Deallocation
     (Linked_List_T, Linked_List_Ptr_T);

   procedure Delete
     (List      : in out List_T;
      Component :        Database_T) is
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
                     exit;
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
      -- Uncomment next line when using debug/storage pools
      -- Memory_Mgmt.Print_Info;
   end Delete;
   --|deletion_routine_end
end Database_List;
