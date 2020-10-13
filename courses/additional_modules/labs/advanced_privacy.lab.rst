----------------------
Advanced Privacy Lab
----------------------
   
* Requirements
   
   - Create a package defining a message type whose implementation is solely in the body

      - You will need accessor functions to set / get the content
      - Create a function to return a string representation of the message contents

   - Create another package that defines the types needed for a linked list of messages

      - Each message in the list should have an identifier not visible to any clients

   - Create a package containing simple operations on the list

      - Typical operations like list creation and list traversal
      - Create a subprogram to print the list contents
 
   - Have your main program add items to the list and then print the list

* Hints

   - You will need to employ some (but not necessarily all) of the techniques discussed in this module
 
----------------------------------------------
Advanced Privacy Lab Solution - Message Type
----------------------------------------------
.. code:: Ada

   package Messages is

      type Message_T is private;

      procedure Set_Content (Message : in out Message_T;
                             Value   :        Integer);
      function Content (Message : Message_T)
                      return Integer;
      function Image (Message : Message_T)
                      return String;

   private
      type Message_Content_T;
      type Message_T is access Message_Content_T;

   end Messages; 
 
---------------------------------------------------
Advanced Privacy Lab Solution - Message List Type
---------------------------------------------------
.. code:: Ada

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
 
---------------------------------------------------------
Advanced Privacy Lab Solution - Message List Operations
---------------------------------------------------------
.. code:: Ada

   with Messages.List_Types;
   package Messages.List_Types.Operations is

      procedure Append (List : in out List_T;
                        Item :        Message_T);
      function Next (List : List_T)
                     return List_T;
      function Is_Null (List : List_T)
                        return Boolean;
      function Image (Message : List_T)
                      return String;

   end Messages.List_Types.Operations;
