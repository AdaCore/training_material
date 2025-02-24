==================
Indefinite Private
==================

---------------
Limited Private
---------------

.. code:: Ada

   type T is limited private;

* Same interface as private

   - Removes :ada:`=` and :ada:`/=`
   - Removes assignments
   - Removes copies

.. note::

   Private type is a **view**

   - Completion should provide **at least** the same set of features
   - Completion can be a :ada:`limited record`
   - ... but doesn't **have** to

---------------
Limited Private
---------------

* No assignment: user cannot duplicate a key
* No equality: user cannot check two keys are the same
* Private type: user cannot access or change the issued date

.. code:: Ada

   package Key_Stuff is
      type Key is limited private;
      function Make_Key ( ... ) return Key;
      ...

   package body Key_Stuff is
      function Make_Key ( ... ) return Key is
      begin
         return New_Key: Key do
            New_Key.Issued := Today;
            ...
         end return;
      end Make_Key;

.. warning::

    * Definite type
    * User **doesn't** have to call :ada:`Make_Key`

------------------
Indefinite Private
------------------

* Indefinite: user **must** use the constructors
* Delegated :ada:`constant` objects are static constructors

.. code:: Ada

  package Binary_Trees is
    type Tree_T (<>) is private;

    Empty_Tree : constant Tree_T;

    type Nodes_T is ...
    type Edges_T is ...
    procedure Make (Nodes : Nodes_T; Edges : Edges_T);
    ...
  private
    type Tree_T is record
    ...

    Empty_Tree : constant Tree_T := ...;
       
  end Binary_Trees;

.. tip::

    Type completion **can** be definite

---------------
Opaque Pointers
---------------

* User can instantiate 
* Completion is an :ada:`access`
* Concrete type being pointed to is **incomplete**
* Implementation is done entirely within the body

.. code:: Ada

  package Black_Boxes is
     type Box_T is private;
     procedure Foo (B : Box_T);
  private
     type Internal_Box_T; -- incomplete
     type Box_T is access all Internal_Box_T;
  end Black_Boxes;

------------------------------
Example: A String Holder (1/2)
------------------------------

* Implementation not discussed here

.. code:: Ada

    package String_Holders is
       type Info is limited private;

       function Contains (Obj : Info; Str : String) return Boolean
          with Ghost;
       function Equals (Left, Right : Info) return Boolean
          with Ghost;

.. tip::

    These are only used for contracts, hence the :ada:`Ghost` aspect

.. code:: Ada

       function To_Info (Str : String) return Info
          with Post => Contains (To_Info'Result, S);

       function To_String (Obj : Info)
                            return String
          with Post => Contains (Obj, To_String'Result);

       procedure Copy (To   : in out Info;
                       From :    	Info)
          with Post => Equals (To, From);

       procedure Append (Obj : in out Info;
                         Str : String)
          with Post => Contains (Obj, To_String (Obj)'Old & S);

       procedure Destroy (Obj : in out Info);

------------------------------
Example: A String Holder (2/2)
------------------------------

.. code:: Ada

    private
       type Info is access String;

       function To_String_Internal (Obj : Info) return String
          is (if Obj = null then "" else Obj.all);
.. tip::

    This can be used by contracts implementation below, and child packages

.. code:: Ada

       function Contains (Obj : Info; Str : String) return Boolean
          is (Obj /= null and then Obj.all = Str);
       function Equals (Left, Right : Info) return Boolean
          is (To_String_Internal (Left)
            = To_String_Internal (Right));
