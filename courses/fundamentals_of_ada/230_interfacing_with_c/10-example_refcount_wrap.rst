==========================================
Refcounting Wrapper for External C Objects
==========================================

-------
Context
-------

* From :url:`https://blog.adacore.com/the-road-to-a-thick-opengl-binding-for-ada-part-2`
* OpenGL API create various objects like textures or vertex buffers
* Creating them gives us an ID

    - Can then be used to refer to the object

* Simple approach: Manually reclaiming them

    - Could cause leaks

* Refcount approach: automatic ID management

    - From an Ada wrapper
    - Automatic reclaim once the last reference vanishes

-----------------
Wrapper Interface
-----------------

* :ada:`type GL_Object is abstract tagged private`

    - Implements smart pointer logic

.. code:: Ada

       procedure Initialize_Id (Object : in out GL_Object);

       procedure Clear (Object : in out GL_Object);

       function Initialized (Object : GL_Object) return Boolean;

* Derived by the **actual** object types

.. code:: Ada

       procedure Internal_Create_Id
         (Object : GL_Object; Id : out UInt) is abstract;

       procedure Internal_Release_Id
         (Object : GL_Object; Id : UInt) is abstract;

* Example usage

.. code:: Ada

    type Shader (Kind : Shader_Type) is new GL_Object with null record;

------------------------------------
Wrapper Implementation: Private part
------------------------------------

* Object ID's holder: :ada:`GL_Object_Reference`

    - All derived types have a handle to this

.. code:: Ada

   type GL_Object_Reference;
   type GL_Object_Reference_Access is access all GL_Object_Reference;

   type GL_Object is abstract new Ada.Finalization.Controlled
     with record
      Reference : GL_Object_Reference_Access := null;
   end record;

* Controlled type implementing **ref-counting**

.. code:: Ada

   overriding procedure Adjust (Object : in out GL_Object);
   -- Increases reference count.

   overriding procedure Finalize (Object : in out GL_Object);
   -- Decreases reference count.
   -- Destroys underlying resource when it reaches zero.

------------------------------------
Wrapper Implementation: Full Picture
------------------------------------

.. image:: controlled_gl_object.svg

.. code:: Ada

   type GL_Object_Reference is record
      GL_Id           : UInt;
      Reference_Count : Natural;
      Is_Owner        : Boolean;
   end record;

------------------------
:ada:`Adjust` Completion
------------------------

* :ada:`Adjust` is called every time a new reference is **created**
* Increments the ref-counter

.. code:: Ada

    overriding procedure Adjust (Object : in out GL_Object) is
    begin
       if Object.Reference /= null then
          Object.Reference.Reference_Count := @ + 1;
       end if;
    end Adjust;

--------------------------
:ada:`Finalize` Completion
--------------------------

.. note::

    * :ada:`Finalize` should always be :dfn:`idempotent`

        - Compiler might call it multiple times on the same object
        - In particular when **exceptions** occur

.. code:: Ada

    overriding procedure Finalize (Object : in out GL_Object) is
       Ref : GL_Object_Reference_Access
            renames Object.Reference;
    begin


.. warning::

    Do **not** decrement the reference counter for every call

    * A given object will own **only one** reference

.. code:: Ada

       --  Idempotence: the next call to Finalize will have no effect
       Ref := null;

       if Ref /= null then
          Ref.Reference_Count := @ - 1;
          if Ref.Reference_Count = 0 then
             Free (Ref.all);  --  Call to user-defined primitive
             Unchecked_Free (Ref);
          end if;
       end if;
