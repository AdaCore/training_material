==============
Introduction
==============

----------------
Topics Covered
----------------

-  :rust:`Box<T>`

   -  Heap allocation
  
   -  Bypassing static size constraints

-  :rust:`Deref`

   -  Overriding the dereference operator
  
   -  Transparent data access via coercion

-  :rust:`Drop`

   -  Automatic resource management
  
   -  Order of destruction and explicit cleanup

-  :rust:`Rc<T>`

   -  Shared ownership of heap data
  
   -  Tracking active references

-------------------------
What are Smart Pointers
-------------------------

-  Common references ( :rust:`&T` ) point to data *Smart Pointers* **own** it

   -  Mostly implemented as *Generic Structs* to wrap any type

   -  Memory is freed when no longer needed via :rust:`Drop` trait

   -  Access data via the :rust:`Deref` trait automatically
