==============
Introduction
==============

----------------
Topics Covered
----------------

-  :rust:`Box<T>`

-  :rust:`Deref`

-  :rust:`Drop`

-  :rust:`Rc<T>`

-------------------------
What are Smart Pointers
-------------------------

Common reference ( :rust:`&T` ) points to data *Smart Pointer* **owns** it

-  Mostly implemented as *Generic Structs* to wrap any type

-  Memory is freed when no longer needed via :rust:`Drop` trait

-  Access data via the :rust:`Deref` trait automatically
