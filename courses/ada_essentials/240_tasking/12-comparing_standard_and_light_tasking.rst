======================================
Comparing Standard and Light-Tasking
======================================

-------------------------------
Ravenscar Tasking Limitations
-------------------------------

* Active synchronization not supported

   - Asymmetric rendezvous
   - Task entries

* Tasks declaration **must be** at library level

   - They must **never** finish

* Protected object entries

   - Only one entry per protected object

      + **Unlimited** in Jorvik

   - Barriers can only be simple boolean values

      + Typically blocking until a flag clears
      + Jorvik allows for more general :dfn:`pure barriers`

-----------------------------------------
Task Types and Protected with Ravenscar
-----------------------------------------

* The **whole** tasking setup must be **static**

    - Compiler "compiles-in" the scheduling
    - Protected and tasks instantiation **must** be at library level

        + No "task hierarchy" or "local protected"

    - No :ada:`new` allocators for tasks or protected objects

* Tasks are activated at the end of their library unit's declarative part

   - Can be deferred to the end of **all** elaboration

