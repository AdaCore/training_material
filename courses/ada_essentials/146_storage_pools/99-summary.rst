=========
Summary
=========

---------
Summary
---------

* Storage pools give more control over memory allocation/deallocation

  * Helpful in real-time systems, embedded buffers
  * Useful for debugging memory issues

* Implementation impact localized

  * Need to define access type to use storage pool
  * But then :ada:`new` and :ada:`Unchecked_Deallocation` calls remain

* :ada:`GNAT.Debug_Pools`

  * Pre-built implementation of :ada:`System.Storage_Pools`
  * Used for runtime memory debugging
