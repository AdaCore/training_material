=========
Summary
=========

---------
Summary
---------

* Access types when used with "dynamic" memory allocation can cause problems

  * Whether actually dynamic or using managed storage pools, memory leaks/lack can occur
  * Storage pools can help diagnose memory issues, but it's still a usage issue

* :ada:`GNAT.Debug_Pools` is useful for debugging memory issues

  * Mostly in low-level testing
  * Could integrate it with an error logging mechanism

* :ada:`System.Storage_Pools` can be used to control memory usage

  * Adds overhead
