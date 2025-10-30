=========
Summary
=========

---------
Summary
---------

* Access types are very similar to C/C++ pointers

   - Pointing to some memory location
   - Deallocation causes problems

* But Ada does a lot to remove the **need** for access types

   - Language has its own ways of dealing with large objects passed as parameters
   - Language has libraries dedicated to memory allocation / deallocation

* At a minimum, create your own generics to do allocation / deallocation

   - Minimize memory leakage and corruption
