=========
Summary
=========

---------
Summary
---------

* Interfaces must be used for multiple inheritance

   * Usually combined with :ada:`tagged` types, but not necessary
   * By using only interfaces, only accessors are allowed

* Typically there are other ways to do the same thing

   * In our example, the conversion routine could be common to simplify things

* But interfaces force the compiler to determine when operations are missing
