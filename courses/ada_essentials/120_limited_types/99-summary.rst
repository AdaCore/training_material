=========
Summary
=========

---------
Summary
---------

* Limited view protects against improper operations

   - Incorrect equality semantics
   - Copying via assignment

* Enclosing composite types are :ada:`limited` too

   - Even if they don't use keyword :ada:`limited` themselves

* Limited types are always passed by-reference
* Extended return statements work for any type

   - Ada 2005 and later

* Don't make types :ada:`limited` unless necessary

   - Users generally expect assignment to be available
