==================
Standard Library
==================

------------------
Standard Library
------------------

Rust comes with a standard library which helps establish a set of common
types used by Rust libraries and programs. This way, two libraries can
work together smoothly because they both use the same :rust:`String` type.

In fact, Rust contains several layers of the Standard Library: :rust:`core`,
:rust:`alloc` and :rust:`std`.

-  :rust:`core` includes the most basic types and functions that don't
   depend on :rust:`libc`, allocator or even the presence of an operating
   system.
-  :rust:`alloc` includes types which require a global heap allocator, such
   as :rust:`Vec`, :rust:`Box` and :rust:`Arc`.
-  Embedded Rust applications often only use :rust:`core`, and sometimes
   :rust:`alloc`.
