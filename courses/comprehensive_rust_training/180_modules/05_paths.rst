==================
use, super, self
==================

------------------
use, super, self
------------------

A module can bring symbols from another module into scope with :rust:`use`.
You will typically see something like this at the top of each module:

.. code:: rust

   use std::collections::HashSet;
   use std::process::abort;

-------
Paths
-------

Paths are resolved as follows:

1. As a relative path:

   -  :rust:`foo` or :rust:`self::foo` refers to :rust:`foo` in the current module,
   -  :rust:`super::foo` refers to :rust:`foo` in the parent module.

2. As an absolute path:

   -  :rust:`crate::foo` refers to :rust:`foo` in the root of the current crate,
   -  :rust:`bar::foo` refers to :rust:`foo` in the :rust:`bar` crate.

---------
Details
---------

-  It is common to "re-export" symbols at a shorter path. For example,
   the top-level :rust:`lib.rs` in a crate might have

   .. code:: rust

      mod storage;

      pub use storage::disk::DiskStorage;
      pub use storage::network::NetworkStorage;

   making :rust:`DiskStorage` and :rust:`NetworkStorage` available to other
   crates with a convenient, short path.

-  For the most part, only items that appear in a module need to be
   :rust:`use`\ 'd. However, a trait must be in scope to call any methods on
   that trait, even if a type implementing that trait is already in
   scope. For example, to use the :rust:`read_to_string` method on a type
   implementing the :rust:`Read` trait, you need to :rust:`use std::io::Read`.

-  The :rust:`use` statement can have a wildcard: :rust:`use std::io::*`. This
   is discouraged because it is not clear which items are imported, and
   those might change over time.
