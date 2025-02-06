=============
Comparisons
=============

-------------
Comparisons
-------------

These traits support comparisons between values. All traits can be
derived for types containing fields that implement these traits.

--------------------------
:rust:`PartialEq` and :rust:`Eq`
--------------------------

:rust:`PartialEq` is a partial equivalence relation, with required method
:rust:`eq` and provided method :rust:`ne`. The :rust:`==` and :rust:`!=` operators will
call these methods.

.. code:: rust

   struct Key {
       id: u32,
       metadata: Option<String>,
   }
   impl PartialEq for Key {
       fn eq(&self, other: &Self) -> bool {
           self.id == other.id
       }
   }

:rust:`Eq` is a full equivalence relation (reflexive, symmetric, and
transitive) and implies :rust:`PartialEq`. Functions that require full
equivalence will use :rust:`Eq` as a trait bound.

----------------------------
:rust:`PartialOrd` and :rust:`Ord`
----------------------------

:rust:`PartialOrd` defines a partial ordering, with a :rust:`partial_cmp`
method. It is used to implement the :rust:`<`, :rust:`<=`, :rust:`>=`, and :rust:`>`
operators.

.. code:: rust

   use std::cmp::Ordering;
   #[derive(Eq, PartialEq)]
   struct Citation {
       author: String,
       year: u32,
   }
   impl PartialOrd for Citation {
       fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
           match self.author.partial_cmp(&other.author) {
               Some(Ordering::Equal) => self.year.partial_cmp(&other.year),
               author_ord => author_ord,
           }
       }
   }

:rust:`Ord` is a total ordering, with :rust:`cmp` returning :rust:`Ordering`.

---------
Details
---------

:rust:`PartialEq` can be implemented between different types, but :rust:`Eq`
cannot, because it is reflexive:

.. code:: rust

   struct Key {
       id: u32,
       metadata: Option<String>,
   }
   impl PartialEq<u32> for Key {
       fn eq(&self, other: &u32) -> bool {
           self.id == *other
       }
   }

In practice, it's common to derive these traits, but uncommon to
implement them.
