===================
Exercise: Counter
===================

-------------------
Counter Problem
-------------------

In this exercise you will take a very simple data structure and make it
generic. It uses a
:url:`std::collections::HashMap <https://doc.rust-lang.org/stable/std/collections/struct.HashMap.html>`
to keep track of which values have been seen and how many times each one
has appeared.

The initial version of :rust:`Counter` is hard coded to only work for
:rust:`u32` values. Make the struct and its methods generic over the type of
value being tracked, that way :rust:`Counter` can track any type of value.

If you finish early, try using the
:url:`entry <https://doc.rust-lang.org/stable/std/collections/struct.HashMap.html#method.entry>`
method to halve the number of hash lookups required to implement the
:rust:`count` method.

::

   use std::collections::HashMap;

   /// Counter counts the number of times each value of type T has been seen.
   struct Counter {
       values: HashMap<u32, u64>,
   }

   impl Counter {
       /// Create a new Counter.
       fn new() -> Self {
           Counter {
               values: HashMap::new(),
           }
       }

       /// Count an occurrence of the given value.
       fn count(&mut self, value: u32) {
           if self.values.contains_key(&value) {
               *self.values.get_mut(&value).unwrap() += 1;
           } else {
               self.values.insert(value, 1);
           }
       }

       /// Return the number of times the given value has been seen.
       fn times_seen(&self, value: u32) -> u64 {
           self.values.get(&value).copied().unwrap_or_default()
       }
   }

------------------------
Counter - Main Program
------------------------

.. container:: source_include 110_std_types/src/110_std_types.rs :start-after://ANCHOR-main :code:rust :number-lines:1

-------------------
Counter Solution
-------------------

.. container:: source_include 110_std_types/src/110_std_types.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust :number-lines:1

