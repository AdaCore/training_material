.. role:: rust(code)
    :language: rust

======================
Introduction to Rust
======================

---------
History
---------

* 2006
* Personal project by Graydon Hoare (working @ Mozilla at the time)
* No specification, instead semantics are based on implementation
* Language changed *a lot* between 2006 and 2015 (and is still changing a lot
  by other languages' standards)
* Nowadays, maintained and evolved by the Rust foundation

-------------------
High Level Vision
-------------------

* Safer alternative to C/C++ for systems programming
* Many inspirations, including ML family languages, C++
* Focus on safety, albeit with a different perspective when compared to Ada
  (memory safety being the most valued kind of safety)

------------
Rust Today
------------

* Use of Rust is spreading like wildfire
* Projects like Android, Linux
* Companies like Google, Amazon
* Well positioned to become a credible alternative to C++, and maybe even C
* Big list of industrial users here: https://www.rust-lang.org/production/users

-------------------------------
In the Safety Critical Market
-------------------------------

* Rust making forays into the critical markets. Big players are assessing the use of Rust in their codebases.
* But lacking industrial support for now
* Will probably become mainstream in the coming decade

--------------------
Rust "Hello World"
--------------------

.. code:: Rust

   fn main() {
       println!("Hello, world!");
   }

