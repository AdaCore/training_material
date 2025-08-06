=============================
Exercise: Health Statistics
=============================

-----------------------------
Health Statistics Problem
-----------------------------

You're working on implementing a health-monitoring system. As part of that, you
need to keep track of users' health statistics.

You'll start with a stubbed function in an :rust:`impl` block as well as a :rust:`User`
struct definition. Your goal is to implement the stubbed out method on the
:rust:`User` :rust:`struct` defined in the :rust:`impl` block.

Copy the code below to https://play.rust-lang.org/ and fill in the
missing method:

.. container:: source_include 150_borrowing/src/150_borrowing.rs :start-after://ANCHOR-setup :end-before://ANCHOR-solution :code:rust

.. code:: rust

   impl User {
       pub fn new(name: String, age: u32, height: f32) -> Self {
           Self { name, age, height, visit_count: 0, last_blood_pressure: None }
       }

       pub fn visit_doctor(&mut self, measurements: Measurements) -> HealthReport {
           todo!("Update a user's statistics based on measurements from a visit to the doctor")
       }
   }


-----------------------------
Health Statistics Main
-----------------------------

.. container:: source_include 150_borrowing/src/150_borrowing.rs :start-after://ANCHOR-main :code:rust

-----------------------------
Health Statistics Solution
-----------------------------

.. container:: source_include 150_borrowing/src/150_borrowing.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust
