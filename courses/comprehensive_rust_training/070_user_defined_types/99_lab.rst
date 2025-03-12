===========================
Exercise: Elevator Events
===========================

---------------------------
Elevator Events Problem
---------------------------

We will create a data structure to represent an event in an elevator
control system. It is up to you to define the types and functions to
construct various events. Use :rust:`#[derive(Debug)]` to allow the types to
be formatted with :rust:`{:?}`.

This exercise only requires creating and populating data structures so
that :rust:`main` runs without errors. The next part of the course will
cover getting data out of these structures.

.. code:: rust

   #[derive(Debug)]
   /// An event in the elevator system that the controller must react to.
   enum Event {
       // TODO: add required variants
   }

   /// A direction of travel.
   #[derive(Debug)]
   enum Direction {
       Up,
       Down,
   }

   /// The car has arrived on the given floor.
   fn car_arrived(floor: i32) -> Event {
       todo!()
   }

   /// The car doors have opened.
   fn car_door_opened() -> Event {
       todo!()
   }

   /// The car doors have closed.
   fn car_door_closed() -> Event {
       todo!()
   }

   /// A directional button was pressed in an elevator lobby on the given floor.
   fn lobby_call_button_pressed(floor: i32, dir: Direction) -> Event {
       todo!()
   }

   /// A floor button was pressed in the elevator car.
   fn car_floor_button_pressed(floor: i32) -> Event {
       todo!()
   }

---------------------------
Main Program
---------------------------

.. container:: source_include 070_user_defined_types/src/070_user_defined_types.rs :start-after://ANCHOR-main :code:rust

----------------------------------
Elevator Events Solution - Types
----------------------------------

.. container:: source_include 070_user_defined_types/src/070_user_defined_types.rs :start-after://ANCHOR-types :end-before://ANCHOR-solution :code:rust

----------------------------------------
Elevator Events Solution - Subprograms
----------------------------------------

.. container:: source_include 070_user_defined_types/src/070_user_defined_types.rs :start-after://ANCHOR-solution :end-before://ANCHOR-main :code:rust

------------------------
Additional Information
------------------------

.. code:: rust

   #![allow(dead_code)]

* Only thing we ever do with :rust:`Event` type is print it
* Compiler thinks the code is unused and emits a warning

  * This directive prevents that
