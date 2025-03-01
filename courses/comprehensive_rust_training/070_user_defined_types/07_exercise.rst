===========================
Exercise: Elevator Events
===========================

---------------------------
Exercise: Elevator Events
---------------------------

We will create a data structure to represent an event in an elevator
control system. It is up to you to define the types and functions to
construct various events. Use :rust:`#[derive(Debug)]` to allow the types to
be formatted with :rust:`{:?}`.

This exercise only requires creating and populating data structures so
that :rust:`main` runs without errors. The next part of the course will
cover getting data out of these structures.

::

   {{#include exercise.rs:event}}
       // TODO: add required variants
   }

   {{#include exercise.rs:direction}}

   {{#include exercise.rs:car_arrived}}
       todo!()
   }

   {{#include exercise.rs:car_door_opened}}
       todo!()
   }

   {{#include exercise.rs:car_door_closed}}
       todo!()
   }

   {{#include exercise.rs:lobby_call_button_pressed}}
       todo!()
   }

   {{#include exercise.rs:car_floor_button_pressed}}
       todo!()
   }

   {{#include exercise.rs:main}}
