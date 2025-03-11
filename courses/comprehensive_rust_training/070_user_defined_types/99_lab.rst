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

.. container:: latex_environment scriptsize 

   .. code:: rust

      fn main() {
          println!(
              "A ground floor passenger has pressed the up button: {:?}",
              lobby_call_button_pressed(0, Direction::Up)
          );
          println!("The car has arrived on the ground floor: {:?}", car_arrived(0));
          println!("The car door opened: {:?}", car_door_opened());
          println!(
              "A passenger has pressed the 3rd floor button: {:?}",
              car_floor_button_pressed(3)
          );
          println!("The car door closed: {:?}", car_door_closed());
          println!("The car has arrived on the 3rd floor: {:?}", car_arrived(3));
      }

----------------------------------
Elevator Events Solution - Types
----------------------------------

.. code:: rust

   #![allow(dead_code)]

   #[derive(Debug)]
   /// An event in the elevator system that the controller must react to.
   enum Event {
       /// A button was pressed.
       ButtonPressed(Button),

       /// The car has arrived at the given floor.
       CarArrived(Floor),

       /// The car's doors have opened.
       CarDoorOpened,

       /// The car's doors have closed.
       CarDoorClosed,
   }

   /// A floor is represented as an integer.
   type Floor = i32;

   /// A direction of travel.
   #[derive(Debug)]
   enum Direction {
       Up,
       Down,
   }

   /// A user-accessible button.
   #[derive(Debug)]
   enum Button {
       /// A button in the elevator lobby on the given floor.
       LobbyCall(Direction, Floor),

       /// A floor button within the car.
       CarFloor(Floor),
   }

----------------------------------------
Elevator Events Solution - Subprograms
----------------------------------------

.. code:: rust

   /// The car has arrived on the given floor.
   fn car_arrived(floor: i32) -> Event {
       Event::CarArrived(floor)
   }

   /// The car doors have opened.
   fn car_door_opened() -> Event {
       Event::CarDoorOpened
   }

   /// The car doors have closed.
   fn car_door_closed() -> Event {
       Event::CarDoorClosed
   }

   /// A directional button was pressed in an elevator lobby on the given floor.
   fn lobby_call_button_pressed(floor: i32, dir: Direction) -> Event {
       Event::ButtonPressed(Button::LobbyCall(dir, floor))
   }

   /// A floor button was pressed in the elevator car.
   fn car_floor_button_pressed(floor: i32) -> Event {
       Event::ButtonPressed(Button::CarFloor(floor))
   }

------------------------
Additional Information
------------------------

.. code:: rust

   #![allow(dead_code)]

* Only thing we ever do with :rust:`Event` type is print it
* Compiler thinks the code is unused, this directive prevents that
