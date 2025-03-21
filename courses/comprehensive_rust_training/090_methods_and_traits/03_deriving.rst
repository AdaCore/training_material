==========
Deriving
==========

----------
Deriving
----------

Supported traits can be automatically implemented for your custom types,
as follows:

.. code:: rust

   #[derive(Debug, Clone, Default)]
   struct Player {
       name: String,
       strength: u8,
       hit_points: u8,
   }

   fn main() {
       let p1 = Player::default(); // Default trait adds `default` constructor.
       let mut p2 = p1.clone(); // Clone trait adds `clone` method.
       p2.name = String::from("EldurScrollz");
       // Debug trait adds support for printing with `{:?}`.
       println!("{p1:?} vs. {p2:?}");
   }

---------
Details
---------

Derivation is implemented with macros, and many crates provide useful
derive macros to add useful functionality. For example, :rust:`serde` can
derive serialization support for a struct using
:rust:`#[derive(Serialize)]`.
