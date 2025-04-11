==============================
Visibility and Encapsulation
==============================

------------------------------
Visibility and Encapsulation
------------------------------

Like with items in a module, struct fields are also private by default.
Private fields are likewise visible within the rest of the module
(including child modules). This allows us to encapsulate implementation
details of struct, controlling what data and functionality is visible
externally.

.. code:: rust
   :number-lines: 1

   use outer::Foo;

   mod outer {
       pub struct Foo {
           pub val: i32,
           is_big: bool,
       }

       impl Foo {
           pub fn new(val: i32) -> Self {
               Self { val, is_big: val > 100 }
           }
       }

       pub mod inner {
           use super::Foo;

           pub fn print_foo(foo: &Foo) {
               println!("Is {} big? {}", foo.val, foo.is_big);
           }
       }
   }

   fn main() {
       let foo = Foo::new(42);
       println!("foo.val = {}", foo.val);
       // let foo = Foo { val: 42, is_big: true };

       outer::inner::print_foo(&foo);
       // println!("Is {} big? {}", foo.val, foo.is_big);
   }

---------------------------
Privacy and Encapsulation
---------------------------

-  Privacy in structs is module-based.

   - In some languages, types are the enpsulation boundary
   - Rust behaves differently

     - But can still achieve encapsulation.

-  Note how the :rust:`is_big` field is fully controlled by :rust:`Foo`,
   allowing :rust:`Foo` to control how it's initialized and enforce any
   invariants it needs to (e.g. that :rust:`is_big` is only :rust:`true` if
   :rust:`val > 100`).

-  Helper functions can be defined in the same module
   (including child modules) in order to get access to the type's
   private fields/methods.

-  Commented-out lines are compiler errors

   - You cannot initialize a struct with private fields

      .. code:: rust
         :number-lines: 27

         let foo = Foo { val: 42, is_big: true };

      ::

         error[E0451]: field `is_big` of struct `Foo` is private


   - You cannot directly access private fields

      .. code:: rust
         :number-lines: 30

         println!("Is {} big? {}", foo.val, foo.is_big);

      ::

         error[E0616]: field `is_big` of struct `Foo` is private

-  Enums do not support privacy: Variants and data within those variants
   is always public.

-----------------
More to Explore
-----------------

-  If students want more information about privacy (or lack thereof) in
   enums, you can bring up :rust:`#[doc_hidden]` and :rust:`#[non_exhaustive]`
   and show how they're used to limit what can be done with an enum.

-  Module privacy still applies when there are :rust:`impl` blocks in other
   modules
   :url:`(example in the playground) <https://play.rust-lang.org/?version=stable&mode=debug&edition=2021&gist=3e61f43c88de12bcdf69c1d6df9ab3da>`.
