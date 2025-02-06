=====================
Owned Trait Objects
=====================

---------------------
Owned Trait Objects
---------------------

We previously saw how trait objects can be used with references, e.g
:rust:`&dyn Pet`. However, we can also use trait objects with smart pointers
like :rust:`Box` to create an owned trait object: :rust:`Box<dyn Pet>`.

.. code:: rust

   struct Dog {
       name: String,
       age: i8,
   }
   struct Cat {
       lives: i8,
   }

   trait Pet {
       fn talk(&self) -> String;
   }

   impl Pet for Dog {
       fn talk(&self) -> String {
           format!("Woof, my name is {}!", self.name)
       }
   }

   impl Pet for Cat {
       fn talk(&self) -> String {
           String::from("Miau!")
       }
   }

   fn main() {
       let pets: Vec<Box<dyn Pet>> = vec![
           Box::new(Cat { lives: 9 }),
           Box::new(Dog { name: String::from("Fido"), age: 5 }),
       ];
       for pet in pets {
           println!("Hello, who are you? {}", pet.talk());
       }
   }

Memory layout after allocating :rust:`pets`:

.. code:: bob

    Stack                             Heap
   .- - - - - - - - - - - - - - - -.     .- - - - - - - - - - - - - - - - - - - - - - -.
   :                               :     :                                             :
   :    "pets: Vec<Box<dyn Pet>>"  :     :   "data: Cat"         +----+----+----+----+ :
   :   +-----------+-------+       :     :  +-------+-------+    | F  | i  | d  | o  | :
   :   | ptr       |   o---+-------+--.  :  | lives |     9 |    +----+----+----+----+ :
   :   | len       |     2 |       :  |  :  +-------+-------+      ^                   :
   :   | capacity  |     2 |       :  |  :       ^                 |                   :
   :   +-----------+-------+       :  |  :       |                 '-------.           :
   :                               :  |  :       |               data:"Dog"|           :
   :                               :  |  :       |              +-------+--|-------+   :
   `- - - - - - - - - - - - - - - -'  |  :   +---|-+-----+      | name  |  o, 4, 4 |   :
                                      `--+-->| o o | o o-|----->| age   |        5 |   :
                                         :   +-|---+-|---+      +-------+----------+   :
                                         :     |     |                                 :
                                         `- - -| - - |- - - - - - - - - - - - - - - - -'
                                               |     |
                                               |     |                      "Program text"
                                         .- - -| - - |- - - - - - - - - - - - - - - - -.
                                         :     |     |       vtable                    :
                                         :     |     |      +----------------------+   :
                                         :     |     `----->| "<Dog as Pet>::talk" |   :
                                         :     |            +----------------------+   :
                                         :     |             vtable                    :
                                         :     |            +----------------------+   :
                                         :     '----------->| "<Cat as Pet>::talk" |   :
                                         :                  +----------------------+   :
                                         :                                             :
                                         '- - - - - - - - - - - - - - - - - - - - - - -'

---------
Details
---------

-  Types that implement a given trait may be of different sizes. This
   makes it impossible to have things like :rust:`Vec<dyn Pet>` in the
   example above.

-  :rust:`dyn Pet` is a way to tell the compiler about a dynamically sized
   type that implements :rust:`Pet`.

-  In the example, :rust:`pets` is allocated on the stack and the vector
   data is on the heap. The two vector elements are *fat pointers*:

   -  A fat pointer is a double-width pointer. It has two components: a
      pointer to the actual object and a pointer to the
      `virtual method table <https://en.wikipedia.org/wiki/Virtual_method_table>`__
      (vtable) for the :rust:`Pet` implementation of that particular object.
   -  The data for the :rust:`Dog` named Fido is the :rust:`name` and :rust:`age`
      fields. The :rust:`Cat` has a :rust:`lives` field.

-  Compare these outputs in the above example:

   .. code:: rust

      println!("{} {}", std::mem::size_of::<Dog>(), std::mem::size_of::<Cat>());
      println!("{} {}", std::mem::size_of::<&Dog>(), std::mem::size_of::<&Cat>());
      println!("{}", std::mem::size_of::<&dyn Pet>());
      println!("{}", std::mem::size_of::<Box<dyn Pet>>());
