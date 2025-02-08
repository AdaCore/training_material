================
Move Semantics
================

----------------
Move Semantics
----------------

An assignment will transfer *ownership* between variables:

.. code:: rust

   fn main() {
       let s1: String = String::from("Hello!");
       let s2: String = s1;
       println!("s2: {s2}");
       // println!("s1: {s1}");
   }

-  The assignment of :rust:`s1` to :rust:`s2` transfers ownership.
-  When :rust:`s1` goes out of scope, nothing happens: it does not own
   anything.
-  When :rust:`s2` goes out of scope, the string data is freed.

Before move to :rust:`s2`:

.. code:: bob

    Stack                             Heap
   .- - - - - - - - - - - - - -.     .- - - - - - - - - - - - - - - - - - -.
   :                           :     :                                     :
   :    s1                     :     :                                     :
   :   +-----------+-------+   :     :   +----+----+----+----+----+----+   :
   :   | ptr       |   o---+---+-----+-->| H  | e  | l  | l  | o  | !  |   :
   :   | len       |     6 |   :     :   +----+----+----+----+----+----+   :
   :   | capacity  |     6 |   :     :                                     :
   :   +-----------+-------+   :     :                                     :
   :                           :     `- - - - - - - - - - - - - - - - - - -'
   :                           :
   `- - - - - - - - - - - - - -'

After move to :rust:`s2`:

.. code:: bob

    Stack                             Heap
   .- - - - - - - - - - - - - -.     .- - - - - - - - - - - - - - - - - - -.
   :                           :     :                                     :
   :    s1 "(inaccessible)"    :     :                                     :
   :   +-----------+-------+   :     :   +----+----+----+----+----+----+   :
   :   | ptr       |   o---+---+--+--+-->| H  | e  | l  | l  | o  | !  |   :
   :   | len       |     6 |   :  |  :   +----+----+----+----+----+----+   :
   :   | capacity  |     6 |   :  |  :                                     :
   :   +-----------+-------+   :  |  :                                     :
   :                           :  |  `- - - - - - - - - - - - - - - - - - -'
   :    s2                     :  |
   :   +-----------+-------+   :  |
   :   | ptr       |   o---+---+--'
   :   | len       |     6 |   :
   :   | capacity  |     6 |   :
   :   +-----------+-------+   :
   :                           :
   `- - - - - - - - - - - - - -'

When you pass a value to a function, the value is assigned to the
function parameter. This transfers ownership:

.. code:: rust

   fn say_hello(name: String) {
       println!("Hello {name}")
   }

   fn main() {
       let name = String::from("Alice");
       say_hello(name);
       // say_hello(name);
   }

---------
Details
---------

-  Mention that this is the opposite of the defaults in C++, which
   copies by value unless you use :rust:`std::move` (and the move
   constructor is defined!).

-  It is only the ownership that moves. Whether any machine code is
   generated to manipulate the data itself is a matter of optimization,
   and such copies are aggressively optimized away.

-  Simple values (such as integers) can be marked :rust:`Copy` (see later
   slides).

-  In Rust, clones are explicit (by using :rust:`clone`).

In the :rust:`say_hello` example:

-  With the first call to :rust:`say_hello`, :rust:`main` gives up ownership of
   :rust:`name`. Afterwards, :rust:`name` cannot be used anymore within
   :rust:`main`.
-  The heap memory allocated for :rust:`name` will be freed at the end of
   the :rust:`say_hello` function.
-  :rust:`main` can retain ownership if it passes :rust:`name` as a reference
   (:rust:`&name`) and if :rust:`say_hello` accepts a reference as a parameter.
-  Alternatively, :rust:`main` can pass a clone of :rust:`name` in the first
   call (:rust:`name.clone()`).
-  Rust makes it harder than C++ to inadvertently create copies by
   making move semantics the default, and by forcing programmers to make
   clones explicit.

--------------------------------
Defensive Copies in Modern C++
--------------------------------

Modern C++ solves this differently:

.. code:: cpp

   std::string s1 = "Cpp";
   std::string s2 = s1;  // Duplicate the data in s1.

-  The heap data from :rust:`s1` is duplicated and :rust:`s2` gets its own
   independent copy.
-  When :rust:`s1` and :rust:`s2` go out of scope, they each free their own
   memory.

Before copy-assignment:

.. code:: bob

    Stack                             Heap
   .- - - - - - - - - - - - - -.     .- - - - - - - - - - - -.
   :                           :     :                       :
   :    s1                     :     :                       :
   :   +-----------+-------+   :     :   +----+----+----+    :
   :   | ptr       |   o---+---+--+--+-->| C  | p  | p  |    :
   :   | len       |     3 |   :     :   +----+----+----+    :
   :   | capacity  |     3 |   :     :                       :
   :   +-----------+-------+   :     :                       :
   :                           :     `- - - - - - - - - - - -'
   `- - - - - - - - - - - - - -'

After copy-assignment:

.. code:: bob

    Stack                             Heap
   .- - - - - - - - - - - - - -.     .- - - - - - - - - - - -.
   :                           :     :                       :
   :    s1                     :     :                       :
   :   +-----------+-------+   :     :   +----+----+----+    :
   :   | ptr       |   o---+---+--+--+-->| C  | p  | p  |    :
   :   | len       |     3 |   :     :   +----+----+----+    :
   :   | capacity  |     3 |   :     :                       :
   :   +-----------+-------+   :     :                       :
   :                           :     :                       :
   :    s2                     :     :                       :
   :   +-----------+-------+   :     :   +----+----+----+    :
   :   | ptr       |   o---+---+-----+-->| C  | p  | p  |    :
   :   | len       |     3 |   :     :   +----+----+----+    :
   :   | capacity  |     3 |   :     :                       :
   :   +-----------+-------+   :     :                       :
   :                           :     `- - - - - - - - - - - -'
   `- - - - - - - - - - - - - -'

Key points:

-  C++ has made a slightly different choice than Rust. Because :rust:`=`
   copies data, the string data has to be cloned. Otherwise we would get
   a double-free when either string goes out of scope.

-  C++ also has
   `std::move <https://en.cppreference.com/w/cpp/utility/move>`__,
   which is used to indicate when a value may be moved from. If the
   example had been :rust:`s2 = std::move(s1)`, no heap allocation would
   take place. After the move, :rust:`s1` would be in a valid but
   unspecified state. Unlike Rust, the programmer is allowed to keep
   using :rust:`s1`.

-  Unlike Rust, :rust:`=` in C++ can run arbitrary code as determined by the
   type which is being copied or moved.
