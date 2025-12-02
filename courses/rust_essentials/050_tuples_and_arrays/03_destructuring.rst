============================
Patterns and Destructuring
============================

------------------------
What is Destructuring?
------------------------

  - **Convenient** data access
  - **Breaking down** a complex data structure into components
    - like a tuple, an array, or other compound types
  - **Assigning** those components to individual variables
    - in a single step!

-----------------------
Destructuring a Tuple
-----------------------

  - Clarity and readability: **meaningful names**! 
    - instead of :rust:`person_data.0`, etc
  - Simplicity: extract multiple values in just **one line of code**
  - Skipping uneeded value: ignore **not needed** elements
    - with the wildcard pattern (:rust:`_`)

.. code:: rust

  let person_data = ("Renoir", 33, "Painter");
  let (name, _, profession) = person_data; 
  println!("Name: {name}, {profession}");

----------------------------------
Irrefutable Patterns with Tuples
----------------------------------

  - Irrefutable tuple pattern = **guaranteed match**
  - Used in :rust:`let` statements
    - :rust:`let` bindings must always succeed
    - the pattern must cover all components

.. code:: rust

  let point: (i32, i32) = (10, 20);
  
  // The pattern (x, y) is IRREFUTABLE
  // It perfectly matches the structure of that tuple
  let (x, y) = point; 
  // We're guaranteed to get an x and a y!

--------------------------
Destructuring Assignment
--------------------------

  - Destructuring can be used as an assignment operation
  - Assigned to variables that were already declared
  - Target variables must be declared as mutable

.. code:: rust

  let mut cat_snacks = 0;
  let mut dog_treats = 0;
  (cat_snacks, dog_treats) = (42, 1);

------------------------
Destructuring an Array
------------------------

  - Assign the array to a pattern that specifies names for each element
  - Pattern must exactly match the array's length and type
  - Compile-time error occurs if it has fewer or more elements

.. code:: rust

  // Destructuring an array into three separate variables
  let bag: [i32; 3] = [10, 20, 30];
  let [shirts, pants, socks] = array;
  println!("shirts: {}", shirts);
  println!("pants: {}", pants);
  println!("socks: {}", socks);

-------------------
Ignoring Elements 
-------------------

  - Ignore specific elements using the underscore (:rust:`_`) 

.. code:: rust

  let colors = ["red", "green", "blue", "yellow"];
  // Destructuring only the second and fourth elements
  let [_, second, _, fourth] = colors;

  println!("Second color: {}", second); // green
  println!("Fourth color: {}", fourth); // yellow

-------------------------
Using ".." for the Rest
-------------------------

  - Ignore multiple elements using the **rest pattern** (:rust:`..`)
  - Useful when you only need elements from the beginning or end
  - Must be placed at the **beginning** or at the **end**
  - Can only be used **once**

.. code:: rust

  let data = [1, 2, 3, 4, 5, 6];
  // Get the first two elements, ignore the rest
  let [first, second, ..] = data;
  println!("First: {}", first);
  println!("Second: {}", second);

----------------------
Nested Destructuring
----------------------

  - Use a pattern within a pattern to destructure an array of arrays

.. code:: rust

  let matrix = [[1, 2], [3, 4]];
  // Destructures the outer array AND the inner array
  let [[a, b], [c, d]] = matrix; 
  // a=1, b=2, c=3, d=4
