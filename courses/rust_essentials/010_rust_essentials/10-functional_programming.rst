========================
Functional programming
========================

----------------------------------
Functional programming: Closures
----------------------------------

* In Rust, functions and closures are different
* Closures can be nested in functions, and can capture functions from their environment, which regular functions cannot

.. code:: Rust

   fn main() {
       let y = 12;
       let adder = |x| x + y;
       println!("{}", adder(12));
   }

----------------------------------
Functional programming: Closures
----------------------------------

* External variables are captured via borrow, so regular borrow rules apply!
* You can explicitly move captured values

.. code:: Rust

   use std::thread;

   fn main() {
       let list = vec![1, 2, 3];
       println!("Before defining closure: {:?}", list);

       thread::spawn(move || println!("From thread: {:?}", list))
           .join()
           .unwrap();
   }

------------------------------------------------
Functional programming: Closures and iterators
------------------------------------------------

.. code:: Rust

   fn main() {
       let v = vec![1, 2, 3, 4, 5];

       let sum = v.iter()
       .map(|el| el * el)
       .reduce(|acc, el| acc + el);

       println!("{}", sum.unwrap());

       v.iter().for_each(|el| {
           println!("{}", el);
       })
   }

* Rust has *many* methods like this on iterators

------------------------------------------------
Functional programming: Closures and iterators
------------------------------------------------

.. code:: Rust

   fn main() {
       let v = HashMap::from([
           ("one", 1),
           ("two", 2)
       ]);

       let v2: HashMap<i32, &str> =
           v.iter().map(|(x, y)| (*y, *x)).collect();

       println!("{:?}", v2);
   }

