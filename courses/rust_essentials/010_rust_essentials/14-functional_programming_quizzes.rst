================================
Functional Programming Quizzes
================================

--------------------------------------
Quiz 1: Is This a Compilation Error?
--------------------------------------

.. code:: Rust

   fn main() {
       let mut y = 12;
       let adder = |x| x + y;
       y = 15
       println!("{}", adder(12));
   }

--------------------------------------
Quiz 2: Is This a Compilation Error?
--------------------------------------

.. code:: Rust

   use std::cell::RefCell;

   fn main() {
       let y = RefCell::new(12);
       let adder = |x| x + *y.borrow();
       *y.borrow_mut() = 15;
       println!("{}", adder(12));
   }

--------------------------------------
Quiz 3: Is This a Compilation Error?
--------------------------------------

.. code:: Rust

   use std::cell::RefCell;

   struct Adder {
       adder_fn: Box<dyn Fn(i32) -> i32>
   }

   fn create_adder(val: RefCell<i32>) -> Adder {
       Adder {adder_fn: Box::new(|x| x + *val.borrow())}
   }

   fn main() {
       let v = RefCell::new(12);
       let adder = create_adder(v);
       println!("{}", *v.borrow());
   }

--------------------------------------
Quiz 4: Is This a Compilation Error?
--------------------------------------

.. code:: Rust

   use std::cell::RefCell;
   use std::rc::Rc;

   struct Adder {
       adder_fn: Box<dyn Fn(i32) -> i32>
   }

   fn create_adder(val: Rc<RefCell<i32>>) -> Adder {
       Adder {adder_fn: Box::new(move |x| x + *val.borrow())}
   }

   fn main() {
       let v = Rc::new(RefCell::new(12));
       let adder = create_adder(v.clone());
       println!("{}", (adder.adder_fn)(12));
       *v.borrow_mut() = 15;
       println!("{}", (adder.adder_fn)(12));
   }

