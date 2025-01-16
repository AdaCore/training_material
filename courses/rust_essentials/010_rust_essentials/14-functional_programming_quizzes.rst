================================
Functional Programming Quizzes
================================

--------------------------------------
Quiz 1: Is This a Compilation Error?
--------------------------------------

.. code:: Rust
   :number-lines: 2

   fn main() {
       let mut y = 12;
       let adder = |x| x + y;
       y = 15;
       println!("{}", adder(12));
   }

.. container:: animate

   :color-red:`error[E0506]: cannot assign to 'y' because it is borrowed --> src/quiz.rs:5:8`

   TBD

--------------------------------------
Quiz 2: Is This a Compilation Error?
--------------------------------------

.. code:: Rust
   :number-lines: 2

   use std::cell::RefCell;

   fn main() {
       let y = RefCell::new(12);
       let adder = |x| x + *y.borrow();
       *y.borrow_mut() = 15;
       println!("{}", adder(12));
   }

.. container:: animate

   :color-green:`No error`

--------------------------------------
Quiz 3: Is This a Compilation Error?
--------------------------------------

.. code:: Rust
   :number-lines: 2

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

.. container:: animate

   :color-red:`error[E0597]: 'val' does not live long enough --> src/quiz.rs:9:43`

   :color-red:`error[E0382]: borrow of moved value: 'v' --> src/quiz.rs:15:24`

   TBD

--------------------------------------
Quiz 4: Is This a Compilation Error?
--------------------------------------

.. code:: Rust
   :number-lines: 2

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

.. container:: animate

   :color-green:`No error`
