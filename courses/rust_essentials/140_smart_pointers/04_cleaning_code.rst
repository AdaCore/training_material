===============
Cleaning Code
===============

--------------------
:rust:`Drop` Trait
--------------------

-  Called automatically by the compiler's ownership tracking

-  Prevents "Double Free" errors by controlling access to the destructor

-  Allows :rust:`drop` method to define custom cleanup logic 

.. code:: rust

  struct CustomPointer { data: String }

  impl Drop for CustomPointer {
    fn drop(&mut self) {
        println!("Cleaning up: {}", self.data);
    }
  }
  
--------------------
:rust:`Drop` Order
--------------------

-  Drops variables in reverse order of their creation (LIFO)

-  Drops outer container before the inner elements

.. code:: rust 

  fn main() {
    let a = Box::new("First");
    let b = Box::new("Second") ;
  } 
  // 'b' is dropped first, then 'Second' is freed
  // 'a' is dropped after, lastly 'First' is freed

------------------------
Explicit Early Cleanup
------------------------

-  Cannot explicitly call method from :rust:`drop` trait to free memory

   -  Rust would still try to drop the value at end of scope

.. code:: rust 

  let shoe = Box::new("shoe");

  shoe.drop(); 

:error:`error[E0040]: explicit use of destructor method`

-  Call :rust:`std::mem::drop(value)` to force immediate cleanup
  
   -  Transfer ownership of value into the function scope

.. code:: rust

  drop(shoe);  //  'shoe' is moved here and destroyed

  println!("wait for the other {} to drop", *shoe ); 
  
:error:`error[E0382]: borrow of moved value: 'shoe'`  