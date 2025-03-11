=====================
Exercise: Fibonacci
=====================

---------------------
Fibonacci Problem
---------------------

The Fibonacci sequence begins with :rust:`[0,1]`. For n>1, the n'th
Fibonacci number is calculated recursively as the sum of the n-1'th and
n-2'th Fibonacci numbers.

Write a function :rust:`fib(n)` that calculates the n'th Fibonacci number.
When will this function panic?

.. code:: rust

   fn fib(n: u32) -> u32 {
       if n < 2 {
           // The base case.
           return todo!("Implement this");
       } else {
           // The recursive case.
           return todo!("Implement this");
       }
   }

   fn main() {
       let n = 20;
       println!("fib({n}) = {}", fib(n));
   }

---------------------
Fibonacci Solution
---------------------

.. code:: rust

   fn fib(n: u32) -> u32 {
       if n < 2 {
           return n;
       } else {
           return fib(n - 1) + fib(n - 2);
       }
   }

   fn main() {
       let n = 20;
       println!("fib({n}) = {}", fib(n));
   }
