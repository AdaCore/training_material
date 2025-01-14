========
Macros
========

---------------
Macro example
---------------

.. code:: Rust

   #[macro_export]
   macro_rules! vec {
       ( $( $x:expr ),* ) => {
           {
               let mut temp_vec = Vec::new();
               $(
                   temp_vec.push($x);
               )*
               temp_vec
           }
       };
   }

--------
Macros
--------

* Macros allow you to extend your language's syntax and semantics, by extending what it's able to do at compile time.

* Bad macros work on text (like C-like macros)

* Good macros work on structured input

    - In LISP, it worked on lists
    - In Rust, it works on ASTs, token trees, or token streams.

--------------------
When to use macros
--------------------

* Never
* Never

----------------------
But seriously though
----------------------

* Macros are used to:

    - Abstract common repetitive programming patterns
    - Embed domain specific languages
    - Provide lazy evaluation

* Generally: Macros are a last resort. Anything that you can solve another way shouldn't be fixed with macros.

---------------------------
The rust parsing pipeline
---------------------------

* First, tokenization: Turn string into tokens.

* From there, Rust can either:

   1. Produce a syntax tree from tokens
   2. Produce a token tree that your macro can match upon

--------------------
Declarative macros
--------------------

* Like the `vec` one.
* You use a form of pattern-matching on the arguments
* Used to provide function-like macros

------------------------
Declarative macros (2)
------------------------

.. code:: Rust

   macro_rules! ok_or_return {
       ($e:expr, $err:expr) => {
           {
               match $e {
                   Ok(value) => value,
                   Err(_) => return Err($err)
               }
           }
       }
   }

   fn main() -> Result<(), &'static str> {
       let mut line = String::new();
       ok_or_return!(std::io::stdin().read_line(&mut line), "Cannot read line"); // including '\n'
       let a = ok_or_return!(line.trim().parse::<i32>(), "Cannot parse string");
       Ok(())
   }

-----------------------------------------
Declarative macros - Variadic arguments
-----------------------------------------

.. code:: Rust

   macro_rules! vec_strs {
       (
           // Start a repetition:
           $(
               $element:expr // Each repeat must contain an expression...
           )
           , // ...separated by commas...
           * // ...zero or more times.
       ) => {
           // Enclose the expansion in a block so that we can use
           // multiple statements.
           {
               let mut v = Vec::new();
               // Start a repetition:
               $(
                   // Each repeat will contain the following statement, with
                   // $element replaced with the corresponding expression.
                   v.push(format!("{}", $element));
               )*
               v
           }
       };
   }

---------
Hygiene
---------

.. code:: C

   #define INCI(i) do { int a=0; ++i; } while (0)
   int main(void)
   {
       int a = 4, b = 8;
       INCI(a);
       INCI(b);
       printf("a is now %d, b is now %d\n", a, b);
       return 0;
   }

------------------
Hygiene and Rust
------------------

.. code:: Rust

   macro_rules! using_a {
       ($e:expr) => {
           {
               let a = 42; 
               $e
           }
       }
   }

   let four = using_a!(a / 10); // Won't work

-------------------
Procedural macros
-------------------

.. code:: Rust

   use proc_macro::TokenStream;

   #[proc_macro]
   pub fn tlborm_fn_macro(input: TokenStream) -> TokenStream {
       input
   }
