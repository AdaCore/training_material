==========
Closures
==========

----------
Closures
----------

Closures or lambda expressions have types which cannot be named.
However, they implement special
:url:`Fn <https://doc.rust-lang.org/std/ops/trait.Fn.html>`,
:url:`FnMut <https://doc.rust-lang.org/std/ops/trait.FnMut.html>`, and
:url:`FnOnce <https://doc.rust-lang.org/std/ops/trait.FnOnce.html>`
traits:

.. code:: rust

   fn apply_and_log(func: impl FnOnce(i32) -> i32, func_name: &str, input: i32) {
       println!("Calling {func_name}({input}): {}", func(input))
   }

   fn main() {
       let n = 3;
       let add_3 = |x| x + n;
       apply_and_log(&add_3, "add_3", 10);
       apply_and_log(&add_3, "add_3", 20);

       let mut v = Vec::new();
       let mut accumulate = |x: i32| {
           v.push(x);
           v.iter().sum::<i32>()
       };
       apply_and_log(&mut accumulate, "accumulate", 4);
       apply_and_log(&mut accumulate, "accumulate", 5);

       let multiply_sum = |x| x * v.into_iter().sum::<i32>();
       apply_and_log(multiply_sum, "multiply_sum", 3);
   }

-------------------------------------------------------
:rust:`Fn` versus :rust:`FnMut` versus :rust:`FnOnce`
-------------------------------------------------------

:rust:`Fn` (e.g. :rust:`add_3`) neither consumes nor mutates captured values.
It can be called needing only a shared reference to the closure, which
means the closure can be executed repeatedly and even concurrently.

:rust:`FnMut` (e.g. :rust:`accumulate`) might mutate captured values. The
closure object is accessed via exclusive reference, so it can be called
repeatedly but not concurrently.

If you have :rust:`FnOnce` (e.g. :rust:`multiply_sum`), you may only call it
once. Doing so consumes the closure and any values captured by move.

:rust:`FnMut` is a subtype of :rust:`FnOnce`. :rust:`Fn` is a subtype of :rust:`FnMut`
and :rust:`FnOnce`. I.e. you can use an :rust:`FnMut` wherever an :rust:`FnOnce` is
called for, and you can use an :rust:`Fn` wherever an :rust:`FnMut` or
:rust:`FnOnce` is called for.

When you define a function that takes a closure, you should take
:rust:`FnOnce` if you can (i.e. you call it once), or :rust:`FnMut` else, and
last :rust:`Fn`. This allows the most flexibility for the caller.

In contrast, when you have a closure, the most flexible you can have is
:rust:`Fn` (which can be passed to a consumer of any of the 3 closure
traits), then :rust:`FnMut`, and lastly :rust:`FnOnce`.

The compiler also infers :rust:`Copy` (e.g. for :rust:`add_3`) and :rust:`Clone`
(e.g. :rust:`multiply_sum`), depending on what the closure captures.
Function pointers (references to :rust:`fn` items) implement :rust:`Copy` and
:rust:`Fn`.

By default, closures will capture each variable from an outer scope by
the least demanding form of access they can (by shared reference if
possible, then exclusive reference, then by move). The :rust:`move` keyword
forces capture by value.

.. code:: rust

   fn make_greeter(prefix: String) -> impl Fn(&str) {
       return move |name| println!("{} {}", prefix, name);
   }

   fn main() {
       let hi = make_greeter("Hi".to_string());
       hi("Greg");
   }
