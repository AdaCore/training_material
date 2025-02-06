=============
"collect"
=============

-------------
"collect"
-------------

The
`collect <https://doc.rust-lang.org/std/iter/trait.Iterator.html#method.collect>`__
method lets you build a collection from an
`Iterator <https://doc.rust-lang.org/std/iter/trait.Iterator.html>`__.

.. code:: rust

   fn main() {
       let primes = vec![2, 3, 5, 7];
       let prime_squares = primes.into_iter().map(|p| p * p).collect::<Vec<_>>();
       println!("prime_squares: {prime_squares:?}");
   }

---------
Details
---------

-  Any iterator can be collected in to a :rust:`Vec`, :rust:`VecDeque`, or
   :rust:`HashSet`. Iterators that produce key-value pairs (i.e. a
   two-element tuple) can also be collected into :rust:`HashMap` and
   :rust:`BTreeMap`.

Show the students the definition for :rust:`collect` in the standard library
docs. There are two ways to specify the generic type :rust:`B` for this
method:

-  With the "turbofish": :rust:`some_iterator.collect::<COLLECTION_TYPE>()`,
   as shown. The :rust:`_` shorthand used here lets Rust infer the type of
   the :rust:`Vec` elements.
-  With type inference:
   :rust:`let prime_squares: Vec<_> = some_iterator.collect()`. Rewrite the
   example to use this form.

-----------------
More to Explore
-----------------

-  If students are curious about how this works, you can bring up the
   `FromIterator <https://doc.rust-lang.org/std/iter/trait.FromIterator.html>`__
   trait, which defines how each type of collection gets built from an
   iterator.
-  In addition to the basic implementations of :rust:`FromIterator` for
   :rust:`Vec`, :rust:`HashMap`, etc., there are also more specialized
   implementations which let you do cool things like convert an
   :rust:`Iterator<Item = Result<V, E>>` into a :rust:`Result<Vec<V>, E>`.
-  The reason type annotations are often needed with :rust:`collect` is
   because it's generic over its return type. This makes it harder for
   the compiler to infer the correct type in a lot of cases.
