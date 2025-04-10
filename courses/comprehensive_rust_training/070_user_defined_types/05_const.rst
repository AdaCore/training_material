===============
:rust:`const`
===============

---------------
:rust:`const`
---------------

Constants are evaluated at compile time and their values are inlined
wherever they are used:

.. code:: rust

   const DIGEST_SIZE: usize = 3;
   const FILL_VALUE: u8 = calculate_fill_value();

   const fn calculate_fill_value() -> u8 {
       if DIGEST_SIZE < 10 {
           42
       } else {
           13
       }
   }

   fn compute_digest(text: &str) -> [u8; DIGEST_SIZE] {
       let mut digest = [FILL_VALUE; DIGEST_SIZE];
       for (idx, &b) in text.as_bytes().iter().enumerate() {
           digest[idx % DIGEST_SIZE] = digest[idx % DIGEST_SIZE].wrapping_add(b);
       }
       digest
   }

   fn main() {
       let digest = compute_digest("Hello");
       println!("digest: {digest:?}");
   }

According to the
:url:`Rust RFC Book <https://rust-lang.github.io/rfcs/0246-const-vs-static.html>`
these are inlined upon use.

Only functions marked :rust:`const` can be called at compile time to
generate :rust:`const` values. :rust:`const` functions can however be called at
runtime.

:rust:`const` behaves semantically similar to C++'s :rust:`constexpr`
