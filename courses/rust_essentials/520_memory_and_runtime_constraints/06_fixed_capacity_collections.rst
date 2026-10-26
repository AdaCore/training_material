============================
Fixed-Capacity Collections
============================

-------------------------------------------
An Owned Collection Without Buffer Growth
-------------------------------------------

* Add under the existing ``[dependencies]`` table in :filename:`Cargo.toml`

.. code:: toml

  heapless = { version = "=0.9.3", default-features = false }

* Add this import to :filename:`src/memory.rs`

.. code:: rust

  use heapless::Vec as FixedVec;

* :rust:`FixedVec<T, N>` contains space for :rust:`N` elements within the value

* Its length changes; its capacity does not grow

* These operations do not require a heap allocator

.. note::

  :rust:`heapless` is an external crate; it is not part of :rust:`core`

------------------------------------
Full Capacity Is an Ordinary Error
------------------------------------

* Example: a :rust:`heapless::Vec` with capacity for two elements

.. code:: rust

  use heapless::Vec;

  let mut values: Vec<u16, 2> = Vec::new();
  assert_eq!(values.push(10), Ok(()));
  assert_eq!(values.push(42), Ok(()));
  assert_eq!(values.push(7), Err(7));
  assert_eq!(values.as_slice(), &[10, 42]);

* A failed :rust:`push` returns the value that could not be inserted

* The buffer stays the same size and its contents are preserved

.. note::

  A full collection has reached its limit, not necessarily the heap limit

--------------------------------
Return a Bounded, Owned Result
--------------------------------

* Add to :filename:`src/memory.rs`

.. code:: rust

  pub fn bounded_sorted<const N: usize>(
      samples: &[u16],
  ) -> Result<FixedVec<u16, N>, BufferTooSmall> {
      let mut sorted = FixedVec::new();
      for &sample in samples {
          sorted.push(sample).map_err(|_| BufferTooSmall)?;
      }
      sorted.as_mut_slice().sort_unstable();
      Ok(sorted)
  }

* :rust:`bounded_sorted::<4>` owns up to four samples; no output borrow is needed

* Extra samples produce :rust:`Err(BufferTooSmall)`

--------------------------------------
A Bounded Destination for Formatting
--------------------------------------

* Add to :filename:`src/memory.rs`

.. code:: rust

  pub fn maximum_report<const N: usize>(
      value: u16,
  ) -> Result<heapless::String<N>, core::fmt::Error> {
      use core::fmt::Write as _;

      let mut text = heapless::String::new();
      write!(&mut text, "Maximum: {value}")?;
      Ok(text)
  }

* :rust:`maximum_report::<16>(42)` returns :rust:`Ok` containing :rust:`"Maximum: 42"`

* Capacity :rust:`N` counts UTF-8 bytes, not characters

* Too little capacity returns a formatting error

.. note::

  No heap-backed :rust:`String` is needed

-----------------------------------
Heapless Does Not Mean Stack-Only
-----------------------------------

* Inline storage follows the value that contains it

  * A local value, a :rust:`static` item, or a heap-allocated owner

* A fixed-capacity container may still contain allocating element types

.. note::

  Heapless backing storage is not a whole-program allocation guarantee

---------------------------------
What Does Fixed Capacity Bound?
---------------------------------

* :rust:`heapless::Vec<u16, 4>`

  * Four inline :rust:`u16` elements

* :rust:`heapless::Vec<alloc::string::String, 4>`

  * Four string handles; text buffers may allocate (requires :rust:`alloc`)

* :rust:`heapless::Vec<heapless::String<16>, 4>`

  * Four inline strings, each with up to 16 bytes of text

.. note::

  Check both the container and its element types
