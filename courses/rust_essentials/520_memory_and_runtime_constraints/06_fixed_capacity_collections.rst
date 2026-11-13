============================
Fixed-Capacity Collections
============================

-----------------------------------
Fixed Capacity vs. Static Storage
-----------------------------------

* :rust:`static` specifies program-duration storage

* Fixed capacity limits the maximum number of elements

  * Collection can be local, static, or part of another value

* :rust:`heapless::Vec<T, N>` contains storage for :rust:`N` elements inline

  * Length can change up to :rust:`N`

  * Capacity does not grow

* No heap allocator is needed to manage its backing storage

.. code:: toml

  [dependencies]
  heapless = { version = "=0.9.3", default-features = false }

.. code:: rust

  use heapless::Vec as FixedVec;

.. note::

  :rust:`heapless` is an external crate, not part of :rust:`core`

------------------------------------
Full Capacity Is an Ordinary Error
------------------------------------

**A full fixed-capacity vector rejects additional elements**

.. code:: rust

  use heapless::Vec as FixedVec;

  let mut values: FixedVec<u16, 2> = FixedVec::new();
  assert_eq!(values.push(10), Ok(()));
  assert_eq!(values.push(42), Ok(()));
  assert_eq!(values.push(7), Err(7));
  assert_eq!(values.as_slice(), &[10, 42]);

* A failed :rust:`push` returns the value that could not be inserted

* Failed :rust:`push` leaves the collection's length and contents unchanged

.. warning::

  At full capacity, :rust:`push` fails even with free heap memory

--------------------------------
Return a Bounded, Owned Result
--------------------------------

**A bounded result can report insufficient capacity**

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

* :rust:`bounded_sorted::<4>` returns up to four samples by value

* Extra samples produce :rust:`Err(BufferTooSmall)`

--------------------------------------
A Bounded Destination for Formatting
--------------------------------------

**Formatting can use a fixed-capacity string**

.. code:: rust

  pub fn maximum_report<const N: usize>(
      value: u16,
  ) -> Result<heapless::String<N>, core::fmt::Error> {
      use core::fmt::Write as _;

      let mut text = heapless::String::new();

      // Propagate an error if the formatted text does not fit
      write!(&mut text, "Maximum: {value}")?;
      Ok(text)
  }

* :rust:`maximum_report::<16>(42)` returns :rust:`Ok` containing :rust:`"Maximum: 42"`

* Capacity :rust:`N` counts UTF-8 bytes, not characters

* Too little capacity returns a formatting error

---------------------------------
What Does Fixed Capacity Bound?
---------------------------------

* Fixed capacity bounds element count, not necessarily memory use

* :rust:`heapless::Vec<u16, 4>`

  * Four inline :rust:`u16` elements

* :rust:`heapless::Vec<std::string::String, 4>`

  * Four string handles; text buffers may allocate

* :rust:`heapless::Vec<heapless::String<16>, 4>`

  * Four inline strings, each with up to 16 bytes of text

.. note::

  Check both the container and its element types
