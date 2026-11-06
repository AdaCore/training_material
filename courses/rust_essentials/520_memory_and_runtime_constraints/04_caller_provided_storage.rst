=========================
Caller-Provided Storage
=========================

------------------------------
Why Caller-Provided Storage?
------------------------------

* Previous examples create a new :rust:`Vec` to hold sorted samples

* Caller provides a fixed-capacity buffer it owns

* Borrowing the buffer allows the function to write the result into it

* No separate output allocation is needed

  * Buffer capacity limits the number of samples

----------------------------
Caller Supplies the Buffer
----------------------------

* :rust:`sort_into` sorts samples into a caller-provided buffer

  * Its implementation follows on the next slide

.. code:: rust

  let mut scratch = [0_u16; 4];
  let sorted = sort_into(&[42, 7, 10], &mut scratch)
      .expect("buffer fits");

* Caller owns :rust:`scratch` and passes a mutable reference to :rust:`sort_into`

* Returned slice :rust:`sorted` borrows from :rust:`scratch`

* No heap allocation is needed

  * Insufficient capacity is rejected before modifying the buffer

* :rust:`BufferTooSmall` represents an insufficient-capacity error

.. code:: rust

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub struct BufferTooSmall;

--------------------------
Implementing "sort_into"
--------------------------

**Reject insufficient capacity before copying**

.. code:: rust

  pub fn sort_into<'a>(
      samples: &[u16],
      scratch: &'a mut [u16],
  ) -> Result<&'a [u16], BufferTooSmall> {
      // Select enough space or return an error
      let output = scratch
          .get_mut(..samples.len())
          .ok_or(BufferTooSmall)?;

      // Copy into the caller-provided buffer
      output.copy_from_slice(samples);

      // Sort the copied elements in place
      output.sort_unstable();
      Ok(output)
  }

---------------------------
Buffer Reuse Across Calls
---------------------------

**Neither call needs to allocate a separate output buffer**

.. code:: rust

  let mut scratch = [0_u16; 4];

  let first = sort_into(&[42, 7, 10], &mut scratch)
      .expect("three samples fit");
  assert_eq!(first, &[7, 10, 42]);

  let second = sort_into(&[9, 2], &mut scratch)
      .expect("two samples fit");
  assert_eq!(second, &[2, 9]);
