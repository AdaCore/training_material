=========================
Caller-Provided Storage
=========================

--------------------------
Borrow the Output Buffer
--------------------------

**The caller supplies the output buffer**

.. code:: rust

  let mut scratch = [0_u16; 4];
  let sorted = sort_into(&[42, 7, 10], &mut scratch)
      .expect("buffer fits");

* The returned slice borrows :rust:`scratch`

* No allocation is needed; insufficient space is rejected before modification

* Error type returned when the supplied buffer is too small

.. code:: rust

  #[derive(Debug, Clone, Copy, PartialEq, Eq)]
  pub struct BufferTooSmall;

--------------------------
Implementing "sort_into"
--------------------------

**The function rejects oversized inputs before copying**

.. code:: rust

  pub fn sort_into<'a>(
      samples: &[u16],
      scratch: &'a mut [u16],
  ) -> Result<&'a [u16], BufferTooSmall> {
      let output = scratch
          .get_mut(..samples.len())
          .ok_or(BufferTooSmall)?;
      output.copy_from_slice(samples);
      output.sort_unstable();
      Ok(output)
  }

-----------------------
Reuse the Same Buffer
-----------------------

**The same caller-owned buffer can be used for successive calls**

.. code:: rust

  let mut scratch = [0_u16; 4];

  let first = sort_into(&[42, 7, 10], &mut scratch)
      .expect("three samples fit");
  assert_eq!(first, &[7, 10, 42]);

  let second = sort_into(&[9, 2], &mut scratch)
      .expect("two samples fit");
  assert_eq!(second, &[2, 9]);
