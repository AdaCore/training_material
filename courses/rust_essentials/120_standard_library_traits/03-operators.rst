===========
Operators
===========

----------------------
Operator Overloading
----------------------

* Rust uses traits in :rust:`std::ops` to overload operators (e.g., :rust:`+`, :rust:`-`, :rust:`*`)

  * Operators delegate to trait methods (:rust:`Add`, :rust:`Sub`, etc.)

.. container:: latex_environment footnotesize

  .. list-table::
     :widths: 10 20 70
     :header-rows: 1

     * - Operator
       - Trait
       - Method Signature
     * - ``+``
       - ``Add``
       - ``fn add(self, rhs: Rhs) -> Self::Output``
     * - ``-``
       - ``Sub``
       - ``fn sub(self, rhs: Rhs) -> Self::Output``
     * - ``*``
       - ``Mul``
       - ``fn mul(self, rhs: Rhs) -> Self::Output``
     * - ``/``
       - ``Div``
       - ``fn div(self, rhs: Rhs) -> Self::Output``
     * - ``%=``
       - ``RemAssign``
       - ``fn rem_assign(&mut self, rhs: Rhs)``
     * - ``!``
       - ``Not``
       - ``fn not(self) -> Self::Output``

.. note::

  :rust:`Self::Output` indicates return type defined by the implementation

-------------------
Overloading "Add"
-------------------

**Define an implementation for** :rust:`Inches` **that returns a number in** :rust:`Feet`

.. code:: rust

  struct Feet(f64);
  struct Inches(f64);

  impl std::ops::Add for Inches {
      type Output = Feet;

      fn add(self, rhs: Self) -> Self::Output {
          Feet((self.0 + rhs.0) / 12.0)
      }
  }

  let measure1 = Inches(10.0);
  let measure2 = Inches(32.0);
    
  println!("{} inches + {} inches", measure1.0, measure2.0);
    
  let feet = measure1 + measure2;
  println!("= {} feet", feet.0);

:command:`10 inches + 32 inches`

:command:`= 3.5 feet`
