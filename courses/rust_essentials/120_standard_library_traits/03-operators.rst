===========
Operators
===========

----------------------
Operator Overloading
----------------------

* Traits in :rust:`std::ops` are used to overload operators

  * E.g., :rust:`+`, :rust:`-`, :rust:`*`

  * Operators are delegated to trait methods (:rust:`Add`, :rust:`Sub`, etc.)

.. container:: latex_environment footnotesize

  .. list-table::
     :widths: 10 20 70
     :header-rows: 1

     * - **Operator**
       - **Trait**
       - **Method Signature**
     * - :rust:`+`
       - :rust:`Add`
       - :rust:`fn add(self, rhs: Rhs) -> Self::Output`
     * - :rust:`-`
       - :rust:`Sub`
       - :rust:`fn sub(self, rhs: Rhs) -> Self::Output`
     * - :rust:`*`
       - :rust:`Mul`
       - :rust:`fn mul(self, rhs: Rhs) -> Self::Output`
     * - :rust:`/`
       - :rust:`Div`
       - :rust:`fn div(self, rhs: Rhs) -> Self::Output`
     * - :rust:`%=`
       - :rust:`RemAssign`
       - :rust:`fn rem_assign(&mut self, rhs: Rhs)`
     * - :rust:`!`
       - :rust:`Not`
       - :rust:`fn not(self) -> Self::Output`

.. note::

  :rust:`Self::Output` indicates return type defined by the implementation

------------------------
Overload "Add" Example
------------------------

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
