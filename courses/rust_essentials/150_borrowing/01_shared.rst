===================
Borrowing a Value
===================

----------------
Why Borrowing?
----------------

- Ownership transfer is not always practical
  - Because cloning is expensive 
    - And moving data back and forth is cumbersome
- Borrowing allows access to data
  - Without taking ownership
  - Implemented using a reference
    - Denoted :rust:`&` or :rust:`&mut`

.. note::

    Borrowed data is not dropped when a reference is no longer used

---------------
Local Borrows
---------------

- Multiple references (:rust:`&T`) can read the data simultaneously
- A single reference (:rust:`&mut T`) can change the data
  - Provided "readers" are gone

.. code:: rust

    struct Sensor(i32);
    let mut scanner = Sensor(42);

    let r1 = &scanner; // First immutable borrow
    let r2 = &scanner; // Second immutable borrow
    println!("Reads: {} and {}", r1.0, r2.0);
    // 'r1' and 'r2' drop here

    let w1 = &mut scanner; // Mutable borrow
    w1.0 += 10; 
    println!("Calibrated to: {}", w1.0);


:command:`Reads: 42 and 42`

:command:`Calibrated to: 52`

--------------------------------------
Mixing Mutable and Immutable Borrows
--------------------------------------

**Guarantees memory safety at compile-time**

.. code:: rust

    struct Sensor(i32);
    let mut scanner = Sensor(42);

    // Immutable borrow starts
    let reader = &scanner;

    // Mutable borrow
    let writer = &mut scanner; // This won't compile

    println!("Read: {}, Write: {}", reader.0, writer.0);

.. container:: latex_environment tiny

    :error:`error[E0502]: cannot borrow 'scanner' as mutable because it is also borrowed as immutable`

------------------
Function Borrows
------------------

**Functions can**

- Access values safely without consuming them
- Update the original variable directly

.. code:: rust

    struct Sensor(i32);

    fn read(device: &Sensor) {
        println!("Read: {}", device.0);
    }

    fn calibrate(device: &mut Sensor) {
        device.0 += 10;
    }

    let mut scanner = Sensor(42);

    read(&scanner);            // Read-only borrow starts and ends
    calibrate(&mut scanner);   // Mutable borrow starts and ends
    read(&scanner);            // Read-only borrow starts and ends

.. note::

    As long as function calls do not overlap in their borrowing, compiler allows it

---------------------
Overlapping Borrows
---------------------

- Passing data to functions is still bound by the same rules
- Cannot call a function that requires a mutable reference 
  - If an immutable reference is still used

.. code:: rust

    struct Sensor(i32);

    fn calibrate(device: &mut Sensor) {
        device.0 += 10;
    }

    let mut scanner = Sensor(42);    
    let active_reader = &scanner; // Immutable borrow starts

    calibrate(&mut scanner); // Mutable borrow - this won't compile
    
    println!("Reader sees: {}", active_reader.0);

.. container:: latex_environment footnotesize

    :error:`[E0502]: cannot borrow 'scanner' as mutable because it is also borrowed as immutable`

--------------------------
Multiple Mutable Borrows
--------------------------

**Functions cannot receive multiple mutable references to the same data**

- Prevented by the borrow checker at compile-time

.. code:: rust

    struct Sensor(i32);

    fn sync_sensors(s1: &mut Sensor, s2: &mut Sensor) {
        s1.0 = s2.0;
    }

    let mut scanner = Sensor(42);
    sync_sensors(&mut scanner, &mut scanner); // Error

.. container:: latex_environment scriptsize

    :error:`error[E0499]: cannot borrow 'scanner' as mutable more than once at a time`

----------------
Method Borrows
----------------

**Methods can take**

- Simultaneous immutable borrows using :rust:`&self`
- Exclusive mutable borrow using :rust:`&mut self`

.. code:: rust

    struct Sensor(i32);

    impl Sensor {
        fn read(&self) -> i32 { self.0 }
        fn calibrate(&mut self) { self.0 += 10; }
    }
    
    let mut scanner = Sensor(42);
    let val = scanner.read(); // '&self' borrow, completes and drops
        
    scanner.calibrate();      // '&mut self' borrow, exclusive    
    scanner.read();           // '&self' borrow

.. note::

    :rust:`&` and :rust:`&mut` referencing handled **automatically** at the call site

--------------------------
Conflicting Self Borrows
--------------------------

- Same overlapping rules enforced for methods as for functions
- Immutable borrow prevents mutable method calls
- Immutable reference must reach final use before value can be modified

.. code:: rust

    struct Sensor(i32);

    impl Sensor {
        fn read(&self) -> i32 { self.0 }
        fn calibrate(&mut self) { self.0 += 10; }
    }

    let mut scanner = Sensor(42);
    
    let snapshot = &scanner; // Immutable borrow starts
    scanner.calibrate(); 
    println!("Reference sees: {}", snapshot.read());

.. container:: latex_environment scriptsize

    :error:`error[E0502]: cannot borrow 'scanner' as mutable because it is also borrowed as immutable`
