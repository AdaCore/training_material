===================
Blocks
===================

--------
Block
--------

- Encloses a sequence of expressions and statements within :rust:`{}`
- Each block has a **value** and a **type**
  - determined by the last expression of the block
- Result is :rust:`()` (the unit type) if the last line ends with an instruction

.. code:: rust

    let bank = 13;
    let cash = {
        let withdraw = 10;
        println!("withdraw: {withdraw}");
        bank - withdraw
    };
    println!("cash: {cash}");

.. container:: speakernote

  You can show how the value of the block changes by changing the last
  line in the block. For instance, adding/removing a semicolon or using
  a :rust:`return`.
