========
Blocks
========

-------
Block
-------

- Encloses a sequence of expressions and statements within :rust:`{}`
- Each block has a **value** and a **type**
  - Determined by the last expression of the block
- Result is :rust:`()` (the unit type) if the last line ends with an instruction

.. code:: rust

    let bank = 13;
    let balance = {
        let withdraw = 10;
        println!("withdraw: {withdraw}");
        bank - withdraw
    };
    println!("balance: {balance}");

:command:`withdraw: 10`

:command:`balance: 3`

