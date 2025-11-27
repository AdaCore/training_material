===================
Blocks
===================

--------
Block
--------

- A block contains a sequence of expressions and statements enclosed by braces :rust:`{}`
- Each block has a value and a type, those of the last expression of the block
- When the last expression, the resulting value is :rust:`()`

.. code:: rust

    let z = 13;
    let x = {
        let y = 10;
        println!("y: {y}");
        z - y
    };
    println!("x: {x}");

.. container:: speakernote

  You can show how the value of the block changes by changing the last
  line in the block. For instance, adding/removing a semicolon or using
  a :rust:`return`.
