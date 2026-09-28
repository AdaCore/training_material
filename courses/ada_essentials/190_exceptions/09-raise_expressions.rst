===================
Raise Expressions
===================

---------------------
"Raise Expressions"
---------------------

* **Expression** raising specified exception **at run-time**

.. code:: Ada
  :font-size: small

    Foo : constant Integer := (case X is
                                when 1 => 10,
                                when 2 => 20,
                                when others => raise Error);

..
  language_version 2012

