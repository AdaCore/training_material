=========
Aspects
=========

-------------------
What Are Aspects?
-------------------

* :dfn:`Aspects` attach metadata or special behavior to declarations

  * Introduced in Ada 2012 
  * Replace *pragmas* and *representation clauses*

* Provide a clean, declarative way to specify properties such as

  * Initialization
  * Representation details
  * Contract conditions (pre/post)

* Benefits include

  * **Readability**: Integrated directly into declarations
  * **Maintainability**: Keeps semantics close to what they modify

..
  language_version 2012

----------------------------
Aspect Clauses for Objects
----------------------------

* An :dfn:`aspect clause` attaches a property or behavior (the *aspect*) to a program entity

  * Influences how it is represented, checked, or executed.

  .. container:: source_include 020_declarations/syntax.bnf :start-after:aspect_clauses_begin :end-before:aspect_clauses_end :code:bnf

* Which allows us to update our object syntax

  .. container:: source_include 020_declarations/syntax.bnf :start-after:aspect_clause_example_begin :end-before:aspect_clause_example_end :code:bnf

* Example

  .. code:: Ada

    CR1 : Control_Register with
          Volatile,
          Size    => 8,
          Address => To_Address (16#DEAD_BEEF#);

.. note::

  Aspect clauses are used for many other entities besides objects,
  and we will show some of them in other modules

..
  language_version 2012

--------------------
Specifying Aspects
--------------------

* Aspects always have a value (or definition)

  .. code:: Ada

    Message  : Integer with Size => 8;
    Register : Integer with Volatile => True;
    
* But boolean aspects can assume **True**

  .. code:: Ada

    Register : Integer with Volatile;

  * No aspect |rightarrow| **False**

    .. code:: Ada

      Register : Integer; -- not volatile!

-----------------------
In the Olden Days ...
-----------------------

* Prior to Ada 2012 there were other mechanisms

  **Pragma**

    .. code:: Ada

      Register : Integer;
      pragma Volatile (Register);

  **Representation clause**
  
    .. code:: Ada

      Message  : Integer;
      for Message'Size use Integer;

* These are still available in Ada 2012 and beyond

  * But they are separating the entity from the property
