===========================
Unconstrained Array Types
===========================

---------------------------------------
Unconstrained Array Type Declarations
---------------------------------------

* Do not specify bounds for objects
* Thus different objects of the same type may have different bounds
* Bounds cannot change once set

**Syntax**

.. code:: bnf

  unconstrained_array_definition ::= 
     type identifier is
        array(index_subtype_definition
              {, index_subtype_definition})
        of subtype_indication;
  
  index_subtype_definition ::= subtype_mark range <>

**Examples**

.. code:: Ada

    type Index is range 1 .. Integer'Last;
    type Char_Arr is array (Index range <>) of Character;

    S1 : Char_Arr(1..10);
    S2 : Char_Arr := ('A', 'B', 'C');
