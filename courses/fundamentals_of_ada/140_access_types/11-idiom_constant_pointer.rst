=================
Protecting Access
=================

------------------------------
Elaboration Dynamic Allocation
------------------------------

1. Deallocations can cause leaks, corruption

    - |rightarrow| Disallow them entirely

2. A dynamically allocated object needs to be deallocated

    - |rightarrow| Unless it is global

* |rightarrow| Allow only allocation onto globals

    - And restrict them to program elaboration

--------------------------------
Constant Access at Library Level
--------------------------------

.. code:: Ada

   type Acc is access T;
   procedure Free is new Ada.Unchecked_Deallocation (T, Acc);

   A : constant Acc := new T;

* :ada:`A` is :ada:`constant`

    * Cannot be deallocated

-------------------------------
Constant Access as Discriminant
-------------------------------

.. code:: Ada

   type R (A : access T) is limited record

* :ada:`A` is :ada:`constant`

    * Cannot be deallocated

* :ada:`R` is :ada:`limited`, cannot be copied

