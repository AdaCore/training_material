===========================
Advanced Access Type Safety
===========================

-----------------------------------
Elaboration-Only Dynamic Allocation
-----------------------------------

* Common in critical contexts
* Rationale:

    1. We (might) need dynamically allocated date

        - e.g. loading configuration data of unknown size

    2. Deallocations can cause leaks, corruption

        - |rightarrow| **Disallow** them entirely

    3. A dynamically allocated object will needs deallocation

        - |rightarrow| Unless it never goes out of **scope**

* |rightarrow| Allow only allocation onto globals

.. tip::

    And restrict allocations to program elaboration

--------------------------
Prevent Heap Deallocations
--------------------------

* :ada:`Ada.Unchecked_Deallocation` cannot be used anymore
* No heap deallocation is possible

    - The total number of allocations should be bounded
    - e.g. elaboration-only allocations

.. code:: Ada

    pragma Restrictions
        (No_Dependence => Unchecked_Deallocation);

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

* :ada:`R` is :ada:`limited`

    * Cannot be copied

------------------------
Idiom: Access to Subtype
------------------------

.. tip::

   :ada:`subtype` improves access-related code safety

* Subtype constraints still apply through the access type

.. code:: Ada

   type Values_T is array (Positive range <>) of Integer;
   subtype Two_Values_T is Values_T (1 .. 2);
   type Two_Values_A is access all Two_Values_T;

   function Get return Values_T is (1 => 10);

   -- O : aliased Two_Values_T := Get;
   -- Runtime FAIL: Constraint check
   O : aliased Values_T := Get; -- Single value, bounds are 1 .. 1
   -- P : Two_Values_A := O'Access;
   -- Compile-time FAIL: Bounds must statically match
