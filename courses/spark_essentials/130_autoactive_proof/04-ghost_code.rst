============
Ghost Code
============

-------------------------
Intermediate Assertions
-------------------------

* Intermediate assertions can help provers

  .. code:: ada

     pragma Assert (Intermediate_Assertion_1);
     pragma Assert (Intermediate_Assertion_2);
     pragma Assert (Complex_Assertion);

* In addition, each assertion can be proven by different prover

* Intermediate assertions help prove each path separately

  .. code:: ada

     if Cond then
        pragma Assert (Assertion_1);
        return;
     end if;

     if Other_Cond then
        pragma Assert (Assertion_2);
     else
        pragma Assert (Assertion_3);
     end if;

* Intermediate assertions are essential to investigate unproved checks

------------
Ghost Code
------------

* :dfn:`Ghost code` is code meant only for verification

  - Intermediate assertions can refer to ghost entities
  - Contracts can also refer to ghost entities

* Special aspect :ada:`Ghost` used to identify ghost entities

  - Ghost functions express properties used in contracts

    .. code:: ada

       function Is_Valid (X : T) return Boolean is (...)
         with Ghost;
       procedure Proc (X : T) with Pre => Is_Valid (X);

  - Ghost variables hold intermediate values referred to in assertions

    .. code:: ada

       X_Saved : constant T := X with Ghost;
       ...
       pragma Assert (X = 3 * X_Saved);

  - But also ghost types, procedures, packages

* Ghost statements are:

  - Calls to ghost procedures
  - Assignments to ghost variables

---------------------------
Compilation of Ghost Code
---------------------------

* Ghost code compiled by GNAT

  - When using switch :command:`-gnata`
  - Or pragma :ada:`Assertion_Policy (Ghost => Check)`

* :toolname:`GNATprove` checks that ghost code has no effect

  .. code:: ada

     X_Saved : constant T := X with Ghost;
     ...
     X_Saved := X; -- ghost assignment
     X := X_Saved; -- error

* Same behavior with or without ghost code

  - Proof using ghost code
  - Even if execution without ghost code

-----------------
Ghost Functions
-----------------

* Most common ghost entities

|

* Ghost functions express properties used in contracts

  - Typically as expression functions
  - Complete the existing API with queries only for verification

|

* Ghost functions can be very costly in running time

  - If objective is not to execute them!
  - Typically when creating models of the actual types
  - e.g. using SPARK functional containers (sets, maps, etc)
  - e.g. like it is done for SPARK formal containers

-----------------
Ghost Variables
-----------------

* Local ghost variable or constant

  - Typically to store intermediate values

    + e.g. value of variable at subprogram entry

  - Also used to build useful data structure supporting proof

    .. code:: Ada

       procedure Sort (T : in out Table)
         with Post => Is_Permutation (T, T'Old)
       is
         Permutation : Index_Array := (for J in T'Range => J)
           with Ghost;
       begin

* Global ghost variable

   - Help specify and verify interprocedural properties
   - Maintain a model of a complex or private data structure
   - Specify properties over sequence of calls

------------------
Ghost Procedures
------------------

* Inlined local ghost procedure without contract

  - Used to group operations on ghost variables
  - Guarantees removal of all the code (e.g. loops, conditionals)

* Ghost procedure with contract and no effects

  - Also called :dfn:`lemma`
  - Isolates the proof that the precondition implies the postcondition
  - Proof of lemma might be full automatic

    .. code:: Ada

       procedure Lemma (X : T)
       with
         Pre  => ...,
         Post => ...;
       procedure Lemma (X : T) is null;

  - Lemma is used by calling it on relevant arguments

    .. code:: Ada

       pragma Assert (precondition-of-lemma);
       Lemma (Y);
       -- postcondition of lemma known here

---------------------
SPARK Lemma Library
---------------------

* Part of SPARK Library in :ada:`SPARK.Lemmas.<unit>`

* Mostly non-linear arithmetic lemmas

  - Generics instantiated for standard numerical types
  - On signed and modular integer arithmetic

    .. code:: Ada

       procedure Lemma_Div_Is_Monotonic
         (Val1  : Int;
          Val2  : Int;
          Denom : Pos)
       with
         Global => null,
         Pre  => Val1 <= Val2,
         Post => Val1 / Denom <= Val2 / Denom;

  - On fixed-point arithmetic (specific to GNAT)

  - On floating-point arithmetic

    + Monotonicity of operations, conversions with integer, rounding

----------------------------
SPARK Higher Order Library
----------------------------

* Higher order functions and lemmas to express:

  - mapping a function over a collection

  - folding a computation over a collection

  - summing a quantity over a collection

  - counting matches over a collection

* Over arrays in :ada:`SPARK.Higher_Order(.Fold)`

  - Fold, sum and count over arrays and matrices

  - Defined as generics to be instantiated

* Over functional containers in
  :ada:`SPARK.Containers.Functional.*.Higher_Order`

  - Available for vectors, lists, sets, maps

  - Functions for mapping, filtering, summing, counting

  - Take access-to-function parameter to apply to all collection

  - Functions and lemmas use `Higher_Order_Specialization`

-------------------------
Automatic Instantiation
-------------------------

* By default, lemma only available where called explicitly

* Annotation `Automatic_Instantiation` available on lemmas

  - Declaration of lemma must follow function declaration

  - Axiom for lemma put in proof context for calls to the function

* Can be combined with `Higher_Order_Specialization`

  * Used in SPARK Higher Order Library

