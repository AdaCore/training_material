===================
Protected Objects
===================

-----------------------------------------
The Problem: Sharing Data is Dangerous!
-----------------------------------------

* What happens if two tasks try to update the same variable at the exact same time?

  * Task 1 reads the value X (it's 10)
  * Task 2 reads the value X (it's also 10)
  * Task 1 calculates 10 + 5 and writes 15 back to X
  * Task 2 calculates 10 + 1 and writes 11 back to X

* The first update is lost! 

  * This is a :dfn:`race condition`

* Race condition

  * Leads to corrupt and unpredictable data
  * Protected objects prevent concurrent modifications

---------------------------------
The Solution: Protected Objects
---------------------------------

* Protected object is designed for safe, concurrent access to shared data

  * Acts as a monitor, guarding the data it holds
  * Has a restricted set of operations

    * Can't manipulate its data directly

  * Guarantees concurrency-safe semantics

    * Prevents concurrent modifications from corrupting data

-------------------------------------
Protected: Functions and Procedures
-------------------------------------

* A :ada:`function` can **get** the state

   - **Multiple-Readers**
   - Protected data is **read-only**
   - Concurrent call to :ada:`function` is **allowed**
   - **No** concurrent call to :ada:`procedure`

* A :ada:`procedure` can **set** the state

   - **Single-Writer**
   - **No** concurrent call to either :ada:`procedure` or :ada:`function`
   - In case of concurrency, other callers get **blocked**

      - Until call finishes

------------------------------------------
Example: Protected Objects - Declaration
------------------------------------------

.. include:: ../examples/protected_objects/src/protected_objects.ads
    :code: Ada

-----------------------------------
Example: Protected Objects - Body
-----------------------------------

.. include:: ../examples/protected_objects/src/protected_objects.adb
    :code: Ada

------
Quiz
------

.. container:: latex_environment footnotesize

  .. code:: Ada

    protected Counter is
       procedure Initialize (V : Integer);
       procedure Increment;
       function Decrement return Integer;
       function Query return Integer;
    private
       Object : Integer := 0;
    end Counter;

Which completion(s) of :ada:`Counter` is (are) illegal?

 .. container:: latex_environment footnotesize

   A. |  ``procedure Initialize (V : Integer) is``
      |  ``begin``
      |     ``Object := V;``
      |  ``end Initialize;``
   B. |  ``procedure Increment is``
      |  ``begin``
      |     ``Object := Object + 1;``
      |  ``end Increment;``
   C. |  :answermono:`function Decrement return Integer is`
      |  :answermono:`begin`
      |     :answermono:`Object := Object - 1;`
      |     :answermono:`return Object;`
      |  :answermono:`end Decrement;`
   D. |  ``function Query return Integer is begin``
      |     ``return Object;``
      |  ``end Query;``

.. container:: animate

   A. Legal - Assignment to protected data allowed in procedure
   B. Legal - subprograms do not need parameters
   C. Functions in a protected object cannot modify protected data
   D. Legal - Reading of protected data allowed in function
