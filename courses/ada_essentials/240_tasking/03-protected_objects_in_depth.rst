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

* Support for read-only locks **depends on OS**

    - Windows has **no** support for those
    - In that case, a :ada:`function` is **blocking** as well

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

------------------------
Protected: Limitations
------------------------

* **No** potentially blocking action

   - :ada:`select`, :ada:`accept`, :ada:`entry` call, :ada:`delay`, :ada:`abort`
   - :ada:`task` creation or activation
   - Some standard lib operations, eg. IO

      + Depends on implementation

* May raise :ada:`Program_Error` or deadlocks
* **Will** cause performance and portability issues
* :ada:`pragma Detect_Blocking` forces a proactive run-time detection
* Solve by deferring blocking operations

   - Using eg. a FIFO

-------------------------------------
Protected: Lock-Free Implementation
-------------------------------------

* GNAT-Specific
* Generates code without any locks
* Best performance
* No deadlock possible
* Very constrained

   - No reference to entities **outside** the scope
   - No direct or indirect :ada:`entry`, :ada:`goto`, :ada:`loop`, :ada:`procedure` call
   - No :ada:`access` dereference
   - No composite parameters
   - See GNAT RM 2.100

.. include:: ../examples/protected_objects_lock_free/extracts/protected_objects.lock_free_declare.ads
    :code: Ada

------
Quiz
------

.. code:: Ada

    protected Sensor is
       function Get return Integer;
       procedure Set (V : Integer);
    private
       Val, Access_Count : Integer := 0;
    end Sensor;

    protected body Sensor is
       function Get return Integer is
       begin
          Access_Count := Access_Count + 1;
          return Val;
       end Get;

       procedure Set (V : Integer) is
       begin
          Access_Count := Access_Count + 1;
          Val := V;
       end Set;
    end Sensor;

What is the result of compiling and running this code?

A. No error
B. :answer:`Compilation error`
C. Run-time error

.. container:: animate

    Cannot set :ada:`Access_Count` from a :ada:`function`

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

   A. Legal - Assignment to global object allowed in procedure
   B. Legal - subprograms do not need parameters
   C. Functions in a protected object cannot modify global objects
   D. Legal - Reading of global object allowed in function
