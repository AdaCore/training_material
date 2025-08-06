===================
Protected Objects
===================

-------------------
Protected Objects
-------------------

* **Multitask-safe** accessors to get and set state
* **No** direct state manipulation
* **No** concurrent modifications
* :ada:`limited` types (No copies allowed)

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
    - In that case, :ada:`function` are **blocking** as well

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

.. code:: Ada

    protected O is
       function Get return Integer;
       procedure Set (V : Integer);
    private
       Val, Access_Count : Integer := 0;
    end O;

    protected body O is
       function Get return Integer is
       begin
          Access_count := Access_Count + 1;
          return Val;
       end Get;

       procedure Set (V : Integer) is
       begin
          Access_count := Access_Count + 1;
          Val := V;
       end Set;
    end O;

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

   protected P is
      procedure Initialize (V : Integer);
      procedure Increment;
      function Decrement return Integer;
      function Query return Integer;
   private
      Object : Integer := 0;
   end P;

Which completion(s) of :ada:`P` is (are) illegal?

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

   A. Legal
   B. Legal - subprograms do not need parameters
   C. Functions in a protected object cannot modify global objects
   D. Legal
