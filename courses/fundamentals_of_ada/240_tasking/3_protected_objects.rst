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

---------
Example
---------

.. code:: Ada

   protected type Protected_Value is
      procedure Set (V : Integer);
      function Get return Integer;
   private
      Value : Integer;
   end Protected_Value;

   protected body Protected_Value is
      procedure Set (V : Integer) is
      begin
         Value := V;
      end Set;

      function Get return Integer is
      begin
         return Value;
      end Get;
   end Protected_Value;
