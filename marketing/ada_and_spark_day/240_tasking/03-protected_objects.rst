===================
Protected Objects
===================

-------------------------------------
Sharing Data with Protected Objects
-------------------------------------

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

--------------------------
Protected Object Example
--------------------------

.. code:: ada

   protected Object is
      procedure Set (Prompt : String; V : Integer);
      function Get (Prompt : String) return Integer;
   private
      Local : Integer := 0;
   end Object;

   protected body Object is

      procedure Set (Prompt : String; V : Integer) is
         Str : constant String := "Set " & Prompt & V'Image;
      begin
         Local := V;
         Put_Line (Str);
      end Set;

      function Get (Prompt : String) return Integer is
         Str : constant String := "Get " & Prompt & Local'Image;
      begin
         Put_Line (Str);
         return Local;
      end Get;

   end Object;
