=================
System Boundary
=================

--------------------------
Volatile Variables (1/2)
--------------------------

* Volatile variable is identified by aspect :ada:`Volatile`

  - Either on the variable or its type
  - Aspect :ada:`Atomic` implies :ada:`Volatile`

* :toolname:`GNATprove` assumes that volatile variable may change value

  - Each read gives a different value
  - Even if read is preceded by a write

.. code:: ada

   Object  : Integer := 42 with Volatile;
   Value1  : Integer := Object;
   Value2  : Integer := Object;
   pragma Assert (Value1 = 42);     -- unprovable
   pragma Assert (Value1 = Value2); -- unprovable

--------------------------
Volatile Variables (2/2)
--------------------------

* Volatile variable typically has its address specified

  .. code:: ada

     Object : T with
       Volatile,
       Address =>
         System.Storage_Elements.To_Address (16#CAFECAFE#);

* A volatile variable can only occur in a :dfn:`non-interfering context`

  - On either side of an assignment

    + As whole variable or as prefix when accessing a component

  - But not as part of a more complex expression

  .. code:: ada

     Object := Object + 1; -- illegal

     Tmp : Integer := Object;
     Object := Tmp + 1; -- legal

-----------------------
Volatility Properties
-----------------------

* Four different properties of volatile variables in SPARK

  - :ada:`Async_Readers` - asynchronous reader may read the variable
  - :ada:`Async_Writers` - asynchronous write may write to the variable
  - :ada:`Effective_Reads` - reading the variable changes its value
  - :ada:`Effective_Writes` - writing the variable changes its value

|

* Each is a Boolean aspect of volatile variables

  - By default a volatile variable has all four set to :ada:`True`
  - When one or more are set explicitly, others default to :ada:`False`

----------------------------------
Volatility Properties - Examples
----------------------------------

* A sensor (program input) has aspect

  - :ada:`Async_Writers => True`

|

* An actuator (program output) has aspect

  - :ada:`Async_Readers => True`

|

* A machine register (single data) has aspects

  - :ada:`Effective_Reads => False`
  - :ada:`Effective_Writes => False`

|

* A serial port (stream of data) has aspects

  - :ada:`Effective_Reads => True`
  - :ada:`Effective_Writes => True`

--------------------
Volatile Functions
--------------------

* Some volatile variables can be read in functions

  - When :ada:`Async_Writers` and :ada:`Effective_Reads` are set to :ada:`False`
  - These correspond to program outputs

* :dfn:`Volatile functions` can read volatile inputs

  - When :ada:`Async_Writers` is set to :ada:`True`
  - Function needs to have the aspect :ada:`Volatile_Function`

* Functions (even volatile ones) cannot read some volatile variables

  - When :ada:`Effective_Reads` is set to :ada:`True`
  - A read is a side-effect, which is forbidding in SPARK functions
  - Unless the function has aspect :ada:`Side_Effects`

* A call to a volatile function must appear in a non-interfering context

  - Same as a read of a volatile variable

----------------
External State
----------------

* Abstract state may have volatile variables as constituents

  - Abstract state needs to have aspect :ada:`External`

|

* An external state is subject to the four volatility properties

  - All volatility properties set to :ada:`True` by default
  - Specific properties can be specified like for volatile variables
  - An external state with :ada:`Prop` set to :ada:`False` can only have

    + Non-volatile constituents
    + Volatile constituents with :ada:`Prop` set to :ada:`False`

|

* Special case for external state always initialized

  - An external state with :ada:`Async_Writers` set to :ada:`True`
  - The asynchronous writer is responsible for initialization

---------------------------------------
Effect of Volatility on Flow Analysis
---------------------------------------

* A variable with :ada:`Effective_Reads` set to :ada:`True`

  - Has its value influenced by conditions on branches where read happens

  .. code:: ada

     Object : Integer := 42 with Volatile, Effective_Reads;
     if Cond then
        Value := Object;
     end if;
     -- value of Object here depends on Cond

* A variable with :ada:`Effective_Writes` set to :ada:`True`

  - Never triggers a warning on unused assignment

  .. code:: ada

     Object : Integer := 42 with Volatile, Effective_Writes;
     Object := 1; -- previous assignment is not useless

-------------------------------
Effect of Volatility on Proof
-------------------------------

* A variable is :dfn:`effectively volatile for reading` if

  - It has :ada:`Async_Writers` set to :ada:`True`
  - Or it has :ada:`Effective_Reads` set to :ada:`True`

* The value of such a variable is never known

* Same for external state with these volatility properties

.. code:: ada

   Object : Integer := 42 with Volatile, Async_Readers;
   pragma Assert (Object = 42); -- proved

   Object : Integer := 42 with Volatile, Async_Writers;
   Value  : Integer := Object;
   pragma Assert (Value = 42); -- unprovable

