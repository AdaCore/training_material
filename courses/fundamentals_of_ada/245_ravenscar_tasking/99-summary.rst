=========
Summary
=========

---------------
Light-Tasking
---------------

.. container:: columns

 .. container:: column

    * Everything is done by the Ada runtime

       - No OS underneath

    * Simple

       - Less than 2800 Logical SLOCs
       - Footprint for simple tasking program is 10KB

    * Static tasking model

       - Static tasks descriptors and stacks created at compile time
       - Task creation and activation is very simple
       - All tasks are created at initialization

 .. container:: column

    * Simple protected operations

       - No queuing
       - Locking/unlocking by increasing/decreasing priority

    * Complex features removed

       - Such as exception handling and propagation

    * ECSS (E-ST-40C and Q-ST-80C) qualification material
