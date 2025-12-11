==============
Type Aliases
==============

----------
Aliasing
----------

-  Creates a name for another type
-  The two types can be used interchangeably

.. code:: rust

   enum OverallGeneralDir {
       Left,
       Right,
   }
   type Dir = OverallGeneralDir;

   
.. tip:: Aliases are more useful with long, complex types!
