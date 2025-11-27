==============
Type Aliases
==============

--------------
Type Aliases
--------------

-  A type alias creates a name for another type
-  The two types can be used interchangeably

.. code:: rust

   enum OverallGeneralDirection {
       Left,
       Right,
   }
   type Direction = OverallGeneralDirection;

   
.. tip:: Aliases are more useful with long, complex types !