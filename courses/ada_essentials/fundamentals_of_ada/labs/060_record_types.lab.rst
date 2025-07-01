========
Lab
========

------------------
Record Types Lab
------------------

* Requirements

   - Create a simple First-In/First-Out (FIFO) queue record type and object
   - Allow the user to:

      + Add ("push") items to the queue
      + Remove ("pop") the next item to be serviced from the queue (Print this item to ensure the order is correct)

   - When the user is done manipulating the queue, print out the remaining items in the queue

* Hints

   - Queue record should at least contain:

      + Array of items
      + Index into array where next item will be added

.. container:: speakernote

   Queue could contain pointer to next item, or user could "slide" array down once an item has been removed
   Sliding may be easier, but expensive for large arrays

-----------------------------------------
Record Types Lab Solution - Declarations
-----------------------------------------

.. container:: source_include labs/answers/060_record_types.txt :start-after:--Declarations :end-before:--Declarations :code:Ada :number-lines:1

--------------------------------------------
Record Types Lab Solution - Implementation
--------------------------------------------

.. container:: source_include labs/answers/060_record_types.txt :start-after:--Implementation :end-before:--Implementation :code:Ada :number-lines:17
