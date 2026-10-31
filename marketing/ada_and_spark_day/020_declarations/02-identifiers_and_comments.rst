==========================
Identifiers and Comments
==========================

-------------
Identifiers
-------------

.. image:: identifier_flow.svg

* Legal identifiers

  .. code:: Ada

    Phase2
    A
    Space_Person

* Not legal identifiers

  .. code:: Ada

    Phase2__1
    A_
    _space_person

* Supports 8-, 16-, and 32-bit characters
* Case **not significant**

  - :ada:`SpacePerson` |equivalent| :ada:`SPACEPERSON`
  - ...but **different** from :ada:`Space_Person`

*Reserved words are* **forbidden**

----------
Comments
----------

* Terminate at end of line (i.e., no comment terminator sequence)

  .. code:: Ada

     -- This is a multi-
     -- line comment
     A : B; -- this is an end-of-line comment
