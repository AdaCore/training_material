:title: Lab 5 - Records
:subtitle: The Satellite around the Earth

The purpose of this exercise is to use records to store information about the celestial
bodies.


.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Create a record type that holds all the information needed to describe an object,
(Speed, Distance, Angle, X, Y, Radius, Color).

Replace the two arrays solution of the previous exercise by one array indexed by
:code:`Bodies_Enum` and containing components of this record.

Remark : Initialize each body using aggregate notation.

==========
Question 2
==========

Add a :code:`Turns_Around` field to the record, in order to store the object around which the
object rotates, instead of forcing it to always rotate around the previous object of the
system.

Have both the :code:`Satellite` and the :code:`Moon` turn around the :code:`Earth`.

==========
Question 3
==========

All the objects are turning counter-clockwise, make the :code:`Moon` turning clockwise.

=====================
Question 4 (Advanced)
=====================

Implement black holes.

Modify your record to handle the case of an invisible object which does not have
:code:`Radius` and :code:`Color` properties (hint: use discriminated record with a
:code:`Boolean` discriminant named :code:`Visible`).
