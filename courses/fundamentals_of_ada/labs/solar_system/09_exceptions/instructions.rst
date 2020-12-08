:title: Lab 9 - Exceptions
:subtitle: :code:`Solar_System` API collides!

The purpose of this exercise is to use exceptions, for some original bodies physics.

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

We will start by declaring an exception :code:`Bodies_Collision` in
:file:`solar_system.adb`.

==========
Question 2
==========

Raise this exception in the :code:`Move` procedure when a collision is detected.

Run the code, an exception should be raised soon.

==========
Question 3
==========

Catch this exception in the :code:`Move_All` procedure and invert the :code:`Speed`
of the body when this happens.
