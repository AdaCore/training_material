:title: Lab 6 - Subprograms
:subtitle: A Simple Comet

The purpose of this exercise is to animate the planets using subprograms.

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Implement :code:`Compute_X` and :code:`Compute_Y` function and use them in the main loop to
update :code:`X` and :code:`Y` coordinates of every objects.

==========
Question 2
==========

Implement the :code:`Move` subprogram and use it to move the objects instead of doing all
computations in the main loop.

:code:`Move` should also update the angle.

==========
Question 3
==========

Implement a procedure :code:`Draw_Body` which is a wrapper to call :code:`Draw_Sphere`.

From the main loop call :code:`Draw_Body` instead of directly drawing :code:`Draw_Sphere`.
:code:`Draw_Body` should only take 2 parameters.

==========
Question 4
==========

Add a comet in motion around the :code:`Sun`.
