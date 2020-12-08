:title: Lab 7 - Packages
:subtitle: The :code:`Solar_System` API

The purpose of this exercise is to factor out the code for stars into a package.

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

From the exercise *03_subprograms*, the structures and subprograms are now declared
in an external package, called :code:`Solar_System`.

Implement the subprogram :code:`Move_All` that moves all the objects of a solar
system.

Use the :code:`Solar_System` package to obtain a behavior similar to
the exercise *03_subprograms*.

==========
Question 2
==========

Create a child package :code:`Graphics` of :code:`Solar_System` to handle graphical
functionalities like :code:`Draw_Body` procedure.

Implement in this package a procedure :code:`Draw_All` that will call
:code:`Draw_Body` for each object.

==========
Question 3
==========

Create a black hole (not visible) rotating around the sun, and create two objects
moving around that invisible point.
