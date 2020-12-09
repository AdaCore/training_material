:title: Lab 22 - Tasking with Ravenscar
:subtitle: The :code:`Solar_System` API with tasking

The purpose of this exercise is to use Ravenscar tasks instead of one
sequential thread.

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Use the code from the privacy exercise and create a protected type
:code:`Body_P` wrapping a record :code:`Body_T`.

==========
Question 2
==========

Modify :code:`Bodies_Array_T` to be an array of :ada:`protected` object of
type :code:`Body_P`.

==========
Question 3
==========

Create an array of tasks where each task is in charge of moving one object.
