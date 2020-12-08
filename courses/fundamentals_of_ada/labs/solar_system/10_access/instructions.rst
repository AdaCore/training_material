:title: Lab 10 - Access
:subtitle: The :code:`Solar_System` API is still not working

The purpose of this exercise is to use :ada:`Access` types to avoid passing
:code:`Bodies` as in out everywhere.

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Define a type :code:`Body_Access_T` as an :ada:`access` type to all :code:`Body_T`.

==========
Question 2
==========

Implement :code:`Get_Body` as defined in the specification of the :code:`Solar_System` package.

Use :ada:`'Access` to return the access to the expected body.

==========
Question 3
==========

Modify :code:`Init_Body` procedure as defined in the specification of the
:code:`Solar_System` package.

Update initialization calls in the :code:`Main` procedure.

==========
Question 4
==========

Modify the :code:`Turns_Around` field of :code:`Body_T` type to be a :code:`Body_Access_T`.

==========
Question 5
==========

Modify :code:`Move` procedure as defined in the specification of the :code:`Solar_System` package.

It’s compiling now!
-------------------

==========
Question 6
==========

What is the problem with accessibility check? Try to fix it.

==========
Question 7
==========

How to avoid using :code:`'Unchecked_Access`?
