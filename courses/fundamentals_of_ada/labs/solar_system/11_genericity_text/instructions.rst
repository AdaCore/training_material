:title: Lab 11 - Genericity
:subtitle: Introduction to generics

The purpose of this exercise is to genericize some existing algorithms.

==========
Question 1
==========

The :code:`Swaps` package contains a :code:`Swap` procedure that takes
:ada:`Integer` parameters.

Try to genericize it as much as you can by creating a :code:`Swap_Generics` package
with a generic :code:`Swap` procedure

==========
Question 2
==========

The :code:`Sorts` package provides types and procedures used to sort lists of :ada:`Integers`.

Create a generic :code:`Sort_Generics` package that provide sorting capabilities for lists
containing any scalar type.

==========
Question 3
==========

Modifythe :code:`Sort_Generics` package to allow sorting capabilities for lists containing
any type (including non-scalar types).

Find a way to resolve the problem that comes with the use of :ada:`'Image` in
:code:`Sort_Generics.Display_List`.
