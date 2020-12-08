:title: Lab 22 - Tasking with interrupts
:subtitle: The :code:`Solar_System` API is not working anymore

The purpose of this exercise is to implement a handler on the button’s interrupt

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

Implement the :ada:`protected` object :code:`Button` in the :code:`My_Button` package.

This protected object should have an interrupt handler called
:code:`Interrupt_Handler` that will be called each time the interrupt occurs.

In addition, implement the :code:`Wait_Press` protected entry that will be used to
know when the button has been pressed.

==========
Question 2
==========

Create a task :code:`Button_Monitor` in the :code:`Solar_System.Button` child package.

This task will have to reverse the direction of the objects when the button
is pressed.

This task will have to wait on a :code:`Button.Wait_Press` protected entry that
will be accessible when a button press has been detected by the button’s interrupt
handler.
