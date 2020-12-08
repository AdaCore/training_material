:title: Lab 29 - Interfaces
:subtitle: The :code:`Solar_System` API is not working anymore

The purpose of this exercise is to write a driver for the STM32F4
RNG (Random Number Generator) module

.. figure:: Labs/Solar_System/05_1.png
    :height: 300px
    :name:

    Expected result

==========
Question 1
==========

The section 24 of the STM32F4 datasheet contains the description of the hardware
Random Number Generator peripheral.

Use this description to write code that will map this peripheral and its associated
registers (24.4 section) in the :code:`STM32_SVD.RNG` package. Don’t forget to use the Ada
constructs specifically designed for this (records, representation clauses etc.).

==========
Question 2
==========

Implement the missing subprograms in the :code:`STM32.RNG` package in
order to fully control the RNG module.
All the documentation needed is present in the 6.3.6 and
24.3 sections of the STM32F4 datasheet.

==========
Question 3
==========

Now that we have all the subprograms needed to enable/disable the RNG module,
implement the missing subprograms in :code:`STM32.RNG.Interrupts` to have the
RNG module working via interrupts.

==========
Question 4
==========

The purpose of this question is to implement a :code:`T_Change_Color_To_Random`
task that will set a random color of all the solar system bodies, every 2 seconds.

This task should preempt all the running tasks.

In order to achieve that, implement a :code:`Random_Color_Timing_Event` type that
will derive from the :ada:`Ada.Real_Time.Timing_Event` type and its associated
:code:`Random_Color_Timing_Event_PO`.
This protected object will be responsible for synchronizing the timing event
with the :code:`T_Change_Color_To_Random` task via a
:code:`Wait_On_Random_Color_Timing_Event` entry.
