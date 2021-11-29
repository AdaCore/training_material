----------------
Visibility Lab
----------------

* Requirements

   - Create a types package for calculating speed in miles per hour

      + At least two different distance measurements (e.g. feet, kilometers)
      + At least two different time measurements (e.g. seconds, minutes)
      + Overloaded operators and/or primitives to handle calculations

   - Create a types child package for converting distance, time, and mph into a string

      + Use :code:`Ada.Text_IO.Float_IO` package to convert floating point to string
      + Create visible global objects to set **Exp** and **Aft** parameters for :code:`Put`

   - Create a main program to enter distance and time and then print speed value

* Hints

   - :code:`use` to get full visibility to `Ada.Text_IO`
   - :code:`use type` to get access to calculations

      + :code:` use all type` if calculations are primitives

   - :code:`renames` to make using **Exp** and **Aft** easier

----------------------------------------
Visibility Lab Solution - Types
----------------------------------------

.. container:: source_include labs/answers/135_visibility.txt :start-after:--Types :end-before:--Types :code:Ada

------------------------------------------------
Visibility Lab Solution - Types.Strings
------------------------------------------------

.. container:: source_include labs/answers/135_visibility.txt :start-after:--Strings :end-before:--Strings :code:Ada

--------------------------------
Visibility Lab Solution - Main
--------------------------------

.. container:: source_include labs/answers/135_visibility.txt :start-after:--Main :end-before:--Main :code:Ada
