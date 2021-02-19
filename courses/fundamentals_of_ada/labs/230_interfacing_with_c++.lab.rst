--------------------------
Interfacing with C++ Lab
--------------------------

* Requirements

   - Create a simplistic I/O package

      + Read from standard input
      + Write to standard output
      + Create / Write to / Close a file

   - Implement I/O package bodies using C functions

      + Use `Interfaces.C` hierarchy for C-style strings

   - Main program should ask user for a file name, and then ask for data to be placed into the file

      - No use of `Ada.Text_IO` in this lab

* Hints

   .. code:: C++

      char *gets(char *str);
      int puts(const char *str);
      FILE *fopen(const char *filename, const char *mode);
      int fputs(const char *str, FILE *stream);
      int fclose(FILE *stream);

------------------------------------------------
Interfacing with C++ Lab Solution - I/O (Spec)
------------------------------------------------

.. container:: source_include labs/answers/230_interfacing_with_c++.txt :start-after:--IO_Spec :end-before:--IO_Spec :code:Ada

------------------------------------------------
Interfacing with C++ Lab Solution - I/O (Body)
------------------------------------------------

.. container:: source_include labs/answers/230_interfacing_with_c++.txt :start-after:--IO_Body :end-before:--IO_Body :code:Ada

------------------------------------------
Interfacing with C++ Lab Solution - Main
------------------------------------------

.. container:: source_include labs/answers/230_interfacing_with_c++.txt :start-after:--Main :end-before:--Main :code:Ada
