---------------------------------
Advanced Exception Analysis Lab
---------------------------------

* This lab uses a simplistic application to load and print a department store inventory.

* The source code has no error handling, so any problem will cause a crash

* The purpose of this lab is to use *Advanced Exception Analysis* to identify and/or handle errors

* Use :toolname:`GNAT Studio` to open source project (:filename:`default.gpr`)

* Build :ada:`Main`

---------------------------
Prevent Application Crash
---------------------------

* Run executable

  ::

    obj\main.exe
    raised CONSTRAINT_ERROR: a-comlin.adb:61 explicit raise

  * (Application needs a filename on the command line)

* First attempt: we want :ada:`Main` to capture any exceptions and display information

  * Add an exception handler to :ada:`Main` and run it again

--------------------------------------
Prevent Application Crash - Solution
--------------------------------------

* Code modifications

  .. container:: latex_environment footnotesize
  
    .. code:: Ada
      :number-lines: 9

      exception
         when The_Err : others =>
            Put_Line ("ERROR: " & Exception_Information (The_Err));

* Output

  .. container:: latex_environment footnotesize

    ::

      obj\main.exe
      ERROR: raised CONSTRAINT_ERROR : a-comlin.adb:61 explicit raise

----------------------------------
Get Better Exception Information
----------------------------------

* Run executable passing in the supplied filename :filename:`file.txt`

.. container:: latex_environment footnotesize

  ::

    obj\main.exe file.txt
    ERROR: raised CONSTRAINT_ERROR : bad input for 'Value: "$39"

* Update the project properties to get stack trace information

---------------------------------------------
Get Better Exception Information - Solution
---------------------------------------------

.. columns::

  .. column::

    * From the GUI

      .. image:: advanced_exception_analysis/properties_dialog.jpg
        :width: 50%

    * In the GPR file:

      .. container:: latex_environment tiny

        ::

          package Binder is
            for Switches("Ada") use ("-Es");
         end Binder;

  .. column::

    .. container:: latex_environment tiny

      ::

        obj\main.exe file.txt
        ERROR: raised CONSTRAINT_ERROR : bad input for 'Value: "$39"
        [C:\temp\advanced_exception_analysis\obj\main.exe]
        0x7ff7964d721d ada__finalization___assign__3 at ???
        0x7ff7964d2642 ada__finalization___assign__3 at ???
        0x7ff7964d2840 ada__finalization___assign__3 at ???
        0x7ff7964d2b54 ada__finalization___assign__3 at ???
        0x7ff7964a3ad9 Numbers.Convert at numbers.adb:34
        0x7ff7964a4a26 Parser.Load at parser.adb:33
        0x7ff7964a1762 Main at main.adb:7
        0x7ff7964a1fe4 Main at b__main.adb:254
        0x7ff7964a1423 __tmainCRTStartup at ???
        0x7ff7964a113b mainCRTStartup at ???
        [C:\Windows\System32\KERNEL32.DLL]
        0x7ffedfa37032
        [C:\Windows\SYSTEM32\ntdll.dll]
        0x7ffedffc264f

----------------------
How Did We Get Here?
----------------------

* We know our problems are coming from parsing the input file

  * Add an exception handler to the :ada:'Parser.Load` loop

    * Just print the exception raised and allow the loop to continue

  * Print the stack in the :ada:`Convert` routines

    * Only the routines that convert *from* strings will be problematic

---------------------------------
How Did We Get Here? - Solution
---------------------------------

* :ada:`Parser.Load`

  .. code:: Ada

    loop
       declare
          Pieces  : Strings_T := Split (Get_Line (File));
          Element : Element_T;
       begin
          Element.Line_Number       := Integer (Line (File) - 1);
          Element.Category          := Convert (Pieces (1).all);
          Element.Description       := Pieces (2);
          Element.Quantity          := Convert (Pieces (3).all);
          Element.Cost              := Convert (Pieces (4).all);
          Database_Count            := Database_Count + 1;
          Database (Database_Count) := Element;
       exception
          when The_Err : others =>
             Put_Line ("Load failure: " & Exception_Name (The_Err));
       end;
    end loop;

* :ada:`Convert` routines (example)

  .. code:: Ada

    function Convert
      (Number : String)
       return Quantity_T is
    begin
       Put_Line
         ("Convert(Quantity_T): " & Number & "> " &
          Gnat.Traceback.Symbolic.Symbolic_Traceback_No_Hex
            (Gnat.Traceback.Call_Chain (3, 2)));
       return Quantity_T'value (Number);
    end Convert;

-------------------------------
How Did We Get Here? - Output
-------------------------------

::

  obj\main.exe file.txt
  Convert(Category_T): groceries> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:31
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Quantity_T): 11> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:33
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Cost_T) 12.34> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:34
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Category_T): clothing> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:31
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Quantity_T): 1> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:33
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Cost_T) 33> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:34
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Category_T): home_goods> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:31
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Quantity_T): 2> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:33
  Main at main.adb:7
  Main at b__main.adb:257

  Convert(Cost_T) $39> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:34
  Main at main.adb:7
  Main at b__main.adb:257

  Load failure: CONSTRAINT_ERROR
  Convert(Category_T): sporting_goods> [C:\temp\advanced_exception_analysis\obj\main.exe]
  Parser.Load at parser.adb:31
  Main at main.adb:7
  Main at b__main.adb:257
