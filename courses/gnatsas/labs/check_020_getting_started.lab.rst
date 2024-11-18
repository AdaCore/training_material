-------------------------------
GNATcheck Getting Started Lab
-------------------------------

* Copy the :filename:`check_020_getting_started` folder from the course materials location

* Contents of the folder:

  * :filename:`simple.gpr` - project file
  * :filename:`include` - source directory
  * :filename:`src` - source directory
  * :filename:`coding_standard.rules` - :toolname:`GNATcheck` rules to apply during analysis

----------------------------
Preparing the Command Line
----------------------------

1. Open a command prompt window and navigate to the :filename:`check_020_getting_started` folder

2. Type :command:`gnatcheck` and press :menu:`Enter` to verify tool is on your path

    * If you see :command:`gnatcheck: No existing file to process`, you can go to the next step

    * If you see something like :command:`'gnatcheck' is not recognized as an internal or external command`

       Add the appropriate folder to your path (On Windows, typically ``C:\GNATSAS\<version>\bin`` where version is the GNAT SAS version number)

       ``set PATH=C:\GNATSAS\24.0\bin;%PATH%``

3. Type :command:`gnatcheck -h` and press :menu:`Enter` to show list of predefined rules

    * Examine the output to see what kinds of rules are available
    * The keyword at the end (*Easy*, *Medium*, *Major*) indicates the difficulty in remediating the issue

-------------------------------
Running From the Command Line
-------------------------------

.. container:: latex_environment scriptsize

   1. | Perform gnatcheck on a single file in the :filename:`src` folder
      |
      | ``gnatcheck src\room.adb -rules -from=coding_standard.rules``
      |
      |

      *Examine the output to see what parts of the code failed analysis*

   2. | Add the switch to indicate which rule caused the message
      |
      | ``gnatcheck src\room.adb --show-rule -rules -from=coding_standard.rules``
      |
      |

      *Note the actual rule now appears at the end of the message*

-------------------
Preparing the GUI
-------------------

1. Use :toolname:`GNAT Studio` to open the project :filename:`simple.gpr`

2. Set the coding standards for the project to :filename:`coding_standard.rules`

   :menu:`Edit` -> :menu:`Project Properties` -> :menu:`Switches` -> :menu:`GNATcheck`

----------------------
Running From the GUI
----------------------

.. container:: columns

  .. container:: column

    1. Perform Coding Analysis on the project

       |
       | :menu:`Analyze` -> :menu:`Coding Standard` -> :menu:`Check Root Project`
       |
       |

    2. Double-click on any source line in the **Locations** window to go to the problematic code

       |
       | Try fixing the problem and re-running the analysis
       |
       |

  .. container:: column

     .. image:: gnatcheck/results_in_gnatstudio.png




