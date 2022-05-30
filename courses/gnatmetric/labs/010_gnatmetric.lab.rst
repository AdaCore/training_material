-------------------------------
GNATmetrics Lab - GNAT Studio
-------------------------------

* Perform GNATmetric analysis on :ada:`Queens` application from :filename:`source` directory

* Find the following information

  - Number of code lines in :ada:`Queens_Pkg.Test`

    .. container:: animate

      - **Underneath** :ada:`Queens_Pkg` **click** :ada:`Test` **to see metrics**
      - **Code Lines is 16**

  - Average complexity in Lab project

    .. container:: animate

      - **Click** :filename:`Lab` **folder icon**
      - **Average Complexity is 5.3**

--------------------------------
GNATmetrics Lab - Command Line
--------------------------------

* Perform GNATmetric analysis on :ada:`Queens` application from a command prompt

* First, just use the default command and see what you get

  .. container:: animate

    - :command:`gnatmetric -P lab.gpr`
    - **Metrics are displayed on the console**

* Then, put detailed metrics into a :filename:`output` folder

  .. container:: animate

    - :command:`gnatmetric -P lab.gpr --output-dir=output`
    - **Metrics are displayed on the console and .metrix file is built for each source file**

* Next, stop outputting to the console (all the information is in :filename:`output`)

  .. container:: animate

    - :command:`gnatmetric -P lab.gpr --output-dir=output --no-text-output`
    - **Not much displayed on the console and .metrix file is built for each source file**

