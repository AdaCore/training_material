=========================
Using VS Code with GNAT
=========================

--------------------
Setting up VS Code
--------------------


* Need to download and install *Ada & SPARK extensions for VS Code*

   * :url:`Visual Studio Marketplace <https://marketplace.visualstudio.com/items?itemName=AdaCore.ada>`
   * Search for **adacore** in VS Code Extensions (:menu:`Ctrl+Shift+X`)

* Make sure GNAT is installed and on your path

   * If not already downloaded, look on :url:`GitHub <https://github.com/AdaCore/ada_language_server/tree/master/integration/vscode/ada\#getting-additional-tools>`

-----------------------------
Using VS Code with the Labs
-----------------------------

.. container:: latex_environment small

  * From a command prompt

     * Navigate to the appropriate folder (:filename:`prompt` or :filename:`answer`)
     * Enter :command:`code .`

        * :menu:`Explorer` tree should show :filename:`default.gpr` and Ada file(s)

  * Set GPR file via **Settings**

     * :menu:`File` |rightarrow| :menu:`Preferences` |rightarrow| :menu:`Settings`

     * In **Settings** window, switch to :menu:`Workspace` tab,
       click :menu:`Extensions/Ada & SPARK/Project` and enter :filename:`default.gpr`

.. image:: vscode_set_project.jpg
   :width: 50%

------------------
Building the Lab
------------------

* Use VS Code predefined task :command:`ada: Build current project`

   * Go to the **Command Palette** (:menu:`Ctrl+Shift+B`)
   * Search for :menu:`ada` commands - you're looking for
     :menu:`ada: Build current project`
   * Press :menu:`Enter` to run the task

      * Select :menu:`View` |rightarrow| :menu:`Problems` to see build output

