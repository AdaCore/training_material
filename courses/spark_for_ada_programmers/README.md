# Overview

This folder is a collection of modules for teaching SPARK concepts. The
course content assumes a knowledge of Ada.
Each module is an RST file that may include files from the *labs* folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "SPARK for Ada Programmers" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

## Naming Scheme

The naming scheme uses a 3-digit prefix of all modules to indicate the general
order that modules should be presented. The module name consists of the
3-digit prefix, followed by an underscore, and the description of the module
(all lower case, words separated by an underscore.

For most module (exceptions described under "Prefix Grouping") the module
base name can be found as a subfolder (containing chapters for content) and
an RST file that uses the ".. include::" construct to list the chapters
to be included in the module.
