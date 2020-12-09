# Overview

This folder is a collection of modules for teaching SPARK/Ada. Each module 
is an RST file that may include files from the *labs* folder or *examples*
folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "Fundamentals of Ada" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

## Naming Scheme

The naming scheme follows the simplistic rule of ordering the modules by
number so they appear in file listings the way they should be presented.
e.g **010_overview** should appear before **020_declarations** etc.

There are some additions to this rule:

* 9xx modules are simple reference materials. Not necessary in many cases, but
some customers like these things called out in their course material

* Three digit numbers are the ordering scheme. If there is an alphabetic character
after the number (e.g. *180a*) then that is an **advanced** module that explains
more in-depth for the module *180*

