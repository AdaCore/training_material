# Overview

This folder is a collection of modules for teaching Ada. Each module
is an RST file that may include files from the *labs* folder or *examples*
folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "Ada Essentials" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

## Supplementary Labs

Supplementary Ada labs are also available through Alire, with the crates

- [`labs_radar`](https://alire.ada.dev/crates/labs_radar)
- [`labs_solar_system`](https://alire.ada.dev/crates/labs_solar_system)
- [`labs_standalone`](https://alire.ada.dev/crates/labs_standalone)

## Naming Scheme

The naming scheme uses a 3-digit prefix of all modules to indicate the general
order that modules should be presented. The module name consists of the
3-digit prefix, followed by an underscore, and the description of the module
(all lower case, words separated by an underscore.

For most module (exceptions described under "Prefix Grouping") the module
base name can be found as a subfolder (containing chapters for content) and
an RST file that uses the ".. include::" construct to list the chapters
to be included in the module. Many modules also include other versions
of this RST file, to indicate different presentations of the content.
The names of these RST files will start with the prefix and description,
followed by a hyphen and then another short description.

For example, the **240_tasking** module has four different presentations:

   * 240_tasking.rst *(basic module)*
   * 240_tasking-in_depth.rst *(add in-depth descriptions)*
   * 240_tasking-light.rst *(basic module for light tasking)*
   * 240_tasking-light_in_depth.rst **add in-depth descriptions to light tasking)*

### Prefix Grouping

All modules below 800 should be considered "normal" content. 800 and above are described below.

* Prefixes 800 - 899

   These prefixes should be used for content related to run-time packages (e.g. Ada.Text_IO).
   These are usually created for a specific customer training class, and are kept here for
   any future reference.

* Prefixes 900 - 999

   These prefixes should indicate reference materials. Typically, these modules would be a
   way to give students a central location for finding answers to simple Ada questions.

   **Note: These modules are simple RST files - there are no folders containing their contents**
