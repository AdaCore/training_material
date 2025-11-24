# Overview

This folder is a collection of modules for teaching Ada. Each module
is an RST file that may include files from the *labs* folder or *examples*
folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "Ada Essentials" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

The file **ada95_course.txt** contains a list of all the modules that
we expect to find in a typical "Ada Essentials" course specifically
geared towards Ada 95.

## Supplementary Labs

Supplementary Ada labs are also available through Alire, with the crates

- [`labs_radar`](https://alire.ada.dev/crates/labs_radar)
- [`labs_solar_system`](https://alire.ada.dev/crates/labs_solar_system)
- [`labs_standalone`](https://alire.ada.dev/crates/labs_standalone)

## Naming Conventions

The default naming conventions for modules are defined in the Style
Guide at the top level of this repository. The following describe
rules specific to this folder.

All modules below 800 should be considered "normal" content. 800 and above are described below.

* Prefixes 800 - 899

   These prefixes should be used for content related to run-time packages (e.g. Ada.Text_IO).
   These are usually created for a specific customer training class, and are kept here for
   any future reference.

* Prefixes 900 - 999

   These prefixes should indicate reference materials. Typically, these modules would be a
   way to give students a central location for finding answers to simple Ada questions.

   **Note: These modules are simple RST files - there are no folders containing their contents**
