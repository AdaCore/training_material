# Overview

This folder is a collection of modules for teaching SPARK/Ada. Each module 
is an RST file that may include files from the *labs* folder or *examples*
folder.

The file **standard_course.txt** contains a list of all the modules that
we expect to find in a typical "Fundamentals of Ada" course. If you need
to modify the list of included modules, this is the file you would change
(but don't check it into MASTER!)

## Naming Scheme

The naming scheme uses a 3-digit prefix, followed by an optional alphabetic
character, and then the description of the module (all lower case, words
separated by "_").

The 3-digit prefix is described below. The optional alphabetic character is used
to indicate that the module is an advanced version of its "parent" module, to be
used as the situation requires. For example, *060_record_types* contains the
basic module for explaining record types, while *060a_discriminated_record_types*
contains an in-depth view of variant records.

### Prefix Grouping

The numeric value of the file prefix should be grouped as follows:

#### Prefixes 000 - 399

These are the topics that should (almost) always be included in the Fundamentals of Ada
course. They should only be excluded when the class has no need for the topic (e.g.
when coding standards preclude tasks you could remove the Tasking module) or, if
enough other modules have been added to make time an issue (e.g. removing a slightly
less important topic like overloading)

These prefixes should ordered as they should be presented, to make generation of the
training material easier. Individual trainers / classes may require a modified order,
but the base order makes the most sense for most situations

#### Prefixes 400 - 699

These should be modules that may have interest to a specific customer or situation.
Numeric order does not matter. For example, a module explaining the behavior of
`Ada.Finalization` 

#### Prefixes 700 - 899

These should be reserved for future use. In time, the SPARK modules may use these 
numbers

#### Prefixes 900 - 999

These prefixes should indicate reference materials. Typically, these modules would be a
way to give students a central location for finding answers to simple Ada questions.
