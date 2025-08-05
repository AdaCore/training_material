===============================
beamer_filter.py User's Guide
===============================

..
   This is a User's Guide for the capabilities of the "beamer_filter.py"
   python file we use to generate slide decks for course material.
   You can generate a slide deck for this RST file the same way you
   would for any of the course material.

*******
Roles
*******

------------------
Simple RST Roles
------------------

.. list-table::

   * - ``:toolname:``
     - :toolname:`GNATcheck`

   * - ``:url:``
     - :url:`https://adacore.com`

   * - ``:menu:``
     - :menu:`File`

   * - ``:command:``
     - :command:`ls -lrt`

   * - ``:dfn:``
     - :dfn:`Polymorphism`

   * - ``:filename:``
     - :filename:`/mnt/c/Users/frank/gitlab/`

---------------------
Animation RST Roles
---------------------

Use "Page Down" to see the differences

.. list-table::

   * - ``:answer:``
     - :answer:`Standard font changes to green and italic`

   * - ``:answermono:``
     - :answermono:`Monospace font changes to green and italic`

   * - ``:animate:``
     - Standard font :animate:`appears without formatting`

------------
Color Role
------------

We do allow coloring text, using a role-like mechanism.

The role is defined as "color-\*", where "\*" is replaced by the colord
you want.  The color can be any string supported by the LaTeX "xcolor"
package, but most likely one of the predefined names will be good enough.

.. list-table::

   * - black
     - blue
     - brown
     - cyan
     - darkgray
   * - gray
     - green
     - lightgray
     - lime
     - magenta
   * - olive
     - orange
     - pink
     - purple
     - red
   * - teal
     - violet
     - white
     - yellow

Some examples:

.. list-table::

   * - ``:color-magenta:``
     - :color-magenta:`This is magenta`
   
   * - ``:color-olive:``
     - :color-olive:`This is olive`

Note that some colors do not show up well on a white background.  Look
for the "latex_colorize" function in beamer_filter.py for ways to fix that.

****************
Warnings, etc.
****************

------------------------------
Callout Boxes ("Admonitions")
------------------------------

::

   .. warning::

      This is a warning box

.. warning::

   This is a warning box

::

   .. tip::

      This is a tip box

.. tip::

   This is a tip box

::

   .. note::

      This is a note box

.. note::

   This is a note box

************
Containers
************

-------
Usage
-------

A **container** is RST's way of applying some process to a block of text.
Some of them have built-in support within Pandoc, but we've created
a lot to do things like simulate PowerPoint or to simplify our process.

Containers are formatted as ``.. container:: <name> [options]``. The
content of the containers will be text that is indented under the
container description.

.. note::

   We obviously need to come up with a better rule for this than
   "sometimes."

   For now, the syntax will be called out for each container.

The following slides detail the containers we use and/or have created.

------------
admonition
------------

.. admonition:: Language Variant

   Ada 2022

``.. admonition:: Language Variant``

This particular admonition is used to add a subtitle to the slide title.
In most cases, this is used to add the language version that the slide
applies to when that version is not the "default" for the document. As
our document default is Ada 2012, you should only see "Ada 2022" as the
subtitle (as demonstrated on this slide), but you can use it for other
reasons (as in the *Future Capabilities* module).

-------------
speakernote
-------------

``.. container:: speakernote``

This is a way to add a speaker note to the slide. There is no easy way to
show this to the presenter and not the audience, so most content that would
be here are already in the slide, but they are nice to have for reference /
review.

----------------
source_include
----------------

Used to include a code block from a text file (typically a real source
code file).

``.. container:: source_include <filename> [:start-after:<start_string>] [:end-before:<end_string>] [:code:<language>] [:number-lines:<number>]``

filename
   Name of source file being included (relative to including directory).

start-after
   Start inserting on line after **start_string** was found. If not
   specified, start at beginning of file.

end-before
   Stop inserting on the line before **end_string** was found. If
   *start-after* is set, then the insertion doesn't take place until after
   that string was found.  If this value is not set, insertion goes until
   the end of the file

code
   Language for the code block. Default to Ada if not set.

number-lines
   If specified, put line numbers on code block, starting with the number
   specified.

------------------------
source_include Example
------------------------

**The following construct**

   ``.. container:: source_include beamer_filter.py :start-after:begin_ug :end-before:end_ug :code:python :number-lines:10``

**produces this code block**

.. container:: source_include beamer_filter.py :start-after:begin_ug :end-before:end_ug :code:python :number-lines:10

---------
animate
---------

This is used to have things appear one after another on the "same slide".
Beamer does this by making each page look just like the previous page but
with new content.  (Page numbers won't change, but Page Down makes it look
like something new "appeared.")

``.. container:: animate [<slide_number>[-]]``

slide_number
   This indicates which "page" of the set of pages the content will appear.
   "1" means it will be there from the start (and, as such, you don't need
   a container for it, although sometimes it makes the context easier to
   understand.) The "-" after the number indicates that the content should
   remain for the following "pages".

Note that "animate" will reserve space for all pages - otherwise the pages
would keep resizing and defeat the purpose.

-----------------
animate Example
-----------------

**The following construct generates the "animation" affect you will**
**see by pressing "Page Down" multiple times**

::

   .. container:: animate 2

      This appears on the first Page Down and then disappears
      on the next one.

   .. container:: animate 3-

      This appears on the second Page Down (notice blank line above)

**Press Page Down twice (slowly!)**

.. container:: animate 2

   This appears on the first Page Down and then disappears
   on the next one.

.. container:: animate 3-

   This appears on the second Page Down (notice blank line above)

---------
overlay
---------

Overlay is similar to "animate", except that the content appears "on top"
of already existing content. This is very useful for making diagrams
change, but it can be used for text as well

-----------------
overlay Example
-----------------

**This structure**

::

   .. container:: overlay 1

      Image

      .. image:: hierarchical_visibility_1.svg

   .. container:: overlay 2

      Image plus some color

      .. image:: hierarchical_visibility_2.svg

**produces the following overlay (Page Down to see the overlay)**

.. container:: overlay 1

   Image

   .. image:: hierarchical_visibility_1.svg

.. container:: overlay 2

   Image plus some color

   .. image:: hierarchical_visibility_2.svg

-------------------
latex_environment
-------------------

Although RST does have a "backdoor" into the underlying mechanism to
produce a document (the "raw" role), as our document is produced using
LaTeX, we have created a container to simplify some of that interface.

The "latex_environment" container is useful when you want to enclose
content in a LaTeX "environment" - something that starts with
"\begin{environment}" and ends with "\end{environment}".

The most common environment we use is for font sizes. RST has no direct way
of indicating font sizes, and Pandoc generates LaTeX pages that get resized
based on content. Wouldn't it be nice to make some text smaller or larger
first? In LaTeX, font sizes are "environments" - so we can create a block
of code to be the size we want (relatively). The available sizes can be
found in any LaTeX documentation, but they are:

.. list-table::

   * - tiny
     - scriptsize
     - footnotesize
     - small
     - normalsize
   * - large
     - Large
     - LARGE
     - huge
     - Huge

We sometimes use them to emphasize things, but more often they are used when
a code block does not resize correctly and we need to shrink it ourself.

---------------------------
latex_environment Example
---------------------------

We might have a code block that looks like this:

.. code:: Ada

   if A_Very_Long_Function_Call (With_A_Long_Parameter_Name) > Some_Constant then
      Do_Something;
   end if;

and we notice that it doesn't fit on the screen. If we insert it into a
"latex_environment", we can shrink the font size of the code.

.. container:: latex_environment scriptsize

   ::

      .. container:: latex_environment scriptsize

         .. code:: Ada

            if A_Very_Long_Function_Call (With_A_Long_Parameter_Name) > Some_Constant then
               Do_Something;
            end if;

.. container:: latex_environment scriptsize

   .. code:: Ada

      if A_Very_Long_Function_Call (With_A_Long_Parameter_Name) > Some_Constant then
         Do_Something;
      end if;

Make sure the size you use is the largest possible, because these are going
to need to be seen from a distance!
