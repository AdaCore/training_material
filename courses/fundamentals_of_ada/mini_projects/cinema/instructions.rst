:title: Mini Cinema
:subtitle: Two-day mini-project

The purpose of this exercise is to read and display sequence of BMP files
on the console, after having converted them to ASCII.


.. figure:: mini_projects/cinema/mini_cinema.png
    :name:

    Expected final app

==========
Question 1
==========

A unit test suite has been written for the two packages situated in the :filename:`src/sprites/`
directory. Those packages are in charge of representing Pixels, which are 4-valued objects
(Red, Green, Blue, Alpha) and Surfaces, which are two-dimensional arrays of Pixels.

You can compile and run the tests by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Sprites -XMain=UT_Sprites
    $ obj/run_test_sprites

Start by implementing the :ada:`package Sprite`, following the ``TODO`` in the specification, then in the body.
Once this is done the test(s) from :filename:`testsuite/pixels-tests.adb` should be passing.

Then implement the :ada:`package Surface`.
Once this is done, the full testsuite should be passing.

.. figure:: mini_projects/cinema/ut_success.png
    :name:

    Passing Test Suite

==========
Question 2
==========

The packages in :filename:`src/display/` are in charge of converting from a Pixel representation to a String representation.

The way they work is through the ``Char_Display_Driver.Closest`` function, which maps to a given Pixel a
String representation.

A test suite has been written for the packages contained in :filename:`src/display/`, you can compile and run it by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Display -XMain=UT_Display
    $ obj/run_test_display

Implement the following packages:

- ``Drawable_Chars``
- ``Char_Display_Drivers``

==========
Question 3
==========

The package ``BMP_File_IO`` contained in :filename:`src/format/` is in charge of reading a file in the BMP format, returning a Surface with the proper pixel values as a result.

An integration has been written for the packages, you can compile and run the integration test by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Format -XMain=Static_Image_Display_Test
    $ obj/static_image_display

Implement the package so that the test passes.

.. figure:: mini_projects/cinema/static_image_display.png
    :name:

    Passing Static Image Test (frame 3/3)

-------------------------
BMP file format mini-spec
-------------------------

A BMP file is composed of 5 data structures:

- The header
- The file info
- An (optional) compression table
- An (optional) palette
- The pixels data

Compression Header is present iif :ada:`Info.Compression /= 0` and :ada:`/= 3`
(this is the case for the files given)

Palette is present iif :ada:`Palette_Size /= 0`, this is the case for the :filename:`resources/sunset.bmp`

The palette offset is stored in the header, starting from the very beginning of the file.

Palette colors are stored as 4 bytes entries

.. list-table:: Palette color Pixel Format
    :header-rows: 1

    * - Byte 1
      - Byte 2
      - Byte 3
      - Byte 4
    * - R
      - G
      - B
      - Reserved

If there is a palette, Pixels are stored as an Index to the palette,
of size Info.Pixel_Size. This is the case for the nian cat movie.

If there is no palette, pixels are directly stored, as either
4 bytes (Pixel_Size = 32), dummy files, and for the rotating_triangle movie.
or 3 bytes (Pixel_Size = 24), :filename:`resources/sunset.bmp`

.. list-table:: BMP Pixel Format
    :header-rows: 1

    * - Pixel size
      - Byte 1
      - Byte 2
      - Byte 3
      - Byte 4
    * - 24
      - R
      - G
      - B
      -
    * - 32
      - R
      - G
      - B
      - Reserved

==========
Question 4
==========

The package in :filename:`src/movie/` is in charge of converting from a series of BMP files stored in a directory to an orderet sequence of Surfaces to be displayed.

A test has been written for it, you can compile and run it by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Movie -XMain=Movie_Test
    $ obj/simple_movie_play

Implement the ``Movies`` package.

==========
Question 5
==========

The package in :filename:`src/server/` is in charge of playing a movie by displaying its frames one by one, at a given frequency.

A test has been written for it, you can compile and run it by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Server -XMain=Server_Test
    $ obj/movie_server_test

Implement the ``Movies`` package.

==========
Question 6
==========

The main in :filename:`src/mini_cinema/` must implement a command-line interface, reading commands from the command-line prompt, and dispatching those to the movie server.
You can compile and run it by doing the following

.. code::
    :language: console

    $ gprbuild -XMode=Question_Mini_Cinema -XMain=Mini_Cinema
    $ obj/mini_cinema

Implement the ``Mini_Cinema`` procedure.

.. list-table:: Mini Cinema CLI Commands
    :header-rows: 1
    
    * - Name
      - Syntax
      - Description
    * - Pause
      - ``pause``
      - Pause the movie being played
    * - Resume Play
      - ``resume``
      - Resume play of the movie from a previous pause
    * - Exit Application
      - ``exit``
      - Exit the application and the server
    * - Stop
      - ``stop``
      - Stop playing the current movie
    * - Change Charset
      - ``char``
      - Change the charset used for rendering
    * - Speed+
      - ``fast``
      - Increase movie frame rate
    * - Speed-
      - ``slow``
      - Slow down movie frame rate
    * - Read Movie
      - ``<path to a directory>``
      - Play all the bmp files from this directory in order, as a movie
