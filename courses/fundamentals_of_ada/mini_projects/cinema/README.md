# Instructions

The goal of the project is to build a cinema application that will read sets of
BMP files sorted in order, convert them to characters through a charset and display
those to the console in order, playing them as a movie.

# Build

This project can be entirely built using gprbuild

`$ gprbuild -XMode=<Mode> -XMain=<Main>`

If using GNAT studio, the Mode and Main can be selected from the scenario tab.

# Build Mode

There are several build modes, providing different implementations

- `Question_Sprite` (default): Build the `src/` implementation, which is incomplete and needs to be
  filled-in to work properly.

  + Run with `-XMode=Question_Sprite`

- `Question_Display`: Use the answer's implementation for all `sprite/` packages. Otherwise use the
  `src/` directory.
  This is a shortcut to start answering `UT_Display` and
  `Static_Image_Display_Test` without having implemented an `UT_Sprites` solution

  + Run with `-XMode=Question_Display`

- `Question_Format`: Use the answer's implementation for all `sprite/` and `display/` packages.
  Otherwise use the `src/` directory.
  This is a shortcut to start answering `Static_Image_Display_Test` without having implemented
  an `UT_Display` solution

  + Run with `-XMode=Question_Format`

- `Question_Movie`: Use the answer's implementation for all `sprite/`, `display/`, and `format/`
  packages.
  Otherwise use the `src/` directory.
  This is a shortcut to start answering `Movie_Test`
  without having implemented an `Static_Image_Display_Test` solution

  + Run with `-XMode=Question_Movie`

- `Question_Server`: Use the answer's implementation for all `sprite/`, `display/`, `format/`, and `movie/`
  packages. Otherwise use the `src/` directory.
  This is a shortcut to start answering `Server_Test`
  without having implemented an `Movie_Test` solution

  + Run with `-XMode=Question_Server`

- `Question_Mini_Cinema`: Use the answer's implementation for all packages, use the `src/` Mini Cinema main.
  This is a shortcut to start answering `Mini_Cinema` without having implemented the packages.

  + Run with `-XMode=Question_Mini_Cinema`

- `Answer` : Build the `answers/` directory, which contains the reference implementation, to
  which the behaviour and code of the Question implementation can be compared.

  + Run with `-XMode=Answer`

## Sources

Several source directories and files are part of the question, their name, role, and the order in which
they are recommended to "update" (filling in the TODO), is described below.

For more information about a package role and expected behaviour, consult its in-code comments, which
will provide more contextual information.

- `sprites/`

    * `Pixels` package

    This package defines Pixels structures, which are point of Red, Green, Blue (RGB)
    colors, and optionaly A (Opacity). For the sake of this implementation those RGB+A components
    are stored as 8 bits each in the ref implementation, allowing 256 levels of color.

    * `Surfaces` package

    This package organizes the pixels into "surfaces" which are col x rows of pixels representing a
    2D displayable image.

- `display/`

    * `Drawable_Char` package

    This package defines series of characters that can be sorted by a metric (e.g. luminosity) and then used
    to display an image.

    * `Char_Display_Driver` package

    This package contains a driver that is in charge of displaying series of characters to the screen as
    an image. That is the package responsible for calling the `Put` IO operations.

- `format/`

    * `BMP_File_IO` package

    This package is in charge of reading a file in the BMP format and converting it to a Surface.

- `movie/`

    * `Movies` package

    A movie is a collection of frames, that are stored as BMP files in their alphabetical order in a
    single directory.

- `server/`

    * `Movie_Server` package

    This package defines a task type that is in charge of playing a movie, by drawing the frame on the screen,
    one by one, with a certain frequency. This will be one of the last package to implement and requires that
    you think long and hard about how to implement it, as you'll have to manipulate and share limited types.

- `mini_cinema/`

    * `Mini_Cinema` procedure

    This procedure is the end of the lab, it puts all the packages and objects together to provide a full
    application.

# Applications (Main)

There are 3 levels of applications that can be run from these sets of files.
It is recommended that they are run in the order given here to progress through the exercises.
E.g. make sure all the tests in Display are PASS before moving on to running the static image
integration test.

- Unit Tests

    * Sprites: Surfaces that hold a pixels, ordered in columns and rows
      run with `-XMain=UT_Sprites`
    * Display: Convert sprites into characters, using a certain metric that
    is mapped to a "charset".
      run with `-XMain=UT_Display`

- Integration Tests

    * Static Image: Reads BMP images one by one, display them, and exit
      run with `-XMain=Static_Image_Display_Test`
    * Movie: Reads an example movie, and displays it frame-by-frame.
      run with `-XMain=Movie_Test`
    * Server: Starts a movie server, and sends pre-planned commands to it
      run with `-XMain=Server_Test`

- Final Application: Mini Cinema

Starts a mini cinema app and server, the app displays a promp and accepts several 
commands.

Run with `-XMain=Mini_Cinema`

The commands available for the ref implementation are:

        + pause: pause the movie being played
        + resume: resume play of the movie from a previous pause
        + exit: exit the application and the server
        + stop: stop playing the current movie
        + char: change the charset used for rendering
        + fast: increase movie frame rate
        + slow: slow down movie frame rate
        + <a directory>: play all the bmp files from this directory in order, as a movie

# Updating the question / answer code

Since, the question code is extracted from the answers code using adacut (installed from pip)
You will need to update the code from the `template/` directory then use the `build.sh` script to generate
the `src/` and `answers/` directories.

At the end of the build, all 6 apps for both directory should compile and run (but for the `src/` app they
would not work properly, since they are incomplete).
