------------------
Access Types Lab
------------------

(Simplified) Music Playlist Device

  * Overview

    * Create a playlist storage application to store and play songs

  * Goal

    * User should be able to add an "infinite" number of songs
    * At a minimum, user should be able:

      * See what's playing
      * Skip to the next song
      * Go back to the previous song
      * Shuffle the playlist
      * Print the playlist

----------------------
Project Requirements
----------------------

* Song Storage

  * We want to use doubly-linked lists

    * Arrays have size constraints
    * *Skip* and *Backup* indicate two directions of traversal
    * Note: There are existing containers to do this, but this is a learning exercise!

* Operations

  * In a doubly-linked list, implementing *Skip* and *Backup* should be trivial

  * *Shuffle* may take some effort

    * *Hint - it probably requires a second list*
    * Look into :ada:`GNAT.Random_Numbers` to help with a random shuffle

---------------------------------------------
Access Types Lab Solution - Playlist (Spec)
---------------------------------------------

.. container:: source_include labs/answers/adv_140_access_types.txt :start-after:--Playlist_Spec :end-before:--Playlist_Spec :code:Ada

----------------------------------
Access Types Lab Solution - Main
----------------------------------

.. container:: source_include labs/answers/adv_140_access_types.txt :start-after:--Main :end-before:--Main :code:Ada

---------------------------------------------
Access Types Lab Solution - Playlist (Body)
---------------------------------------------

.. container:: source_include labs/answers/adv_140_access_types.txt :start-after:--Playlist_Creation :end-before:--Playlist_Creation :code:Ada

---------------------------------------------
Access Types Lab Solution - Playlist (Cont)
---------------------------------------------

.. container:: source_include labs/answers/adv_140_access_types.txt :start-after:--Playlist_Manipulation :end-before:--Playlist_Manipulation :code:Ada

