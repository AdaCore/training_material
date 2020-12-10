pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with Interfaces.C.Strings;
limited with SDL_video_h;

package SDL_messagebox_h is

  --  Simple DirectMedia Layer
  --  Copyright (C) 1997-2018 Sam Lantinga <slouken@libsdl.org>
  --  This software is provided 'as-is', without any express or implied
  --  warranty.  In no event will the authors be held liable for any damages
  --  arising from the use of this software.
  --  Permission is granted to anyone to use this software for any purpose,
  --  including commercial applications, and to alter it and redistribute it
  --  freely, subject to the following restrictions:
  --  1. The origin of this software must not be misrepresented; you must not
  --     claim that you wrote the original software. If you use this software
  --     in a product, an acknowledgment in the product documentation would be
  --     appreciated but is not required.
  --  2. Altered source versions must be plainly marked as such, and must not be
  --     misrepresented as being the original software.
  --  3. This notice may not be removed or altered from any source distribution.
  -- 

  -- For SDL_Window  
  -- Set up for C function definitions, even when using C++  
  --*
  -- * \brief SDL_MessageBox flags. If supported will display warning icon, etc.
  --  

  --*< error dialog  
  --*< warning dialog  
  --*< informational dialog  
   subtype SDL_MessageBoxFlags is unsigned;
   SDL_MESSAGEBOX_ERROR : constant unsigned := 16;
   SDL_MESSAGEBOX_WARNING : constant unsigned := 32;
   SDL_MESSAGEBOX_INFORMATION : constant unsigned := 64;  -- ..\SDL2_tmp\SDL_messagebox.h:42

  --*
  -- * \brief Flags for SDL_MessageBoxButtonData.
  --  

  --*< Marks the default button when return is hit  
  --*< Marks the default button when escape is hit  
   subtype SDL_MessageBoxButtonFlags is unsigned;
   SDL_MESSAGEBOX_BUTTON_RETURNKEY_DEFAULT : constant unsigned := 1;
   SDL_MESSAGEBOX_BUTTON_ESCAPEKEY_DEFAULT : constant unsigned := 2;  -- ..\SDL2_tmp\SDL_messagebox.h:51

  --*
  -- *  \brief Individual button data.
  --  

  --*< ::SDL_MessageBoxButtonFlags  
   type SDL_MessageBoxButtonData is record
      flags : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_messagebox.h:58
      buttonid : aliased int;  -- ..\SDL2_tmp\SDL_messagebox.h:59
      text : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_messagebox.h:60
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MessageBoxButtonData);  -- ..\SDL2_tmp\SDL_messagebox.h:61

   --  skipped anonymous struct anon_68

  --*< User defined button id (value returned via SDL_ShowMessageBox)  
  --*< The UTF-8 button text  
  --*
  -- * \brief RGB value used in a message box color scheme
  --  

   type SDL_MessageBoxColor is record
      r : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_messagebox.h:68
      g : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_messagebox.h:68
      b : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_messagebox.h:68
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MessageBoxColor);  -- ..\SDL2_tmp\SDL_messagebox.h:69

   --  skipped anonymous struct anon_69

   type SDL_MessageBoxColorType is 
     (SDL_MESSAGEBOX_COLOR_BACKGROUND,
      SDL_MESSAGEBOX_COLOR_TEXT,
      SDL_MESSAGEBOX_COLOR_BUTTON_BORDER,
      SDL_MESSAGEBOX_COLOR_BUTTON_BACKGROUND,
      SDL_MESSAGEBOX_COLOR_BUTTON_SELECTED,
      SDL_MESSAGEBOX_COLOR_MAX);
   pragma Convention (C, SDL_MessageBoxColorType);  -- ..\SDL2_tmp\SDL_messagebox.h:79

  --*
  -- * \brief A set of colors to use for message box dialogs
  --  

   type SDL_MessageBoxColorScheme_colors_array is array (0 .. 4) of aliased SDL_MessageBoxColor;
   type SDL_MessageBoxColorScheme is record
      colors : aliased SDL_MessageBoxColorScheme_colors_array;  -- ..\SDL2_tmp\SDL_messagebox.h:86
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MessageBoxColorScheme);  -- ..\SDL2_tmp\SDL_messagebox.h:87

   --  skipped anonymous struct anon_71

  --*
  -- *  \brief MessageBox structure containing title, text, window, etc.
  --  

  --*< ::SDL_MessageBoxFlags  
   type SDL_MessageBoxData is record
      flags : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_messagebox.h:94
      window : access SDL_video_h.Class_SDL_Window.SDL_Window;  -- ..\SDL2_tmp\SDL_messagebox.h:95
      title : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_messagebox.h:96
      message : Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_messagebox.h:97
      numbuttons : aliased int;  -- ..\SDL2_tmp\SDL_messagebox.h:99
      buttons : access constant SDL_MessageBoxButtonData;  -- ..\SDL2_tmp\SDL_messagebox.h:100
      colorScheme : access constant SDL_MessageBoxColorScheme;  -- ..\SDL2_tmp\SDL_messagebox.h:102
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_MessageBoxData);  -- ..\SDL2_tmp\SDL_messagebox.h:103

   --  skipped anonymous struct anon_72

  --*< Parent window, can be NULL  
  --*< UTF-8 title  
  --*< UTF-8 message text  
  --*< ::SDL_MessageBoxColorScheme, can be NULL to use system settings  
  --*
  -- *  \brief Create a modal message box.
  -- *
  -- *  \param messageboxdata The SDL_MessageBoxData structure with title, text, etc.
  -- *  \param buttonid The pointer to which user id of hit button should be copied.
  -- *
  -- *  \return -1 on error, otherwise 0 and buttonid contains user id of button
  -- *          hit or -1 if dialog was closed.
  -- *
  -- *  \note This function should be called on the thread that created the parent
  -- *        window, or on the main thread if the messagebox has no parent.  It will
  -- *        block execution of that thread until the user clicks a button or
  -- *        closes the messagebox.
  --  

   function SDL_ShowMessageBox (messageboxdata : access constant SDL_MessageBoxData; buttonid : access int) return int;  -- ..\SDL2_tmp\SDL_messagebox.h:119
   pragma Import (C, SDL_ShowMessageBox, "SDL_ShowMessageBox");

  --*
  -- *  \brief Create a simple modal message box
  -- *
  -- *  \param flags    ::SDL_MessageBoxFlags
  -- *  \param title    UTF-8 title text
  -- *  \param message  UTF-8 message text
  -- *  \param window   The parent window, or NULL for no parent
  -- *
  -- *  \return 0 on success, -1 on error
  -- *
  -- *  \sa SDL_ShowMessageBox
  --  

   function SDL_ShowSimpleMessageBox
     (flags : SDL_stdinc_h.Uint32;
      title : Interfaces.C.Strings.chars_ptr;
      message : Interfaces.C.Strings.chars_ptr;
      window : access SDL_video_h.Class_SDL_Window.SDL_Window) return int;  -- ..\SDL2_tmp\SDL_messagebox.h:133
   pragma Import (C, SDL_ShowSimpleMessageBox, "SDL_ShowSimpleMessageBox");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_messagebox_h;
