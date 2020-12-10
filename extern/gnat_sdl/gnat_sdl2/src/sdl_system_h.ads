pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;
with SDL_stdinc_h;
limited with SDL_render_h;

package SDL_system_h is

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

  --*
  -- *  \file SDL_system.h
  -- *
  -- *  Include file for platform specific SDL API functions
  --  

  -- Set up for C function definitions, even when using C++  
  -- Platform specific functions for Windows  
  --*
  --   \brief Set a function that is called for every windows message, before TranslateMessage()
  -- 

   type SDL_WindowsMessageHook is access procedure
        (arg1 : System.Address;
         arg2 : System.Address;
         arg3 : unsigned;
         arg4 : SDL_stdinc_h.Uint64;
         arg5 : SDL_stdinc_h.Sint64);
   pragma Convention (C, SDL_WindowsMessageHook);  -- ..\SDL2_tmp\SDL_system.h:49

   procedure SDL_SetWindowsMessageHook (callback : SDL_WindowsMessageHook; userdata : System.Address);  -- ..\SDL2_tmp\SDL_system.h:50
   pragma Import (C, SDL_SetWindowsMessageHook, "SDL_SetWindowsMessageHook");

  --*
  --   \brief Returns the D3D9 adapter index that matches the specified display index.
  --   This adapter index can be passed to IDirect3D9::CreateDevice and controls
  --   on which monitor a full screen application will appear.
  -- 

   function SDL_Direct3D9GetAdapterIndex (displayIndex : int) return int;  -- ..\SDL2_tmp\SDL_system.h:58
   pragma Import (C, SDL_Direct3D9GetAdapterIndex, "SDL_Direct3D9GetAdapterIndex");

   type IDirect3DDevice9 is null record;   -- incomplete struct

  --*
  --   \brief Returns the D3D device associated with a renderer, or NULL if it's not a D3D renderer.
  --   Once you are done using the device, you should release it to avoid a resource leak.
  --  

   function SDL_RenderGetD3D9Device (renderer : access SDL_render_h.Class_SDL_Renderer.SDL_Renderer) return access IDirect3DDevice9;  -- ..\SDL2_tmp\SDL_system.h:66
   pragma Import (C, SDL_RenderGetD3D9Device, "SDL_RenderGetD3D9Device");

  --*
  --   \brief Returns the DXGI Adapter and Output indices for the specified display index.
  --   These can be passed to EnumAdapters and EnumOutputs respectively to get the objects
  --   required to create a DX10 or DX11 device and swap chain.
  --  

   function SDL_DXGIGetOutputInfo
     (displayIndex : int;
      adapterIndex : access int;
      outputIndex : access int) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_system.h:74
   pragma Import (C, SDL_DXGIGetOutputInfo, "SDL_DXGIGetOutputInfo");

  -- Platform specific functions for Linux  
  --*
  --   \brief Sets the UNIX nice value for a thread, using setpriority() if possible, and RealtimeKit if available.
  --   \return 0 on success, or -1 on error.
  --  

  -- Platform specific functions for iOS  
  -- Platform specific functions for Android  
  --*
  --   \brief Get the JNI environment for the current thread
  --   This returns JNIEnv*, but the prototype is void* so we don't need jni.h
  --  

  --*
  --   \brief Get the SDL Activity object for the application
  --   This returns jobject, but the prototype is void* so we don't need jni.h
  --   The jobject returned by SDL_AndroidGetActivity is a local reference.
  --   It is the caller's responsibility to properly release it
  --   (using env->Push/PopLocalFrame or manually with env->DeleteLocalRef)
  --  

  --*
  --   \brief Return true if the application is running on Android TV
  --  

  --*
  --   \brief Return true if the application is running on a Chromebook
  --  

  --*
  --  \brief Return true is the application is running on a Samsung DeX docking station
  --  

  --*
  -- \brief Trigger the Android system back button behavior.
  --  

  --*
  --   See the official Android developer guide for more information:
  --   http://developer.android.com/guide/topics/data/data-storage.html
  -- 

  --*
  --   \brief Get the path used for internal storage for this application.
  --   This path is unique to your application and cannot be written to
  --   by other applications.
  --  

  --*
  --   \brief Get the current state of external storage, a bitmask of these values:
  --    SDL_ANDROID_EXTERNAL_STORAGE_READ
  --    SDL_ANDROID_EXTERNAL_STORAGE_WRITE
  --   If external storage is currently unavailable, this will return 0.
  -- 

  --*
  --   \brief Get the path used for external storage for this application.
  --   This path is unique to your application, but is public and can be
  --   written to by other applications.
  --  

  -- Platform specific functions for WinRT  
  --*
  -- *  \brief WinRT / Windows Phone path types
  --  

  --* \brief The installed app's root directory.
  --        Files here are likely to be read-only.  

  --* \brief The app's local data store.  Files may be written here  
  --* \brief The app's roaming data store.  Unsupported on Windows Phone.
  --        Files written here may be copied to other machines via a network
  --        connection.
  --     

  --* \brief The app's temporary data store.  Unsupported on Windows Phone.
  --        Files written here may be deleted at any time.  

  --*
  -- *  \brief WinRT Device Family
  --  

  --* \brief Unknown family   
  --* \brief Desktop family 
  --* \brief Mobile family (for example smartphone)  
  --* \brief XBox family  
  --*
  -- *  \brief Retrieves a WinRT defined path on the local file system
  -- *
  -- *  \note Documentation on most app-specific path types on WinRT
  -- *      can be found on MSDN, at the URL:
  -- *      http://msdn.microsoft.com/en-us/library/windows/apps/hh464917.aspx
  -- *
  -- *  \param pathType The type of path to retrieve.
  -- *  \return A UCS-2 string (16-bit, wide-char) containing the path, or NULL
  -- *      if the path is not available for any reason.  Not all paths are
  -- *      available on all versions of Windows.  This is especially true on
  -- *      Windows Phone.  Check the documentation for the given
  -- *      SDL_WinRT_Path for more information on which path types are
  -- *      supported where.
  --  

  --*
  -- *  \brief Retrieves a WinRT defined path on the local file system
  -- *
  -- *  \note Documentation on most app-specific path types on WinRT
  -- *      can be found on MSDN, at the URL:
  -- *      http://msdn.microsoft.com/en-us/library/windows/apps/hh464917.aspx
  -- *
  -- *  \param pathType The type of path to retrieve.
  -- *  \return A UTF-8 string (8-bit, multi-byte) containing the path, or NULL
  -- *      if the path is not available for any reason.  Not all paths are
  -- *      available on all versions of Windows.  This is especially true on
  -- *      Windows Phone.  Check the documentation for the given
  -- *      SDL_WinRT_Path for more information on which path types are
  -- *      supported where.
  --  

  --*
  -- *  \brief Detects the device family of WinRT plattform on runtime
  -- *
  -- *  \return Device family
  --  

  --*
  -- \brief Return true if the current device is a tablet.
  --  

   function SDL_IsTablet return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_system.h:269
   pragma Import (C, SDL_IsTablet, "SDL_IsTablet");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_system_h;
