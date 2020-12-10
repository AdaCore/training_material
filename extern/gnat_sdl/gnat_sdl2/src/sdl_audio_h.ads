pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with System;
with Interfaces.C.Strings;
limited with SDL_rwops_h;

package SDL_audio_h is

   SDL_AUDIO_MASK_BITSIZE : constant := (16#FF#);  --  ..\SDL2_tmp\SDL_audio.h:71
   SDL_AUDIO_MASK_DATATYPE : constant := (2**8);  --  ..\SDL2_tmp\SDL_audio.h:72
   SDL_AUDIO_MASK_ENDIAN : constant := (2**12);  --  ..\SDL2_tmp\SDL_audio.h:73
   SDL_AUDIO_MASK_SIGNED : constant := (2**15);  --  ..\SDL2_tmp\SDL_audio.h:74
   --  arg-macro: function SDL_AUDIO_BITSIZE (x)
   --    return x and SDL_AUDIO_MASK_BITSIZE;
   --  arg-macro: function SDL_AUDIO_ISFLOAT (x)
   --    return x and SDL_AUDIO_MASK_DATATYPE;
   --  arg-macro: function SDL_AUDIO_ISBIGENDIAN (x)
   --    return x and SDL_AUDIO_MASK_ENDIAN;
   --  arg-macro: function SDL_AUDIO_ISSIGNED (x)
   --    return x and SDL_AUDIO_MASK_SIGNED;
   --  arg-macro: function SDL_AUDIO_ISINT (x)
   --    return notSDL_AUDIO_ISFLOAT(x);
   --  arg-macro: function SDL_AUDIO_ISLITTLEENDIAN (x)
   --    return notSDL_AUDIO_ISBIGENDIAN(x);
   --  arg-macro: function SDL_AUDIO_ISUNSIGNED (x)
   --    return notSDL_AUDIO_ISSIGNED(x);

   AUDIO_U8 : constant := 16#0008#;  --  ..\SDL2_tmp\SDL_audio.h:89
   AUDIO_S8 : constant := 16#8008#;  --  ..\SDL2_tmp\SDL_audio.h:90
   AUDIO_U16LSB : constant := 16#0010#;  --  ..\SDL2_tmp\SDL_audio.h:91
   AUDIO_S16LSB : constant := 16#8010#;  --  ..\SDL2_tmp\SDL_audio.h:92
   AUDIO_U16MSB : constant := 16#1010#;  --  ..\SDL2_tmp\SDL_audio.h:93
   AUDIO_S16MSB : constant := 16#9010#;  --  ..\SDL2_tmp\SDL_audio.h:94
   --  unsupported macro: AUDIO_U16 AUDIO_U16LSB
   --  unsupported macro: AUDIO_S16 AUDIO_S16LSB

   AUDIO_S32LSB : constant := 16#8020#;  --  ..\SDL2_tmp\SDL_audio.h:103
   AUDIO_S32MSB : constant := 16#9020#;  --  ..\SDL2_tmp\SDL_audio.h:104
   --  unsupported macro: AUDIO_S32 AUDIO_S32LSB

   AUDIO_F32LSB : constant := 16#8120#;  --  ..\SDL2_tmp\SDL_audio.h:112
   AUDIO_F32MSB : constant := 16#9120#;  --  ..\SDL2_tmp\SDL_audio.h:113
   --  unsupported macro: AUDIO_F32 AUDIO_F32LSB
   --  unsupported macro: AUDIO_U16SYS AUDIO_U16LSB
   --  unsupported macro: AUDIO_S16SYS AUDIO_S16LSB
   --  unsupported macro: AUDIO_S32SYS AUDIO_S32LSB
   --  unsupported macro: AUDIO_F32SYS AUDIO_F32LSB

   SDL_AUDIO_ALLOW_FREQUENCY_CHANGE : constant := 16#00000001#;  --  ..\SDL2_tmp\SDL_audio.h:140
   SDL_AUDIO_ALLOW_FORMAT_CHANGE : constant := 16#00000002#;  --  ..\SDL2_tmp\SDL_audio.h:141
   SDL_AUDIO_ALLOW_CHANNELS_CHANGE : constant := 16#00000004#;  --  ..\SDL2_tmp\SDL_audio.h:142
   SDL_AUDIO_ALLOW_SAMPLES_CHANGE : constant := 16#00000008#;  --  ..\SDL2_tmp\SDL_audio.h:143
   --  unsupported macro: SDL_AUDIO_ALLOW_ANY_CHANGE (SDL_AUDIO_ALLOW_FREQUENCY_CHANGE|SDL_AUDIO_ALLOW_FORMAT_CHANGE|SDL_AUDIO_ALLOW_CHANNELS_CHANGE|SDL_AUDIO_ALLOW_SAMPLES_CHANGE)

   SDL_AUDIOCVT_MAX_FILTERS : constant := 9;  --  ..\SDL2_tmp\SDL_audio.h:203
   --  unsupported macro: SDL_AUDIOCVT_PACKED __attribute__((packed))
   --  arg-macro: procedure SDL_LoadWAV (file, spec, audio_buf, audio_len)
   --    SDL_LoadWAV_RW(SDL_RWFromFile(file, "rb"),1, spec,audio_buf,audio_len)

   SDL_MIX_MAXVOLUME : constant := 128;  --  ..\SDL2_tmp\SDL_audio.h:616

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
  -- *  \file SDL_audio.h
  -- *
  -- *  Access to the raw audio mixing buffer for the SDL library.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief Audio format flags.
  -- *
  -- *  These are what the 16 bits in SDL_AudioFormat currently mean...
  -- *  (Unspecified bits are always zero).
  -- *
  -- *  \verbatim
  --    ++-----------------------sample is signed if set
  --    ||
  --    ||       ++-----------sample is bigendian if set
  --    ||       ||
  --    ||       ||          ++---sample is float if set
  --    ||       ||          ||
  --    ||       ||          || +---sample bit size---+
  --    ||       ||          || |                     |
  --    15 14 13 12 11 10 09 08 07 06 05 04 03 02 01 00
  --    \endverbatim
  -- *
  -- *  There are macros in SDL 2.0 and later to query these bits.
  --  

   subtype SDL_AudioFormat is SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_audio.h:64

  --*
  -- *  \name Audio flags
  --  

  -- @{  
  --*
  -- *  \name Audio format flags
  -- *
  -- *  Defaults to LSB byte order.
  --  

  -- @{  
  -- @}  
  --*
  -- *  \name int32 support
  --  

  -- @{  
  -- @}  
  --*
  -- *  \name float32 support
  --  

  -- @{  
  -- @}  
  --*
  -- *  \name Native audio byte ordering
  --  

  -- @{  
  -- @}  
  --*
  -- *  \name Allow change flags
  -- *
  -- *  Which audio format changes are allowed when opening a device.
  --  

  -- @{  
  -- @}  
  -- @}  
  -- Audio flags  
  --*
  -- *  This function is called when the audio device needs more data.
  -- *
  -- *  \param userdata An application-specific parameter saved in
  -- *                  the SDL_AudioSpec structure
  -- *  \param stream A pointer to the audio data buffer.
  -- *  \param len    The length of that buffer in bytes.
  -- *
  -- *  Once the callback returns, the buffer will no longer be valid.
  -- *  Stereo samples are stored in a LRLRLR ordering.
  -- *
  -- *  You can choose to avoid callbacks and use SDL_QueueAudio() instead, if
  -- *  you like. Just open your audio device with a NULL callback.
  --  

   type SDL_AudioCallback is access procedure
        (arg1 : System.Address;
         arg2 : access SDL_stdinc_h.Uint8;
         arg3 : int);
   pragma Convention (C, SDL_AudioCallback);  -- ..\SDL2_tmp\SDL_audio.h:163

  --*
  -- *  The calculated values in this structure are calculated by SDL_OpenAudio().
  -- *
  -- *  For multi-channel audio, the default SDL channel mapping is:
  -- *  2:  FL FR                       (stereo)
  -- *  3:  FL FR LFE                   (2.1 surround)
  -- *  4:  FL FR BL BR                 (quad)
  -- *  5:  FL FR FC BL BR              (quad + center)
  -- *  6:  FL FR FC LFE SL SR          (5.1 surround - last two can also be BL BR)
  -- *  7:  FL FR FC LFE BC SL SR       (6.1 surround)
  -- *  8:  FL FR FC LFE BL BR SL SR    (7.1 surround)
  --  

  --*< DSP frequency -- samples per second  
   type SDL_AudioSpec is record
      freq : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:180
      format : aliased SDL_AudioFormat;  -- ..\SDL2_tmp\SDL_audio.h:181
      channels : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_audio.h:182
      silence : aliased SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_audio.h:183
      samples : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_audio.h:184
      padding : aliased SDL_stdinc_h.Uint16;  -- ..\SDL2_tmp\SDL_audio.h:185
      size : aliased SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_audio.h:186
      callback : SDL_AudioCallback;  -- ..\SDL2_tmp\SDL_audio.h:187
      userdata : System.Address;  -- ..\SDL2_tmp\SDL_audio.h:188
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AudioSpec);  -- ..\SDL2_tmp\SDL_audio.h:178

  --*< Audio data format  
  --*< Number of channels: 1 mono, 2 stereo  
  --*< Audio buffer silence value (calculated)  
  --*< Audio buffer size in sample FRAMES (total samples divided by channel count)  
  --*< Necessary for some compile environments  
  --*< Audio buffer size in bytes (calculated)  
  --*< Callback that feeds the audio device (NULL to use SDL_QueueAudio()).  
  --*< Userdata passed to callback (ignored for NULL callbacks).  
   type SDL_AudioCVT;
   type SDL_AudioFilter is access procedure (arg1 : access SDL_AudioCVT; arg2 : SDL_AudioFormat);
   pragma Convention (C, SDL_AudioFilter);  -- ..\SDL2_tmp\SDL_audio.h:193

  --*
  -- *  \brief Upper limit of filters in SDL_AudioCVT
  -- *
  -- *  The maximum number of SDL_AudioFilter functions in SDL_AudioCVT is
  -- *  currently limited to 9. The SDL_AudioCVT.filters array has 10 pointers,
  -- *  one of which is the terminating NULL pointer.
  --  

  --*
  -- *  \struct SDL_AudioCVT
  -- *  \brief A structure to hold a set of audio conversion filters and buffers.
  -- *
  -- *  Note that various parts of the conversion pipeline can take advantage
  -- *  of SIMD operations (like SSE2, for example). SDL_AudioCVT doesn't require
  -- *  you to pass it aligned data, but can possibly run much faster if you
  -- *  set both its (buf) field to a pointer that is aligned to 16 bytes, and its
  -- *  (len) field to something that's a multiple of 16, if possible.
  --  

  -- This structure is 84 bytes on 32-bit architectures, make sure GCC doesn't
  --   pad it out to 88 bytes to guarantee ABI compatibility between compilers.
  --   vvv
  --   The next time we rev the ABI, make sure to size the ints and add padding.
  -- 

  --  
  --*< Set to 1 if conversion possible  
   type SDL_AudioCVT_filters_array is array (0 .. 9) of SDL_AudioFilter;
   type SDL_AudioCVT is record
      needed : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:228
      src_format : aliased SDL_AudioFormat;  -- ..\SDL2_tmp\SDL_audio.h:229
      dst_format : aliased SDL_AudioFormat;  -- ..\SDL2_tmp\SDL_audio.h:230
      rate_incr : aliased double;  -- ..\SDL2_tmp\SDL_audio.h:231
      buf : access SDL_stdinc_h.Uint8;  -- ..\SDL2_tmp\SDL_audio.h:232
      len : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:233
      len_cvt : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:234
      len_mult : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:235
      len_ratio : aliased double;  -- ..\SDL2_tmp\SDL_audio.h:236
      filters : SDL_AudioCVT_filters_array;  -- ..\SDL2_tmp\SDL_audio.h:237
      filter_index : aliased int;  -- ..\SDL2_tmp\SDL_audio.h:238
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_AudioCVT);  -- ..\SDL2_tmp\SDL_audio.h:226

  --*< Source audio format  
  --*< Target audio format  
  --*< Rate conversion increment  
  --*< Buffer to hold entire audio data  
  --*< Length of original audio buffer  
  --*< Length of converted audio buffer  
  --*< buffer must be len*len_mult big  
  --*< Given len, final size is len*len_ratio  
  --*< NULL-terminated list of filter functions  
  --*< Current audio conversion function  
  -- Function prototypes  
  --*
  -- *  \name Driver discovery functions
  -- *
  -- *  These functions return the list of built in audio drivers, in the
  -- *  order that they are normally initialized by default.
  --  

  -- @{  
   function SDL_GetNumAudioDrivers return int;  -- ..\SDL2_tmp\SDL_audio.h:251
   pragma Import (C, SDL_GetNumAudioDrivers, "SDL_GetNumAudioDrivers");

   function SDL_GetAudioDriver (index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_audio.h:252
   pragma Import (C, SDL_GetAudioDriver, "SDL_GetAudioDriver");

  -- @}  
  --*
  -- *  \name Initialization and cleanup
  -- *
  -- *  \internal These functions are used internally, and should not be used unless
  -- *            you have a specific need to specify the audio driver you want to
  -- *            use.  You should normally use SDL_Init() or SDL_InitSubSystem().
  --  

  -- @{  
   function SDL_AudioInit (driver_name : Interfaces.C.Strings.chars_ptr) return int;  -- ..\SDL2_tmp\SDL_audio.h:263
   pragma Import (C, SDL_AudioInit, "SDL_AudioInit");

   procedure SDL_AudioQuit;  -- ..\SDL2_tmp\SDL_audio.h:264
   pragma Import (C, SDL_AudioQuit, "SDL_AudioQuit");

  -- @}  
  --*
  -- *  This function returns the name of the current audio driver, or NULL
  -- *  if no driver has been initialized.
  --  

   function SDL_GetCurrentAudioDriver return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_audio.h:271
   pragma Import (C, SDL_GetCurrentAudioDriver, "SDL_GetCurrentAudioDriver");

  --*
  -- *  This function opens the audio device with the desired parameters, and
  -- *  returns 0 if successful, placing the actual hardware parameters in the
  -- *  structure pointed to by \c obtained.  If \c obtained is NULL, the audio
  -- *  data passed to the callback function will be guaranteed to be in the
  -- *  requested format, and will be automatically converted to the hardware
  -- *  audio format if necessary.  This function returns -1 if it failed
  -- *  to open the audio device, or couldn't set up the audio thread.
  -- *
  -- *  When filling in the desired audio spec structure,
  -- *    - \c desired->freq should be the desired audio frequency in samples-per-
  -- *      second.
  -- *    - \c desired->format should be the desired audio format.
  -- *    - \c desired->samples is the desired size of the audio buffer, in
  -- *      samples.  This number should be a power of two, and may be adjusted by
  -- *      the audio driver to a value more suitable for the hardware.  Good values
  -- *      seem to range between 512 and 8096 inclusive, depending on the
  -- *      application and CPU speed.  Smaller values yield faster response time,
  -- *      but can lead to underflow if the application is doing heavy processing
  -- *      and cannot fill the audio buffer in time.  A stereo sample consists of
  -- *      both right and left channels in LR ordering.
  -- *      Note that the number of samples is directly related to time by the
  -- *      following formula:  \code ms = (samples*1000)/freq \endcode
  -- *    - \c desired->size is the size in bytes of the audio buffer, and is
  -- *      calculated by SDL_OpenAudio().
  -- *    - \c desired->silence is the value used to set the buffer to silence,
  -- *      and is calculated by SDL_OpenAudio().
  -- *    - \c desired->callback should be set to a function that will be called
  -- *      when the audio device is ready for more data.  It is passed a pointer
  -- *      to the audio buffer, and the length in bytes of the audio buffer.
  -- *      This function usually runs in a separate thread, and so you should
  -- *      protect data structures that it accesses by calling SDL_LockAudio()
  -- *      and SDL_UnlockAudio() in your code. Alternately, you may pass a NULL
  -- *      pointer here, and call SDL_QueueAudio() with some frequency, to queue
  -- *      more audio samples to be played (or for capture devices, call
  -- *      SDL_DequeueAudio() with some frequency, to obtain audio samples).
  -- *    - \c desired->userdata is passed as the first parameter to your callback
  -- *      function. If you passed a NULL callback, this value is ignored.
  -- *
  -- *  The audio device starts out playing silence when it's opened, and should
  -- *  be enabled for playing by calling \c SDL_PauseAudio(0) when you are ready
  -- *  for your audio callback function to be called.  Since the audio driver
  -- *  may modify the requested size of the audio buffer, you should allocate
  -- *  any local mixing buffers after you open the audio device.
  --  

   function SDL_OpenAudio (desired : access SDL_AudioSpec; obtained : access SDL_AudioSpec) return int;  -- ..\SDL2_tmp\SDL_audio.h:318
   pragma Import (C, SDL_OpenAudio, "SDL_OpenAudio");

  --*
  -- *  SDL Audio Device IDs.
  -- *
  -- *  A successful call to SDL_OpenAudio() is always device id 1, and legacy
  -- *  SDL audio APIs assume you want this device ID. SDL_OpenAudioDevice() calls
  -- *  always returns devices >= 2 on success. The legacy calls are good both
  -- *  for backwards compatibility and when you don't care about multiple,
  -- *  specific, or capture devices.
  --  

   subtype SDL_AudioDeviceID is SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_audio.h:330

  --*
  -- *  Get the number of available devices exposed by the current driver.
  -- *  Only valid after a successfully initializing the audio subsystem.
  -- *  Returns -1 if an explicit list of devices can't be determined; this is
  -- *  not an error. For example, if SDL is set up to talk to a remote audio
  -- *  server, it can't list every one available on the Internet, but it will
  -- *  still allow a specific host to be specified to SDL_OpenAudioDevice().
  -- *
  -- *  In many common cases, when this function returns a value <= 0, it can still
  -- *  successfully open the default device (NULL for first argument of
  -- *  SDL_OpenAudioDevice()).
  --  

   function SDL_GetNumAudioDevices (iscapture : int) return int;  -- ..\SDL2_tmp\SDL_audio.h:344
   pragma Import (C, SDL_GetNumAudioDevices, "SDL_GetNumAudioDevices");

  --*
  -- *  Get the human-readable name of a specific audio device.
  -- *  Must be a value between 0 and (number of audio devices-1).
  -- *  Only valid after a successfully initializing the audio subsystem.
  -- *  The values returned by this function reflect the latest call to
  -- *  SDL_GetNumAudioDevices(); recall that function to redetect available
  -- *  hardware.
  -- *
  -- *  The string returned by this function is UTF-8 encoded, read-only, and
  -- *  managed internally. You are not to free it. If you need to keep the
  -- *  string for any length of time, you should make your own copy of it, as it
  -- *  will be invalid next time any of several other SDL functions is called.
  --  

   function SDL_GetAudioDeviceName (index : int; iscapture : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_audio.h:359
   pragma Import (C, SDL_GetAudioDeviceName, "SDL_GetAudioDeviceName");

  --*
  -- *  Open a specific audio device. Passing in a device name of NULL requests
  -- *  the most reasonable default (and is equivalent to calling SDL_OpenAudio()).
  -- *
  -- *  The device name is a UTF-8 string reported by SDL_GetAudioDeviceName(), but
  -- *  some drivers allow arbitrary and driver-specific strings, such as a
  -- *  hostname/IP address for a remote audio server, or a filename in the
  -- *  diskaudio driver.
  -- *
  -- *  \return 0 on error, a valid device ID that is >= 2 on success.
  -- *
  -- *  SDL_OpenAudio(), unlike this function, always acts on device ID 1.
  --  

   function SDL_OpenAudioDevice
     (device : Interfaces.C.Strings.chars_ptr;
      iscapture : int;
      desired : access constant SDL_AudioSpec;
      obtained : access SDL_AudioSpec;
      allowed_changes : int) return SDL_AudioDeviceID;  -- ..\SDL2_tmp\SDL_audio.h:376
   pragma Import (C, SDL_OpenAudioDevice, "SDL_OpenAudioDevice");

  --*
  -- *  \name Audio state
  -- *
  -- *  Get the current audio state.
  --  

  -- @{  
   type SDL_AudioStatus is 
     (SDL_AUDIO_STOPPED,
      SDL_AUDIO_PLAYING,
      SDL_AUDIO_PAUSED);
   pragma Convention (C, SDL_AudioStatus);  -- ..\SDL2_tmp\SDL_audio.h:400

   function SDL_GetAudioStatus return SDL_AudioStatus;  -- ..\SDL2_tmp\SDL_audio.h:401
   pragma Import (C, SDL_GetAudioStatus, "SDL_GetAudioStatus");

   function SDL_GetAudioDeviceStatus (dev : SDL_AudioDeviceID) return SDL_AudioStatus;  -- ..\SDL2_tmp\SDL_audio.h:404
   pragma Import (C, SDL_GetAudioDeviceStatus, "SDL_GetAudioDeviceStatus");

  -- @}  
  -- Audio State  
  --*
  -- *  \name Pause audio functions
  -- *
  -- *  These functions pause and unpause the audio callback processing.
  -- *  They should be called with a parameter of 0 after opening the audio
  -- *  device to start playing sound.  This is so you can safely initialize
  -- *  data for your callback function after opening the audio device.
  -- *  Silence will be written to the audio device during the pause.
  --  

  -- @{  
   procedure SDL_PauseAudio (pause_on : int);  -- ..\SDL2_tmp\SDL_audio.h:417
   pragma Import (C, SDL_PauseAudio, "SDL_PauseAudio");

   procedure SDL_PauseAudioDevice (dev : SDL_AudioDeviceID; pause_on : int);  -- ..\SDL2_tmp\SDL_audio.h:418
   pragma Import (C, SDL_PauseAudioDevice, "SDL_PauseAudioDevice");

  -- @}  
  -- Pause audio functions  
  --*
  -- *  This function loads a WAVE from the data source, automatically freeing
  -- *  that source if \c freesrc is non-zero.  For example, to load a WAVE file,
  -- *  you could do:
  -- *  \code
  -- *      SDL_LoadWAV_RW(SDL_RWFromFile("sample.wav", "rb"), 1, ...);
  -- *  \endcode
  -- *
  -- *  If this function succeeds, it returns the given SDL_AudioSpec,
  -- *  filled with the audio data format of the wave data, and sets
  -- *  \c *audio_buf to a malloc()'d buffer containing the audio data,
  -- *  and sets \c *audio_len to the length of that audio buffer, in bytes.
  -- *  You need to free the audio buffer with SDL_FreeWAV() when you are
  -- *  done with it.
  -- *
  -- *  This function returns NULL and sets the SDL error message if the
  -- *  wave file cannot be opened, uses an unknown data format, or is
  -- *  corrupt.  Currently raw and MS-ADPCM WAVE files are supported.
  --  

   function SDL_LoadWAV_RW
     (src : access SDL_rwops_h.SDL_RWops;
      freesrc : int;
      spec : access SDL_AudioSpec;
      audio_buf : System.Address;
      audio_len : access SDL_stdinc_h.Uint32) return access SDL_AudioSpec;  -- ..\SDL2_tmp\SDL_audio.h:441
   pragma Import (C, SDL_LoadWAV_RW, "SDL_LoadWAV_RW");

  --*
  -- *  Loads a WAV from a file.
  -- *  Compatibility convenience function.
  --  

  --*
  -- *  This function frees data previously allocated with SDL_LoadWAV_RW()
  --  

   procedure SDL_FreeWAV (audio_buf : access SDL_stdinc_h.Uint8);  -- ..\SDL2_tmp\SDL_audio.h:457
   pragma Import (C, SDL_FreeWAV, "SDL_FreeWAV");

  --*
  -- *  This function takes a source format and rate and a destination format
  -- *  and rate, and initializes the \c cvt structure with information needed
  -- *  by SDL_ConvertAudio() to convert a buffer of audio data from one format
  -- *  to the other. An unsupported format causes an error and -1 will be returned.
  -- *
  -- *  \return 0 if no conversion is needed, 1 if the audio filter is set up,
  -- *  or -1 on error.
  --  

   function SDL_BuildAudioCVT
     (cvt : access SDL_AudioCVT;
      src_format : SDL_AudioFormat;
      src_channels : SDL_stdinc_h.Uint8;
      src_rate : int;
      dst_format : SDL_AudioFormat;
      dst_channels : SDL_stdinc_h.Uint8;
      dst_rate : int) return int;  -- ..\SDL2_tmp\SDL_audio.h:468
   pragma Import (C, SDL_BuildAudioCVT, "SDL_BuildAudioCVT");

  --*
  -- *  Once you have initialized the \c cvt structure using SDL_BuildAudioCVT(),
  -- *  created an audio buffer \c cvt->buf, and filled it with \c cvt->len bytes of
  -- *  audio data in the source format, this function will convert it in-place
  -- *  to the desired format.
  -- *
  -- *  The data conversion may expand the size of the audio data, so the buffer
  -- *  \c cvt->buf should be allocated after the \c cvt structure is initialized by
  -- *  SDL_BuildAudioCVT(), and should be \c cvt->len*cvt->len_mult bytes long.
  -- *
  -- *  \return 0 on success or -1 if \c cvt->buf is NULL.
  --  

   function SDL_ConvertAudio (cvt : access SDL_AudioCVT) return int;  -- ..\SDL2_tmp\SDL_audio.h:488
   pragma Import (C, SDL_ConvertAudio, "SDL_ConvertAudio");

  -- SDL_AudioStream is a new audio conversion interface.
  --   The benefits vs SDL_AudioCVT:
  --    - it can handle resampling data in chunks without generating
  --      artifacts, when it doesn't have the complete buffer available.
  --    - it can handle incoming data in any variable size.
  --    - You push data as you have it, and pull it when you need it
  --  

  -- this is opaque to the outside world.  
   type u_SDL_AudioStream is null record;   -- incomplete struct

   subtype SDL_AudioStream is u_SDL_AudioStream;  -- ..\SDL2_tmp\SDL_audio.h:499

  --*
  -- *  Create a new audio stream
  -- *
  -- *  \param src_format The format of the source audio
  -- *  \param src_channels The number of channels of the source audio
  -- *  \param src_rate The sampling rate of the source audio
  -- *  \param dst_format The format of the desired audio output
  -- *  \param dst_channels The number of channels of the desired audio output
  -- *  \param dst_rate The sampling rate of the desired audio output
  -- *  \return 0 on success, or -1 on error.
  -- *
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_AudioStreamClear
  -- *  \sa SDL_FreeAudioStream
  --  

   function SDL_NewAudioStream
     (src_format : SDL_AudioFormat;
      src_channels : SDL_stdinc_h.Uint8;
      src_rate : int;
      dst_format : SDL_AudioFormat;
      dst_channels : SDL_stdinc_h.Uint8;
      dst_rate : int) return access SDL_AudioStream;  -- ..\SDL2_tmp\SDL_audio.h:519
   pragma Import (C, SDL_NewAudioStream, "SDL_NewAudioStream");

  --*
  -- *  Add data to be converted/resampled to the stream
  -- *
  -- *  \param stream The stream the audio data is being added to
  -- *  \param buf A pointer to the audio data to add
  -- *  \param len The number of bytes to write to the stream
  -- *  \return 0 on success, or -1 on error.
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_AudioStreamClear
  -- *  \sa SDL_FreeAudioStream
  --  

   function SDL_AudioStreamPut
     (stream : access SDL_AudioStream;
      buf : System.Address;
      len : int) return int;  -- ..\SDL2_tmp\SDL_audio.h:541
   pragma Import (C, SDL_AudioStreamPut, "SDL_AudioStreamPut");

  --*
  -- *  Get converted/resampled data from the stream
  -- *
  -- *  \param stream The stream the audio is being requested from
  -- *  \param buf A buffer to fill with audio data
  -- *  \param len The maximum number of bytes to fill
  -- *  \return The number of bytes read from the stream, or -1 on error
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_AudioStreamClear
  -- *  \sa SDL_FreeAudioStream
  --  

   function SDL_AudioStreamGet
     (stream : access SDL_AudioStream;
      buf : System.Address;
      len : int) return int;  -- ..\SDL2_tmp\SDL_audio.h:558
   pragma Import (C, SDL_AudioStreamGet, "SDL_AudioStreamGet");

  --*
  -- * Get the number of converted/resampled bytes available. The stream may be
  -- *  buffering data behind the scenes until it has enough to resample
  -- *  correctly, so this number might be lower than what you expect, or even
  -- *  be zero. Add more data or flush the stream if you need the data now.
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_AudioStreamClear
  -- *  \sa SDL_FreeAudioStream
  --  

   function SDL_AudioStreamAvailable (stream : access SDL_AudioStream) return int;  -- ..\SDL2_tmp\SDL_audio.h:573
   pragma Import (C, SDL_AudioStreamAvailable, "SDL_AudioStreamAvailable");

  --*
  -- * Tell the stream that you're done sending data, and anything being buffered
  -- *  should be converted/resampled and made available immediately.
  -- *
  -- * It is legal to add more data to a stream after flushing, but there will
  -- *  be audio gaps in the output. Generally this is intended to signal the
  -- *  end of input, so the complete output becomes available.
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamClear
  -- *  \sa SDL_FreeAudioStream
  --  

   function SDL_AudioStreamFlush (stream : access SDL_AudioStream) return int;  -- ..\SDL2_tmp\SDL_audio.h:590
   pragma Import (C, SDL_AudioStreamFlush, "SDL_AudioStreamFlush");

  --*
  -- *  Clear any pending data in the stream without converting it
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_FreeAudioStream
  --  

   procedure SDL_AudioStreamClear (stream : access SDL_AudioStream);  -- ..\SDL2_tmp\SDL_audio.h:602
   pragma Import (C, SDL_AudioStreamClear, "SDL_AudioStreamClear");

  --*
  -- * Free an audio stream
  -- *
  -- *  \sa SDL_NewAudioStream
  -- *  \sa SDL_AudioStreamPut
  -- *  \sa SDL_AudioStreamGet
  -- *  \sa SDL_AudioStreamAvailable
  -- *  \sa SDL_AudioStreamFlush
  -- *  \sa SDL_AudioStreamClear
  --  

   procedure SDL_FreeAudioStream (stream : access SDL_AudioStream);  -- ..\SDL2_tmp\SDL_audio.h:614
   pragma Import (C, SDL_FreeAudioStream, "SDL_FreeAudioStream");

  --*
  -- *  This takes two audio buffers of the playing audio format and mixes
  -- *  them, performing addition, volume adjustment, and overflow clipping.
  -- *  The volume ranges from 0 - 128, and should be set to ::SDL_MIX_MAXVOLUME
  -- *  for full audio volume.  Note this does not change hardware volume.
  -- *  This is provided for convenience -- you can mix your own audio data.
  --  

   procedure SDL_MixAudio
     (dst : access SDL_stdinc_h.Uint8;
      src : access SDL_stdinc_h.Uint8;
      len : SDL_stdinc_h.Uint32;
      volume : int);  -- ..\SDL2_tmp\SDL_audio.h:624
   pragma Import (C, SDL_MixAudio, "SDL_MixAudio");

  --*
  -- *  This works like SDL_MixAudio(), but you specify the audio format instead of
  -- *  using the format of audio device 1. Thus it can be used when no audio
  -- *  device is open at all.
  --  

   procedure SDL_MixAudioFormat
     (dst : access SDL_stdinc_h.Uint8;
      src : access SDL_stdinc_h.Uint8;
      format : SDL_AudioFormat;
      len : SDL_stdinc_h.Uint32;
      volume : int);  -- ..\SDL2_tmp\SDL_audio.h:632
   pragma Import (C, SDL_MixAudioFormat, "SDL_MixAudioFormat");

  --*
  -- *  Queue more audio on non-callback devices.
  -- *
  -- *  (If you are looking to retrieve queued audio from a non-callback capture
  -- *  device, you want SDL_DequeueAudio() instead. This will return -1 to
  -- *  signify an error if you use it with capture devices.)
  -- *
  -- *  SDL offers two ways to feed audio to the device: you can either supply a
  -- *  callback that SDL triggers with some frequency to obtain more audio
  -- *  (pull method), or you can supply no callback, and then SDL will expect
  -- *  you to supply data at regular intervals (push method) with this function.
  -- *
  -- *  There are no limits on the amount of data you can queue, short of
  -- *  exhaustion of address space. Queued data will drain to the device as
  -- *  necessary without further intervention from you. If the device needs
  -- *  audio but there is not enough queued, it will play silence to make up
  -- *  the difference. This means you will have skips in your audio playback
  -- *  if you aren't routinely queueing sufficient data.
  -- *
  -- *  This function copies the supplied data, so you are safe to free it when
  -- *  the function returns. This function is thread-safe, but queueing to the
  -- *  same device from two threads at once does not promise which buffer will
  -- *  be queued first.
  -- *
  -- *  You may not queue audio on a device that is using an application-supplied
  -- *  callback; doing so returns an error. You have to use the audio callback
  -- *  or queue audio with this function, but not both.
  -- *
  -- *  You should not call SDL_LockAudio() on the device before queueing; SDL
  -- *  handles locking internally for this function.
  -- *
  -- *  \param dev The device ID to which we will queue audio.
  -- *  \param data The data to queue to the device for later playback.
  -- *  \param len The number of bytes (not samples!) to which (data) points.
  -- *  \return 0 on success, or -1 on error.
  -- *
  -- *  \sa SDL_GetQueuedAudioSize
  -- *  \sa SDL_ClearQueuedAudio
  --  

   function SDL_QueueAudio
     (dev : SDL_AudioDeviceID;
      data : System.Address;
      len : SDL_stdinc_h.Uint32) return int;  -- ..\SDL2_tmp\SDL_audio.h:676
   pragma Import (C, SDL_QueueAudio, "SDL_QueueAudio");

  --*
  -- *  Dequeue more audio on non-callback devices.
  -- *
  -- *  (If you are looking to queue audio for output on a non-callback playback
  -- *  device, you want SDL_QueueAudio() instead. This will always return 0
  -- *  if you use it with playback devices.)
  -- *
  -- *  SDL offers two ways to retrieve audio from a capture device: you can
  -- *  either supply a callback that SDL triggers with some frequency as the
  -- *  device records more audio data, (push method), or you can supply no
  -- *  callback, and then SDL will expect you to retrieve data at regular
  -- *  intervals (pull method) with this function.
  -- *
  -- *  There are no limits on the amount of data you can queue, short of
  -- *  exhaustion of address space. Data from the device will keep queuing as
  -- *  necessary without further intervention from you. This means you will
  -- *  eventually run out of memory if you aren't routinely dequeueing data.
  -- *
  -- *  Capture devices will not queue data when paused; if you are expecting
  -- *  to not need captured audio for some length of time, use
  -- *  SDL_PauseAudioDevice() to stop the capture device from queueing more
  -- *  data. This can be useful during, say, level loading times. When
  -- *  unpaused, capture devices will start queueing data from that point,
  -- *  having flushed any capturable data available while paused.
  -- *
  -- *  This function is thread-safe, but dequeueing from the same device from
  -- *  two threads at once does not promise which thread will dequeued data
  -- *  first.
  -- *
  -- *  You may not dequeue audio from a device that is using an
  -- *  application-supplied callback; doing so returns an error. You have to use
  -- *  the audio callback, or dequeue audio with this function, but not both.
  -- *
  -- *  You should not call SDL_LockAudio() on the device before queueing; SDL
  -- *  handles locking internally for this function.
  -- *
  -- *  \param dev The device ID from which we will dequeue audio.
  -- *  \param data A pointer into where audio data should be copied.
  -- *  \param len The number of bytes (not samples!) to which (data) points.
  -- *  \return number of bytes dequeued, which could be less than requested.
  -- *
  -- *  \sa SDL_GetQueuedAudioSize
  -- *  \sa SDL_ClearQueuedAudio
  --  

   function SDL_DequeueAudio
     (dev : SDL_AudioDeviceID;
      data : System.Address;
      len : SDL_stdinc_h.Uint32) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_audio.h:722
   pragma Import (C, SDL_DequeueAudio, "SDL_DequeueAudio");

  --*
  -- *  Get the number of bytes of still-queued audio.
  -- *
  -- *  For playback device:
  -- *
  -- *    This is the number of bytes that have been queued for playback with
  -- *    SDL_QueueAudio(), but have not yet been sent to the hardware. This
  -- *    number may shrink at any time, so this only informs of pending data.
  -- *
  -- *    Once we've sent it to the hardware, this function can not decide the
  -- *    exact byte boundary of what has been played. It's possible that we just
  -- *    gave the hardware several kilobytes right before you called this
  -- *    function, but it hasn't played any of it yet, or maybe half of it, etc.
  -- *
  -- *  For capture devices:
  -- *
  -- *    This is the number of bytes that have been captured by the device and
  -- *    are waiting for you to dequeue. This number may grow at any time, so
  -- *    this only informs of the lower-bound of available data.
  -- *
  -- *  You may not queue audio on a device that is using an application-supplied
  -- *  callback; calling this function on such a device always returns 0.
  -- *  You have to queue audio with SDL_QueueAudio()/SDL_DequeueAudio(), or use
  -- *  the audio callback, but not both.
  -- *
  -- *  You should not call SDL_LockAudio() on the device before querying; SDL
  -- *  handles locking internally for this function.
  -- *
  -- *  \param dev The device ID of which we will query queued audio size.
  -- *  \return Number of bytes (not samples!) of queued audio.
  -- *
  -- *  \sa SDL_QueueAudio
  -- *  \sa SDL_ClearQueuedAudio
  --  

   function SDL_GetQueuedAudioSize (dev : SDL_AudioDeviceID) return SDL_stdinc_h.Uint32;  -- ..\SDL2_tmp\SDL_audio.h:758
   pragma Import (C, SDL_GetQueuedAudioSize, "SDL_GetQueuedAudioSize");

  --*
  -- *  Drop any queued audio data. For playback devices, this is any queued data
  -- *  still waiting to be submitted to the hardware. For capture devices, this
  -- *  is any data that was queued by the device that hasn't yet been dequeued by
  -- *  the application.
  -- *
  -- *  Immediately after this call, SDL_GetQueuedAudioSize() will return 0. For
  -- *  playback devices, the hardware will start playing silence if more audio
  -- *  isn't queued. Unpaused capture devices will start filling the queue again
  -- *  as soon as they have more data available (which, depending on the state
  -- *  of the hardware and the thread, could be before this function call
  -- *  returns!).
  -- *
  -- *  This will not prevent playback of queued audio that's already been sent
  -- *  to the hardware, as we can not undo that, so expect there to be some
  -- *  fraction of a second of audio that might still be heard. This can be
  -- *  useful if you want to, say, drop any pending music during a level change
  -- *  in your game.
  -- *
  -- *  You may not queue audio on a device that is using an application-supplied
  -- *  callback; calling this function on such a device is always a no-op.
  -- *  You have to queue audio with SDL_QueueAudio()/SDL_DequeueAudio(), or use
  -- *  the audio callback, but not both.
  -- *
  -- *  You should not call SDL_LockAudio() on the device before clearing the
  -- *  queue; SDL handles locking internally for this function.
  -- *
  -- *  This function always succeeds and thus returns void.
  -- *
  -- *  \param dev The device ID of which to clear the audio queue.
  -- *
  -- *  \sa SDL_QueueAudio
  -- *  \sa SDL_GetQueuedAudioSize
  --  

   procedure SDL_ClearQueuedAudio (dev : SDL_AudioDeviceID);  -- ..\SDL2_tmp\SDL_audio.h:794
   pragma Import (C, SDL_ClearQueuedAudio, "SDL_ClearQueuedAudio");

  --*
  -- *  \name Audio lock functions
  -- *
  -- *  The lock manipulated by these functions protects the callback function.
  -- *  During a SDL_LockAudio()/SDL_UnlockAudio() pair, you can be guaranteed that
  -- *  the callback function is not running.  Do not call these from the callback
  -- *  function or you will cause deadlock.
  --  

  -- @{  
   procedure SDL_LockAudio;  -- ..\SDL2_tmp\SDL_audio.h:806
   pragma Import (C, SDL_LockAudio, "SDL_LockAudio");

   procedure SDL_LockAudioDevice (dev : SDL_AudioDeviceID);  -- ..\SDL2_tmp\SDL_audio.h:807
   pragma Import (C, SDL_LockAudioDevice, "SDL_LockAudioDevice");

   procedure SDL_UnlockAudio;  -- ..\SDL2_tmp\SDL_audio.h:808
   pragma Import (C, SDL_UnlockAudio, "SDL_UnlockAudio");

   procedure SDL_UnlockAudioDevice (dev : SDL_AudioDeviceID);  -- ..\SDL2_tmp\SDL_audio.h:809
   pragma Import (C, SDL_UnlockAudioDevice, "SDL_UnlockAudioDevice");

  -- @}  
  -- Audio lock functions  
  --*
  -- *  This function shuts down audio processing and closes the audio device.
  --  

   procedure SDL_CloseAudio;  -- ..\SDL2_tmp\SDL_audio.h:815
   pragma Import (C, SDL_CloseAudio, "SDL_CloseAudio");

   procedure SDL_CloseAudioDevice (dev : SDL_AudioDeviceID);  -- ..\SDL2_tmp\SDL_audio.h:816
   pragma Import (C, SDL_CloseAudioDevice, "SDL_CloseAudioDevice");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_audio_h;
