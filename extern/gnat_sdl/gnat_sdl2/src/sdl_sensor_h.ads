pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;
with Interfaces.C.Strings;

package SDL_sensor_h is

   SDL_STANDARD_GRAVITY : constant := 9.80665;  --  ..\SDL2_tmp\SDL_sensor.h:97

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
  -- *  \file SDL_sensor.h
  -- *
  -- *  Include file for SDL sensor event handling
  -- *
  --  

  -- Set up for C function definitions, even when using C++  
  -- *INDENT-OFF*  
  -- *INDENT-ON*  
  --*
  -- *  \brief SDL_sensor.h
  -- *
  -- *  In order to use these functions, SDL_Init() must have been called
  -- *  with the ::SDL_INIT_SENSOR flag.  This causes SDL to scan the system
  -- *  for sensors, and load appropriate drivers.
  --  

   type u_SDL_Sensor is null record;   -- incomplete struct

   subtype SDL_Sensor is u_SDL_Sensor;  -- ..\SDL2_tmp\SDL_sensor.h:52

  --*
  -- * This is a unique ID for a sensor for the time it is connected to the system,
  -- * and is never reused for the lifetime of the application.
  -- *
  -- * The ID value starts at 0 and increments from there. The value -1 is an invalid ID.
  --  

   subtype SDL_SensorID is SDL_stdinc_h.Sint32;  -- ..\SDL2_tmp\SDL_sensor.h:60

  -- The different sensors defined by SDL
  -- *
  -- * Additional sensors may be available, using platform dependent semantics.
  -- *
  -- * Hare are the additional Android sensors:
  -- * https://developer.android.com/reference/android/hardware/SensorEvent.html#values
  --  

  --*< Returned for an invalid sensor  
  --*< Unknown sensor type  
  --*< Accelerometer  
  --*< Gyroscope  
   subtype SDL_SensorType is int;
   SDL_SENSOR_INVALID : constant int := -1;
   SDL_SENSOR_UNKNOWN : constant int := 0;
   SDL_SENSOR_ACCEL : constant int := 1;
   SDL_SENSOR_GYRO : constant int := 2;  -- ..\SDL2_tmp\SDL_sensor.h:75

  --*
  -- * Accelerometer sensor
  -- *
  -- * The accelerometer returns the current acceleration in SI meters per
  -- * second squared. This includes gravity, so a device at rest will have
  -- * an acceleration of SDL_STANDARD_GRAVITY straight down.
  -- *
  -- * values[0]: Acceleration on the x axis
  -- * values[1]: Acceleration on the y axis
  -- * values[2]: Acceleration on the z axis
  -- *
  -- * For phones held in portrait mode, the axes are defined as follows:
  -- * -X ... +X : left ... right
  -- * -Y ... +Y : bottom ... top
  -- * -Z ... +Z : farther ... closer
  -- * 
  -- * The axis data is not changed when the phone is rotated.
  -- *
  -- * \sa SDL_GetDisplayOrientation()
  --  

  --*
  -- * Gyroscope sensor
  -- *
  -- * The gyroscope returns the current rate of rotation in radians per second.
  -- * The rotation is positive in the counter-clockwise direction. That is,
  -- * an observer looking from a positive location on one of the axes would
  -- * see positive rotation on that axis when it appeared to be rotating
  -- * counter-clockwise.
  -- *
  -- * values[0]: Angular speed around the x axis
  -- * values[1]: Angular speed around the y axis
  -- * values[2]: Angular speed around the z axis
  -- *
  -- * For phones held in portrait mode, the axes are defined as follows:
  -- * -X ... +X : left ... right
  -- * -Y ... +Y : bottom ... top
  -- * -Z ... +Z : farther ... closer
  -- * 
  -- * The axis data is not changed when the phone is rotated.
  -- *
  -- * \sa SDL_GetDisplayOrientation()
  --  

  -- Function prototypes  
  --*
  -- *  \brief Count the number of sensors attached to the system right now
  --  

   function SDL_NumSensors return int;  -- ..\SDL2_tmp\SDL_sensor.h:127
   pragma Import (C, SDL_NumSensors, "SDL_NumSensors");

  --*
  -- *  \brief Get the implementation dependent name of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- * 
  -- *  \return The sensor name, or NULL if device_index is out of range.
  --  

   function SDL_SensorGetDeviceName (device_index : int) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_sensor.h:136
   pragma Import (C, SDL_SensorGetDeviceName, "SDL_SensorGetDeviceName");

  --*
  -- *  \brief Get the type of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor type, or SDL_SENSOR_INVALID if device_index is out of range.
  --  

   function SDL_SensorGetDeviceType (device_index : int) return SDL_SensorType;  -- ..\SDL2_tmp\SDL_sensor.h:145
   pragma Import (C, SDL_SensorGetDeviceType, "SDL_SensorGetDeviceType");

  --*
  -- *  \brief Get the platform dependent type of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor platform dependent type, or -1 if device_index is out of range.
  --  

   function SDL_SensorGetDeviceNonPortableType (device_index : int) return int;  -- ..\SDL2_tmp\SDL_sensor.h:154
   pragma Import (C, SDL_SensorGetDeviceNonPortableType, "SDL_SensorGetDeviceNonPortableType");

  --*
  -- *  \brief Get the instance ID of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor instance ID, or -1 if device_index is out of range.
  --  

   function SDL_SensorGetDeviceInstanceID (device_index : int) return SDL_SensorID;  -- ..\SDL2_tmp\SDL_sensor.h:163
   pragma Import (C, SDL_SensorGetDeviceInstanceID, "SDL_SensorGetDeviceInstanceID");

  --*
  -- *  \brief Open a sensor for use.
  -- *
  -- *  The index passed as an argument refers to the N'th sensor on the system.
  -- *
  -- *  \return A sensor identifier, or NULL if an error occurred.
  --  

   function SDL_SensorOpen (device_index : int) return access SDL_Sensor;  -- ..\SDL2_tmp\SDL_sensor.h:172
   pragma Import (C, SDL_SensorOpen, "SDL_SensorOpen");

  --*
  -- * Return the SDL_Sensor associated with an instance id.
  --  

   function SDL_SensorFromInstanceID (instance_id : SDL_SensorID) return access SDL_Sensor;  -- ..\SDL2_tmp\SDL_sensor.h:177
   pragma Import (C, SDL_SensorFromInstanceID, "SDL_SensorFromInstanceID");

  --*
  -- *  \brief Get the implementation dependent name of a sensor.
  -- *
  -- *  \return The sensor name, or NULL if the sensor is NULL.
  --  

   function SDL_SensorGetName (sensor : access SDL_Sensor) return Interfaces.C.Strings.chars_ptr;  -- ..\SDL2_tmp\SDL_sensor.h:184
   pragma Import (C, SDL_SensorGetName, "SDL_SensorGetName");

  --*
  -- *  \brief Get the type of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor type, or SDL_SENSOR_INVALID if the sensor is NULL.
  --  

   function SDL_SensorGetType (sensor : access SDL_Sensor) return SDL_SensorType;  -- ..\SDL2_tmp\SDL_sensor.h:193
   pragma Import (C, SDL_SensorGetType, "SDL_SensorGetType");

  --*
  -- *  \brief Get the platform dependent type of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor platform dependent type, or -1 if the sensor is NULL.
  --  

   function SDL_SensorGetNonPortableType (sensor : access SDL_Sensor) return int;  -- ..\SDL2_tmp\SDL_sensor.h:202
   pragma Import (C, SDL_SensorGetNonPortableType, "SDL_SensorGetNonPortableType");

  --*
  -- *  \brief Get the instance ID of a sensor.
  -- *
  -- *  This can be called before any sensors are opened.
  -- *
  -- *  \return The sensor instance ID, or -1 if the sensor is NULL.
  --  

   function SDL_SensorGetInstanceID (sensor : access SDL_Sensor) return SDL_SensorID;  -- ..\SDL2_tmp\SDL_sensor.h:211
   pragma Import (C, SDL_SensorGetInstanceID, "SDL_SensorGetInstanceID");

  --*
  -- *  Get the current state of an opened sensor.
  -- *
  -- *  The number of values and interpretation of the data is sensor dependent.
  -- *
  -- *  \param sensor The sensor to query
  -- *  \param data A pointer filled with the current sensor state
  -- *  \param num_values The number of values to write to data
  -- *
  -- *  \return 0 or -1 if an error occurred.
  --  

   function SDL_SensorGetData
     (sensor : access SDL_Sensor;
      data : access float;
      num_values : int) return int;  -- ..\SDL2_tmp\SDL_sensor.h:224
   pragma Import (C, SDL_SensorGetData, "SDL_SensorGetData");

  --*
  -- *  Close a sensor previously opened with SDL_SensorOpen()
  --  

   procedure SDL_SensorClose (sensor : access SDL_Sensor);  -- ..\SDL2_tmp\SDL_sensor.h:229
   pragma Import (C, SDL_SensorClose, "SDL_SensorClose");

  --*
  -- *  Update the current state of the open sensors.
  -- *
  -- *  This is called automatically by the event loop if sensor events are enabled.
  -- *
  -- *  This needs to be called from the thread that initialized the sensor subsystem.
  --  

   procedure SDL_SensorUpdate;  -- ..\SDL2_tmp\SDL_sensor.h:238
   pragma Import (C, SDL_SensorUpdate, "SDL_SensorUpdate");

  -- Ends C function definitions when using C++  
  -- *INDENT-OFF*  
  -- *INDENT-ON*  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_sensor_h;
