pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package umingw_secapi_h is

  --*
  -- * This file has no copyright assigned and is placed in the Public Domain.
  -- * This file is part of the mingw-w64 runtime package.
  -- * No warranty is given; refer to the file DISCLAIMER.PD within this package.
  --  

  -- http://msdn.microsoft.com/en-us/library/ms175759%28v=VS.100%29.aspx  
  -- Templates won't work in C, will break if secure API is not enabled, disabled  
  --  The macro _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT requires that _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES
  --  is also defined as 1. If _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES_COUNT is defined as 1 and
  --  _CRT_SECURE_CPP_OVERLOAD_STANDARD_NAMES is defined as 0, the application will not perform any template overloads.
  -- 

  -- Fallback on insecure mode if not possible to know destination size at compile time, NULL is appended for strncpy  
  -- For *_l locale types  
  -- https://blogs.msdn.com/b/sdl/archive/2010/02/16/vc-2010-and-memcpy.aspx?Redirected=true  
  -- fallback on default implementation if we can't know the size of the destination  
end umingw_secapi_h;
