pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Extensions;

package std_abs_h is

  -- -*- C++ -*- C library enhancements header.
  -- Copyright (C) 2016-2017 Free Software Foundation, Inc.
  -- This file is part of the GNU ISO C++ Library.  This library is free
  -- software; you can redistribute it and/or modify it under the
  -- terms of the GNU General Public License as published by the
  -- Free Software Foundation; either version 3, or (at your option)
  -- any later version.
  -- This library is distributed in the hope that it will be useful,
  -- but WITHOUT ANY WARRANTY; without even the implied warranty of
  -- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  -- GNU General Public License for more details.
  -- Under Section 7 of GPL version 3, you are granted additional
  -- permissions described in the GCC Runtime Library Exception, version
  -- 3.1, as published by the Free Software Foundation.
  -- You should have received a copy of the GNU General Public License and
  -- a copy of the GCC Runtime Library Exception along with this program;
  -- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  -- <http://www.gnu.org/licenses/>.
  --* @file include/bits/std_abs.h
  -- *  This is an internal header file, included by other library headers.
  -- *  Do not attempt to use it directly. @headername{cmath, cstdlib}
  --  

   function c_abs (uu_i : long) return long;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:56
   pragma Import (CPP, c_abs, "_ZSt3absl");

   function c_abs_u_2 (uu_x : Long_Long_Integer) return Long_Long_Integer;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:61
   pragma Import (CPP, c_abs_u_2, "_ZSt3absx");

  -- _GLIBCXX_RESOLVE_LIB_DEFECTS
  -- 2192. Validity and return type of std::abs(0u) is unclear
  -- 2294. <cstdlib> should declare abs(double)
   function c_abs_u_3 (uu_x : double) return double;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:70
   pragma Import (CPP, c_abs_u_3, "_ZSt3absd");

   function c_abs_u_4 (uu_x : float) return float;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:74
   pragma Import (CPP, c_abs_u_4, "_ZSt3absf");

   function c_abs_u_5 (uu_x : long_double) return long_double;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:78
   pragma Import (CPP, c_abs_u_5, "_ZSt3abse");

   function c_abs_u_6 (uu_x : uu_int128) return uu_int128;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:84
   pragma Import (CPP, c_abs_u_6, "_ZSt3absn");

   function c_abs_u_7 (uu_x : Extensions.Float_128) return Extensions.Float_128;  -- d:\install\gpl2018\include\c++\7.3.1\bits\std_abs.h:102
   pragma Import (CPP, c_abs_u_7, "_ZSt3absg");

  -- namespace
end std_abs_h;
