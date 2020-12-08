pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package rtmintrin_h is

  -- Copyright (C) 2012-2017 Free Software Foundation, Inc.
  --   This file is part of GCC.
  --   GCC is free software; you can redistribute it and/or modify
  --   it under the terms of the GNU General Public License as published by
  --   the Free Software Foundation; either version 3, or (at your option)
  --   any later version.
  --   GCC is distributed in the hope that it will be useful,
  --   but WITHOUT ANY WARRANTY; without even the implied warranty of
  --   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  --   GNU General Public License for more details.
  --   Under Section 7 of GPL version 3, you are granted additional
  --   permissions described in the GCC Runtime Library Exception, version
  --   3.1, as published by the Free Software Foundation.
  --   You should have received a copy of the GNU General Public License and
  --   a copy of the GCC Runtime Library Exception along with this program;
  --   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
  --   <http://www.gnu.org/licenses/>.   

  -- Start an RTM code region.  Return _XBEGIN_STARTED on success and the
  --   abort condition otherwise.   

   --  skipped func _xbegin

  -- Specify the end of an RTM code region.  If it corresponds to the
  --   outermost transaction, then attempts the transaction commit.  If the
  --   commit fails, then control is transferred to the outermost transaction
  --   fallback handler.   

   --  skipped func _xend

  -- Force an RTM abort condition. The control is transferred to the
  --   outermost transaction fallback handler with the abort condition IMM.   

end rtmintrin_h;
