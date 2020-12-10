pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with SDL_stdinc_h;

package SDL_rect_h is

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
  -- *  \file SDL_rect.h
  -- *
  -- *  Header file for SDL_rect definition and management functions.
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief  The structure that defines a point
  -- *
  -- *  \sa SDL_EnclosePoints
  -- *  \sa SDL_PointInRect
  --  

   type SDL_Point is record
      x : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:50
      y : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:51
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Point);  -- ..\SDL2_tmp\SDL_rect.h:48

  --*
  -- *  \brief A rectangle, with the origin at the upper left.
  -- *
  -- *  \sa SDL_RectEmpty
  -- *  \sa SDL_RectEquals
  -- *  \sa SDL_HasIntersection
  -- *  \sa SDL_IntersectRect
  -- *  \sa SDL_UnionRect
  -- *  \sa SDL_EnclosePoints
  --  

   type SDL_Rect is record
      x : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:66
      y : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:66
      w : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:67
      h : aliased int;  -- ..\SDL2_tmp\SDL_rect.h:67
   end record;
   pragma Convention (C_Pass_By_Copy, SDL_Rect);  -- ..\SDL2_tmp\SDL_rect.h:64

  --*
  -- *  \brief Returns true if point resides inside a rectangle.
  --  

   function SDL_PointInRect (p : access constant SDL_Point; r : access constant SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:73
   pragma Import (C, SDL_PointInRect, "SDL_PointInRect");

  --*
  -- *  \brief Returns true if the rectangle has no area.
  --  

   function SDL_RectEmpty (r : access constant SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:82
   pragma Import (C, SDL_RectEmpty, "SDL_RectEmpty");

  --*
  -- *  \brief Returns true if the two rectangles are equal.
  --  

   function SDL_RectEquals (a : access constant SDL_Rect; b : access constant SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:90
   pragma Import (C, SDL_RectEquals, "SDL_RectEquals");

  --*
  -- *  \brief Determine whether two rectangles intersect.
  -- *
  -- *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
  --  

   function SDL_HasIntersection (A : access constant SDL_Rect; B : access constant SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:101
   pragma Import (C, SDL_HasIntersection, "SDL_HasIntersection");

  --*
  -- *  \brief Calculate the intersection of two rectangles.
  -- *
  -- *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
  --  

   function SDL_IntersectRect
     (A : access constant SDL_Rect;
      B : access constant SDL_Rect;
      result : access SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:109
   pragma Import (C, SDL_IntersectRect, "SDL_IntersectRect");

  --*
  -- *  \brief Calculate the union of two rectangles.
  --  

   procedure SDL_UnionRect
     (A : access constant SDL_Rect;
      B : access constant SDL_Rect;
      result : access SDL_Rect);  -- ..\SDL2_tmp\SDL_rect.h:116
   pragma Import (C, SDL_UnionRect, "SDL_UnionRect");

  --*
  -- *  \brief Calculate a minimal rectangle enclosing a set of points
  -- *
  -- *  \return SDL_TRUE if any points were within the clipping rect
  --  

   function SDL_EnclosePoints
     (points : access constant SDL_Point;
      count : int;
      clip : access constant SDL_Rect;
      result : access SDL_Rect) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:125
   pragma Import (C, SDL_EnclosePoints, "SDL_EnclosePoints");

  --*
  -- *  \brief Calculate the intersection of a rectangle and line segment.
  -- *
  -- *  \return SDL_TRUE if there is an intersection, SDL_FALSE otherwise.
  --  

   function SDL_IntersectRectAndLine
     (rect : access constant SDL_Rect;
      X1 : access int;
      Y1 : access int;
      X2 : access int;
      Y2 : access int) return SDL_stdinc_h.SDL_bool;  -- ..\SDL2_tmp\SDL_rect.h:135
   pragma Import (C, SDL_IntersectRectAndLine, "SDL_IntersectRectAndLine");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_rect_h;
