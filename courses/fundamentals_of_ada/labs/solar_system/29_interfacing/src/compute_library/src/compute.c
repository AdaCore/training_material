// -----------------------------------------------------------------------
//                              Ada Labs                             --
//                                                                   --
//                 Copyright (C) 2008-2009, AdaCore                  --
//                                                                   --
// Labs is free  software; you can redistribute it and/or modify  it --
// under the terms of the GNU General Public License as published by --
// the Free Software Foundation; either version 2 of the License, or --
// (at your option) any later version.                               --
//                                                                   --
// This program is  distributed in the hope that it will be  useful, --
// but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
// General Public License for more details. You should have received --
// a copy of the GNU General Public License along with this program; --
// if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
// Place - Suite 330, Boston, MA 02111-1307, USA.                    --
// -----------------------------------------------------------------------

extern float ada_cos (float x);
extern float ada_sin (float x);

typedef struct rgba_t {
  unsigned char r;
  unsigned char g;
  unsigned char b;
  unsigned char a;
} rgba_t;

typedef struct body_t {
  float x;
  float y;
  float distance;
  float speed;
  float angle;
  float radius;
  rgba_t color;
  int visible;
  struct body_t* turns_around;
} body_t;

float compute_x (body_t* body_to_move) {
  return body_to_move->turns_around->x +
      body_to_move->distance * ada_cos (body_to_move->angle);
}

float compute_y (body_t* body_to_move) {
  return body_to_move->turns_around->y +
      body_to_move->distance * ada_sin (body_to_move->angle);
}
