pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with GL_gl_h;
with System;

package GL_glu_h is

   --  arg-macro: function gluErrorStringWIN (errCode)
   --    return (LPCWSTR) gluErrorString(errCode);

   GLU_VERSION_1_1 : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:111
   GLU_VERSION_1_2 : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:112

   GLU_INVALID_ENUM : constant := 100900;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:114
   GLU_INVALID_VALUE : constant := 100901;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:115
   GLU_OUT_OF_MEMORY : constant := 100902;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:116
   GLU_INCOMPATIBLE_GL_VERSION : constant := 100903;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:117

   GLU_VERSION : constant := 100800;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:119
   GLU_EXTENSIONS : constant := 100801;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:120
   --  unsupported macro: GLU_TRUE GL_TRUE
   --  unsupported macro: GLU_FALSE GL_FALSE

   GLU_SMOOTH : constant := 100000;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:125
   GLU_FLAT : constant := 100001;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:126
   GLU_NONE : constant := 100002;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:127

   GLU_POINT : constant := 100010;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:129
   GLU_LINE : constant := 100011;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:130
   GLU_FILL : constant := 100012;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:131
   GLU_SILHOUETTE : constant := 100013;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:132

   GLU_OUTSIDE : constant := 100020;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:134
   GLU_INSIDE : constant := 100021;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:135

   GLU_TESS_MAX_COORD : constant := 1.0e150;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:137

   GLU_TESS_WINDING_RULE : constant := 100140;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:139
   GLU_TESS_BOUNDARY_ONLY : constant := 100141;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:140
   GLU_TESS_TOLERANCE : constant := 100142;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:141

   GLU_TESS_WINDING_ODD : constant := 100130;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:143
   GLU_TESS_WINDING_NONZERO : constant := 100131;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:144
   GLU_TESS_WINDING_POSITIVE : constant := 100132;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:145
   GLU_TESS_WINDING_NEGATIVE : constant := 100133;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:146
   GLU_TESS_WINDING_ABS_GEQ_TWO : constant := 100134;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:147

   GLU_TESS_BEGIN : constant := 100100;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:149
   GLU_TESS_VERTEX : constant := 100101;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:150
   GLU_TESS_END : constant := 100102;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:151
   GLU_TESS_ERROR : constant := 100103;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:152
   GLU_TESS_EDGE_FLAG : constant := 100104;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:153
   GLU_TESS_COMBINE : constant := 100105;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:154
   GLU_TESS_BEGIN_DATA : constant := 100106;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:155
   GLU_TESS_VERTEX_DATA : constant := 100107;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:156
   GLU_TESS_END_DATA : constant := 100108;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:157
   GLU_TESS_ERROR_DATA : constant := 100109;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:158
   GLU_TESS_EDGE_FLAG_DATA : constant := 100110;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:159
   GLU_TESS_COMBINE_DATA : constant := 100111;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:160

   GLU_TESS_ERROR1 : constant := 100151;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:162
   GLU_TESS_ERROR2 : constant := 100152;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:163
   GLU_TESS_ERROR3 : constant := 100153;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:164
   GLU_TESS_ERROR4 : constant := 100154;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:165
   GLU_TESS_ERROR5 : constant := 100155;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:166
   GLU_TESS_ERROR6 : constant := 100156;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:167
   GLU_TESS_ERROR7 : constant := 100157;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:168
   GLU_TESS_ERROR8 : constant := 100158;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:169
   --  unsupported macro: GLU_TESS_MISSING_BEGIN_POLYGON GLU_TESS_ERROR1
   --  unsupported macro: GLU_TESS_MISSING_BEGIN_CONTOUR GLU_TESS_ERROR2
   --  unsupported macro: GLU_TESS_MISSING_END_POLYGON GLU_TESS_ERROR3
   --  unsupported macro: GLU_TESS_MISSING_END_CONTOUR GLU_TESS_ERROR4
   --  unsupported macro: GLU_TESS_COORD_TOO_LARGE GLU_TESS_ERROR5
   --  unsupported macro: GLU_TESS_NEED_COMBINE_CALLBACK GLU_TESS_ERROR6

   GLU_AUTO_LOAD_MATRIX : constant := 100200;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:178
   GLU_CULLING : constant := 100201;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:179
   GLU_SAMPLING_TOLERANCE : constant := 100203;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:180
   GLU_DISPLAY_MODE : constant := 100204;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:181
   GLU_PARAMETRIC_TOLERANCE : constant := 100202;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:182
   GLU_SAMPLING_METHOD : constant := 100205;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:183
   GLU_U_STEP : constant := 100206;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:184
   GLU_V_STEP : constant := 100207;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:185

   GLU_PATH_LENGTH : constant := 100215;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:187
   GLU_PARAMETRIC_ERROR : constant := 100216;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:188
   GLU_DOMAIN_DISTANCE : constant := 100217;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:189

   GLU_MAP1_TRIM_2 : constant := 100210;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:191
   GLU_MAP1_TRIM_3 : constant := 100211;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:192

   GLU_OUTLINE_POLYGON : constant := 100240;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:194
   GLU_OUTLINE_PATCH : constant := 100241;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:195

   GLU_NURBS_ERROR1 : constant := 100251;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:197
   GLU_NURBS_ERROR2 : constant := 100252;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:198
   GLU_NURBS_ERROR3 : constant := 100253;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:199
   GLU_NURBS_ERROR4 : constant := 100254;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:200
   GLU_NURBS_ERROR5 : constant := 100255;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:201
   GLU_NURBS_ERROR6 : constant := 100256;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:202
   GLU_NURBS_ERROR7 : constant := 100257;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:203
   GLU_NURBS_ERROR8 : constant := 100258;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:204
   GLU_NURBS_ERROR9 : constant := 100259;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:205
   GLU_NURBS_ERROR10 : constant := 100260;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:206
   GLU_NURBS_ERROR11 : constant := 100261;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:207
   GLU_NURBS_ERROR12 : constant := 100262;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:208
   GLU_NURBS_ERROR13 : constant := 100263;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:209
   GLU_NURBS_ERROR14 : constant := 100264;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:210
   GLU_NURBS_ERROR15 : constant := 100265;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:211
   GLU_NURBS_ERROR16 : constant := 100266;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:212
   GLU_NURBS_ERROR17 : constant := 100267;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:213
   GLU_NURBS_ERROR18 : constant := 100268;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:214
   GLU_NURBS_ERROR19 : constant := 100269;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:215
   GLU_NURBS_ERROR20 : constant := 100270;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:216
   GLU_NURBS_ERROR21 : constant := 100271;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:217
   GLU_NURBS_ERROR22 : constant := 100272;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:218
   GLU_NURBS_ERROR23 : constant := 100273;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:219
   GLU_NURBS_ERROR24 : constant := 100274;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:220
   GLU_NURBS_ERROR25 : constant := 100275;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:221
   GLU_NURBS_ERROR26 : constant := 100276;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:222
   GLU_NURBS_ERROR27 : constant := 100277;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:223
   GLU_NURBS_ERROR28 : constant := 100278;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:224
   GLU_NURBS_ERROR29 : constant := 100279;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:225
   GLU_NURBS_ERROR30 : constant := 100280;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:226
   GLU_NURBS_ERROR31 : constant := 100281;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:227
   GLU_NURBS_ERROR32 : constant := 100282;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:228
   GLU_NURBS_ERROR33 : constant := 100283;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:229
   GLU_NURBS_ERROR34 : constant := 100284;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:230
   GLU_NURBS_ERROR35 : constant := 100285;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:231
   GLU_NURBS_ERROR36 : constant := 100286;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:232
   GLU_NURBS_ERROR37 : constant := 100287;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:233

   GLU_CW : constant := 100120;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:239
   GLU_CCW : constant := 100121;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:240
   GLU_INTERIOR : constant := 100122;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:241
   GLU_EXTERIOR : constant := 100123;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:242
   GLU_UNKNOWN : constant := 100124;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:243
   --  unsupported macro: GLU_BEGIN GLU_TESS_BEGIN
   --  unsupported macro: GLU_VERTEX GLU_TESS_VERTEX
   --  unsupported macro: GLU_END GLU_TESS_END
   --  unsupported macro: GLU_ERROR GLU_TESS_ERROR
   --  unsupported macro: GLU_EDGE_FLAG GLU_TESS_EDGE_FLAG

   function gluErrorString (errCode : GL_gl_h.GLenum) return access GL_gl_h.GLubyte;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:24
   pragma Import (Stdcall, gluErrorString, "gluErrorString");

   function gluErrorUnicodeStringEXT (errCode : GL_gl_h.GLenum) return access wchar_t;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:25
   pragma Import (Stdcall, gluErrorUnicodeStringEXT, "gluErrorUnicodeStringEXT");

   function gluGetString (name : GL_gl_h.GLenum) return access GL_gl_h.GLubyte;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:26
   pragma Import (Stdcall, gluGetString, "gluGetString");

   procedure gluOrtho2D
     (left : GL_gl_h.GLdouble;
      right : GL_gl_h.GLdouble;
      bottom : GL_gl_h.GLdouble;
      top : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:27
   pragma Import (Stdcall, gluOrtho2D, "gluOrtho2D");

   procedure gluPerspective
     (fovy : GL_gl_h.GLdouble;
      aspect : GL_gl_h.GLdouble;
      zNear : GL_gl_h.GLdouble;
      zFar : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:28
   pragma Import (Stdcall, gluPerspective, "gluPerspective");

   procedure gluPickMatrix
     (x : GL_gl_h.GLdouble;
      y : GL_gl_h.GLdouble;
      width : GL_gl_h.GLdouble;
      height : GL_gl_h.GLdouble;
      viewport : access GL_gl_h.GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:29
   pragma Import (Stdcall, gluPickMatrix, "gluPickMatrix");

   procedure gluLookAt
     (eyex : GL_gl_h.GLdouble;
      eyey : GL_gl_h.GLdouble;
      eyez : GL_gl_h.GLdouble;
      centerx : GL_gl_h.GLdouble;
      centery : GL_gl_h.GLdouble;
      centerz : GL_gl_h.GLdouble;
      upx : GL_gl_h.GLdouble;
      upy : GL_gl_h.GLdouble;
      upz : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:30
   pragma Import (Stdcall, gluLookAt, "gluLookAt");

   function gluProject
     (objx : GL_gl_h.GLdouble;
      objy : GL_gl_h.GLdouble;
      objz : GL_gl_h.GLdouble;
      modelMatrix : access GL_gl_h.GLdouble;
      projMatrix : access GL_gl_h.GLdouble;
      viewport : access GL_gl_h.GLint;
      winx : access GL_gl_h.GLdouble;
      winy : access GL_gl_h.GLdouble;
      winz : access GL_gl_h.GLdouble) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:31
   pragma Import (Stdcall, gluProject, "gluProject");

   function gluUnProject
     (winx : GL_gl_h.GLdouble;
      winy : GL_gl_h.GLdouble;
      winz : GL_gl_h.GLdouble;
      modelMatrix : access GL_gl_h.GLdouble;
      projMatrix : access GL_gl_h.GLdouble;
      viewport : access GL_gl_h.GLint;
      objx : access GL_gl_h.GLdouble;
      objy : access GL_gl_h.GLdouble;
      objz : access GL_gl_h.GLdouble) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:32
   pragma Import (Stdcall, gluUnProject, "gluUnProject");

   function gluScaleImage
     (format : GL_gl_h.GLenum;
      widthin : GL_gl_h.GLint;
      heightin : GL_gl_h.GLint;
      typein : GL_gl_h.GLenum;
      datain : System.Address;
      widthout : GL_gl_h.GLint;
      heightout : GL_gl_h.GLint;
      typeout : GL_gl_h.GLenum;
      dataout : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:33
   pragma Import (Stdcall, gluScaleImage, "gluScaleImage");

   function gluBuild1DMipmaps
     (target : GL_gl_h.GLenum;
      components : GL_gl_h.GLint;
      width : GL_gl_h.GLint;
      format : GL_gl_h.GLenum;
      c_type : GL_gl_h.GLenum;
      data : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:34
   pragma Import (Stdcall, gluBuild1DMipmaps, "gluBuild1DMipmaps");

   function gluBuild2DMipmaps
     (target : GL_gl_h.GLenum;
      components : GL_gl_h.GLint;
      width : GL_gl_h.GLint;
      height : GL_gl_h.GLint;
      format : GL_gl_h.GLenum;
      c_type : GL_gl_h.GLenum;
      data : System.Address) return int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:35
   pragma Import (Stdcall, gluBuild2DMipmaps, "gluBuild2DMipmaps");

   --  skipped empty struct GLUnurbs

   --  skipped empty struct GLUquadric

   --  skipped empty struct GLUtesselator

   --  skipped empty struct GLUnurbsObj

   --  skipped empty struct GLUquadricObj

   --  skipped empty struct GLUtesselatorObj

   --  skipped empty struct GLUtriangulatorObj

   function gluNewQuadric return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:58
   pragma Import (Stdcall, gluNewQuadric, "gluNewQuadric");

   procedure gluDeleteQuadric (state : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:59
   pragma Import (Stdcall, gluDeleteQuadric, "gluDeleteQuadric");

   procedure gluQuadricNormals (quadObject : System.Address; normals : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:60
   pragma Import (Stdcall, gluQuadricNormals, "gluQuadricNormals");

   procedure gluQuadricTexture (quadObject : System.Address; textureCoords : GL_gl_h.GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:61
   pragma Import (Stdcall, gluQuadricTexture, "gluQuadricTexture");

   procedure gluQuadricOrientation (quadObject : System.Address; orientation : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:62
   pragma Import (Stdcall, gluQuadricOrientation, "gluQuadricOrientation");

   procedure gluQuadricDrawStyle (quadObject : System.Address; drawStyle : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:63
   pragma Import (Stdcall, gluQuadricDrawStyle, "gluQuadricDrawStyle");

   procedure gluCylinder
     (qobj : System.Address;
      baseRadius : GL_gl_h.GLdouble;
      topRadius : GL_gl_h.GLdouble;
      height : GL_gl_h.GLdouble;
      slices : GL_gl_h.GLint;
      stacks : GL_gl_h.GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:64
   pragma Import (Stdcall, gluCylinder, "gluCylinder");

   procedure gluDisk
     (qobj : System.Address;
      innerRadius : GL_gl_h.GLdouble;
      outerRadius : GL_gl_h.GLdouble;
      slices : GL_gl_h.GLint;
      loops : GL_gl_h.GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:65
   pragma Import (Stdcall, gluDisk, "gluDisk");

   procedure gluPartialDisk
     (qobj : System.Address;
      innerRadius : GL_gl_h.GLdouble;
      outerRadius : GL_gl_h.GLdouble;
      slices : GL_gl_h.GLint;
      loops : GL_gl_h.GLint;
      startAngle : GL_gl_h.GLdouble;
      sweepAngle : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:66
   pragma Import (Stdcall, gluPartialDisk, "gluPartialDisk");

   procedure gluSphere
     (qobj : System.Address;
      radius : GL_gl_h.GLdouble;
      slices : GL_gl_h.GLint;
      stacks : GL_gl_h.GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:67
   pragma Import (Stdcall, gluSphere, "gluSphere");

   procedure gluQuadricCallback
     (qobj : System.Address;
      which : GL_gl_h.GLenum;
      fn : access procedure);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:68
   pragma Import (Stdcall, gluQuadricCallback, "gluQuadricCallback");

   function gluNewTess return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:69
   pragma Import (Stdcall, gluNewTess, "gluNewTess");

   procedure gluDeleteTess (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:70
   pragma Import (Stdcall, gluDeleteTess, "gluDeleteTess");

   procedure gluTessBeginPolygon (tess : System.Address; polygon_data : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:71
   pragma Import (Stdcall, gluTessBeginPolygon, "gluTessBeginPolygon");

   procedure gluTessBeginContour (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:72
   pragma Import (Stdcall, gluTessBeginContour, "gluTessBeginContour");

   procedure gluTessVertex
     (tess : System.Address;
      coords : access GL_gl_h.GLdouble;
      data : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:73
   pragma Import (Stdcall, gluTessVertex, "gluTessVertex");

   procedure gluTessEndContour (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:74
   pragma Import (Stdcall, gluTessEndContour, "gluTessEndContour");

   procedure gluTessEndPolygon (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:75
   pragma Import (Stdcall, gluTessEndPolygon, "gluTessEndPolygon");

   procedure gluTessProperty
     (tess : System.Address;
      which : GL_gl_h.GLenum;
      value : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:76
   pragma Import (Stdcall, gluTessProperty, "gluTessProperty");

   procedure gluTessNormal
     (tess : System.Address;
      x : GL_gl_h.GLdouble;
      y : GL_gl_h.GLdouble;
      z : GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:77
   pragma Import (Stdcall, gluTessNormal, "gluTessNormal");

   procedure gluTessCallback
     (tess : System.Address;
      which : GL_gl_h.GLenum;
      fn : access procedure);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:78
   pragma Import (Stdcall, gluTessCallback, "gluTessCallback");

   procedure gluGetTessProperty
     (tess : System.Address;
      which : GL_gl_h.GLenum;
      value : access GL_gl_h.GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:79
   pragma Import (Stdcall, gluGetTessProperty, "gluGetTessProperty");

   function gluNewNurbsRenderer return System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:80
   pragma Import (Stdcall, gluNewNurbsRenderer, "gluNewNurbsRenderer");

   procedure gluDeleteNurbsRenderer (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:81
   pragma Import (Stdcall, gluDeleteNurbsRenderer, "gluDeleteNurbsRenderer");

   procedure gluBeginSurface (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:82
   pragma Import (Stdcall, gluBeginSurface, "gluBeginSurface");

   procedure gluBeginCurve (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:83
   pragma Import (Stdcall, gluBeginCurve, "gluBeginCurve");

   procedure gluEndCurve (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:84
   pragma Import (Stdcall, gluEndCurve, "gluEndCurve");

   procedure gluEndSurface (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:85
   pragma Import (Stdcall, gluEndSurface, "gluEndSurface");

   procedure gluBeginTrim (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:86
   pragma Import (Stdcall, gluBeginTrim, "gluBeginTrim");

   procedure gluEndTrim (nobj : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:87
   pragma Import (Stdcall, gluEndTrim, "gluEndTrim");

   procedure gluPwlCurve
     (nobj : System.Address;
      count : GL_gl_h.GLint;
      c_array : access GL_gl_h.GLfloat;
      stride : GL_gl_h.GLint;
      c_type : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:88
   pragma Import (Stdcall, gluPwlCurve, "gluPwlCurve");

   procedure gluNurbsCurve
     (nobj : System.Address;
      nknots : GL_gl_h.GLint;
      knot : access GL_gl_h.GLfloat;
      stride : GL_gl_h.GLint;
      ctlarray : access GL_gl_h.GLfloat;
      order : GL_gl_h.GLint;
      c_type : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:89
   pragma Import (Stdcall, gluNurbsCurve, "gluNurbsCurve");

   procedure gluNurbsSurface
     (nobj : System.Address;
      sknot_count : GL_gl_h.GLint;
      sknot : access float;
      tknot_count : GL_gl_h.GLint;
      tknot : access GL_gl_h.GLfloat;
      s_stride : GL_gl_h.GLint;
      t_stride : GL_gl_h.GLint;
      ctlarray : access GL_gl_h.GLfloat;
      sorder : GL_gl_h.GLint;
      torder : GL_gl_h.GLint;
      c_type : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:90
   pragma Import (Stdcall, gluNurbsSurface, "gluNurbsSurface");

   procedure gluLoadSamplingMatrices
     (nobj : System.Address;
      modelMatrix : access GL_gl_h.GLfloat;
      projMatrix : access GL_gl_h.GLfloat;
      viewport : access GL_gl_h.GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:91
   pragma Import (Stdcall, gluLoadSamplingMatrices, "gluLoadSamplingMatrices");

   procedure gluNurbsProperty
     (nobj : System.Address;
      property : GL_gl_h.GLenum;
      value : GL_gl_h.GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:92
   pragma Import (Stdcall, gluNurbsProperty, "gluNurbsProperty");

   procedure gluGetNurbsProperty
     (nobj : System.Address;
      property : GL_gl_h.GLenum;
      value : access GL_gl_h.GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:93
   pragma Import (Stdcall, gluGetNurbsProperty, "gluGetNurbsProperty");

   procedure gluNurbsCallback
     (nobj : System.Address;
      which : GL_gl_h.GLenum;
      fn : access procedure);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:94
   pragma Import (Stdcall, gluNurbsCallback, "gluNurbsCallback");

   type GLUquadricErrorProc is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, GLUquadricErrorProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:96

   type GLUtessBeginProc is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, GLUtessBeginProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:97

   type GLUtessEdgeFlagProc is access procedure (arg1 : GL_gl_h.GLboolean);
   pragma Convention (C, GLUtessEdgeFlagProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:98

   type GLUtessVertexProc is access procedure (arg1 : System.Address);
   pragma Convention (C, GLUtessVertexProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:99

   type GLUtessEndProc is access procedure;
   pragma Convention (C, GLUtessEndProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:100

   type GLUtessErrorProc is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, GLUtessErrorProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:101

   type GLUtessCombineProc is access procedure
        (arg1 : access GL_gl_h.GLdouble;
         arg2 : System.Address;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : System.Address);
   pragma Convention (C, GLUtessCombineProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:102

   type GLUtessBeginDataProc is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, GLUtessBeginDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:103

   type GLUtessEdgeFlagDataProc is access procedure (arg1 : GL_gl_h.GLboolean; arg2 : System.Address);
   pragma Convention (C, GLUtessEdgeFlagDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:104

   type GLUtessVertexDataProc is access procedure (arg1 : System.Address; arg2 : System.Address);
   pragma Convention (C, GLUtessVertexDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:105

   type GLUtessEndDataProc is access procedure (arg1 : System.Address);
   pragma Convention (C, GLUtessEndDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:106

   type GLUtessErrorDataProc is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, GLUtessErrorDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:107

   type GLUtessCombineDataProc is access procedure
        (arg1 : access GL_gl_h.GLdouble;
         arg2 : System.Address;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : System.Address;
         arg5 : System.Address);
   pragma Convention (C, GLUtessCombineDataProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:108

   type GLUnurbsErrorProc is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, GLUnurbsErrorProc);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:109

   procedure gluBeginPolygon (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:235
   pragma Import (Stdcall, gluBeginPolygon, "gluBeginPolygon");

   procedure gluNextContour (tess : System.Address; c_type : GL_gl_h.GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:236
   pragma Import (Stdcall, gluNextContour, "gluNextContour");

   procedure gluEndPolygon (tess : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/glu.h:237
   pragma Import (Stdcall, gluEndPolygon, "gluEndPolygon");

end GL_glu_h;
