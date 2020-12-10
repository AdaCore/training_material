pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with System;

package GL_gl_h is


   GL_VERSION_1_1 : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:38

   GL_ACCUM : constant := 16#0100#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:40
   GL_LOAD : constant := 16#0101#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:41
   GL_RETURN : constant := 16#0102#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:42
   GL_MULT : constant := 16#0103#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:43
   GL_ADD : constant := 16#0104#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:44

   GL_NEVER : constant := 16#0200#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:46
   GL_LESS : constant := 16#0201#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:47
   GL_EQUAL : constant := 16#0202#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:48
   GL_LEQUAL : constant := 16#0203#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:49
   GL_GREATER : constant := 16#0204#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:50
   GL_NOTEQUAL : constant := 16#0205#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:51
   GL_GEQUAL : constant := 16#0206#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:52
   GL_ALWAYS : constant := 16#0207#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:53

   GL_CURRENT_BIT : constant := 16#00000001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:55
   GL_POINT_BIT : constant := 16#00000002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:56
   GL_LINE_BIT : constant := 16#00000004#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:57
   GL_POLYGON_BIT : constant := 16#00000008#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:58
   GL_POLYGON_STIPPLE_BIT : constant := 16#00000010#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:59
   GL_PIXEL_MODE_BIT : constant := 16#00000020#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:60
   GL_LIGHTING_BIT : constant := 16#00000040#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:61
   GL_FOG_BIT : constant := 16#00000080#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:62
   GL_DEPTH_BUFFER_BIT : constant := 16#00000100#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:63
   GL_ACCUM_BUFFER_BIT : constant := 16#00000200#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:64
   GL_STENCIL_BUFFER_BIT : constant := 16#00000400#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:65
   GL_VIEWPORT_BIT : constant := 16#00000800#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:66
   GL_TRANSFORM_BIT : constant := 16#00001000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:67
   GL_ENABLE_BIT : constant := 16#00002000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:68
   GL_COLOR_BUFFER_BIT : constant := 16#00004000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:69
   GL_HINT_BIT : constant := 16#00008000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:70
   GL_EVAL_BIT : constant := 16#00010000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:71
   GL_LIST_BIT : constant := 16#00020000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:72
   GL_TEXTURE_BIT : constant := 16#00040000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:73
   GL_SCISSOR_BIT : constant := 16#00080000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:74
   GL_ALL_ATTRIB_BITS : constant := 16#000fffff#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:75

   GL_POINTS : constant := 16#0000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:77
   GL_LINES : constant := 16#0001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:78
   GL_LINE_LOOP : constant := 16#0002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:79
   GL_LINE_STRIP : constant := 16#0003#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:80
   GL_TRIANGLES : constant := 16#0004#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:81
   GL_TRIANGLE_STRIP : constant := 16#0005#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:82
   GL_TRIANGLE_FAN : constant := 16#0006#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:83
   GL_QUADS : constant := 16#0007#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:84
   GL_QUAD_STRIP : constant := 16#0008#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:85
   GL_POLYGON : constant := 16#0009#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:86

   GL_ZERO : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:88
   GL_ONE : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:89
   GL_SRC_COLOR : constant := 16#0300#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:90
   GL_ONE_MINUS_SRC_COLOR : constant := 16#0301#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:91
   GL_SRC_ALPHA : constant := 16#0302#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:92
   GL_ONE_MINUS_SRC_ALPHA : constant := 16#0303#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:93
   GL_DST_ALPHA : constant := 16#0304#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:94
   GL_ONE_MINUS_DST_ALPHA : constant := 16#0305#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:95

   GL_DST_COLOR : constant := 16#0306#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:97
   GL_ONE_MINUS_DST_COLOR : constant := 16#0307#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:98
   GL_SRC_ALPHA_SATURATE : constant := 16#0308#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:99

   GL_TRUE : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:101
   GL_FALSE : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:102

   GL_CLIP_PLANE0 : constant := 16#3000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:104
   GL_CLIP_PLANE1 : constant := 16#3001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:105
   GL_CLIP_PLANE2 : constant := 16#3002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:106
   GL_CLIP_PLANE3 : constant := 16#3003#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:107
   GL_CLIP_PLANE4 : constant := 16#3004#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:108
   GL_CLIP_PLANE5 : constant := 16#3005#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:109

   GL_BYTE : constant := 16#1400#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:111
   GL_UNSIGNED_BYTE : constant := 16#1401#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:112
   GL_SHORT : constant := 16#1402#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:113
   GL_UNSIGNED_SHORT : constant := 16#1403#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:114
   GL_INT : constant := 16#1404#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:115
   GL_UNSIGNED_INT : constant := 16#1405#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:116
   GL_FLOAT : constant := 16#1406#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:117
   GL_2_BYTES : constant := 16#1407#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:118
   GL_3_BYTES : constant := 16#1408#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:119
   GL_4_BYTES : constant := 16#1409#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:120
   GL_DOUBLE : constant := 16#140A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:121

   GL_NONE : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:123
   GL_FRONT_LEFT : constant := 16#0400#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:124
   GL_FRONT_RIGHT : constant := 16#0401#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:125
   GL_BACK_LEFT : constant := 16#0402#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:126
   GL_BACK_RIGHT : constant := 16#0403#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:127
   GL_FRONT : constant := 16#0404#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:128
   GL_BACK : constant := 16#0405#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:129
   GL_LEFT : constant := 16#0406#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:130
   GL_RIGHT : constant := 16#0407#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:131
   GL_FRONT_AND_BACK : constant := 16#0408#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:132
   GL_AUX0 : constant := 16#0409#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:133
   GL_AUX1 : constant := 16#040A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:134
   GL_AUX2 : constant := 16#040B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:135
   GL_AUX3 : constant := 16#040C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:136

   GL_NO_ERROR : constant := 0;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:138
   GL_INVALID_ENUM : constant := 16#0500#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:139
   GL_INVALID_VALUE : constant := 16#0501#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:140
   GL_INVALID_OPERATION : constant := 16#0502#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:141
   GL_STACK_OVERFLOW : constant := 16#0503#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:142
   GL_STACK_UNDERFLOW : constant := 16#0504#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:143
   GL_OUT_OF_MEMORY : constant := 16#0505#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:144

   GL_2D : constant := 16#0600#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:146
   GL_3D : constant := 16#0601#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:147
   GL_3D_COLOR : constant := 16#0602#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:148
   GL_3D_COLOR_TEXTURE : constant := 16#0603#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:149
   GL_4D_COLOR_TEXTURE : constant := 16#0604#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:150

   GL_PASS_THROUGH_TOKEN : constant := 16#0700#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:152
   GL_POINT_TOKEN : constant := 16#0701#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:153
   GL_LINE_TOKEN : constant := 16#0702#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:154
   GL_POLYGON_TOKEN : constant := 16#0703#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:155
   GL_BITMAP_TOKEN : constant := 16#0704#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:156
   GL_DRAW_PIXEL_TOKEN : constant := 16#0705#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:157
   GL_COPY_PIXEL_TOKEN : constant := 16#0706#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:158
   GL_LINE_RESET_TOKEN : constant := 16#0707#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:159

   GL_EXP : constant := 16#0800#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:161
   GL_EXP2 : constant := 16#0801#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:162

   GL_CW : constant := 16#0900#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:164
   GL_CCW : constant := 16#0901#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:165

   GL_COEFF : constant := 16#0A00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:167
   GL_ORDER : constant := 16#0A01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:168
   GL_DOMAIN : constant := 16#0A02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:169

   GL_CURRENT_COLOR : constant := 16#0B00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:171
   GL_CURRENT_INDEX : constant := 16#0B01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:172
   GL_CURRENT_NORMAL : constant := 16#0B02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:173
   GL_CURRENT_TEXTURE_COORDS : constant := 16#0B03#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:174
   GL_CURRENT_RASTER_COLOR : constant := 16#0B04#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:175
   GL_CURRENT_RASTER_INDEX : constant := 16#0B05#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:176
   GL_CURRENT_RASTER_TEXTURE_COORDS : constant := 16#0B06#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:177
   GL_CURRENT_RASTER_POSITION : constant := 16#0B07#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:178
   GL_CURRENT_RASTER_POSITION_VALID : constant := 16#0B08#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:179
   GL_CURRENT_RASTER_DISTANCE : constant := 16#0B09#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:180
   GL_POINT_SMOOTH : constant := 16#0B10#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:181
   GL_POINT_SIZE : constant := 16#0B11#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:182
   GL_POINT_SIZE_RANGE : constant := 16#0B12#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:183
   GL_POINT_SIZE_GRANULARITY : constant := 16#0B13#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:184
   GL_LINE_SMOOTH : constant := 16#0B20#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:185
   GL_LINE_WIDTH : constant := 16#0B21#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:186
   GL_LINE_WIDTH_RANGE : constant := 16#0B22#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:187
   GL_LINE_WIDTH_GRANULARITY : constant := 16#0B23#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:188
   GL_LINE_STIPPLE : constant := 16#0B24#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:189
   GL_LINE_STIPPLE_PATTERN : constant := 16#0B25#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:190
   GL_LINE_STIPPLE_REPEAT : constant := 16#0B26#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:191
   GL_LIST_MODE : constant := 16#0B30#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:192
   GL_MAX_LIST_NESTING : constant := 16#0B31#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:193
   GL_LIST_BASE : constant := 16#0B32#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:194
   GL_LIST_INDEX : constant := 16#0B33#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:195
   GL_POLYGON_MODE : constant := 16#0B40#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:196
   GL_POLYGON_SMOOTH : constant := 16#0B41#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:197
   GL_POLYGON_STIPPLE : constant := 16#0B42#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:198
   GL_EDGE_FLAG : constant := 16#0B43#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:199
   GL_CULL_FACE : constant := 16#0B44#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:200
   GL_CULL_FACE_MODE : constant := 16#0B45#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:201
   GL_FRONT_FACE : constant := 16#0B46#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:202
   GL_LIGHTING : constant := 16#0B50#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:203
   GL_LIGHT_MODEL_LOCAL_VIEWER : constant := 16#0B51#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:204
   GL_LIGHT_MODEL_TWO_SIDE : constant := 16#0B52#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:205
   GL_LIGHT_MODEL_AMBIENT : constant := 16#0B53#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:206
   GL_SHADE_MODEL : constant := 16#0B54#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:207
   GL_COLOR_MATERIAL_FACE : constant := 16#0B55#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:208
   GL_COLOR_MATERIAL_PARAMETER : constant := 16#0B56#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:209
   GL_COLOR_MATERIAL : constant := 16#0B57#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:210
   GL_FOG : constant := 16#0B60#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:211
   GL_FOG_INDEX : constant := 16#0B61#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:212
   GL_FOG_DENSITY : constant := 16#0B62#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:213
   GL_FOG_START : constant := 16#0B63#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:214
   GL_FOG_END : constant := 16#0B64#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:215
   GL_FOG_MODE : constant := 16#0B65#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:216
   GL_FOG_COLOR : constant := 16#0B66#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:217
   GL_DEPTH_RANGE : constant := 16#0B70#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:218
   GL_DEPTH_TEST : constant := 16#0B71#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:219
   GL_DEPTH_WRITEMASK : constant := 16#0B72#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:220
   GL_DEPTH_CLEAR_VALUE : constant := 16#0B73#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:221
   GL_DEPTH_FUNC : constant := 16#0B74#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:222
   GL_ACCUM_CLEAR_VALUE : constant := 16#0B80#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:223
   GL_STENCIL_TEST : constant := 16#0B90#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:224
   GL_STENCIL_CLEAR_VALUE : constant := 16#0B91#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:225
   GL_STENCIL_FUNC : constant := 16#0B92#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:226
   GL_STENCIL_VALUE_MASK : constant := 16#0B93#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:227
   GL_STENCIL_FAIL : constant := 16#0B94#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:228
   GL_STENCIL_PASS_DEPTH_FAIL : constant := 16#0B95#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:229
   GL_STENCIL_PASS_DEPTH_PASS : constant := 16#0B96#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:230
   GL_STENCIL_REF : constant := 16#0B97#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:231
   GL_STENCIL_WRITEMASK : constant := 16#0B98#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:232
   GL_MATRIX_MODE : constant := 16#0BA0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:233
   GL_NORMALIZE : constant := 16#0BA1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:234
   GL_VIEWPORT : constant := 16#0BA2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:235
   GL_MODELVIEW_STACK_DEPTH : constant := 16#0BA3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:236
   GL_PROJECTION_STACK_DEPTH : constant := 16#0BA4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:237
   GL_TEXTURE_STACK_DEPTH : constant := 16#0BA5#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:238
   GL_MODELVIEW_MATRIX : constant := 16#0BA6#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:239
   GL_PROJECTION_MATRIX : constant := 16#0BA7#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:240
   GL_TEXTURE_MATRIX : constant := 16#0BA8#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:241
   GL_ATTRIB_STACK_DEPTH : constant := 16#0BB0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:242
   GL_CLIENT_ATTRIB_STACK_DEPTH : constant := 16#0BB1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:243
   GL_ALPHA_TEST : constant := 16#0BC0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:244
   GL_ALPHA_TEST_FUNC : constant := 16#0BC1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:245
   GL_ALPHA_TEST_REF : constant := 16#0BC2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:246
   GL_DITHER : constant := 16#0BD0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:247
   GL_BLEND_DST : constant := 16#0BE0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:248
   GL_BLEND_SRC : constant := 16#0BE1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:249
   GL_BLEND : constant := 16#0BE2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:250
   GL_LOGIC_OP_MODE : constant := 16#0BF0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:251
   GL_INDEX_LOGIC_OP : constant := 16#0BF1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:252
   GL_COLOR_LOGIC_OP : constant := 16#0BF2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:253
   GL_AUX_BUFFERS : constant := 16#0C00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:254
   GL_DRAW_BUFFER : constant := 16#0C01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:255
   GL_READ_BUFFER : constant := 16#0C02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:256
   GL_SCISSOR_BOX : constant := 16#0C10#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:257
   GL_SCISSOR_TEST : constant := 16#0C11#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:258
   GL_INDEX_CLEAR_VALUE : constant := 16#0C20#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:259
   GL_INDEX_WRITEMASK : constant := 16#0C21#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:260
   GL_COLOR_CLEAR_VALUE : constant := 16#0C22#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:261
   GL_COLOR_WRITEMASK : constant := 16#0C23#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:262
   GL_INDEX_MODE : constant := 16#0C30#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:263
   GL_RGBA_MODE : constant := 16#0C31#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:264
   GL_DOUBLEBUFFER : constant := 16#0C32#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:265
   GL_STEREO : constant := 16#0C33#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:266
   GL_RENDER_MODE : constant := 16#0C40#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:267
   GL_PERSPECTIVE_CORRECTION_HINT : constant := 16#0C50#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:268
   GL_POINT_SMOOTH_HINT : constant := 16#0C51#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:269
   GL_LINE_SMOOTH_HINT : constant := 16#0C52#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:270
   GL_POLYGON_SMOOTH_HINT : constant := 16#0C53#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:271
   GL_FOG_HINT : constant := 16#0C54#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:272
   GL_TEXTURE_GEN_S : constant := 16#0C60#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:273
   GL_TEXTURE_GEN_T : constant := 16#0C61#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:274
   GL_TEXTURE_GEN_R : constant := 16#0C62#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:275
   GL_TEXTURE_GEN_Q : constant := 16#0C63#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:276
   GL_PIXEL_MAP_I_TO_I : constant := 16#0C70#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:277
   GL_PIXEL_MAP_S_TO_S : constant := 16#0C71#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:278
   GL_PIXEL_MAP_I_TO_R : constant := 16#0C72#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:279
   GL_PIXEL_MAP_I_TO_G : constant := 16#0C73#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:280
   GL_PIXEL_MAP_I_TO_B : constant := 16#0C74#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:281
   GL_PIXEL_MAP_I_TO_A : constant := 16#0C75#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:282
   GL_PIXEL_MAP_R_TO_R : constant := 16#0C76#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:283
   GL_PIXEL_MAP_G_TO_G : constant := 16#0C77#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:284
   GL_PIXEL_MAP_B_TO_B : constant := 16#0C78#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:285
   GL_PIXEL_MAP_A_TO_A : constant := 16#0C79#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:286
   GL_PIXEL_MAP_I_TO_I_SIZE : constant := 16#0CB0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:287
   GL_PIXEL_MAP_S_TO_S_SIZE : constant := 16#0CB1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:288
   GL_PIXEL_MAP_I_TO_R_SIZE : constant := 16#0CB2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:289
   GL_PIXEL_MAP_I_TO_G_SIZE : constant := 16#0CB3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:290
   GL_PIXEL_MAP_I_TO_B_SIZE : constant := 16#0CB4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:291
   GL_PIXEL_MAP_I_TO_A_SIZE : constant := 16#0CB5#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:292
   GL_PIXEL_MAP_R_TO_R_SIZE : constant := 16#0CB6#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:293
   GL_PIXEL_MAP_G_TO_G_SIZE : constant := 16#0CB7#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:294
   GL_PIXEL_MAP_B_TO_B_SIZE : constant := 16#0CB8#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:295
   GL_PIXEL_MAP_A_TO_A_SIZE : constant := 16#0CB9#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:296
   GL_UNPACK_SWAP_BYTES : constant := 16#0CF0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:297
   GL_UNPACK_LSB_FIRST : constant := 16#0CF1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:298
   GL_UNPACK_ROW_LENGTH : constant := 16#0CF2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:299
   GL_UNPACK_SKIP_ROWS : constant := 16#0CF3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:300
   GL_UNPACK_SKIP_PIXELS : constant := 16#0CF4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:301
   GL_UNPACK_ALIGNMENT : constant := 16#0CF5#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:302
   GL_PACK_SWAP_BYTES : constant := 16#0D00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:303
   GL_PACK_LSB_FIRST : constant := 16#0D01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:304
   GL_PACK_ROW_LENGTH : constant := 16#0D02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:305
   GL_PACK_SKIP_ROWS : constant := 16#0D03#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:306
   GL_PACK_SKIP_PIXELS : constant := 16#0D04#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:307
   GL_PACK_ALIGNMENT : constant := 16#0D05#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:308
   GL_MAP_COLOR : constant := 16#0D10#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:309
   GL_MAP_STENCIL : constant := 16#0D11#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:310
   GL_INDEX_SHIFT : constant := 16#0D12#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:311
   GL_INDEX_OFFSET : constant := 16#0D13#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:312
   GL_RED_SCALE : constant := 16#0D14#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:313
   GL_RED_BIAS : constant := 16#0D15#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:314
   GL_ZOOM_X : constant := 16#0D16#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:315
   GL_ZOOM_Y : constant := 16#0D17#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:316
   GL_GREEN_SCALE : constant := 16#0D18#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:317
   GL_GREEN_BIAS : constant := 16#0D19#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:318
   GL_BLUE_SCALE : constant := 16#0D1A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:319
   GL_BLUE_BIAS : constant := 16#0D1B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:320
   GL_ALPHA_SCALE : constant := 16#0D1C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:321
   GL_ALPHA_BIAS : constant := 16#0D1D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:322
   GL_DEPTH_SCALE : constant := 16#0D1E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:323
   GL_DEPTH_BIAS : constant := 16#0D1F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:324
   GL_MAX_EVAL_ORDER : constant := 16#0D30#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:325
   GL_MAX_LIGHTS : constant := 16#0D31#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:326
   GL_MAX_CLIP_PLANES : constant := 16#0D32#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:327
   GL_MAX_TEXTURE_SIZE : constant := 16#0D33#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:328
   GL_MAX_PIXEL_MAP_TABLE : constant := 16#0D34#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:329
   GL_MAX_ATTRIB_STACK_DEPTH : constant := 16#0D35#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:330
   GL_MAX_MODELVIEW_STACK_DEPTH : constant := 16#0D36#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:331
   GL_MAX_NAME_STACK_DEPTH : constant := 16#0D37#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:332
   GL_MAX_PROJECTION_STACK_DEPTH : constant := 16#0D38#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:333
   GL_MAX_TEXTURE_STACK_DEPTH : constant := 16#0D39#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:334
   GL_MAX_VIEWPORT_DIMS : constant := 16#0D3A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:335
   GL_MAX_CLIENT_ATTRIB_STACK_DEPTH : constant := 16#0D3B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:336
   GL_SUBPIXEL_BITS : constant := 16#0D50#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:337
   GL_INDEX_BITS : constant := 16#0D51#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:338
   GL_RED_BITS : constant := 16#0D52#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:339
   GL_GREEN_BITS : constant := 16#0D53#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:340
   GL_BLUE_BITS : constant := 16#0D54#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:341
   GL_ALPHA_BITS : constant := 16#0D55#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:342
   GL_DEPTH_BITS : constant := 16#0D56#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:343
   GL_STENCIL_BITS : constant := 16#0D57#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:344
   GL_ACCUM_RED_BITS : constant := 16#0D58#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:345
   GL_ACCUM_GREEN_BITS : constant := 16#0D59#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:346
   GL_ACCUM_BLUE_BITS : constant := 16#0D5A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:347
   GL_ACCUM_ALPHA_BITS : constant := 16#0D5B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:348
   GL_NAME_STACK_DEPTH : constant := 16#0D70#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:349
   GL_AUTO_NORMAL : constant := 16#0D80#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:350
   GL_MAP1_COLOR_4 : constant := 16#0D90#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:351
   GL_MAP1_INDEX : constant := 16#0D91#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:352
   GL_MAP1_NORMAL : constant := 16#0D92#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:353
   GL_MAP1_TEXTURE_COORD_1 : constant := 16#0D93#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:354
   GL_MAP1_TEXTURE_COORD_2 : constant := 16#0D94#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:355
   GL_MAP1_TEXTURE_COORD_3 : constant := 16#0D95#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:356
   GL_MAP1_TEXTURE_COORD_4 : constant := 16#0D96#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:357
   GL_MAP1_VERTEX_3 : constant := 16#0D97#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:358
   GL_MAP1_VERTEX_4 : constant := 16#0D98#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:359
   GL_MAP2_COLOR_4 : constant := 16#0DB0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:360
   GL_MAP2_INDEX : constant := 16#0DB1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:361
   GL_MAP2_NORMAL : constant := 16#0DB2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:362
   GL_MAP2_TEXTURE_COORD_1 : constant := 16#0DB3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:363
   GL_MAP2_TEXTURE_COORD_2 : constant := 16#0DB4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:364
   GL_MAP2_TEXTURE_COORD_3 : constant := 16#0DB5#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:365
   GL_MAP2_TEXTURE_COORD_4 : constant := 16#0DB6#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:366
   GL_MAP2_VERTEX_3 : constant := 16#0DB7#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:367
   GL_MAP2_VERTEX_4 : constant := 16#0DB8#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:368
   GL_MAP1_GRID_DOMAIN : constant := 16#0DD0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:369
   GL_MAP1_GRID_SEGMENTS : constant := 16#0DD1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:370
   GL_MAP2_GRID_DOMAIN : constant := 16#0DD2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:371
   GL_MAP2_GRID_SEGMENTS : constant := 16#0DD3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:372
   GL_TEXTURE_1D : constant := 16#0DE0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:373
   GL_TEXTURE_2D : constant := 16#0DE1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:374
   GL_FEEDBACK_BUFFER_POINTER : constant := 16#0DF0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:375
   GL_FEEDBACK_BUFFER_SIZE : constant := 16#0DF1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:376
   GL_FEEDBACK_BUFFER_TYPE : constant := 16#0DF2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:377
   GL_SELECTION_BUFFER_POINTER : constant := 16#0DF3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:378
   GL_SELECTION_BUFFER_SIZE : constant := 16#0DF4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:379

   GL_TEXTURE_WIDTH : constant := 16#1000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:381
   GL_TEXTURE_HEIGHT : constant := 16#1001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:382
   GL_TEXTURE_INTERNAL_FORMAT : constant := 16#1003#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:383
   GL_TEXTURE_BORDER_COLOR : constant := 16#1004#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:384
   GL_TEXTURE_BORDER : constant := 16#1005#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:385

   GL_DONT_CARE : constant := 16#1100#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:387
   GL_FASTEST : constant := 16#1101#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:388
   GL_NICEST : constant := 16#1102#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:389

   GL_LIGHT0 : constant := 16#4000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:391
   GL_LIGHT1 : constant := 16#4001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:392
   GL_LIGHT2 : constant := 16#4002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:393
   GL_LIGHT3 : constant := 16#4003#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:394
   GL_LIGHT4 : constant := 16#4004#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:395
   GL_LIGHT5 : constant := 16#4005#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:396
   GL_LIGHT6 : constant := 16#4006#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:397
   GL_LIGHT7 : constant := 16#4007#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:398

   GL_AMBIENT : constant := 16#1200#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:400
   GL_DIFFUSE : constant := 16#1201#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:401
   GL_SPECULAR : constant := 16#1202#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:402
   GL_POSITION : constant := 16#1203#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:403
   GL_SPOT_DIRECTION : constant := 16#1204#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:404
   GL_SPOT_EXPONENT : constant := 16#1205#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:405
   GL_SPOT_CUTOFF : constant := 16#1206#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:406
   GL_CONSTANT_ATTENUATION : constant := 16#1207#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:407
   GL_LINEAR_ATTENUATION : constant := 16#1208#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:408
   GL_QUADRATIC_ATTENUATION : constant := 16#1209#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:409

   GL_COMPILE : constant := 16#1300#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:411
   GL_COMPILE_AND_EXECUTE : constant := 16#1301#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:412

   GL_CLEAR : constant := 16#1500#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:414
   GL_AND : constant := 16#1501#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:415
   GL_AND_REVERSE : constant := 16#1502#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:416
   GL_COPY : constant := 16#1503#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:417
   GL_AND_INVERTED : constant := 16#1504#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:418
   GL_NOOP : constant := 16#1505#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:419
   GL_XOR : constant := 16#1506#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:420
   GL_OR : constant := 16#1507#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:421
   GL_NOR : constant := 16#1508#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:422
   GL_EQUIV : constant := 16#1509#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:423
   GL_INVERT : constant := 16#150A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:424
   GL_OR_REVERSE : constant := 16#150B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:425
   GL_COPY_INVERTED : constant := 16#150C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:426
   GL_OR_INVERTED : constant := 16#150D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:427
   GL_NAND : constant := 16#150E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:428
   GL_SET : constant := 16#150F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:429

   GL_EMISSION : constant := 16#1600#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:431
   GL_SHININESS : constant := 16#1601#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:432
   GL_AMBIENT_AND_DIFFUSE : constant := 16#1602#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:433
   GL_COLOR_INDEXES : constant := 16#1603#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:434

   GL_MODELVIEW : constant := 16#1700#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:436
   GL_PROJECTION : constant := 16#1701#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:437
   GL_TEXTURE : constant := 16#1702#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:438

   GL_COLOR : constant := 16#1800#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:440
   GL_DEPTH : constant := 16#1801#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:441
   GL_STENCIL : constant := 16#1802#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:442

   GL_COLOR_INDEX : constant := 16#1900#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:444
   GL_STENCIL_INDEX : constant := 16#1901#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:445
   GL_DEPTH_COMPONENT : constant := 16#1902#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:446
   GL_RED : constant := 16#1903#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:447
   GL_GREEN : constant := 16#1904#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:448
   GL_BLUE : constant := 16#1905#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:449
   GL_ALPHA : constant := 16#1906#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:450
   GL_RGB : constant := 16#1907#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:451
   GL_RGBA : constant := 16#1908#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:452
   GL_LUMINANCE : constant := 16#1909#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:453
   GL_LUMINANCE_ALPHA : constant := 16#190A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:454

   GL_BITMAP : constant := 16#1A00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:456

   GL_POINT : constant := 16#1B00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:458
   GL_LINE : constant := 16#1B01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:459
   GL_FILL : constant := 16#1B02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:460

   GL_RENDER : constant := 16#1C00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:462
   GL_FEEDBACK : constant := 16#1C01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:463
   GL_SELECT : constant := 16#1C02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:464

   GL_FLAT : constant := 16#1D00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:466
   GL_SMOOTH : constant := 16#1D01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:467

   GL_KEEP : constant := 16#1E00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:469
   GL_REPLACE : constant := 16#1E01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:470
   GL_INCR : constant := 16#1E02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:471
   GL_DECR : constant := 16#1E03#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:472

   GL_VENDOR : constant := 16#1F00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:474
   GL_RENDERER : constant := 16#1F01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:475
   GL_VERSION : constant := 16#1F02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:476
   GL_EXTENSIONS : constant := 16#1F03#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:477

   GL_S : constant := 16#2000#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:479
   GL_T : constant := 16#2001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:480
   GL_R : constant := 16#2002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:481
   GL_Q : constant := 16#2003#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:482

   GL_MODULATE : constant := 16#2100#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:484
   GL_DECAL : constant := 16#2101#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:485

   GL_TEXTURE_ENV_MODE : constant := 16#2200#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:487
   GL_TEXTURE_ENV_COLOR : constant := 16#2201#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:488

   GL_TEXTURE_ENV : constant := 16#2300#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:490

   GL_EYE_LINEAR : constant := 16#2400#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:492
   GL_OBJECT_LINEAR : constant := 16#2401#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:493
   GL_SPHERE_MAP : constant := 16#2402#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:494

   GL_TEXTURE_GEN_MODE : constant := 16#2500#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:496
   GL_OBJECT_PLANE : constant := 16#2501#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:497
   GL_EYE_PLANE : constant := 16#2502#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:498

   GL_NEAREST : constant := 16#2600#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:500
   GL_LINEAR : constant := 16#2601#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:501

   GL_NEAREST_MIPMAP_NEAREST : constant := 16#2700#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:503
   GL_LINEAR_MIPMAP_NEAREST : constant := 16#2701#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:504
   GL_NEAREST_MIPMAP_LINEAR : constant := 16#2702#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:505
   GL_LINEAR_MIPMAP_LINEAR : constant := 16#2703#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:506

   GL_TEXTURE_MAG_FILTER : constant := 16#2800#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:508
   GL_TEXTURE_MIN_FILTER : constant := 16#2801#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:509
   GL_TEXTURE_WRAP_S : constant := 16#2802#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:510
   GL_TEXTURE_WRAP_T : constant := 16#2803#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:511

   GL_CLAMP : constant := 16#2900#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:513
   GL_REPEAT : constant := 16#2901#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:514

   GL_CLIENT_PIXEL_STORE_BIT : constant := 16#00000001#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:516
   GL_CLIENT_VERTEX_ARRAY_BIT : constant := 16#00000002#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:517
   GL_CLIENT_ALL_ATTRIB_BITS : constant := 16#ffffffff#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:518

   GL_POLYGON_OFFSET_FACTOR : constant := 16#8038#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:520
   GL_POLYGON_OFFSET_UNITS : constant := 16#2A00#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:521
   GL_POLYGON_OFFSET_POINT : constant := 16#2A01#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:522
   GL_POLYGON_OFFSET_LINE : constant := 16#2A02#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:523
   GL_POLYGON_OFFSET_FILL : constant := 16#8037#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:524

   GL_ALPHA4 : constant := 16#803B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:526
   GL_ALPHA8 : constant := 16#803C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:527
   GL_ALPHA12 : constant := 16#803D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:528
   GL_ALPHA16 : constant := 16#803E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:529
   GL_LUMINANCE4 : constant := 16#803F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:530
   GL_LUMINANCE8 : constant := 16#8040#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:531
   GL_LUMINANCE12 : constant := 16#8041#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:532
   GL_LUMINANCE16 : constant := 16#8042#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:533
   GL_LUMINANCE4_ALPHA4 : constant := 16#8043#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:534
   GL_LUMINANCE6_ALPHA2 : constant := 16#8044#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:535
   GL_LUMINANCE8_ALPHA8 : constant := 16#8045#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:536
   GL_LUMINANCE12_ALPHA4 : constant := 16#8046#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:537
   GL_LUMINANCE12_ALPHA12 : constant := 16#8047#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:538
   GL_LUMINANCE16_ALPHA16 : constant := 16#8048#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:539
   GL_INTENSITY : constant := 16#8049#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:540
   GL_INTENSITY4 : constant := 16#804A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:541
   GL_INTENSITY8 : constant := 16#804B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:542
   GL_INTENSITY12 : constant := 16#804C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:543
   GL_INTENSITY16 : constant := 16#804D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:544
   GL_R3_G3_B2 : constant := 16#2A10#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:545
   GL_RGB4 : constant := 16#804F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:546
   GL_RGB5 : constant := 16#8050#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:547
   GL_RGB8 : constant := 16#8051#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:548
   GL_RGB10 : constant := 16#8052#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:549
   GL_RGB12 : constant := 16#8053#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:550
   GL_RGB16 : constant := 16#8054#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:551
   GL_RGBA2 : constant := 16#8055#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:552
   GL_RGBA4 : constant := 16#8056#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:553
   GL_RGB5_A1 : constant := 16#8057#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:554
   GL_RGBA8 : constant := 16#8058#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:555
   GL_RGB10_A2 : constant := 16#8059#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:556
   GL_RGBA12 : constant := 16#805A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:557
   GL_RGBA16 : constant := 16#805B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:558
   GL_TEXTURE_RED_SIZE : constant := 16#805C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:559
   GL_TEXTURE_GREEN_SIZE : constant := 16#805D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:560
   GL_TEXTURE_BLUE_SIZE : constant := 16#805E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:561
   GL_TEXTURE_ALPHA_SIZE : constant := 16#805F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:562
   GL_TEXTURE_LUMINANCE_SIZE : constant := 16#8060#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:563
   GL_TEXTURE_INTENSITY_SIZE : constant := 16#8061#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:564
   GL_PROXY_TEXTURE_1D : constant := 16#8063#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:565
   GL_PROXY_TEXTURE_2D : constant := 16#8064#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:566

   GL_TEXTURE_PRIORITY : constant := 16#8066#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:568
   GL_TEXTURE_RESIDENT : constant := 16#8067#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:569
   GL_TEXTURE_BINDING_1D : constant := 16#8068#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:570
   GL_TEXTURE_BINDING_2D : constant := 16#8069#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:571

   GL_VERTEX_ARRAY : constant := 16#8074#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:573
   GL_NORMAL_ARRAY : constant := 16#8075#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:574
   GL_COLOR_ARRAY : constant := 16#8076#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:575
   GL_INDEX_ARRAY : constant := 16#8077#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:576
   GL_TEXTURE_COORD_ARRAY : constant := 16#8078#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:577
   GL_EDGE_FLAG_ARRAY : constant := 16#8079#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:578
   GL_VERTEX_ARRAY_SIZE : constant := 16#807A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:579
   GL_VERTEX_ARRAY_TYPE : constant := 16#807B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:580
   GL_VERTEX_ARRAY_STRIDE : constant := 16#807C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:581
   GL_NORMAL_ARRAY_TYPE : constant := 16#807E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:582
   GL_NORMAL_ARRAY_STRIDE : constant := 16#807F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:583
   GL_COLOR_ARRAY_SIZE : constant := 16#8081#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:584
   GL_COLOR_ARRAY_TYPE : constant := 16#8082#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:585
   GL_COLOR_ARRAY_STRIDE : constant := 16#8083#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:586
   GL_INDEX_ARRAY_TYPE : constant := 16#8085#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:587
   GL_INDEX_ARRAY_STRIDE : constant := 16#8086#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:588
   GL_TEXTURE_COORD_ARRAY_SIZE : constant := 16#8088#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:589
   GL_TEXTURE_COORD_ARRAY_TYPE : constant := 16#8089#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:590
   GL_TEXTURE_COORD_ARRAY_STRIDE : constant := 16#808A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:591
   GL_EDGE_FLAG_ARRAY_STRIDE : constant := 16#808C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:592
   GL_VERTEX_ARRAY_POINTER : constant := 16#808E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:593
   GL_NORMAL_ARRAY_POINTER : constant := 16#808F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:594
   GL_COLOR_ARRAY_POINTER : constant := 16#8090#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:595
   GL_INDEX_ARRAY_POINTER : constant := 16#8091#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:596
   GL_TEXTURE_COORD_ARRAY_POINTER : constant := 16#8092#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:597
   GL_EDGE_FLAG_ARRAY_POINTER : constant := 16#8093#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:598
   GL_V2F : constant := 16#2A20#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:599
   GL_V3F : constant := 16#2A21#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:600
   GL_C4UB_V2F : constant := 16#2A22#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:601
   GL_C4UB_V3F : constant := 16#2A23#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:602
   GL_C3F_V3F : constant := 16#2A24#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:603
   GL_N3F_V3F : constant := 16#2A25#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:604
   GL_C4F_N3F_V3F : constant := 16#2A26#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:605
   GL_T2F_V3F : constant := 16#2A27#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:606
   GL_T4F_V4F : constant := 16#2A28#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:607
   GL_T2F_C4UB_V3F : constant := 16#2A29#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:608
   GL_T2F_C3F_V3F : constant := 16#2A2A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:609
   GL_T2F_N3F_V3F : constant := 16#2A2B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:610
   GL_T2F_C4F_N3F_V3F : constant := 16#2A2C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:611
   GL_T4F_C4F_N3F_V4F : constant := 16#2A2D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:612

   GL_EXT_vertex_array : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:614
   GL_EXT_bgra : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:615
   GL_EXT_paletted_texture : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:616
   GL_WIN_swap_hint : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:617
   GL_WIN_draw_range_elements : constant := 1;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:618

   GL_VERTEX_ARRAY_EXT : constant := 16#8074#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:620
   GL_NORMAL_ARRAY_EXT : constant := 16#8075#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:621
   GL_COLOR_ARRAY_EXT : constant := 16#8076#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:622
   GL_INDEX_ARRAY_EXT : constant := 16#8077#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:623
   GL_TEXTURE_COORD_ARRAY_EXT : constant := 16#8078#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:624
   GL_EDGE_FLAG_ARRAY_EXT : constant := 16#8079#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:625
   GL_VERTEX_ARRAY_SIZE_EXT : constant := 16#807A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:626
   GL_VERTEX_ARRAY_TYPE_EXT : constant := 16#807B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:627
   GL_VERTEX_ARRAY_STRIDE_EXT : constant := 16#807C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:628
   GL_VERTEX_ARRAY_COUNT_EXT : constant := 16#807D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:629
   GL_NORMAL_ARRAY_TYPE_EXT : constant := 16#807E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:630
   GL_NORMAL_ARRAY_STRIDE_EXT : constant := 16#807F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:631
   GL_NORMAL_ARRAY_COUNT_EXT : constant := 16#8080#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:632
   GL_COLOR_ARRAY_SIZE_EXT : constant := 16#8081#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:633
   GL_COLOR_ARRAY_TYPE_EXT : constant := 16#8082#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:634
   GL_COLOR_ARRAY_STRIDE_EXT : constant := 16#8083#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:635
   GL_COLOR_ARRAY_COUNT_EXT : constant := 16#8084#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:636
   GL_INDEX_ARRAY_TYPE_EXT : constant := 16#8085#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:637
   GL_INDEX_ARRAY_STRIDE_EXT : constant := 16#8086#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:638
   GL_INDEX_ARRAY_COUNT_EXT : constant := 16#8087#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:639
   GL_TEXTURE_COORD_ARRAY_SIZE_EXT : constant := 16#8088#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:640
   GL_TEXTURE_COORD_ARRAY_TYPE_EXT : constant := 16#8089#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:641
   GL_TEXTURE_COORD_ARRAY_STRIDE_EXT : constant := 16#808A#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:642
   GL_TEXTURE_COORD_ARRAY_COUNT_EXT : constant := 16#808B#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:643
   GL_EDGE_FLAG_ARRAY_STRIDE_EXT : constant := 16#808C#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:644
   GL_EDGE_FLAG_ARRAY_COUNT_EXT : constant := 16#808D#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:645
   GL_VERTEX_ARRAY_POINTER_EXT : constant := 16#808E#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:646
   GL_NORMAL_ARRAY_POINTER_EXT : constant := 16#808F#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:647
   GL_COLOR_ARRAY_POINTER_EXT : constant := 16#8090#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:648
   GL_INDEX_ARRAY_POINTER_EXT : constant := 16#8091#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:649
   GL_TEXTURE_COORD_ARRAY_POINTER_EXT : constant := 16#8092#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:650
   GL_EDGE_FLAG_ARRAY_POINTER_EXT : constant := 16#8093#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:651
   --  unsupported macro: GL_DOUBLE_EXT GL_DOUBLE

   GL_BGR_EXT : constant := 16#80E0#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:654
   GL_BGRA_EXT : constant := 16#80E1#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:655

   GL_COLOR_TABLE_FORMAT_EXT : constant := 16#80D8#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:657
   GL_COLOR_TABLE_WIDTH_EXT : constant := 16#80D9#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:658
   GL_COLOR_TABLE_RED_SIZE_EXT : constant := 16#80DA#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:659
   GL_COLOR_TABLE_GREEN_SIZE_EXT : constant := 16#80DB#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:660
   GL_COLOR_TABLE_BLUE_SIZE_EXT : constant := 16#80DC#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:661
   GL_COLOR_TABLE_ALPHA_SIZE_EXT : constant := 16#80DD#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:662
   GL_COLOR_TABLE_LUMINANCE_SIZE_EXT : constant := 16#80DE#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:663
   GL_COLOR_TABLE_INTENSITY_SIZE_EXT : constant := 16#80DF#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:664

   GL_COLOR_INDEX1_EXT : constant := 16#80E2#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:666
   GL_COLOR_INDEX2_EXT : constant := 16#80E3#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:667
   GL_COLOR_INDEX4_EXT : constant := 16#80E4#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:668
   GL_COLOR_INDEX8_EXT : constant := 16#80E5#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:669
   GL_COLOR_INDEX12_EXT : constant := 16#80E6#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:670
   GL_COLOR_INDEX16_EXT : constant := 16#80E7#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:671

   GL_MAX_ELEMENTS_VERTICES_WIN : constant := 16#80E8#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:673
   GL_MAX_ELEMENTS_INDICES_WIN : constant := 16#80E9#;  --  c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:674
   --  unsupported macro: GL_LOGIC_OP GL_INDEX_LOGIC_OP
   --  unsupported macro: GL_TEXTURE_COMPONENTS GL_TEXTURE_INTERNAL_FORMAT

   subtype GLenum is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:22

   subtype GLboolean is unsigned_char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:23

   subtype GLbitfield is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:24

   subtype GLbyte is signed_char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:25

   subtype GLshort is short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:26

   subtype GLint is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:27

   subtype GLsizei is int;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:28

   subtype GLubyte is unsigned_char;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:29

   subtype GLushort is unsigned_short;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:30

   subtype GLuint is unsigned;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:31

   subtype GLfloat is float;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:32

   subtype GLclampf is float;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:33

   subtype GLdouble is double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:34

   subtype GLclampd is double;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:35

   subtype GLvoid is System.Address;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:36

   procedure glAccum (op : GLenum; value : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:684
   pragma Import (Stdcall, glAccum, "glAccum");

   procedure glAlphaFunc (func : GLenum; ref : GLclampf);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:685
   pragma Import (Stdcall, glAlphaFunc, "glAlphaFunc");

   function glAreTexturesResident
     (n : GLsizei;
      textures : access GLuint;
      residences : access GLboolean) return GLboolean;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:686
   pragma Import (Stdcall, glAreTexturesResident, "glAreTexturesResident");

   procedure glArrayElement (i : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:687
   pragma Import (Stdcall, glArrayElement, "glArrayElement");

   procedure glBegin (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:688
   pragma Import (Stdcall, glBegin, "glBegin");

   procedure glBindTexture (target : GLenum; texture : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:689
   pragma Import (Stdcall, glBindTexture, "glBindTexture");

   procedure glBitmap
     (width : GLsizei;
      height : GLsizei;
      xorig : GLfloat;
      yorig : GLfloat;
      xmove : GLfloat;
      ymove : GLfloat;
      bitmap : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:690
   pragma Import (Stdcall, glBitmap, "glBitmap");

   procedure glBlendFunc (sfactor : GLenum; dfactor : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:691
   pragma Import (Stdcall, glBlendFunc, "glBlendFunc");

   procedure glCallList (list : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:692
   pragma Import (Stdcall, glCallList, "glCallList");

   procedure glCallLists
     (n : GLsizei;
      c_type : GLenum;
      lists : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:693
   pragma Import (Stdcall, glCallLists, "glCallLists");

   procedure glClear (mask : GLbitfield);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:694
   pragma Import (Stdcall, glClear, "glClear");

   procedure glClearAccum
     (red : GLfloat;
      green : GLfloat;
      blue : GLfloat;
      alpha : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:695
   pragma Import (Stdcall, glClearAccum, "glClearAccum");

   procedure glClearColor
     (red : GLclampf;
      green : GLclampf;
      blue : GLclampf;
      alpha : GLclampf);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:696
   pragma Import (Stdcall, glClearColor, "glClearColor");

   procedure glClearDepth (depth : GLclampd);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:697
   pragma Import (Stdcall, glClearDepth, "glClearDepth");

   procedure glClearIndex (c : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:698
   pragma Import (Stdcall, glClearIndex, "glClearIndex");

   procedure glClearStencil (s : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:699
   pragma Import (Stdcall, glClearStencil, "glClearStencil");

   procedure glClipPlane (plane : GLenum; equation : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:700
   pragma Import (Stdcall, glClipPlane, "glClipPlane");

   procedure glColor3b
     (red : GLbyte;
      green : GLbyte;
      blue : GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:701
   pragma Import (Stdcall, glColor3b, "glColor3b");

   procedure glColor3bv (v : access GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:702
   pragma Import (Stdcall, glColor3bv, "glColor3bv");

   procedure glColor3d
     (red : GLdouble;
      green : GLdouble;
      blue : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:703
   pragma Import (Stdcall, glColor3d, "glColor3d");

   procedure glColor3dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:704
   pragma Import (Stdcall, glColor3dv, "glColor3dv");

   procedure glColor3f
     (red : GLfloat;
      green : GLfloat;
      blue : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:705
   pragma Import (Stdcall, glColor3f, "glColor3f");

   procedure glColor3fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:706
   pragma Import (Stdcall, glColor3fv, "glColor3fv");

   procedure glColor3i
     (red : GLint;
      green : GLint;
      blue : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:707
   pragma Import (Stdcall, glColor3i, "glColor3i");

   procedure glColor3iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:708
   pragma Import (Stdcall, glColor3iv, "glColor3iv");

   procedure glColor3s
     (red : GLshort;
      green : GLshort;
      blue : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:709
   pragma Import (Stdcall, glColor3s, "glColor3s");

   procedure glColor3sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:710
   pragma Import (Stdcall, glColor3sv, "glColor3sv");

   procedure glColor3ub
     (red : GLubyte;
      green : GLubyte;
      blue : GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:711
   pragma Import (Stdcall, glColor3ub, "glColor3ub");

   procedure glColor3ubv (v : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:712
   pragma Import (Stdcall, glColor3ubv, "glColor3ubv");

   procedure glColor3ui
     (red : GLuint;
      green : GLuint;
      blue : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:713
   pragma Import (Stdcall, glColor3ui, "glColor3ui");

   procedure glColor3uiv (v : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:714
   pragma Import (Stdcall, glColor3uiv, "glColor3uiv");

   procedure glColor3us
     (red : GLushort;
      green : GLushort;
      blue : GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:715
   pragma Import (Stdcall, glColor3us, "glColor3us");

   procedure glColor3usv (v : access GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:716
   pragma Import (Stdcall, glColor3usv, "glColor3usv");

   procedure glColor4b
     (red : GLbyte;
      green : GLbyte;
      blue : GLbyte;
      alpha : GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:717
   pragma Import (Stdcall, glColor4b, "glColor4b");

   procedure glColor4bv (v : access GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:718
   pragma Import (Stdcall, glColor4bv, "glColor4bv");

   procedure glColor4d
     (red : GLdouble;
      green : GLdouble;
      blue : GLdouble;
      alpha : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:719
   pragma Import (Stdcall, glColor4d, "glColor4d");

   procedure glColor4dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:720
   pragma Import (Stdcall, glColor4dv, "glColor4dv");

   procedure glColor4f
     (red : GLfloat;
      green : GLfloat;
      blue : GLfloat;
      alpha : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:721
   pragma Import (Stdcall, glColor4f, "glColor4f");

   procedure glColor4fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:722
   pragma Import (Stdcall, glColor4fv, "glColor4fv");

   procedure glColor4i
     (red : GLint;
      green : GLint;
      blue : GLint;
      alpha : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:723
   pragma Import (Stdcall, glColor4i, "glColor4i");

   procedure glColor4iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:724
   pragma Import (Stdcall, glColor4iv, "glColor4iv");

   procedure glColor4s
     (red : GLshort;
      green : GLshort;
      blue : GLshort;
      alpha : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:725
   pragma Import (Stdcall, glColor4s, "glColor4s");

   procedure glColor4sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:726
   pragma Import (Stdcall, glColor4sv, "glColor4sv");

   procedure glColor4ub
     (red : GLubyte;
      green : GLubyte;
      blue : GLubyte;
      alpha : GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:727
   pragma Import (Stdcall, glColor4ub, "glColor4ub");

   procedure glColor4ubv (v : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:728
   pragma Import (Stdcall, glColor4ubv, "glColor4ubv");

   procedure glColor4ui
     (red : GLuint;
      green : GLuint;
      blue : GLuint;
      alpha : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:729
   pragma Import (Stdcall, glColor4ui, "glColor4ui");

   procedure glColor4uiv (v : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:730
   pragma Import (Stdcall, glColor4uiv, "glColor4uiv");

   procedure glColor4us
     (red : GLushort;
      green : GLushort;
      blue : GLushort;
      alpha : GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:731
   pragma Import (Stdcall, glColor4us, "glColor4us");

   procedure glColor4usv (v : access GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:732
   pragma Import (Stdcall, glColor4usv, "glColor4usv");

   procedure glColorMask
     (red : GLboolean;
      green : GLboolean;
      blue : GLboolean;
      alpha : GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:733
   pragma Import (Stdcall, glColorMask, "glColorMask");

   procedure glColorMaterial (face : GLenum; mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:734
   pragma Import (Stdcall, glColorMaterial, "glColorMaterial");

   procedure glColorPointer
     (size : GLint;
      c_type : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:735
   pragma Import (Stdcall, glColorPointer, "glColorPointer");

   procedure glCopyPixels
     (x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei;
      c_type : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:736
   pragma Import (Stdcall, glCopyPixels, "glCopyPixels");

   procedure glCopyTexImage1D
     (target : GLenum;
      level : GLint;
      internalFormat : GLenum;
      x : GLint;
      y : GLint;
      width : GLsizei;
      border : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:737
   pragma Import (Stdcall, glCopyTexImage1D, "glCopyTexImage1D");

   procedure glCopyTexImage2D
     (target : GLenum;
      level : GLint;
      internalFormat : GLenum;
      x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei;
      border : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:738
   pragma Import (Stdcall, glCopyTexImage2D, "glCopyTexImage2D");

   procedure glCopyTexSubImage1D
     (target : GLenum;
      level : GLint;
      xoffset : GLint;
      x : GLint;
      y : GLint;
      width : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:739
   pragma Import (Stdcall, glCopyTexSubImage1D, "glCopyTexSubImage1D");

   procedure glCopyTexSubImage2D
     (target : GLenum;
      level : GLint;
      xoffset : GLint;
      yoffset : GLint;
      x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:740
   pragma Import (Stdcall, glCopyTexSubImage2D, "glCopyTexSubImage2D");

   procedure glCullFace (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:741
   pragma Import (Stdcall, glCullFace, "glCullFace");

   procedure glDeleteLists (list : GLuint; c_range : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:742
   pragma Import (Stdcall, glDeleteLists, "glDeleteLists");

   procedure glDeleteTextures (n : GLsizei; textures : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:743
   pragma Import (Stdcall, glDeleteTextures, "glDeleteTextures");

   procedure glDepthFunc (func : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:744
   pragma Import (Stdcall, glDepthFunc, "glDepthFunc");

   procedure glDepthMask (flag : GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:745
   pragma Import (Stdcall, glDepthMask, "glDepthMask");

   procedure glDepthRange (zNear : GLclampd; zFar : GLclampd);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:746
   pragma Import (Stdcall, glDepthRange, "glDepthRange");

   procedure glDisable (cap : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:747
   pragma Import (Stdcall, glDisable, "glDisable");

   procedure glDisableClientState (c_array : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:748
   pragma Import (Stdcall, glDisableClientState, "glDisableClientState");

   procedure glDrawArrays
     (mode : GLenum;
      first : GLint;
      count : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:749
   pragma Import (Stdcall, glDrawArrays, "glDrawArrays");

   procedure glDrawBuffer (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:750
   pragma Import (Stdcall, glDrawBuffer, "glDrawBuffer");

   procedure glDrawElements
     (mode : GLenum;
      count : GLsizei;
      c_type : GLenum;
      indices : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:751
   pragma Import (Stdcall, glDrawElements, "glDrawElements");

   procedure glDrawPixels
     (width : GLsizei;
      height : GLsizei;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:752
   pragma Import (Stdcall, glDrawPixels, "glDrawPixels");

   procedure glEdgeFlag (flag : GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:753
   pragma Import (Stdcall, glEdgeFlag, "glEdgeFlag");

   procedure glEdgeFlagPointer (stride : GLsizei; pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:754
   pragma Import (Stdcall, glEdgeFlagPointer, "glEdgeFlagPointer");

   procedure glEdgeFlagv (flag : access GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:755
   pragma Import (Stdcall, glEdgeFlagv, "glEdgeFlagv");

   procedure glEnable (cap : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:756
   pragma Import (Stdcall, glEnable, "glEnable");

   procedure glEnableClientState (c_array : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:757
   pragma Import (Stdcall, glEnableClientState, "glEnableClientState");

   procedure glEnd;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:758
   pragma Import (Stdcall, glEnd, "glEnd");

   procedure glEndList;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:759
   pragma Import (Stdcall, glEndList, "glEndList");

   procedure glEvalCoord1d (u : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:760
   pragma Import (Stdcall, glEvalCoord1d, "glEvalCoord1d");

   procedure glEvalCoord1dv (u : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:761
   pragma Import (Stdcall, glEvalCoord1dv, "glEvalCoord1dv");

   procedure glEvalCoord1f (u : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:762
   pragma Import (Stdcall, glEvalCoord1f, "glEvalCoord1f");

   procedure glEvalCoord1fv (u : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:763
   pragma Import (Stdcall, glEvalCoord1fv, "glEvalCoord1fv");

   procedure glEvalCoord2d (u : GLdouble; v : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:764
   pragma Import (Stdcall, glEvalCoord2d, "glEvalCoord2d");

   procedure glEvalCoord2dv (u : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:765
   pragma Import (Stdcall, glEvalCoord2dv, "glEvalCoord2dv");

   procedure glEvalCoord2f (u : GLfloat; v : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:766
   pragma Import (Stdcall, glEvalCoord2f, "glEvalCoord2f");

   procedure glEvalCoord2fv (u : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:767
   pragma Import (Stdcall, glEvalCoord2fv, "glEvalCoord2fv");

   procedure glEvalMesh1
     (mode : GLenum;
      i1 : GLint;
      i2 : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:768
   pragma Import (Stdcall, glEvalMesh1, "glEvalMesh1");

   procedure glEvalMesh2
     (mode : GLenum;
      i1 : GLint;
      i2 : GLint;
      j1 : GLint;
      j2 : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:769
   pragma Import (Stdcall, glEvalMesh2, "glEvalMesh2");

   procedure glEvalPoint1 (i : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:770
   pragma Import (Stdcall, glEvalPoint1, "glEvalPoint1");

   procedure glEvalPoint2 (i : GLint; j : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:771
   pragma Import (Stdcall, glEvalPoint2, "glEvalPoint2");

   procedure glFeedbackBuffer
     (size : GLsizei;
      c_type : GLenum;
      buffer : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:772
   pragma Import (Stdcall, glFeedbackBuffer, "glFeedbackBuffer");

   procedure glFinish;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:773
   pragma Import (Stdcall, glFinish, "glFinish");

   procedure glFlush;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:774
   pragma Import (Stdcall, glFlush, "glFlush");

   procedure glFogf (pname : GLenum; param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:775
   pragma Import (Stdcall, glFogf, "glFogf");

   procedure glFogfv (pname : GLenum; params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:776
   pragma Import (Stdcall, glFogfv, "glFogfv");

   procedure glFogi (pname : GLenum; param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:777
   pragma Import (Stdcall, glFogi, "glFogi");

   procedure glFogiv (pname : GLenum; params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:778
   pragma Import (Stdcall, glFogiv, "glFogiv");

   procedure glFrontFace (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:779
   pragma Import (Stdcall, glFrontFace, "glFrontFace");

   procedure glFrustum
     (left : GLdouble;
      right : GLdouble;
      bottom : GLdouble;
      top : GLdouble;
      zNear : GLdouble;
      zFar : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:780
   pragma Import (Stdcall, glFrustum, "glFrustum");

   function glGenLists (c_range : GLsizei) return GLuint;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:781
   pragma Import (Stdcall, glGenLists, "glGenLists");

   procedure glGenTextures (n : GLsizei; textures : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:782
   pragma Import (Stdcall, glGenTextures, "glGenTextures");

   procedure glGetBooleanv (pname : GLenum; params : access GLboolean);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:783
   pragma Import (Stdcall, glGetBooleanv, "glGetBooleanv");

   procedure glGetClipPlane (plane : GLenum; equation : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:784
   pragma Import (Stdcall, glGetClipPlane, "glGetClipPlane");

   procedure glGetDoublev (pname : GLenum; params : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:785
   pragma Import (Stdcall, glGetDoublev, "glGetDoublev");

   function glGetError return GLenum;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:786
   pragma Import (Stdcall, glGetError, "glGetError");

   procedure glGetFloatv (pname : GLenum; params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:787
   pragma Import (Stdcall, glGetFloatv, "glGetFloatv");

   procedure glGetIntegerv (pname : GLenum; params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:788
   pragma Import (Stdcall, glGetIntegerv, "glGetIntegerv");

   procedure glGetLightfv
     (light : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:789
   pragma Import (Stdcall, glGetLightfv, "glGetLightfv");

   procedure glGetLightiv
     (light : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:790
   pragma Import (Stdcall, glGetLightiv, "glGetLightiv");

   procedure glGetMapdv
     (target : GLenum;
      query : GLenum;
      v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:791
   pragma Import (Stdcall, glGetMapdv, "glGetMapdv");

   procedure glGetMapfv
     (target : GLenum;
      query : GLenum;
      v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:792
   pragma Import (Stdcall, glGetMapfv, "glGetMapfv");

   procedure glGetMapiv
     (target : GLenum;
      query : GLenum;
      v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:793
   pragma Import (Stdcall, glGetMapiv, "glGetMapiv");

   procedure glGetMaterialfv
     (face : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:794
   pragma Import (Stdcall, glGetMaterialfv, "glGetMaterialfv");

   procedure glGetMaterialiv
     (face : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:795
   pragma Import (Stdcall, glGetMaterialiv, "glGetMaterialiv");

   procedure glGetPixelMapfv (map : GLenum; values : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:796
   pragma Import (Stdcall, glGetPixelMapfv, "glGetPixelMapfv");

   procedure glGetPixelMapuiv (map : GLenum; values : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:797
   pragma Import (Stdcall, glGetPixelMapuiv, "glGetPixelMapuiv");

   procedure glGetPixelMapusv (map : GLenum; values : access GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:798
   pragma Import (Stdcall, glGetPixelMapusv, "glGetPixelMapusv");

   procedure glGetPointerv (pname : GLenum; params : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:799
   pragma Import (Stdcall, glGetPointerv, "glGetPointerv");

   procedure glGetPolygonStipple (mask : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:800
   pragma Import (Stdcall, glGetPolygonStipple, "glGetPolygonStipple");

   function glGetString (name : GLenum) return access GLubyte;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:801
   pragma Import (Stdcall, glGetString, "glGetString");

   procedure glGetTexEnvfv
     (target : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:802
   pragma Import (Stdcall, glGetTexEnvfv, "glGetTexEnvfv");

   procedure glGetTexEnviv
     (target : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:803
   pragma Import (Stdcall, glGetTexEnviv, "glGetTexEnviv");

   procedure glGetTexGendv
     (coord : GLenum;
      pname : GLenum;
      params : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:804
   pragma Import (Stdcall, glGetTexGendv, "glGetTexGendv");

   procedure glGetTexGenfv
     (coord : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:805
   pragma Import (Stdcall, glGetTexGenfv, "glGetTexGenfv");

   procedure glGetTexGeniv
     (coord : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:806
   pragma Import (Stdcall, glGetTexGeniv, "glGetTexGeniv");

   procedure glGetTexImage
     (target : GLenum;
      level : GLint;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:807
   pragma Import (Stdcall, glGetTexImage, "glGetTexImage");

   procedure glGetTexLevelParameterfv
     (target : GLenum;
      level : GLint;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:808
   pragma Import (Stdcall, glGetTexLevelParameterfv, "glGetTexLevelParameterfv");

   procedure glGetTexLevelParameteriv
     (target : GLenum;
      level : GLint;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:809
   pragma Import (Stdcall, glGetTexLevelParameteriv, "glGetTexLevelParameteriv");

   procedure glGetTexParameterfv
     (target : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:810
   pragma Import (Stdcall, glGetTexParameterfv, "glGetTexParameterfv");

   procedure glGetTexParameteriv
     (target : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:811
   pragma Import (Stdcall, glGetTexParameteriv, "glGetTexParameteriv");

   procedure glHint (target : GLenum; mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:812
   pragma Import (Stdcall, glHint, "glHint");

   procedure glIndexMask (mask : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:813
   pragma Import (Stdcall, glIndexMask, "glIndexMask");

   procedure glIndexPointer
     (c_type : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:814
   pragma Import (Stdcall, glIndexPointer, "glIndexPointer");

   procedure glIndexd (c : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:815
   pragma Import (Stdcall, glIndexd, "glIndexd");

   procedure glIndexdv (c : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:816
   pragma Import (Stdcall, glIndexdv, "glIndexdv");

   procedure glIndexf (c : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:817
   pragma Import (Stdcall, glIndexf, "glIndexf");

   procedure glIndexfv (c : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:818
   pragma Import (Stdcall, glIndexfv, "glIndexfv");

   procedure glIndexi (c : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:819
   pragma Import (Stdcall, glIndexi, "glIndexi");

   procedure glIndexiv (c : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:820
   pragma Import (Stdcall, glIndexiv, "glIndexiv");

   procedure glIndexs (c : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:821
   pragma Import (Stdcall, glIndexs, "glIndexs");

   procedure glIndexsv (c : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:822
   pragma Import (Stdcall, glIndexsv, "glIndexsv");

   procedure glIndexub (c : GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:823
   pragma Import (Stdcall, glIndexub, "glIndexub");

   procedure glIndexubv (c : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:824
   pragma Import (Stdcall, glIndexubv, "glIndexubv");

   procedure glInitNames;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:825
   pragma Import (Stdcall, glInitNames, "glInitNames");

   procedure glInterleavedArrays
     (format : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:826
   pragma Import (Stdcall, glInterleavedArrays, "glInterleavedArrays");

   function glIsEnabled (cap : GLenum) return GLboolean;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:827
   pragma Import (Stdcall, glIsEnabled, "glIsEnabled");

   function glIsList (list : GLuint) return GLboolean;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:828
   pragma Import (Stdcall, glIsList, "glIsList");

   function glIsTexture (texture : GLuint) return GLboolean;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:829
   pragma Import (Stdcall, glIsTexture, "glIsTexture");

   procedure glLightModelf (pname : GLenum; param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:830
   pragma Import (Stdcall, glLightModelf, "glLightModelf");

   procedure glLightModelfv (pname : GLenum; params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:831
   pragma Import (Stdcall, glLightModelfv, "glLightModelfv");

   procedure glLightModeli (pname : GLenum; param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:832
   pragma Import (Stdcall, glLightModeli, "glLightModeli");

   procedure glLightModeliv (pname : GLenum; params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:833
   pragma Import (Stdcall, glLightModeliv, "glLightModeliv");

   procedure glLightf
     (light : GLenum;
      pname : GLenum;
      param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:834
   pragma Import (Stdcall, glLightf, "glLightf");

   procedure glLightfv
     (light : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:835
   pragma Import (Stdcall, glLightfv, "glLightfv");

   procedure glLighti
     (light : GLenum;
      pname : GLenum;
      param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:836
   pragma Import (Stdcall, glLighti, "glLighti");

   procedure glLightiv
     (light : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:837
   pragma Import (Stdcall, glLightiv, "glLightiv");

   procedure glLineStipple (factor : GLint; pattern : GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:838
   pragma Import (Stdcall, glLineStipple, "glLineStipple");

   procedure glLineWidth (width : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:839
   pragma Import (Stdcall, glLineWidth, "glLineWidth");

   procedure glListBase (base : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:840
   pragma Import (Stdcall, glListBase, "glListBase");

   procedure glLoadIdentity;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:841
   pragma Import (Stdcall, glLoadIdentity, "glLoadIdentity");

   procedure glLoadMatrixd (m : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:842
   pragma Import (Stdcall, glLoadMatrixd, "glLoadMatrixd");

   procedure glLoadMatrixf (m : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:843
   pragma Import (Stdcall, glLoadMatrixf, "glLoadMatrixf");

   procedure glLoadName (name : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:844
   pragma Import (Stdcall, glLoadName, "glLoadName");

   procedure glLogicOp (opcode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:845
   pragma Import (Stdcall, glLogicOp, "glLogicOp");

   procedure glMap1d
     (target : GLenum;
      u1 : GLdouble;
      u2 : GLdouble;
      stride : GLint;
      order : GLint;
      points : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:846
   pragma Import (Stdcall, glMap1d, "glMap1d");

   procedure glMap1f
     (target : GLenum;
      u1 : GLfloat;
      u2 : GLfloat;
      stride : GLint;
      order : GLint;
      points : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:847
   pragma Import (Stdcall, glMap1f, "glMap1f");

   procedure glMap2d
     (target : GLenum;
      u1 : GLdouble;
      u2 : GLdouble;
      ustride : GLint;
      uorder : GLint;
      v1 : GLdouble;
      v2 : GLdouble;
      vstride : GLint;
      vorder : GLint;
      points : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:848
   pragma Import (Stdcall, glMap2d, "glMap2d");

   procedure glMap2f
     (target : GLenum;
      u1 : GLfloat;
      u2 : GLfloat;
      ustride : GLint;
      uorder : GLint;
      v1 : GLfloat;
      v2 : GLfloat;
      vstride : GLint;
      vorder : GLint;
      points : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:849
   pragma Import (Stdcall, glMap2f, "glMap2f");

   procedure glMapGrid1d
     (un : GLint;
      u1 : GLdouble;
      u2 : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:850
   pragma Import (Stdcall, glMapGrid1d, "glMapGrid1d");

   procedure glMapGrid1f
     (un : GLint;
      u1 : GLfloat;
      u2 : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:851
   pragma Import (Stdcall, glMapGrid1f, "glMapGrid1f");

   procedure glMapGrid2d
     (un : GLint;
      u1 : GLdouble;
      u2 : GLdouble;
      vn : GLint;
      v1 : GLdouble;
      v2 : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:852
   pragma Import (Stdcall, glMapGrid2d, "glMapGrid2d");

   procedure glMapGrid2f
     (un : GLint;
      u1 : GLfloat;
      u2 : GLfloat;
      vn : GLint;
      v1 : GLfloat;
      v2 : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:853
   pragma Import (Stdcall, glMapGrid2f, "glMapGrid2f");

   procedure glMaterialf
     (face : GLenum;
      pname : GLenum;
      param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:854
   pragma Import (Stdcall, glMaterialf, "glMaterialf");

   procedure glMaterialfv
     (face : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:855
   pragma Import (Stdcall, glMaterialfv, "glMaterialfv");

   procedure glMateriali
     (face : GLenum;
      pname : GLenum;
      param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:856
   pragma Import (Stdcall, glMateriali, "glMateriali");

   procedure glMaterialiv
     (face : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:857
   pragma Import (Stdcall, glMaterialiv, "glMaterialiv");

   procedure glMatrixMode (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:858
   pragma Import (Stdcall, glMatrixMode, "glMatrixMode");

   procedure glMultMatrixd (m : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:859
   pragma Import (Stdcall, glMultMatrixd, "glMultMatrixd");

   procedure glMultMatrixf (m : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:860
   pragma Import (Stdcall, glMultMatrixf, "glMultMatrixf");

   procedure glNewList (list : GLuint; mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:861
   pragma Import (Stdcall, glNewList, "glNewList");

   procedure glNormal3b
     (nx : GLbyte;
      ny : GLbyte;
      nz : GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:862
   pragma Import (Stdcall, glNormal3b, "glNormal3b");

   procedure glNormal3bv (v : access GLbyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:863
   pragma Import (Stdcall, glNormal3bv, "glNormal3bv");

   procedure glNormal3d
     (nx : GLdouble;
      ny : GLdouble;
      nz : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:864
   pragma Import (Stdcall, glNormal3d, "glNormal3d");

   procedure glNormal3dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:865
   pragma Import (Stdcall, glNormal3dv, "glNormal3dv");

   procedure glNormal3f
     (nx : GLfloat;
      ny : GLfloat;
      nz : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:866
   pragma Import (Stdcall, glNormal3f, "glNormal3f");

   procedure glNormal3fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:867
   pragma Import (Stdcall, glNormal3fv, "glNormal3fv");

   procedure glNormal3i
     (nx : GLint;
      ny : GLint;
      nz : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:868
   pragma Import (Stdcall, glNormal3i, "glNormal3i");

   procedure glNormal3iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:869
   pragma Import (Stdcall, glNormal3iv, "glNormal3iv");

   procedure glNormal3s
     (nx : GLshort;
      ny : GLshort;
      nz : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:870
   pragma Import (Stdcall, glNormal3s, "glNormal3s");

   procedure glNormal3sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:871
   pragma Import (Stdcall, glNormal3sv, "glNormal3sv");

   procedure glNormalPointer
     (c_type : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:872
   pragma Import (Stdcall, glNormalPointer, "glNormalPointer");

   procedure glOrtho
     (left : GLdouble;
      right : GLdouble;
      bottom : GLdouble;
      top : GLdouble;
      zNear : GLdouble;
      zFar : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:873
   pragma Import (Stdcall, glOrtho, "glOrtho");

   procedure glPassThrough (token : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:874
   pragma Import (Stdcall, glPassThrough, "glPassThrough");

   procedure glPixelMapfv
     (map : GLenum;
      mapsize : GLsizei;
      values : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:875
   pragma Import (Stdcall, glPixelMapfv, "glPixelMapfv");

   procedure glPixelMapuiv
     (map : GLenum;
      mapsize : GLsizei;
      values : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:876
   pragma Import (Stdcall, glPixelMapuiv, "glPixelMapuiv");

   procedure glPixelMapusv
     (map : GLenum;
      mapsize : GLsizei;
      values : access GLushort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:877
   pragma Import (Stdcall, glPixelMapusv, "glPixelMapusv");

   procedure glPixelStoref (pname : GLenum; param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:878
   pragma Import (Stdcall, glPixelStoref, "glPixelStoref");

   procedure glPixelStorei (pname : GLenum; param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:879
   pragma Import (Stdcall, glPixelStorei, "glPixelStorei");

   procedure glPixelTransferf (pname : GLenum; param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:880
   pragma Import (Stdcall, glPixelTransferf, "glPixelTransferf");

   procedure glPixelTransferi (pname : GLenum; param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:881
   pragma Import (Stdcall, glPixelTransferi, "glPixelTransferi");

   procedure glPixelZoom (xfactor : GLfloat; yfactor : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:882
   pragma Import (Stdcall, glPixelZoom, "glPixelZoom");

   procedure glPointSize (size : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:883
   pragma Import (Stdcall, glPointSize, "glPointSize");

   procedure glPolygonMode (face : GLenum; mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:884
   pragma Import (Stdcall, glPolygonMode, "glPolygonMode");

   procedure glPolygonOffset (factor : GLfloat; units : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:885
   pragma Import (Stdcall, glPolygonOffset, "glPolygonOffset");

   procedure glPolygonStipple (mask : access GLubyte);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:886
   pragma Import (Stdcall, glPolygonStipple, "glPolygonStipple");

   procedure glPopAttrib;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:887
   pragma Import (Stdcall, glPopAttrib, "glPopAttrib");

   procedure glPopClientAttrib;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:888
   pragma Import (Stdcall, glPopClientAttrib, "glPopClientAttrib");

   procedure glPopMatrix;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:889
   pragma Import (Stdcall, glPopMatrix, "glPopMatrix");

   procedure glPopName;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:890
   pragma Import (Stdcall, glPopName, "glPopName");

   procedure glPrioritizeTextures
     (n : GLsizei;
      textures : access GLuint;
      priorities : access GLclampf);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:891
   pragma Import (Stdcall, glPrioritizeTextures, "glPrioritizeTextures");

   procedure glPushAttrib (mask : GLbitfield);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:892
   pragma Import (Stdcall, glPushAttrib, "glPushAttrib");

   procedure glPushClientAttrib (mask : GLbitfield);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:893
   pragma Import (Stdcall, glPushClientAttrib, "glPushClientAttrib");

   procedure glPushMatrix;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:894
   pragma Import (Stdcall, glPushMatrix, "glPushMatrix");

   procedure glPushName (name : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:895
   pragma Import (Stdcall, glPushName, "glPushName");

   procedure glRasterPos2d (x : GLdouble; y : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:896
   pragma Import (Stdcall, glRasterPos2d, "glRasterPos2d");

   procedure glRasterPos2dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:897
   pragma Import (Stdcall, glRasterPos2dv, "glRasterPos2dv");

   procedure glRasterPos2f (x : GLfloat; y : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:898
   pragma Import (Stdcall, glRasterPos2f, "glRasterPos2f");

   procedure glRasterPos2fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:899
   pragma Import (Stdcall, glRasterPos2fv, "glRasterPos2fv");

   procedure glRasterPos2i (x : GLint; y : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:900
   pragma Import (Stdcall, glRasterPos2i, "glRasterPos2i");

   procedure glRasterPos2iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:901
   pragma Import (Stdcall, glRasterPos2iv, "glRasterPos2iv");

   procedure glRasterPos2s (x : GLshort; y : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:902
   pragma Import (Stdcall, glRasterPos2s, "glRasterPos2s");

   procedure glRasterPos2sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:903
   pragma Import (Stdcall, glRasterPos2sv, "glRasterPos2sv");

   procedure glRasterPos3d
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:904
   pragma Import (Stdcall, glRasterPos3d, "glRasterPos3d");

   procedure glRasterPos3dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:905
   pragma Import (Stdcall, glRasterPos3dv, "glRasterPos3dv");

   procedure glRasterPos3f
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:906
   pragma Import (Stdcall, glRasterPos3f, "glRasterPos3f");

   procedure glRasterPos3fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:907
   pragma Import (Stdcall, glRasterPos3fv, "glRasterPos3fv");

   procedure glRasterPos3i
     (x : GLint;
      y : GLint;
      z : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:908
   pragma Import (Stdcall, glRasterPos3i, "glRasterPos3i");

   procedure glRasterPos3iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:909
   pragma Import (Stdcall, glRasterPos3iv, "glRasterPos3iv");

   procedure glRasterPos3s
     (x : GLshort;
      y : GLshort;
      z : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:910
   pragma Import (Stdcall, glRasterPos3s, "glRasterPos3s");

   procedure glRasterPos3sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:911
   pragma Import (Stdcall, glRasterPos3sv, "glRasterPos3sv");

   procedure glRasterPos4d
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble;
      w : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:912
   pragma Import (Stdcall, glRasterPos4d, "glRasterPos4d");

   procedure glRasterPos4dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:913
   pragma Import (Stdcall, glRasterPos4dv, "glRasterPos4dv");

   procedure glRasterPos4f
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat;
      w : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:914
   pragma Import (Stdcall, glRasterPos4f, "glRasterPos4f");

   procedure glRasterPos4fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:915
   pragma Import (Stdcall, glRasterPos4fv, "glRasterPos4fv");

   procedure glRasterPos4i
     (x : GLint;
      y : GLint;
      z : GLint;
      w : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:916
   pragma Import (Stdcall, glRasterPos4i, "glRasterPos4i");

   procedure glRasterPos4iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:917
   pragma Import (Stdcall, glRasterPos4iv, "glRasterPos4iv");

   procedure glRasterPos4s
     (x : GLshort;
      y : GLshort;
      z : GLshort;
      w : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:918
   pragma Import (Stdcall, glRasterPos4s, "glRasterPos4s");

   procedure glRasterPos4sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:919
   pragma Import (Stdcall, glRasterPos4sv, "glRasterPos4sv");

   procedure glReadBuffer (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:920
   pragma Import (Stdcall, glReadBuffer, "glReadBuffer");

   procedure glReadPixels
     (x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:921
   pragma Import (Stdcall, glReadPixels, "glReadPixels");

   procedure glRectd
     (x1 : GLdouble;
      y1 : GLdouble;
      x2 : GLdouble;
      y2 : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:922
   pragma Import (Stdcall, glRectd, "glRectd");

   procedure glRectdv (v1 : access GLdouble; v2 : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:923
   pragma Import (Stdcall, glRectdv, "glRectdv");

   procedure glRectf
     (x1 : GLfloat;
      y1 : GLfloat;
      x2 : GLfloat;
      y2 : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:924
   pragma Import (Stdcall, glRectf, "glRectf");

   procedure glRectfv (v1 : access GLfloat; v2 : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:925
   pragma Import (Stdcall, glRectfv, "glRectfv");

   procedure glRecti
     (x1 : GLint;
      y1 : GLint;
      x2 : GLint;
      y2 : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:926
   pragma Import (Stdcall, glRecti, "glRecti");

   procedure glRectiv (v1 : access GLint; v2 : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:927
   pragma Import (Stdcall, glRectiv, "glRectiv");

   procedure glRects
     (x1 : GLshort;
      y1 : GLshort;
      x2 : GLshort;
      y2 : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:928
   pragma Import (Stdcall, glRects, "glRects");

   procedure glRectsv (v1 : access GLshort; v2 : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:929
   pragma Import (Stdcall, glRectsv, "glRectsv");

   function glRenderMode (mode : GLenum) return GLint;  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:930
   pragma Import (Stdcall, glRenderMode, "glRenderMode");

   procedure glRotated
     (angle : GLdouble;
      x : GLdouble;
      y : GLdouble;
      z : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:931
   pragma Import (Stdcall, glRotated, "glRotated");

   procedure glRotatef
     (angle : GLfloat;
      x : GLfloat;
      y : GLfloat;
      z : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:932
   pragma Import (Stdcall, glRotatef, "glRotatef");

   procedure glScaled
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:933
   pragma Import (Stdcall, glScaled, "glScaled");

   procedure glScalef
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:934
   pragma Import (Stdcall, glScalef, "glScalef");

   procedure glScissor
     (x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:935
   pragma Import (Stdcall, glScissor, "glScissor");

   procedure glSelectBuffer (size : GLsizei; buffer : access GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:936
   pragma Import (Stdcall, glSelectBuffer, "glSelectBuffer");

   procedure glShadeModel (mode : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:937
   pragma Import (Stdcall, glShadeModel, "glShadeModel");

   procedure glStencilFunc
     (func : GLenum;
      ref : GLint;
      mask : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:938
   pragma Import (Stdcall, glStencilFunc, "glStencilFunc");

   procedure glStencilMask (mask : GLuint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:939
   pragma Import (Stdcall, glStencilMask, "glStencilMask");

   procedure glStencilOp
     (fail : GLenum;
      zfail : GLenum;
      zpass : GLenum);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:940
   pragma Import (Stdcall, glStencilOp, "glStencilOp");

   procedure glTexCoord1d (s : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:941
   pragma Import (Stdcall, glTexCoord1d, "glTexCoord1d");

   procedure glTexCoord1dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:942
   pragma Import (Stdcall, glTexCoord1dv, "glTexCoord1dv");

   procedure glTexCoord1f (s : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:943
   pragma Import (Stdcall, glTexCoord1f, "glTexCoord1f");

   procedure glTexCoord1fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:944
   pragma Import (Stdcall, glTexCoord1fv, "glTexCoord1fv");

   procedure glTexCoord1i (s : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:945
   pragma Import (Stdcall, glTexCoord1i, "glTexCoord1i");

   procedure glTexCoord1iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:946
   pragma Import (Stdcall, glTexCoord1iv, "glTexCoord1iv");

   procedure glTexCoord1s (s : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:947
   pragma Import (Stdcall, glTexCoord1s, "glTexCoord1s");

   procedure glTexCoord1sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:948
   pragma Import (Stdcall, glTexCoord1sv, "glTexCoord1sv");

   procedure glTexCoord2d (s : GLdouble; t : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:949
   pragma Import (Stdcall, glTexCoord2d, "glTexCoord2d");

   procedure glTexCoord2dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:950
   pragma Import (Stdcall, glTexCoord2dv, "glTexCoord2dv");

   procedure glTexCoord2f (s : GLfloat; t : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:951
   pragma Import (Stdcall, glTexCoord2f, "glTexCoord2f");

   procedure glTexCoord2fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:952
   pragma Import (Stdcall, glTexCoord2fv, "glTexCoord2fv");

   procedure glTexCoord2i (s : GLint; t : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:953
   pragma Import (Stdcall, glTexCoord2i, "glTexCoord2i");

   procedure glTexCoord2iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:954
   pragma Import (Stdcall, glTexCoord2iv, "glTexCoord2iv");

   procedure glTexCoord2s (s : GLshort; t : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:955
   pragma Import (Stdcall, glTexCoord2s, "glTexCoord2s");

   procedure glTexCoord2sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:956
   pragma Import (Stdcall, glTexCoord2sv, "glTexCoord2sv");

   procedure glTexCoord3d
     (s : GLdouble;
      t : GLdouble;
      r : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:957
   pragma Import (Stdcall, glTexCoord3d, "glTexCoord3d");

   procedure glTexCoord3dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:958
   pragma Import (Stdcall, glTexCoord3dv, "glTexCoord3dv");

   procedure glTexCoord3f
     (s : GLfloat;
      t : GLfloat;
      r : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:959
   pragma Import (Stdcall, glTexCoord3f, "glTexCoord3f");

   procedure glTexCoord3fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:960
   pragma Import (Stdcall, glTexCoord3fv, "glTexCoord3fv");

   procedure glTexCoord3i
     (s : GLint;
      t : GLint;
      r : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:961
   pragma Import (Stdcall, glTexCoord3i, "glTexCoord3i");

   procedure glTexCoord3iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:962
   pragma Import (Stdcall, glTexCoord3iv, "glTexCoord3iv");

   procedure glTexCoord3s
     (s : GLshort;
      t : GLshort;
      r : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:963
   pragma Import (Stdcall, glTexCoord3s, "glTexCoord3s");

   procedure glTexCoord3sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:964
   pragma Import (Stdcall, glTexCoord3sv, "glTexCoord3sv");

   procedure glTexCoord4d
     (s : GLdouble;
      t : GLdouble;
      r : GLdouble;
      q : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:965
   pragma Import (Stdcall, glTexCoord4d, "glTexCoord4d");

   procedure glTexCoord4dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:966
   pragma Import (Stdcall, glTexCoord4dv, "glTexCoord4dv");

   procedure glTexCoord4f
     (s : GLfloat;
      t : GLfloat;
      r : GLfloat;
      q : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:967
   pragma Import (Stdcall, glTexCoord4f, "glTexCoord4f");

   procedure glTexCoord4fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:968
   pragma Import (Stdcall, glTexCoord4fv, "glTexCoord4fv");

   procedure glTexCoord4i
     (s : GLint;
      t : GLint;
      r : GLint;
      q : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:969
   pragma Import (Stdcall, glTexCoord4i, "glTexCoord4i");

   procedure glTexCoord4iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:970
   pragma Import (Stdcall, glTexCoord4iv, "glTexCoord4iv");

   procedure glTexCoord4s
     (s : GLshort;
      t : GLshort;
      r : GLshort;
      q : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:971
   pragma Import (Stdcall, glTexCoord4s, "glTexCoord4s");

   procedure glTexCoord4sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:972
   pragma Import (Stdcall, glTexCoord4sv, "glTexCoord4sv");

   procedure glTexCoordPointer
     (size : GLint;
      c_type : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:973
   pragma Import (Stdcall, glTexCoordPointer, "glTexCoordPointer");

   procedure glTexEnvf
     (target : GLenum;
      pname : GLenum;
      param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:974
   pragma Import (Stdcall, glTexEnvf, "glTexEnvf");

   procedure glTexEnvfv
     (target : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:975
   pragma Import (Stdcall, glTexEnvfv, "glTexEnvfv");

   procedure glTexEnvi
     (target : GLenum;
      pname : GLenum;
      param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:976
   pragma Import (Stdcall, glTexEnvi, "glTexEnvi");

   procedure glTexEnviv
     (target : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:977
   pragma Import (Stdcall, glTexEnviv, "glTexEnviv");

   procedure glTexGend
     (coord : GLenum;
      pname : GLenum;
      param : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:978
   pragma Import (Stdcall, glTexGend, "glTexGend");

   procedure glTexGendv
     (coord : GLenum;
      pname : GLenum;
      params : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:979
   pragma Import (Stdcall, glTexGendv, "glTexGendv");

   procedure glTexGenf
     (coord : GLenum;
      pname : GLenum;
      param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:980
   pragma Import (Stdcall, glTexGenf, "glTexGenf");

   procedure glTexGenfv
     (coord : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:981
   pragma Import (Stdcall, glTexGenfv, "glTexGenfv");

   procedure glTexGeni
     (coord : GLenum;
      pname : GLenum;
      param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:982
   pragma Import (Stdcall, glTexGeni, "glTexGeni");

   procedure glTexGeniv
     (coord : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:983
   pragma Import (Stdcall, glTexGeniv, "glTexGeniv");

   procedure glTexImage1D
     (target : GLenum;
      level : GLint;
      internalformat : GLint;
      width : GLsizei;
      border : GLint;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:984
   pragma Import (Stdcall, glTexImage1D, "glTexImage1D");

   procedure glTexImage2D
     (target : GLenum;
      level : GLint;
      internalformat : GLint;
      width : GLsizei;
      height : GLsizei;
      border : GLint;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:985
   pragma Import (Stdcall, glTexImage2D, "glTexImage2D");

   procedure glTexParameterf
     (target : GLenum;
      pname : GLenum;
      param : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:986
   pragma Import (Stdcall, glTexParameterf, "glTexParameterf");

   procedure glTexParameterfv
     (target : GLenum;
      pname : GLenum;
      params : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:987
   pragma Import (Stdcall, glTexParameterfv, "glTexParameterfv");

   procedure glTexParameteri
     (target : GLenum;
      pname : GLenum;
      param : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:988
   pragma Import (Stdcall, glTexParameteri, "glTexParameteri");

   procedure glTexParameteriv
     (target : GLenum;
      pname : GLenum;
      params : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:989
   pragma Import (Stdcall, glTexParameteriv, "glTexParameteriv");

   procedure glTexSubImage1D
     (target : GLenum;
      level : GLint;
      xoffset : GLint;
      width : GLsizei;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:990
   pragma Import (Stdcall, glTexSubImage1D, "glTexSubImage1D");

   procedure glTexSubImage2D
     (target : GLenum;
      level : GLint;
      xoffset : GLint;
      yoffset : GLint;
      width : GLsizei;
      height : GLsizei;
      format : GLenum;
      c_type : GLenum;
      pixels : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:991
   pragma Import (Stdcall, glTexSubImage2D, "glTexSubImage2D");

   procedure glTranslated
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:992
   pragma Import (Stdcall, glTranslated, "glTranslated");

   procedure glTranslatef
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:993
   pragma Import (Stdcall, glTranslatef, "glTranslatef");

   procedure glVertex2d (x : GLdouble; y : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:994
   pragma Import (Stdcall, glVertex2d, "glVertex2d");

   procedure glVertex2dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:995
   pragma Import (Stdcall, glVertex2dv, "glVertex2dv");

   procedure glVertex2f (x : GLfloat; y : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:996
   pragma Import (Stdcall, glVertex2f, "glVertex2f");

   procedure glVertex2fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:997
   pragma Import (Stdcall, glVertex2fv, "glVertex2fv");

   procedure glVertex2i (x : GLint; y : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:998
   pragma Import (Stdcall, glVertex2i, "glVertex2i");

   procedure glVertex2iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:999
   pragma Import (Stdcall, glVertex2iv, "glVertex2iv");

   procedure glVertex2s (x : GLshort; y : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1000
   pragma Import (Stdcall, glVertex2s, "glVertex2s");

   procedure glVertex2sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1001
   pragma Import (Stdcall, glVertex2sv, "glVertex2sv");

   procedure glVertex3d
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1002
   pragma Import (Stdcall, glVertex3d, "glVertex3d");

   procedure glVertex3dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1003
   pragma Import (Stdcall, glVertex3dv, "glVertex3dv");

   procedure glVertex3f
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1004
   pragma Import (Stdcall, glVertex3f, "glVertex3f");

   procedure glVertex3fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1005
   pragma Import (Stdcall, glVertex3fv, "glVertex3fv");

   procedure glVertex3i
     (x : GLint;
      y : GLint;
      z : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1006
   pragma Import (Stdcall, glVertex3i, "glVertex3i");

   procedure glVertex3iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1007
   pragma Import (Stdcall, glVertex3iv, "glVertex3iv");

   procedure glVertex3s
     (x : GLshort;
      y : GLshort;
      z : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1008
   pragma Import (Stdcall, glVertex3s, "glVertex3s");

   procedure glVertex3sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1009
   pragma Import (Stdcall, glVertex3sv, "glVertex3sv");

   procedure glVertex4d
     (x : GLdouble;
      y : GLdouble;
      z : GLdouble;
      w : GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1010
   pragma Import (Stdcall, glVertex4d, "glVertex4d");

   procedure glVertex4dv (v : access GLdouble);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1011
   pragma Import (Stdcall, glVertex4dv, "glVertex4dv");

   procedure glVertex4f
     (x : GLfloat;
      y : GLfloat;
      z : GLfloat;
      w : GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1012
   pragma Import (Stdcall, glVertex4f, "glVertex4f");

   procedure glVertex4fv (v : access GLfloat);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1013
   pragma Import (Stdcall, glVertex4fv, "glVertex4fv");

   procedure glVertex4i
     (x : GLint;
      y : GLint;
      z : GLint;
      w : GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1014
   pragma Import (Stdcall, glVertex4i, "glVertex4i");

   procedure glVertex4iv (v : access GLint);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1015
   pragma Import (Stdcall, glVertex4iv, "glVertex4iv");

   procedure glVertex4s
     (x : GLshort;
      y : GLshort;
      z : GLshort;
      w : GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1016
   pragma Import (Stdcall, glVertex4s, "glVertex4s");

   procedure glVertex4sv (v : access GLshort);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1017
   pragma Import (Stdcall, glVertex4sv, "glVertex4sv");

   procedure glVertexPointer
     (size : GLint;
      c_type : GLenum;
      stride : GLsizei;
      pointer : System.Address);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1018
   pragma Import (Stdcall, glVertexPointer, "glVertexPointer");

   procedure glViewport
     (x : GLint;
      y : GLint;
      width : GLsizei;
      height : GLsizei);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1019
   pragma Import (Stdcall, glViewport, "glViewport");

   type PFNGLARRAYELEMENTEXTPROC is access procedure (arg1 : GLint);
   pragma Convention (C, PFNGLARRAYELEMENTEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1021

   type PFNGLDRAWARRAYSEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLint;
         arg3 : GLsizei);
   pragma Convention (C, PFNGLDRAWARRAYSEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1022

   type PFNGLVERTEXPOINTEREXTPROC is access procedure
        (arg1 : GLint;
         arg2 : GLenum;
         arg3 : GLsizei;
         arg4 : GLsizei;
         arg5 : System.Address);
   pragma Convention (C, PFNGLVERTEXPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1023

   type PFNGLNORMALPOINTEREXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLsizei;
         arg3 : GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLNORMALPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1024

   type PFNGLCOLORPOINTEREXTPROC is access procedure
        (arg1 : GLint;
         arg2 : GLenum;
         arg3 : GLsizei;
         arg4 : GLsizei;
         arg5 : System.Address);
   pragma Convention (C, PFNGLCOLORPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1025

   type PFNGLINDEXPOINTEREXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLsizei;
         arg3 : GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLINDEXPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1026

   type PFNGLTEXCOORDPOINTEREXTPROC is access procedure
        (arg1 : GLint;
         arg2 : GLenum;
         arg3 : GLsizei;
         arg4 : GLsizei;
         arg5 : System.Address);
   pragma Convention (C, PFNGLTEXCOORDPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1027

   type PFNGLEDGEFLAGPOINTEREXTPROC is access procedure
        (arg1 : GLsizei;
         arg2 : GLsizei;
         arg3 : access GLboolean);
   pragma Convention (C, PFNGLEDGEFLAGPOINTEREXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1028

   type PFNGLGETPOINTERVEXTPROC is access procedure (arg1 : GLenum; arg2 : System.Address);
   pragma Convention (C, PFNGLGETPOINTERVEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1029

   type PFNGLARRAYELEMENTARRAYEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLARRAYELEMENTARRAYEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1030

   type PFNGLDRAWRANGEELEMENTSWINPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLuint;
         arg3 : GLuint;
         arg4 : GLsizei;
         arg5 : GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLDRAWRANGEELEMENTSWINPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1031

   type PFNGLADDSWAPHINTRECTWINPROC is access procedure
        (arg1 : GLint;
         arg2 : GLint;
         arg3 : GLsizei;
         arg4 : GLsizei);
   pragma Convention (C, PFNGLADDSWAPHINTRECTWINPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1032

   type PFNGLCOLORTABLEEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLenum;
         arg3 : GLsizei;
         arg4 : GLenum;
         arg5 : GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCOLORTABLEEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1033

   type PFNGLGETCOLORTABLEEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLenum;
         arg3 : GLenum;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETCOLORTABLEEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1035

   type PFNGLGETCOLORTABLEPARAMETERIVEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLenum;
         arg3 : access GLint);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERIVEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1036

   type PFNGLGETCOLORTABLEPARAMETERFVEXTPROC is access procedure
        (arg1 : GLenum;
         arg2 : GLenum;
         arg3 : access GLfloat);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERFVEXTPROC);  -- c:\home\ochem\install\bin\../lib/gcc/i686-pc-mingw32/4.7.3/../../../../i686-pc-mingw32/include/GL/gl.h:1037

end GL_gl_h;
