pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;
with umingw_h;
with GL_gl_h;
with System;

package SDL_SDL_opengl_h is

   --  unsupported macro: APIENTRYP APIENTRY *
   --  unsupported macro: GLAPI extern

   GL_GLEXT_VERSION : constant := 29;  --  ../include/SDL/SDL_opengl.h:116

   GL_UNSIGNED_BYTE_3_3_2 : constant := 16#8032#;  --  ../include/SDL/SDL_opengl.h:119
   GL_UNSIGNED_SHORT_4_4_4_4 : constant := 16#8033#;  --  ../include/SDL/SDL_opengl.h:120
   GL_UNSIGNED_SHORT_5_5_5_1 : constant := 16#8034#;  --  ../include/SDL/SDL_opengl.h:121
   GL_UNSIGNED_INT_8_8_8_8 : constant := 16#8035#;  --  ../include/SDL/SDL_opengl.h:122
   GL_UNSIGNED_INT_10_10_10_2 : constant := 16#8036#;  --  ../include/SDL/SDL_opengl.h:123
   GL_RESCALE_NORMAL : constant := 16#803A#;  --  ../include/SDL/SDL_opengl.h:124
   GL_TEXTURE_BINDING_3D : constant := 16#806A#;  --  ../include/SDL/SDL_opengl.h:125
   GL_PACK_SKIP_IMAGES : constant := 16#806B#;  --  ../include/SDL/SDL_opengl.h:126
   GL_PACK_IMAGE_HEIGHT : constant := 16#806C#;  --  ../include/SDL/SDL_opengl.h:127
   GL_UNPACK_SKIP_IMAGES : constant := 16#806D#;  --  ../include/SDL/SDL_opengl.h:128
   GL_UNPACK_IMAGE_HEIGHT : constant := 16#806E#;  --  ../include/SDL/SDL_opengl.h:129
   GL_TEXTURE_3D : constant := 16#806F#;  --  ../include/SDL/SDL_opengl.h:130
   GL_PROXY_TEXTURE_3D : constant := 16#8070#;  --  ../include/SDL/SDL_opengl.h:131
   GL_TEXTURE_DEPTH : constant := 16#8071#;  --  ../include/SDL/SDL_opengl.h:132
   GL_TEXTURE_WRAP_R : constant := 16#8072#;  --  ../include/SDL/SDL_opengl.h:133
   GL_MAX_3D_TEXTURE_SIZE : constant := 16#8073#;  --  ../include/SDL/SDL_opengl.h:134
   GL_UNSIGNED_BYTE_2_3_3_REV : constant := 16#8362#;  --  ../include/SDL/SDL_opengl.h:135
   GL_UNSIGNED_SHORT_5_6_5 : constant := 16#8363#;  --  ../include/SDL/SDL_opengl.h:136
   GL_UNSIGNED_SHORT_5_6_5_REV : constant := 16#8364#;  --  ../include/SDL/SDL_opengl.h:137
   GL_UNSIGNED_SHORT_4_4_4_4_REV : constant := 16#8365#;  --  ../include/SDL/SDL_opengl.h:138
   GL_UNSIGNED_SHORT_1_5_5_5_REV : constant := 16#8366#;  --  ../include/SDL/SDL_opengl.h:139
   GL_UNSIGNED_INT_8_8_8_8_REV : constant := 16#8367#;  --  ../include/SDL/SDL_opengl.h:140
   GL_UNSIGNED_INT_2_10_10_10_REV : constant := 16#8368#;  --  ../include/SDL/SDL_opengl.h:141
   GL_BGR : constant := 16#80E0#;  --  ../include/SDL/SDL_opengl.h:142
   GL_BGRA : constant := 16#80E1#;  --  ../include/SDL/SDL_opengl.h:143
   GL_MAX_ELEMENTS_VERTICES : constant := 16#80E8#;  --  ../include/SDL/SDL_opengl.h:144
   GL_MAX_ELEMENTS_INDICES : constant := 16#80E9#;  --  ../include/SDL/SDL_opengl.h:145
   GL_CLAMP_TO_EDGE : constant := 16#812F#;  --  ../include/SDL/SDL_opengl.h:146
   GL_TEXTURE_MIN_LOD : constant := 16#813A#;  --  ../include/SDL/SDL_opengl.h:147
   GL_TEXTURE_MAX_LOD : constant := 16#813B#;  --  ../include/SDL/SDL_opengl.h:148
   GL_TEXTURE_BASE_LEVEL : constant := 16#813C#;  --  ../include/SDL/SDL_opengl.h:149
   GL_TEXTURE_MAX_LEVEL : constant := 16#813D#;  --  ../include/SDL/SDL_opengl.h:150
   GL_LIGHT_MODEL_COLOR_CONTROL : constant := 16#81F8#;  --  ../include/SDL/SDL_opengl.h:151
   GL_SINGLE_COLOR : constant := 16#81F9#;  --  ../include/SDL/SDL_opengl.h:152
   GL_SEPARATE_SPECULAR_COLOR : constant := 16#81FA#;  --  ../include/SDL/SDL_opengl.h:153
   GL_SMOOTH_POINT_SIZE_RANGE : constant := 16#0B12#;  --  ../include/SDL/SDL_opengl.h:154
   GL_SMOOTH_POINT_SIZE_GRANULARITY : constant := 16#0B13#;  --  ../include/SDL/SDL_opengl.h:155
   GL_SMOOTH_LINE_WIDTH_RANGE : constant := 16#0B22#;  --  ../include/SDL/SDL_opengl.h:156
   GL_SMOOTH_LINE_WIDTH_GRANULARITY : constant := 16#0B23#;  --  ../include/SDL/SDL_opengl.h:157
   GL_ALIASED_POINT_SIZE_RANGE : constant := 16#846D#;  --  ../include/SDL/SDL_opengl.h:158
   GL_ALIASED_LINE_WIDTH_RANGE : constant := 16#846E#;  --  ../include/SDL/SDL_opengl.h:159

   GL_CONSTANT_COLOR : constant := 16#8001#;  --  ../include/SDL/SDL_opengl.h:163
   GL_ONE_MINUS_CONSTANT_COLOR : constant := 16#8002#;  --  ../include/SDL/SDL_opengl.h:164
   GL_CONSTANT_ALPHA : constant := 16#8003#;  --  ../include/SDL/SDL_opengl.h:165
   GL_ONE_MINUS_CONSTANT_ALPHA : constant := 16#8004#;  --  ../include/SDL/SDL_opengl.h:166
   GL_BLEND_COLOR : constant := 16#8005#;  --  ../include/SDL/SDL_opengl.h:167
   GL_FUNC_ADD : constant := 16#8006#;  --  ../include/SDL/SDL_opengl.h:168
   GL_MIN : constant := 16#8007#;  --  ../include/SDL/SDL_opengl.h:169
   GL_MAX : constant := 16#8008#;  --  ../include/SDL/SDL_opengl.h:170
   GL_BLEND_EQUATION : constant := 16#8009#;  --  ../include/SDL/SDL_opengl.h:171
   GL_FUNC_SUBTRACT : constant := 16#800A#;  --  ../include/SDL/SDL_opengl.h:172
   GL_FUNC_REVERSE_SUBTRACT : constant := 16#800B#;  --  ../include/SDL/SDL_opengl.h:173
   GL_CONVOLUTION_1D : constant := 16#8010#;  --  ../include/SDL/SDL_opengl.h:174
   GL_CONVOLUTION_2D : constant := 16#8011#;  --  ../include/SDL/SDL_opengl.h:175
   GL_SEPARABLE_2D : constant := 16#8012#;  --  ../include/SDL/SDL_opengl.h:176
   GL_CONVOLUTION_BORDER_MODE : constant := 16#8013#;  --  ../include/SDL/SDL_opengl.h:177
   GL_CONVOLUTION_FILTER_SCALE : constant := 16#8014#;  --  ../include/SDL/SDL_opengl.h:178
   GL_CONVOLUTION_FILTER_BIAS : constant := 16#8015#;  --  ../include/SDL/SDL_opengl.h:179
   GL_REDUCE : constant := 16#8016#;  --  ../include/SDL/SDL_opengl.h:180
   GL_CONVOLUTION_FORMAT : constant := 16#8017#;  --  ../include/SDL/SDL_opengl.h:181
   GL_CONVOLUTION_WIDTH : constant := 16#8018#;  --  ../include/SDL/SDL_opengl.h:182
   GL_CONVOLUTION_HEIGHT : constant := 16#8019#;  --  ../include/SDL/SDL_opengl.h:183
   GL_MAX_CONVOLUTION_WIDTH : constant := 16#801A#;  --  ../include/SDL/SDL_opengl.h:184
   GL_MAX_CONVOLUTION_HEIGHT : constant := 16#801B#;  --  ../include/SDL/SDL_opengl.h:185
   GL_POST_CONVOLUTION_RED_SCALE : constant := 16#801C#;  --  ../include/SDL/SDL_opengl.h:186
   GL_POST_CONVOLUTION_GREEN_SCALE : constant := 16#801D#;  --  ../include/SDL/SDL_opengl.h:187
   GL_POST_CONVOLUTION_BLUE_SCALE : constant := 16#801E#;  --  ../include/SDL/SDL_opengl.h:188
   GL_POST_CONVOLUTION_ALPHA_SCALE : constant := 16#801F#;  --  ../include/SDL/SDL_opengl.h:189
   GL_POST_CONVOLUTION_RED_BIAS : constant := 16#8020#;  --  ../include/SDL/SDL_opengl.h:190
   GL_POST_CONVOLUTION_GREEN_BIAS : constant := 16#8021#;  --  ../include/SDL/SDL_opengl.h:191
   GL_POST_CONVOLUTION_BLUE_BIAS : constant := 16#8022#;  --  ../include/SDL/SDL_opengl.h:192
   GL_POST_CONVOLUTION_ALPHA_BIAS : constant := 16#8023#;  --  ../include/SDL/SDL_opengl.h:193
   GL_HISTOGRAM : constant := 16#8024#;  --  ../include/SDL/SDL_opengl.h:194
   GL_PROXY_HISTOGRAM : constant := 16#8025#;  --  ../include/SDL/SDL_opengl.h:195
   GL_HISTOGRAM_WIDTH : constant := 16#8026#;  --  ../include/SDL/SDL_opengl.h:196
   GL_HISTOGRAM_FORMAT : constant := 16#8027#;  --  ../include/SDL/SDL_opengl.h:197
   GL_HISTOGRAM_RED_SIZE : constant := 16#8028#;  --  ../include/SDL/SDL_opengl.h:198
   GL_HISTOGRAM_GREEN_SIZE : constant := 16#8029#;  --  ../include/SDL/SDL_opengl.h:199
   GL_HISTOGRAM_BLUE_SIZE : constant := 16#802A#;  --  ../include/SDL/SDL_opengl.h:200
   GL_HISTOGRAM_ALPHA_SIZE : constant := 16#802B#;  --  ../include/SDL/SDL_opengl.h:201
   GL_HISTOGRAM_LUMINANCE_SIZE : constant := 16#802C#;  --  ../include/SDL/SDL_opengl.h:202
   GL_HISTOGRAM_SINK : constant := 16#802D#;  --  ../include/SDL/SDL_opengl.h:203
   GL_MINMAX : constant := 16#802E#;  --  ../include/SDL/SDL_opengl.h:204
   GL_MINMAX_FORMAT : constant := 16#802F#;  --  ../include/SDL/SDL_opengl.h:205
   GL_MINMAX_SINK : constant := 16#8030#;  --  ../include/SDL/SDL_opengl.h:206
   GL_TABLE_TOO_LARGE : constant := 16#8031#;  --  ../include/SDL/SDL_opengl.h:207
   GL_COLOR_MATRIX : constant := 16#80B1#;  --  ../include/SDL/SDL_opengl.h:208
   GL_COLOR_MATRIX_STACK_DEPTH : constant := 16#80B2#;  --  ../include/SDL/SDL_opengl.h:209
   GL_MAX_COLOR_MATRIX_STACK_DEPTH : constant := 16#80B3#;  --  ../include/SDL/SDL_opengl.h:210
   GL_POST_COLOR_MATRIX_RED_SCALE : constant := 16#80B4#;  --  ../include/SDL/SDL_opengl.h:211
   GL_POST_COLOR_MATRIX_GREEN_SCALE : constant := 16#80B5#;  --  ../include/SDL/SDL_opengl.h:212
   GL_POST_COLOR_MATRIX_BLUE_SCALE : constant := 16#80B6#;  --  ../include/SDL/SDL_opengl.h:213
   GL_POST_COLOR_MATRIX_ALPHA_SCALE : constant := 16#80B7#;  --  ../include/SDL/SDL_opengl.h:214
   GL_POST_COLOR_MATRIX_RED_BIAS : constant := 16#80B8#;  --  ../include/SDL/SDL_opengl.h:215
   GL_POST_COLOR_MATRIX_GREEN_BIAS : constant := 16#80B9#;  --  ../include/SDL/SDL_opengl.h:216
   GL_POST_COLOR_MATRIX_BLUE_BIAS : constant := 16#80BA#;  --  ../include/SDL/SDL_opengl.h:217
   GL_POST_COLOR_MATRIX_ALPHA_BIAS : constant := 16#80BB#;  --  ../include/SDL/SDL_opengl.h:218
   GL_COLOR_TABLE : constant := 16#80D0#;  --  ../include/SDL/SDL_opengl.h:219
   GL_POST_CONVOLUTION_COLOR_TABLE : constant := 16#80D1#;  --  ../include/SDL/SDL_opengl.h:220
   GL_POST_COLOR_MATRIX_COLOR_TABLE : constant := 16#80D2#;  --  ../include/SDL/SDL_opengl.h:221
   GL_PROXY_COLOR_TABLE : constant := 16#80D3#;  --  ../include/SDL/SDL_opengl.h:222
   GL_PROXY_POST_CONVOLUTION_COLOR_TABLE : constant := 16#80D4#;  --  ../include/SDL/SDL_opengl.h:223
   GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE : constant := 16#80D5#;  --  ../include/SDL/SDL_opengl.h:224
   GL_COLOR_TABLE_SCALE : constant := 16#80D6#;  --  ../include/SDL/SDL_opengl.h:225
   GL_COLOR_TABLE_BIAS : constant := 16#80D7#;  --  ../include/SDL/SDL_opengl.h:226
   GL_COLOR_TABLE_FORMAT : constant := 16#80D8#;  --  ../include/SDL/SDL_opengl.h:227
   GL_COLOR_TABLE_WIDTH : constant := 16#80D9#;  --  ../include/SDL/SDL_opengl.h:228
   GL_COLOR_TABLE_RED_SIZE : constant := 16#80DA#;  --  ../include/SDL/SDL_opengl.h:229
   GL_COLOR_TABLE_GREEN_SIZE : constant := 16#80DB#;  --  ../include/SDL/SDL_opengl.h:230
   GL_COLOR_TABLE_BLUE_SIZE : constant := 16#80DC#;  --  ../include/SDL/SDL_opengl.h:231
   GL_COLOR_TABLE_ALPHA_SIZE : constant := 16#80DD#;  --  ../include/SDL/SDL_opengl.h:232
   GL_COLOR_TABLE_LUMINANCE_SIZE : constant := 16#80DE#;  --  ../include/SDL/SDL_opengl.h:233
   GL_COLOR_TABLE_INTENSITY_SIZE : constant := 16#80DF#;  --  ../include/SDL/SDL_opengl.h:234
   GL_CONSTANT_BORDER : constant := 16#8151#;  --  ../include/SDL/SDL_opengl.h:235
   GL_REPLICATE_BORDER : constant := 16#8153#;  --  ../include/SDL/SDL_opengl.h:236
   GL_CONVOLUTION_BORDER_COLOR : constant := 16#8154#;  --  ../include/SDL/SDL_opengl.h:237

   GL_TEXTURE0 : constant := 16#84C0#;  --  ../include/SDL/SDL_opengl.h:241
   GL_TEXTURE1 : constant := 16#84C1#;  --  ../include/SDL/SDL_opengl.h:242
   GL_TEXTURE2 : constant := 16#84C2#;  --  ../include/SDL/SDL_opengl.h:243
   GL_TEXTURE3 : constant := 16#84C3#;  --  ../include/SDL/SDL_opengl.h:244
   GL_TEXTURE4 : constant := 16#84C4#;  --  ../include/SDL/SDL_opengl.h:245
   GL_TEXTURE5 : constant := 16#84C5#;  --  ../include/SDL/SDL_opengl.h:246
   GL_TEXTURE6 : constant := 16#84C6#;  --  ../include/SDL/SDL_opengl.h:247
   GL_TEXTURE7 : constant := 16#84C7#;  --  ../include/SDL/SDL_opengl.h:248
   GL_TEXTURE8 : constant := 16#84C8#;  --  ../include/SDL/SDL_opengl.h:249
   GL_TEXTURE9 : constant := 16#84C9#;  --  ../include/SDL/SDL_opengl.h:250
   GL_TEXTURE10 : constant := 16#84CA#;  --  ../include/SDL/SDL_opengl.h:251
   GL_TEXTURE11 : constant := 16#84CB#;  --  ../include/SDL/SDL_opengl.h:252
   GL_TEXTURE12 : constant := 16#84CC#;  --  ../include/SDL/SDL_opengl.h:253
   GL_TEXTURE13 : constant := 16#84CD#;  --  ../include/SDL/SDL_opengl.h:254
   GL_TEXTURE14 : constant := 16#84CE#;  --  ../include/SDL/SDL_opengl.h:255
   GL_TEXTURE15 : constant := 16#84CF#;  --  ../include/SDL/SDL_opengl.h:256
   GL_TEXTURE16 : constant := 16#84D0#;  --  ../include/SDL/SDL_opengl.h:257
   GL_TEXTURE17 : constant := 16#84D1#;  --  ../include/SDL/SDL_opengl.h:258
   GL_TEXTURE18 : constant := 16#84D2#;  --  ../include/SDL/SDL_opengl.h:259
   GL_TEXTURE19 : constant := 16#84D3#;  --  ../include/SDL/SDL_opengl.h:260
   GL_TEXTURE20 : constant := 16#84D4#;  --  ../include/SDL/SDL_opengl.h:261
   GL_TEXTURE21 : constant := 16#84D5#;  --  ../include/SDL/SDL_opengl.h:262
   GL_TEXTURE22 : constant := 16#84D6#;  --  ../include/SDL/SDL_opengl.h:263
   GL_TEXTURE23 : constant := 16#84D7#;  --  ../include/SDL/SDL_opengl.h:264
   GL_TEXTURE24 : constant := 16#84D8#;  --  ../include/SDL/SDL_opengl.h:265
   GL_TEXTURE25 : constant := 16#84D9#;  --  ../include/SDL/SDL_opengl.h:266
   GL_TEXTURE26 : constant := 16#84DA#;  --  ../include/SDL/SDL_opengl.h:267
   GL_TEXTURE27 : constant := 16#84DB#;  --  ../include/SDL/SDL_opengl.h:268
   GL_TEXTURE28 : constant := 16#84DC#;  --  ../include/SDL/SDL_opengl.h:269
   GL_TEXTURE29 : constant := 16#84DD#;  --  ../include/SDL/SDL_opengl.h:270
   GL_TEXTURE30 : constant := 16#84DE#;  --  ../include/SDL/SDL_opengl.h:271
   GL_TEXTURE31 : constant := 16#84DF#;  --  ../include/SDL/SDL_opengl.h:272
   GL_ACTIVE_TEXTURE : constant := 16#84E0#;  --  ../include/SDL/SDL_opengl.h:273
   GL_CLIENT_ACTIVE_TEXTURE : constant := 16#84E1#;  --  ../include/SDL/SDL_opengl.h:274
   GL_MAX_TEXTURE_UNITS : constant := 16#84E2#;  --  ../include/SDL/SDL_opengl.h:275
   GL_TRANSPOSE_MODELVIEW_MATRIX : constant := 16#84E3#;  --  ../include/SDL/SDL_opengl.h:276
   GL_TRANSPOSE_PROJECTION_MATRIX : constant := 16#84E4#;  --  ../include/SDL/SDL_opengl.h:277
   GL_TRANSPOSE_TEXTURE_MATRIX : constant := 16#84E5#;  --  ../include/SDL/SDL_opengl.h:278
   GL_TRANSPOSE_COLOR_MATRIX : constant := 16#84E6#;  --  ../include/SDL/SDL_opengl.h:279
   GL_MULTISAMPLE : constant := 16#809D#;  --  ../include/SDL/SDL_opengl.h:280
   GL_SAMPLE_ALPHA_TO_COVERAGE : constant := 16#809E#;  --  ../include/SDL/SDL_opengl.h:281
   GL_SAMPLE_ALPHA_TO_ONE : constant := 16#809F#;  --  ../include/SDL/SDL_opengl.h:282
   GL_SAMPLE_COVERAGE : constant := 16#80A0#;  --  ../include/SDL/SDL_opengl.h:283
   GL_SAMPLE_BUFFERS : constant := 16#80A8#;  --  ../include/SDL/SDL_opengl.h:284
   GL_SAMPLES : constant := 16#80A9#;  --  ../include/SDL/SDL_opengl.h:285
   GL_SAMPLE_COVERAGE_VALUE : constant := 16#80AA#;  --  ../include/SDL/SDL_opengl.h:286
   GL_SAMPLE_COVERAGE_INVERT : constant := 16#80AB#;  --  ../include/SDL/SDL_opengl.h:287
   GL_MULTISAMPLE_BIT : constant := 16#20000000#;  --  ../include/SDL/SDL_opengl.h:288
   GL_NORMAL_MAP : constant := 16#8511#;  --  ../include/SDL/SDL_opengl.h:289
   GL_REFLECTION_MAP : constant := 16#8512#;  --  ../include/SDL/SDL_opengl.h:290
   GL_TEXTURE_CUBE_MAP : constant := 16#8513#;  --  ../include/SDL/SDL_opengl.h:291
   GL_TEXTURE_BINDING_CUBE_MAP : constant := 16#8514#;  --  ../include/SDL/SDL_opengl.h:292
   GL_TEXTURE_CUBE_MAP_POSITIVE_X : constant := 16#8515#;  --  ../include/SDL/SDL_opengl.h:293
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X : constant := 16#8516#;  --  ../include/SDL/SDL_opengl.h:294
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y : constant := 16#8517#;  --  ../include/SDL/SDL_opengl.h:295
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y : constant := 16#8518#;  --  ../include/SDL/SDL_opengl.h:296
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z : constant := 16#8519#;  --  ../include/SDL/SDL_opengl.h:297
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z : constant := 16#851A#;  --  ../include/SDL/SDL_opengl.h:298
   GL_PROXY_TEXTURE_CUBE_MAP : constant := 16#851B#;  --  ../include/SDL/SDL_opengl.h:299
   GL_MAX_CUBE_MAP_TEXTURE_SIZE : constant := 16#851C#;  --  ../include/SDL/SDL_opengl.h:300
   GL_COMPRESSED_ALPHA : constant := 16#84E9#;  --  ../include/SDL/SDL_opengl.h:301
   GL_COMPRESSED_LUMINANCE : constant := 16#84EA#;  --  ../include/SDL/SDL_opengl.h:302
   GL_COMPRESSED_LUMINANCE_ALPHA : constant := 16#84EB#;  --  ../include/SDL/SDL_opengl.h:303
   GL_COMPRESSED_INTENSITY : constant := 16#84EC#;  --  ../include/SDL/SDL_opengl.h:304
   GL_COMPRESSED_RGB : constant := 16#84ED#;  --  ../include/SDL/SDL_opengl.h:305
   GL_COMPRESSED_RGBA : constant := 16#84EE#;  --  ../include/SDL/SDL_opengl.h:306
   GL_TEXTURE_COMPRESSION_HINT : constant := 16#84EF#;  --  ../include/SDL/SDL_opengl.h:307
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE : constant := 16#86A0#;  --  ../include/SDL/SDL_opengl.h:308
   GL_TEXTURE_COMPRESSED : constant := 16#86A1#;  --  ../include/SDL/SDL_opengl.h:309
   GL_NUM_COMPRESSED_TEXTURE_FORMATS : constant := 16#86A2#;  --  ../include/SDL/SDL_opengl.h:310
   GL_COMPRESSED_TEXTURE_FORMATS : constant := 16#86A3#;  --  ../include/SDL/SDL_opengl.h:311
   GL_CLAMP_TO_BORDER : constant := 16#812D#;  --  ../include/SDL/SDL_opengl.h:312
   GL_COMBINE : constant := 16#8570#;  --  ../include/SDL/SDL_opengl.h:313
   GL_COMBINE_RGB : constant := 16#8571#;  --  ../include/SDL/SDL_opengl.h:314
   GL_COMBINE_ALPHA : constant := 16#8572#;  --  ../include/SDL/SDL_opengl.h:315
   GL_SOURCE0_RGB : constant := 16#8580#;  --  ../include/SDL/SDL_opengl.h:316
   GL_SOURCE1_RGB : constant := 16#8581#;  --  ../include/SDL/SDL_opengl.h:317
   GL_SOURCE2_RGB : constant := 16#8582#;  --  ../include/SDL/SDL_opengl.h:318
   GL_SOURCE0_ALPHA : constant := 16#8588#;  --  ../include/SDL/SDL_opengl.h:319
   GL_SOURCE1_ALPHA : constant := 16#8589#;  --  ../include/SDL/SDL_opengl.h:320
   GL_SOURCE2_ALPHA : constant := 16#858A#;  --  ../include/SDL/SDL_opengl.h:321
   GL_OPERAND0_RGB : constant := 16#8590#;  --  ../include/SDL/SDL_opengl.h:322
   GL_OPERAND1_RGB : constant := 16#8591#;  --  ../include/SDL/SDL_opengl.h:323
   GL_OPERAND2_RGB : constant := 16#8592#;  --  ../include/SDL/SDL_opengl.h:324
   GL_OPERAND0_ALPHA : constant := 16#8598#;  --  ../include/SDL/SDL_opengl.h:325
   GL_OPERAND1_ALPHA : constant := 16#8599#;  --  ../include/SDL/SDL_opengl.h:326
   GL_OPERAND2_ALPHA : constant := 16#859A#;  --  ../include/SDL/SDL_opengl.h:327
   GL_RGB_SCALE : constant := 16#8573#;  --  ../include/SDL/SDL_opengl.h:328
   GL_ADD_SIGNED : constant := 16#8574#;  --  ../include/SDL/SDL_opengl.h:329
   GL_INTERPOLATE : constant := 16#8575#;  --  ../include/SDL/SDL_opengl.h:330
   GL_SUBTRACT : constant := 16#84E7#;  --  ../include/SDL/SDL_opengl.h:331
   GL_CONSTANT : constant := 16#8576#;  --  ../include/SDL/SDL_opengl.h:332
   GL_PRIMARY_COLOR : constant := 16#8577#;  --  ../include/SDL/SDL_opengl.h:333
   GL_PREVIOUS : constant := 16#8578#;  --  ../include/SDL/SDL_opengl.h:334
   GL_DOT3_RGB : constant := 16#86AE#;  --  ../include/SDL/SDL_opengl.h:335
   GL_DOT3_RGBA : constant := 16#86AF#;  --  ../include/SDL/SDL_opengl.h:336

   GL_BLEND_DST_RGB : constant := 16#80C8#;  --  ../include/SDL/SDL_opengl.h:340
   GL_BLEND_SRC_RGB : constant := 16#80C9#;  --  ../include/SDL/SDL_opengl.h:341
   GL_BLEND_DST_ALPHA : constant := 16#80CA#;  --  ../include/SDL/SDL_opengl.h:342
   GL_BLEND_SRC_ALPHA : constant := 16#80CB#;  --  ../include/SDL/SDL_opengl.h:343
   GL_POINT_SIZE_MIN : constant := 16#8126#;  --  ../include/SDL/SDL_opengl.h:344
   GL_POINT_SIZE_MAX : constant := 16#8127#;  --  ../include/SDL/SDL_opengl.h:345
   GL_POINT_FADE_THRESHOLD_SIZE : constant := 16#8128#;  --  ../include/SDL/SDL_opengl.h:346
   GL_POINT_DISTANCE_ATTENUATION : constant := 16#8129#;  --  ../include/SDL/SDL_opengl.h:347
   GL_GENERATE_MIPMAP : constant := 16#8191#;  --  ../include/SDL/SDL_opengl.h:348
   GL_GENERATE_MIPMAP_HINT : constant := 16#8192#;  --  ../include/SDL/SDL_opengl.h:349
   GL_DEPTH_COMPONENT16 : constant := 16#81A5#;  --  ../include/SDL/SDL_opengl.h:350
   GL_DEPTH_COMPONENT24 : constant := 16#81A6#;  --  ../include/SDL/SDL_opengl.h:351
   GL_DEPTH_COMPONENT32 : constant := 16#81A7#;  --  ../include/SDL/SDL_opengl.h:352
   GL_MIRRORED_REPEAT : constant := 16#8370#;  --  ../include/SDL/SDL_opengl.h:353
   GL_FOG_COORDINATE_SOURCE : constant := 16#8450#;  --  ../include/SDL/SDL_opengl.h:354
   GL_FOG_COORDINATE : constant := 16#8451#;  --  ../include/SDL/SDL_opengl.h:355
   GL_FRAGMENT_DEPTH : constant := 16#8452#;  --  ../include/SDL/SDL_opengl.h:356
   GL_CURRENT_FOG_COORDINATE : constant := 16#8453#;  --  ../include/SDL/SDL_opengl.h:357
   GL_FOG_COORDINATE_ARRAY_TYPE : constant := 16#8454#;  --  ../include/SDL/SDL_opengl.h:358
   GL_FOG_COORDINATE_ARRAY_STRIDE : constant := 16#8455#;  --  ../include/SDL/SDL_opengl.h:359
   GL_FOG_COORDINATE_ARRAY_POINTER : constant := 16#8456#;  --  ../include/SDL/SDL_opengl.h:360
   GL_FOG_COORDINATE_ARRAY : constant := 16#8457#;  --  ../include/SDL/SDL_opengl.h:361
   GL_COLOR_SUM : constant := 16#8458#;  --  ../include/SDL/SDL_opengl.h:362
   GL_CURRENT_SECONDARY_COLOR : constant := 16#8459#;  --  ../include/SDL/SDL_opengl.h:363
   GL_SECONDARY_COLOR_ARRAY_SIZE : constant := 16#845A#;  --  ../include/SDL/SDL_opengl.h:364
   GL_SECONDARY_COLOR_ARRAY_TYPE : constant := 16#845B#;  --  ../include/SDL/SDL_opengl.h:365
   GL_SECONDARY_COLOR_ARRAY_STRIDE : constant := 16#845C#;  --  ../include/SDL/SDL_opengl.h:366
   GL_SECONDARY_COLOR_ARRAY_POINTER : constant := 16#845D#;  --  ../include/SDL/SDL_opengl.h:367
   GL_SECONDARY_COLOR_ARRAY : constant := 16#845E#;  --  ../include/SDL/SDL_opengl.h:368
   GL_MAX_TEXTURE_LOD_BIAS : constant := 16#84FD#;  --  ../include/SDL/SDL_opengl.h:369
   GL_TEXTURE_FILTER_CONTROL : constant := 16#8500#;  --  ../include/SDL/SDL_opengl.h:370
   GL_TEXTURE_LOD_BIAS : constant := 16#8501#;  --  ../include/SDL/SDL_opengl.h:371
   GL_INCR_WRAP : constant := 16#8507#;  --  ../include/SDL/SDL_opengl.h:372
   GL_DECR_WRAP : constant := 16#8508#;  --  ../include/SDL/SDL_opengl.h:373
   GL_TEXTURE_DEPTH_SIZE : constant := 16#884A#;  --  ../include/SDL/SDL_opengl.h:374
   GL_DEPTH_TEXTURE_MODE : constant := 16#884B#;  --  ../include/SDL/SDL_opengl.h:375
   GL_TEXTURE_COMPARE_MODE : constant := 16#884C#;  --  ../include/SDL/SDL_opengl.h:376
   GL_TEXTURE_COMPARE_FUNC : constant := 16#884D#;  --  ../include/SDL/SDL_opengl.h:377
   GL_COMPARE_R_TO_TEXTURE : constant := 16#884E#;  --  ../include/SDL/SDL_opengl.h:378

   GL_BUFFER_SIZE : constant := 16#8764#;  --  ../include/SDL/SDL_opengl.h:382
   GL_BUFFER_USAGE : constant := 16#8765#;  --  ../include/SDL/SDL_opengl.h:383
   GL_QUERY_COUNTER_BITS : constant := 16#8864#;  --  ../include/SDL/SDL_opengl.h:384
   GL_CURRENT_QUERY : constant := 16#8865#;  --  ../include/SDL/SDL_opengl.h:385
   GL_QUERY_RESULT : constant := 16#8866#;  --  ../include/SDL/SDL_opengl.h:386
   GL_QUERY_RESULT_AVAILABLE : constant := 16#8867#;  --  ../include/SDL/SDL_opengl.h:387
   GL_ARRAY_BUFFER : constant := 16#8892#;  --  ../include/SDL/SDL_opengl.h:388
   GL_ELEMENT_ARRAY_BUFFER : constant := 16#8893#;  --  ../include/SDL/SDL_opengl.h:389
   GL_ARRAY_BUFFER_BINDING : constant := 16#8894#;  --  ../include/SDL/SDL_opengl.h:390
   GL_ELEMENT_ARRAY_BUFFER_BINDING : constant := 16#8895#;  --  ../include/SDL/SDL_opengl.h:391
   GL_VERTEX_ARRAY_BUFFER_BINDING : constant := 16#8896#;  --  ../include/SDL/SDL_opengl.h:392
   GL_NORMAL_ARRAY_BUFFER_BINDING : constant := 16#8897#;  --  ../include/SDL/SDL_opengl.h:393
   GL_COLOR_ARRAY_BUFFER_BINDING : constant := 16#8898#;  --  ../include/SDL/SDL_opengl.h:394
   GL_INDEX_ARRAY_BUFFER_BINDING : constant := 16#8899#;  --  ../include/SDL/SDL_opengl.h:395
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING : constant := 16#889A#;  --  ../include/SDL/SDL_opengl.h:396
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING : constant := 16#889B#;  --  ../include/SDL/SDL_opengl.h:397
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING : constant := 16#889C#;  --  ../include/SDL/SDL_opengl.h:398
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING : constant := 16#889D#;  --  ../include/SDL/SDL_opengl.h:399
   GL_WEIGHT_ARRAY_BUFFER_BINDING : constant := 16#889E#;  --  ../include/SDL/SDL_opengl.h:400
   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING : constant := 16#889F#;  --  ../include/SDL/SDL_opengl.h:401
   GL_READ_ONLY : constant := 16#88B8#;  --  ../include/SDL/SDL_opengl.h:402
   GL_WRITE_ONLY : constant := 16#88B9#;  --  ../include/SDL/SDL_opengl.h:403
   GL_READ_WRITE : constant := 16#88BA#;  --  ../include/SDL/SDL_opengl.h:404
   GL_BUFFER_ACCESS : constant := 16#88BB#;  --  ../include/SDL/SDL_opengl.h:405
   GL_BUFFER_MAPPED : constant := 16#88BC#;  --  ../include/SDL/SDL_opengl.h:406
   GL_BUFFER_MAP_POINTER : constant := 16#88BD#;  --  ../include/SDL/SDL_opengl.h:407
   GL_STREAM_DRAW : constant := 16#88E0#;  --  ../include/SDL/SDL_opengl.h:408
   GL_STREAM_READ : constant := 16#88E1#;  --  ../include/SDL/SDL_opengl.h:409
   GL_STREAM_COPY : constant := 16#88E2#;  --  ../include/SDL/SDL_opengl.h:410
   GL_STATIC_DRAW : constant := 16#88E4#;  --  ../include/SDL/SDL_opengl.h:411
   GL_STATIC_READ : constant := 16#88E5#;  --  ../include/SDL/SDL_opengl.h:412
   GL_STATIC_COPY : constant := 16#88E6#;  --  ../include/SDL/SDL_opengl.h:413
   GL_DYNAMIC_DRAW : constant := 16#88E8#;  --  ../include/SDL/SDL_opengl.h:414
   GL_DYNAMIC_READ : constant := 16#88E9#;  --  ../include/SDL/SDL_opengl.h:415
   GL_DYNAMIC_COPY : constant := 16#88EA#;  --  ../include/SDL/SDL_opengl.h:416
   GL_SAMPLES_PASSED : constant := 16#8914#;  --  ../include/SDL/SDL_opengl.h:417
   --  unsupported macro: GL_FOG_COORD_SRC GL_FOG_COORDINATE_SOURCE
   --  unsupported macro: GL_FOG_COORD GL_FOG_COORDINATE
   --  unsupported macro: GL_CURRENT_FOG_COORD GL_CURRENT_FOG_COORDINATE
   --  unsupported macro: GL_FOG_COORD_ARRAY_TYPE GL_FOG_COORDINATE_ARRAY_TYPE
   --  unsupported macro: GL_FOG_COORD_ARRAY_STRIDE GL_FOG_COORDINATE_ARRAY_STRIDE
   --  unsupported macro: GL_FOG_COORD_ARRAY_POINTER GL_FOG_COORDINATE_ARRAY_POINTER
   --  unsupported macro: GL_FOG_COORD_ARRAY GL_FOG_COORDINATE_ARRAY
   --  unsupported macro: GL_FOG_COORD_ARRAY_BUFFER_BINDING GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING
   --  unsupported macro: GL_SRC0_RGB GL_SOURCE0_RGB
   --  unsupported macro: GL_SRC1_RGB GL_SOURCE1_RGB
   --  unsupported macro: GL_SRC2_RGB GL_SOURCE2_RGB
   --  unsupported macro: GL_SRC0_ALPHA GL_SOURCE0_ALPHA
   --  unsupported macro: GL_SRC1_ALPHA GL_SOURCE1_ALPHA
   --  unsupported macro: GL_SRC2_ALPHA GL_SOURCE2_ALPHA
   --  unsupported macro: GL_BLEND_EQUATION_RGB GL_BLEND_EQUATION

   GL_VERTEX_ATTRIB_ARRAY_ENABLED : constant := 16#8622#;  --  ../include/SDL/SDL_opengl.h:436
   GL_VERTEX_ATTRIB_ARRAY_SIZE : constant := 16#8623#;  --  ../include/SDL/SDL_opengl.h:437
   GL_VERTEX_ATTRIB_ARRAY_STRIDE : constant := 16#8624#;  --  ../include/SDL/SDL_opengl.h:438
   GL_VERTEX_ATTRIB_ARRAY_TYPE : constant := 16#8625#;  --  ../include/SDL/SDL_opengl.h:439
   GL_CURRENT_VERTEX_ATTRIB : constant := 16#8626#;  --  ../include/SDL/SDL_opengl.h:440
   GL_VERTEX_PROGRAM_POINT_SIZE : constant := 16#8642#;  --  ../include/SDL/SDL_opengl.h:441
   GL_VERTEX_PROGRAM_TWO_SIDE : constant := 16#8643#;  --  ../include/SDL/SDL_opengl.h:442
   GL_VERTEX_ATTRIB_ARRAY_POINTER : constant := 16#8645#;  --  ../include/SDL/SDL_opengl.h:443
   GL_STENCIL_BACK_FUNC : constant := 16#8800#;  --  ../include/SDL/SDL_opengl.h:444
   GL_STENCIL_BACK_FAIL : constant := 16#8801#;  --  ../include/SDL/SDL_opengl.h:445
   GL_STENCIL_BACK_PASS_DEPTH_FAIL : constant := 16#8802#;  --  ../include/SDL/SDL_opengl.h:446
   GL_STENCIL_BACK_PASS_DEPTH_PASS : constant := 16#8803#;  --  ../include/SDL/SDL_opengl.h:447
   GL_MAX_DRAW_BUFFERS : constant := 16#8824#;  --  ../include/SDL/SDL_opengl.h:448
   GL_DRAW_BUFFER0 : constant := 16#8825#;  --  ../include/SDL/SDL_opengl.h:449
   GL_DRAW_BUFFER1 : constant := 16#8826#;  --  ../include/SDL/SDL_opengl.h:450
   GL_DRAW_BUFFER2 : constant := 16#8827#;  --  ../include/SDL/SDL_opengl.h:451
   GL_DRAW_BUFFER3 : constant := 16#8828#;  --  ../include/SDL/SDL_opengl.h:452
   GL_DRAW_BUFFER4 : constant := 16#8829#;  --  ../include/SDL/SDL_opengl.h:453
   GL_DRAW_BUFFER5 : constant := 16#882A#;  --  ../include/SDL/SDL_opengl.h:454
   GL_DRAW_BUFFER6 : constant := 16#882B#;  --  ../include/SDL/SDL_opengl.h:455
   GL_DRAW_BUFFER7 : constant := 16#882C#;  --  ../include/SDL/SDL_opengl.h:456
   GL_DRAW_BUFFER8 : constant := 16#882D#;  --  ../include/SDL/SDL_opengl.h:457
   GL_DRAW_BUFFER9 : constant := 16#882E#;  --  ../include/SDL/SDL_opengl.h:458
   GL_DRAW_BUFFER10 : constant := 16#882F#;  --  ../include/SDL/SDL_opengl.h:459
   GL_DRAW_BUFFER11 : constant := 16#8830#;  --  ../include/SDL/SDL_opengl.h:460
   GL_DRAW_BUFFER12 : constant := 16#8831#;  --  ../include/SDL/SDL_opengl.h:461
   GL_DRAW_BUFFER13 : constant := 16#8832#;  --  ../include/SDL/SDL_opengl.h:462
   GL_DRAW_BUFFER14 : constant := 16#8833#;  --  ../include/SDL/SDL_opengl.h:463
   GL_DRAW_BUFFER15 : constant := 16#8834#;  --  ../include/SDL/SDL_opengl.h:464
   GL_BLEND_EQUATION_ALPHA : constant := 16#883D#;  --  ../include/SDL/SDL_opengl.h:465
   GL_POINT_SPRITE : constant := 16#8861#;  --  ../include/SDL/SDL_opengl.h:466
   GL_COORD_REPLACE : constant := 16#8862#;  --  ../include/SDL/SDL_opengl.h:467
   GL_MAX_VERTEX_ATTRIBS : constant := 16#8869#;  --  ../include/SDL/SDL_opengl.h:468
   GL_VERTEX_ATTRIB_ARRAY_NORMALIZED : constant := 16#886A#;  --  ../include/SDL/SDL_opengl.h:469
   GL_MAX_TEXTURE_COORDS : constant := 16#8871#;  --  ../include/SDL/SDL_opengl.h:470
   GL_MAX_TEXTURE_IMAGE_UNITS : constant := 16#8872#;  --  ../include/SDL/SDL_opengl.h:471
   GL_FRAGMENT_SHADER : constant := 16#8B30#;  --  ../include/SDL/SDL_opengl.h:472
   GL_VERTEX_SHADER : constant := 16#8B31#;  --  ../include/SDL/SDL_opengl.h:473
   GL_MAX_FRAGMENT_UNIFORM_COMPONENTS : constant := 16#8B49#;  --  ../include/SDL/SDL_opengl.h:474
   GL_MAX_VERTEX_UNIFORM_COMPONENTS : constant := 16#8B4A#;  --  ../include/SDL/SDL_opengl.h:475
   GL_MAX_VARYING_FLOATS : constant := 16#8B4B#;  --  ../include/SDL/SDL_opengl.h:476
   GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS : constant := 16#8B4C#;  --  ../include/SDL/SDL_opengl.h:477
   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS : constant := 16#8B4D#;  --  ../include/SDL/SDL_opengl.h:478
   GL_SHADER_TYPE : constant := 16#8B4F#;  --  ../include/SDL/SDL_opengl.h:479
   GL_FLOAT_VEC2 : constant := 16#8B50#;  --  ../include/SDL/SDL_opengl.h:480
   GL_FLOAT_VEC3 : constant := 16#8B51#;  --  ../include/SDL/SDL_opengl.h:481
   GL_FLOAT_VEC4 : constant := 16#8B52#;  --  ../include/SDL/SDL_opengl.h:482
   GL_INT_VEC2 : constant := 16#8B53#;  --  ../include/SDL/SDL_opengl.h:483
   GL_INT_VEC3 : constant := 16#8B54#;  --  ../include/SDL/SDL_opengl.h:484
   GL_INT_VEC4 : constant := 16#8B55#;  --  ../include/SDL/SDL_opengl.h:485
   GL_BOOL : constant := 16#8B56#;  --  ../include/SDL/SDL_opengl.h:486
   GL_BOOL_VEC2 : constant := 16#8B57#;  --  ../include/SDL/SDL_opengl.h:487
   GL_BOOL_VEC3 : constant := 16#8B58#;  --  ../include/SDL/SDL_opengl.h:488
   GL_BOOL_VEC4 : constant := 16#8B59#;  --  ../include/SDL/SDL_opengl.h:489
   GL_FLOAT_MAT2 : constant := 16#8B5A#;  --  ../include/SDL/SDL_opengl.h:490
   GL_FLOAT_MAT3 : constant := 16#8B5B#;  --  ../include/SDL/SDL_opengl.h:491
   GL_FLOAT_MAT4 : constant := 16#8B5C#;  --  ../include/SDL/SDL_opengl.h:492
   GL_SAMPLER_1D : constant := 16#8B5D#;  --  ../include/SDL/SDL_opengl.h:493
   GL_SAMPLER_2D : constant := 16#8B5E#;  --  ../include/SDL/SDL_opengl.h:494
   GL_SAMPLER_3D : constant := 16#8B5F#;  --  ../include/SDL/SDL_opengl.h:495
   GL_SAMPLER_CUBE : constant := 16#8B60#;  --  ../include/SDL/SDL_opengl.h:496
   GL_SAMPLER_1D_SHADOW : constant := 16#8B61#;  --  ../include/SDL/SDL_opengl.h:497
   GL_SAMPLER_2D_SHADOW : constant := 16#8B62#;  --  ../include/SDL/SDL_opengl.h:498
   GL_DELETE_STATUS : constant := 16#8B80#;  --  ../include/SDL/SDL_opengl.h:499
   GL_COMPILE_STATUS : constant := 16#8B81#;  --  ../include/SDL/SDL_opengl.h:500
   GL_LINK_STATUS : constant := 16#8B82#;  --  ../include/SDL/SDL_opengl.h:501
   GL_VALIDATE_STATUS : constant := 16#8B83#;  --  ../include/SDL/SDL_opengl.h:502
   GL_INFO_LOG_LENGTH : constant := 16#8B84#;  --  ../include/SDL/SDL_opengl.h:503
   GL_ATTACHED_SHADERS : constant := 16#8B85#;  --  ../include/SDL/SDL_opengl.h:504
   GL_ACTIVE_UNIFORMS : constant := 16#8B86#;  --  ../include/SDL/SDL_opengl.h:505
   GL_ACTIVE_UNIFORM_MAX_LENGTH : constant := 16#8B87#;  --  ../include/SDL/SDL_opengl.h:506
   GL_SHADER_SOURCE_LENGTH : constant := 16#8B88#;  --  ../include/SDL/SDL_opengl.h:507
   GL_ACTIVE_ATTRIBUTES : constant := 16#8B89#;  --  ../include/SDL/SDL_opengl.h:508
   GL_ACTIVE_ATTRIBUTE_MAX_LENGTH : constant := 16#8B8A#;  --  ../include/SDL/SDL_opengl.h:509
   GL_FRAGMENT_SHADER_DERIVATIVE_HINT : constant := 16#8B8B#;  --  ../include/SDL/SDL_opengl.h:510
   GL_SHADING_LANGUAGE_VERSION : constant := 16#8B8C#;  --  ../include/SDL/SDL_opengl.h:511
   GL_CURRENT_PROGRAM : constant := 16#8B8D#;  --  ../include/SDL/SDL_opengl.h:512
   GL_POINT_SPRITE_COORD_ORIGIN : constant := 16#8CA0#;  --  ../include/SDL/SDL_opengl.h:513
   GL_LOWER_LEFT : constant := 16#8CA1#;  --  ../include/SDL/SDL_opengl.h:514
   GL_UPPER_LEFT : constant := 16#8CA2#;  --  ../include/SDL/SDL_opengl.h:515
   GL_STENCIL_BACK_REF : constant := 16#8CA3#;  --  ../include/SDL/SDL_opengl.h:516
   GL_STENCIL_BACK_VALUE_MASK : constant := 16#8CA4#;  --  ../include/SDL/SDL_opengl.h:517
   GL_STENCIL_BACK_WRITEMASK : constant := 16#8CA5#;  --  ../include/SDL/SDL_opengl.h:518

   GL_TEXTURE0_ARB : constant := 16#84C0#;  --  ../include/SDL/SDL_opengl.h:522
   GL_TEXTURE1_ARB : constant := 16#84C1#;  --  ../include/SDL/SDL_opengl.h:523
   GL_TEXTURE2_ARB : constant := 16#84C2#;  --  ../include/SDL/SDL_opengl.h:524
   GL_TEXTURE3_ARB : constant := 16#84C3#;  --  ../include/SDL/SDL_opengl.h:525
   GL_TEXTURE4_ARB : constant := 16#84C4#;  --  ../include/SDL/SDL_opengl.h:526
   GL_TEXTURE5_ARB : constant := 16#84C5#;  --  ../include/SDL/SDL_opengl.h:527
   GL_TEXTURE6_ARB : constant := 16#84C6#;  --  ../include/SDL/SDL_opengl.h:528
   GL_TEXTURE7_ARB : constant := 16#84C7#;  --  ../include/SDL/SDL_opengl.h:529
   GL_TEXTURE8_ARB : constant := 16#84C8#;  --  ../include/SDL/SDL_opengl.h:530
   GL_TEXTURE9_ARB : constant := 16#84C9#;  --  ../include/SDL/SDL_opengl.h:531
   GL_TEXTURE10_ARB : constant := 16#84CA#;  --  ../include/SDL/SDL_opengl.h:532
   GL_TEXTURE11_ARB : constant := 16#84CB#;  --  ../include/SDL/SDL_opengl.h:533
   GL_TEXTURE12_ARB : constant := 16#84CC#;  --  ../include/SDL/SDL_opengl.h:534
   GL_TEXTURE13_ARB : constant := 16#84CD#;  --  ../include/SDL/SDL_opengl.h:535
   GL_TEXTURE14_ARB : constant := 16#84CE#;  --  ../include/SDL/SDL_opengl.h:536
   GL_TEXTURE15_ARB : constant := 16#84CF#;  --  ../include/SDL/SDL_opengl.h:537
   GL_TEXTURE16_ARB : constant := 16#84D0#;  --  ../include/SDL/SDL_opengl.h:538
   GL_TEXTURE17_ARB : constant := 16#84D1#;  --  ../include/SDL/SDL_opengl.h:539
   GL_TEXTURE18_ARB : constant := 16#84D2#;  --  ../include/SDL/SDL_opengl.h:540
   GL_TEXTURE19_ARB : constant := 16#84D3#;  --  ../include/SDL/SDL_opengl.h:541
   GL_TEXTURE20_ARB : constant := 16#84D4#;  --  ../include/SDL/SDL_opengl.h:542
   GL_TEXTURE21_ARB : constant := 16#84D5#;  --  ../include/SDL/SDL_opengl.h:543
   GL_TEXTURE22_ARB : constant := 16#84D6#;  --  ../include/SDL/SDL_opengl.h:544
   GL_TEXTURE23_ARB : constant := 16#84D7#;  --  ../include/SDL/SDL_opengl.h:545
   GL_TEXTURE24_ARB : constant := 16#84D8#;  --  ../include/SDL/SDL_opengl.h:546
   GL_TEXTURE25_ARB : constant := 16#84D9#;  --  ../include/SDL/SDL_opengl.h:547
   GL_TEXTURE26_ARB : constant := 16#84DA#;  --  ../include/SDL/SDL_opengl.h:548
   GL_TEXTURE27_ARB : constant := 16#84DB#;  --  ../include/SDL/SDL_opengl.h:549
   GL_TEXTURE28_ARB : constant := 16#84DC#;  --  ../include/SDL/SDL_opengl.h:550
   GL_TEXTURE29_ARB : constant := 16#84DD#;  --  ../include/SDL/SDL_opengl.h:551
   GL_TEXTURE30_ARB : constant := 16#84DE#;  --  ../include/SDL/SDL_opengl.h:552
   GL_TEXTURE31_ARB : constant := 16#84DF#;  --  ../include/SDL/SDL_opengl.h:553
   GL_ACTIVE_TEXTURE_ARB : constant := 16#84E0#;  --  ../include/SDL/SDL_opengl.h:554
   GL_CLIENT_ACTIVE_TEXTURE_ARB : constant := 16#84E1#;  --  ../include/SDL/SDL_opengl.h:555
   GL_MAX_TEXTURE_UNITS_ARB : constant := 16#84E2#;  --  ../include/SDL/SDL_opengl.h:556

   GL_TRANSPOSE_MODELVIEW_MATRIX_ARB : constant := 16#84E3#;  --  ../include/SDL/SDL_opengl.h:560
   GL_TRANSPOSE_PROJECTION_MATRIX_ARB : constant := 16#84E4#;  --  ../include/SDL/SDL_opengl.h:561
   GL_TRANSPOSE_TEXTURE_MATRIX_ARB : constant := 16#84E5#;  --  ../include/SDL/SDL_opengl.h:562
   GL_TRANSPOSE_COLOR_MATRIX_ARB : constant := 16#84E6#;  --  ../include/SDL/SDL_opengl.h:563

   GL_MULTISAMPLE_ARB : constant := 16#809D#;  --  ../include/SDL/SDL_opengl.h:567
   GL_SAMPLE_ALPHA_TO_COVERAGE_ARB : constant := 16#809E#;  --  ../include/SDL/SDL_opengl.h:568
   GL_SAMPLE_ALPHA_TO_ONE_ARB : constant := 16#809F#;  --  ../include/SDL/SDL_opengl.h:569
   GL_SAMPLE_COVERAGE_ARB : constant := 16#80A0#;  --  ../include/SDL/SDL_opengl.h:570
   GL_SAMPLE_BUFFERS_ARB : constant := 16#80A8#;  --  ../include/SDL/SDL_opengl.h:571
   GL_SAMPLES_ARB : constant := 16#80A9#;  --  ../include/SDL/SDL_opengl.h:572
   GL_SAMPLE_COVERAGE_VALUE_ARB : constant := 16#80AA#;  --  ../include/SDL/SDL_opengl.h:573
   GL_SAMPLE_COVERAGE_INVERT_ARB : constant := 16#80AB#;  --  ../include/SDL/SDL_opengl.h:574
   GL_MULTISAMPLE_BIT_ARB : constant := 16#20000000#;  --  ../include/SDL/SDL_opengl.h:575

   GL_NORMAL_MAP_ARB : constant := 16#8511#;  --  ../include/SDL/SDL_opengl.h:582
   GL_REFLECTION_MAP_ARB : constant := 16#8512#;  --  ../include/SDL/SDL_opengl.h:583
   GL_TEXTURE_CUBE_MAP_ARB : constant := 16#8513#;  --  ../include/SDL/SDL_opengl.h:584
   GL_TEXTURE_BINDING_CUBE_MAP_ARB : constant := 16#8514#;  --  ../include/SDL/SDL_opengl.h:585
   GL_TEXTURE_CUBE_MAP_POSITIVE_X_ARB : constant := 16#8515#;  --  ../include/SDL/SDL_opengl.h:586
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X_ARB : constant := 16#8516#;  --  ../include/SDL/SDL_opengl.h:587
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y_ARB : constant := 16#8517#;  --  ../include/SDL/SDL_opengl.h:588
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_ARB : constant := 16#8518#;  --  ../include/SDL/SDL_opengl.h:589
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z_ARB : constant := 16#8519#;  --  ../include/SDL/SDL_opengl.h:590
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_ARB : constant := 16#851A#;  --  ../include/SDL/SDL_opengl.h:591
   GL_PROXY_TEXTURE_CUBE_MAP_ARB : constant := 16#851B#;  --  ../include/SDL/SDL_opengl.h:592
   GL_MAX_CUBE_MAP_TEXTURE_SIZE_ARB : constant := 16#851C#;  --  ../include/SDL/SDL_opengl.h:593

   GL_COMPRESSED_ALPHA_ARB : constant := 16#84E9#;  --  ../include/SDL/SDL_opengl.h:597
   GL_COMPRESSED_LUMINANCE_ARB : constant := 16#84EA#;  --  ../include/SDL/SDL_opengl.h:598
   GL_COMPRESSED_LUMINANCE_ALPHA_ARB : constant := 16#84EB#;  --  ../include/SDL/SDL_opengl.h:599
   GL_COMPRESSED_INTENSITY_ARB : constant := 16#84EC#;  --  ../include/SDL/SDL_opengl.h:600
   GL_COMPRESSED_RGB_ARB : constant := 16#84ED#;  --  ../include/SDL/SDL_opengl.h:601
   GL_COMPRESSED_RGBA_ARB : constant := 16#84EE#;  --  ../include/SDL/SDL_opengl.h:602
   GL_TEXTURE_COMPRESSION_HINT_ARB : constant := 16#84EF#;  --  ../include/SDL/SDL_opengl.h:603
   GL_TEXTURE_COMPRESSED_IMAGE_SIZE_ARB : constant := 16#86A0#;  --  ../include/SDL/SDL_opengl.h:604
   GL_TEXTURE_COMPRESSED_ARB : constant := 16#86A1#;  --  ../include/SDL/SDL_opengl.h:605
   GL_NUM_COMPRESSED_TEXTURE_FORMATS_ARB : constant := 16#86A2#;  --  ../include/SDL/SDL_opengl.h:606
   GL_COMPRESSED_TEXTURE_FORMATS_ARB : constant := 16#86A3#;  --  ../include/SDL/SDL_opengl.h:607

   GL_CLAMP_TO_BORDER_ARB : constant := 16#812D#;  --  ../include/SDL/SDL_opengl.h:611

   GL_POINT_SIZE_MIN_ARB : constant := 16#8126#;  --  ../include/SDL/SDL_opengl.h:615
   GL_POINT_SIZE_MAX_ARB : constant := 16#8127#;  --  ../include/SDL/SDL_opengl.h:616
   GL_POINT_FADE_THRESHOLD_SIZE_ARB : constant := 16#8128#;  --  ../include/SDL/SDL_opengl.h:617
   GL_POINT_DISTANCE_ATTENUATION_ARB : constant := 16#8129#;  --  ../include/SDL/SDL_opengl.h:618

   GL_MAX_VERTEX_UNITS_ARB : constant := 16#86A4#;  --  ../include/SDL/SDL_opengl.h:622
   GL_ACTIVE_VERTEX_UNITS_ARB : constant := 16#86A5#;  --  ../include/SDL/SDL_opengl.h:623
   GL_WEIGHT_SUM_UNITY_ARB : constant := 16#86A6#;  --  ../include/SDL/SDL_opengl.h:624
   GL_VERTEX_BLEND_ARB : constant := 16#86A7#;  --  ../include/SDL/SDL_opengl.h:625
   GL_CURRENT_WEIGHT_ARB : constant := 16#86A8#;  --  ../include/SDL/SDL_opengl.h:626
   GL_WEIGHT_ARRAY_TYPE_ARB : constant := 16#86A9#;  --  ../include/SDL/SDL_opengl.h:627
   GL_WEIGHT_ARRAY_STRIDE_ARB : constant := 16#86AA#;  --  ../include/SDL/SDL_opengl.h:628
   GL_WEIGHT_ARRAY_SIZE_ARB : constant := 16#86AB#;  --  ../include/SDL/SDL_opengl.h:629
   GL_WEIGHT_ARRAY_POINTER_ARB : constant := 16#86AC#;  --  ../include/SDL/SDL_opengl.h:630
   GL_WEIGHT_ARRAY_ARB : constant := 16#86AD#;  --  ../include/SDL/SDL_opengl.h:631
   GL_MODELVIEW0_ARB : constant := 16#1700#;  --  ../include/SDL/SDL_opengl.h:632
   GL_MODELVIEW1_ARB : constant := 16#850A#;  --  ../include/SDL/SDL_opengl.h:633
   GL_MODELVIEW2_ARB : constant := 16#8722#;  --  ../include/SDL/SDL_opengl.h:634
   GL_MODELVIEW3_ARB : constant := 16#8723#;  --  ../include/SDL/SDL_opengl.h:635
   GL_MODELVIEW4_ARB : constant := 16#8724#;  --  ../include/SDL/SDL_opengl.h:636
   GL_MODELVIEW5_ARB : constant := 16#8725#;  --  ../include/SDL/SDL_opengl.h:637
   GL_MODELVIEW6_ARB : constant := 16#8726#;  --  ../include/SDL/SDL_opengl.h:638
   GL_MODELVIEW7_ARB : constant := 16#8727#;  --  ../include/SDL/SDL_opengl.h:639
   GL_MODELVIEW8_ARB : constant := 16#8728#;  --  ../include/SDL/SDL_opengl.h:640
   GL_MODELVIEW9_ARB : constant := 16#8729#;  --  ../include/SDL/SDL_opengl.h:641
   GL_MODELVIEW10_ARB : constant := 16#872A#;  --  ../include/SDL/SDL_opengl.h:642
   GL_MODELVIEW11_ARB : constant := 16#872B#;  --  ../include/SDL/SDL_opengl.h:643
   GL_MODELVIEW12_ARB : constant := 16#872C#;  --  ../include/SDL/SDL_opengl.h:644
   GL_MODELVIEW13_ARB : constant := 16#872D#;  --  ../include/SDL/SDL_opengl.h:645
   GL_MODELVIEW14_ARB : constant := 16#872E#;  --  ../include/SDL/SDL_opengl.h:646
   GL_MODELVIEW15_ARB : constant := 16#872F#;  --  ../include/SDL/SDL_opengl.h:647
   GL_MODELVIEW16_ARB : constant := 16#8730#;  --  ../include/SDL/SDL_opengl.h:648
   GL_MODELVIEW17_ARB : constant := 16#8731#;  --  ../include/SDL/SDL_opengl.h:649
   GL_MODELVIEW18_ARB : constant := 16#8732#;  --  ../include/SDL/SDL_opengl.h:650
   GL_MODELVIEW19_ARB : constant := 16#8733#;  --  ../include/SDL/SDL_opengl.h:651
   GL_MODELVIEW20_ARB : constant := 16#8734#;  --  ../include/SDL/SDL_opengl.h:652
   GL_MODELVIEW21_ARB : constant := 16#8735#;  --  ../include/SDL/SDL_opengl.h:653
   GL_MODELVIEW22_ARB : constant := 16#8736#;  --  ../include/SDL/SDL_opengl.h:654
   GL_MODELVIEW23_ARB : constant := 16#8737#;  --  ../include/SDL/SDL_opengl.h:655
   GL_MODELVIEW24_ARB : constant := 16#8738#;  --  ../include/SDL/SDL_opengl.h:656
   GL_MODELVIEW25_ARB : constant := 16#8739#;  --  ../include/SDL/SDL_opengl.h:657
   GL_MODELVIEW26_ARB : constant := 16#873A#;  --  ../include/SDL/SDL_opengl.h:658
   GL_MODELVIEW27_ARB : constant := 16#873B#;  --  ../include/SDL/SDL_opengl.h:659
   GL_MODELVIEW28_ARB : constant := 16#873C#;  --  ../include/SDL/SDL_opengl.h:660
   GL_MODELVIEW29_ARB : constant := 16#873D#;  --  ../include/SDL/SDL_opengl.h:661
   GL_MODELVIEW30_ARB : constant := 16#873E#;  --  ../include/SDL/SDL_opengl.h:662
   GL_MODELVIEW31_ARB : constant := 16#873F#;  --  ../include/SDL/SDL_opengl.h:663

   GL_MATRIX_PALETTE_ARB : constant := 16#8840#;  --  ../include/SDL/SDL_opengl.h:667
   GL_MAX_MATRIX_PALETTE_STACK_DEPTH_ARB : constant := 16#8841#;  --  ../include/SDL/SDL_opengl.h:668
   GL_MAX_PALETTE_MATRICES_ARB : constant := 16#8842#;  --  ../include/SDL/SDL_opengl.h:669
   GL_CURRENT_PALETTE_MATRIX_ARB : constant := 16#8843#;  --  ../include/SDL/SDL_opengl.h:670
   GL_MATRIX_INDEX_ARRAY_ARB : constant := 16#8844#;  --  ../include/SDL/SDL_opengl.h:671
   GL_CURRENT_MATRIX_INDEX_ARB : constant := 16#8845#;  --  ../include/SDL/SDL_opengl.h:672
   GL_MATRIX_INDEX_ARRAY_SIZE_ARB : constant := 16#8846#;  --  ../include/SDL/SDL_opengl.h:673
   GL_MATRIX_INDEX_ARRAY_TYPE_ARB : constant := 16#8847#;  --  ../include/SDL/SDL_opengl.h:674
   GL_MATRIX_INDEX_ARRAY_STRIDE_ARB : constant := 16#8848#;  --  ../include/SDL/SDL_opengl.h:675
   GL_MATRIX_INDEX_ARRAY_POINTER_ARB : constant := 16#8849#;  --  ../include/SDL/SDL_opengl.h:676

   GL_COMBINE_ARB : constant := 16#8570#;  --  ../include/SDL/SDL_opengl.h:680
   GL_COMBINE_RGB_ARB : constant := 16#8571#;  --  ../include/SDL/SDL_opengl.h:681
   GL_COMBINE_ALPHA_ARB : constant := 16#8572#;  --  ../include/SDL/SDL_opengl.h:682
   GL_SOURCE0_RGB_ARB : constant := 16#8580#;  --  ../include/SDL/SDL_opengl.h:683
   GL_SOURCE1_RGB_ARB : constant := 16#8581#;  --  ../include/SDL/SDL_opengl.h:684
   GL_SOURCE2_RGB_ARB : constant := 16#8582#;  --  ../include/SDL/SDL_opengl.h:685
   GL_SOURCE0_ALPHA_ARB : constant := 16#8588#;  --  ../include/SDL/SDL_opengl.h:686
   GL_SOURCE1_ALPHA_ARB : constant := 16#8589#;  --  ../include/SDL/SDL_opengl.h:687
   GL_SOURCE2_ALPHA_ARB : constant := 16#858A#;  --  ../include/SDL/SDL_opengl.h:688
   GL_OPERAND0_RGB_ARB : constant := 16#8590#;  --  ../include/SDL/SDL_opengl.h:689
   GL_OPERAND1_RGB_ARB : constant := 16#8591#;  --  ../include/SDL/SDL_opengl.h:690
   GL_OPERAND2_RGB_ARB : constant := 16#8592#;  --  ../include/SDL/SDL_opengl.h:691
   GL_OPERAND0_ALPHA_ARB : constant := 16#8598#;  --  ../include/SDL/SDL_opengl.h:692
   GL_OPERAND1_ALPHA_ARB : constant := 16#8599#;  --  ../include/SDL/SDL_opengl.h:693
   GL_OPERAND2_ALPHA_ARB : constant := 16#859A#;  --  ../include/SDL/SDL_opengl.h:694
   GL_RGB_SCALE_ARB : constant := 16#8573#;  --  ../include/SDL/SDL_opengl.h:695
   GL_ADD_SIGNED_ARB : constant := 16#8574#;  --  ../include/SDL/SDL_opengl.h:696
   GL_INTERPOLATE_ARB : constant := 16#8575#;  --  ../include/SDL/SDL_opengl.h:697
   GL_SUBTRACT_ARB : constant := 16#84E7#;  --  ../include/SDL/SDL_opengl.h:698
   GL_CONSTANT_ARB : constant := 16#8576#;  --  ../include/SDL/SDL_opengl.h:699
   GL_PRIMARY_COLOR_ARB : constant := 16#8577#;  --  ../include/SDL/SDL_opengl.h:700
   GL_PREVIOUS_ARB : constant := 16#8578#;  --  ../include/SDL/SDL_opengl.h:701

   GL_DOT3_RGB_ARB : constant := 16#86AE#;  --  ../include/SDL/SDL_opengl.h:708
   GL_DOT3_RGBA_ARB : constant := 16#86AF#;  --  ../include/SDL/SDL_opengl.h:709

   GL_MIRRORED_REPEAT_ARB : constant := 16#8370#;  --  ../include/SDL/SDL_opengl.h:713

   GL_DEPTH_COMPONENT16_ARB : constant := 16#81A5#;  --  ../include/SDL/SDL_opengl.h:717
   GL_DEPTH_COMPONENT24_ARB : constant := 16#81A6#;  --  ../include/SDL/SDL_opengl.h:718
   GL_DEPTH_COMPONENT32_ARB : constant := 16#81A7#;  --  ../include/SDL/SDL_opengl.h:719
   GL_TEXTURE_DEPTH_SIZE_ARB : constant := 16#884A#;  --  ../include/SDL/SDL_opengl.h:720
   GL_DEPTH_TEXTURE_MODE_ARB : constant := 16#884B#;  --  ../include/SDL/SDL_opengl.h:721

   GL_TEXTURE_COMPARE_MODE_ARB : constant := 16#884C#;  --  ../include/SDL/SDL_opengl.h:725
   GL_TEXTURE_COMPARE_FUNC_ARB : constant := 16#884D#;  --  ../include/SDL/SDL_opengl.h:726
   GL_COMPARE_R_TO_TEXTURE_ARB : constant := 16#884E#;  --  ../include/SDL/SDL_opengl.h:727

   GL_TEXTURE_COMPARE_FAIL_VALUE_ARB : constant := 16#80BF#;  --  ../include/SDL/SDL_opengl.h:731

   GL_COLOR_SUM_ARB : constant := 16#8458#;  --  ../include/SDL/SDL_opengl.h:738
   GL_VERTEX_PROGRAM_ARB : constant := 16#8620#;  --  ../include/SDL/SDL_opengl.h:739
   GL_VERTEX_ATTRIB_ARRAY_ENABLED_ARB : constant := 16#8622#;  --  ../include/SDL/SDL_opengl.h:740
   GL_VERTEX_ATTRIB_ARRAY_SIZE_ARB : constant := 16#8623#;  --  ../include/SDL/SDL_opengl.h:741
   GL_VERTEX_ATTRIB_ARRAY_STRIDE_ARB : constant := 16#8624#;  --  ../include/SDL/SDL_opengl.h:742
   GL_VERTEX_ATTRIB_ARRAY_TYPE_ARB : constant := 16#8625#;  --  ../include/SDL/SDL_opengl.h:743
   GL_CURRENT_VERTEX_ATTRIB_ARB : constant := 16#8626#;  --  ../include/SDL/SDL_opengl.h:744
   GL_PROGRAM_LENGTH_ARB : constant := 16#8627#;  --  ../include/SDL/SDL_opengl.h:745
   GL_PROGRAM_STRING_ARB : constant := 16#8628#;  --  ../include/SDL/SDL_opengl.h:746
   GL_MAX_PROGRAM_MATRIX_STACK_DEPTH_ARB : constant := 16#862E#;  --  ../include/SDL/SDL_opengl.h:747
   GL_MAX_PROGRAM_MATRICES_ARB : constant := 16#862F#;  --  ../include/SDL/SDL_opengl.h:748
   GL_CURRENT_MATRIX_STACK_DEPTH_ARB : constant := 16#8640#;  --  ../include/SDL/SDL_opengl.h:749
   GL_CURRENT_MATRIX_ARB : constant := 16#8641#;  --  ../include/SDL/SDL_opengl.h:750
   GL_VERTEX_PROGRAM_POINT_SIZE_ARB : constant := 16#8642#;  --  ../include/SDL/SDL_opengl.h:751
   GL_VERTEX_PROGRAM_TWO_SIDE_ARB : constant := 16#8643#;  --  ../include/SDL/SDL_opengl.h:752
   GL_VERTEX_ATTRIB_ARRAY_POINTER_ARB : constant := 16#8645#;  --  ../include/SDL/SDL_opengl.h:753
   GL_PROGRAM_ERROR_POSITION_ARB : constant := 16#864B#;  --  ../include/SDL/SDL_opengl.h:754
   GL_PROGRAM_BINDING_ARB : constant := 16#8677#;  --  ../include/SDL/SDL_opengl.h:755
   GL_MAX_VERTEX_ATTRIBS_ARB : constant := 16#8869#;  --  ../include/SDL/SDL_opengl.h:756
   GL_VERTEX_ATTRIB_ARRAY_NORMALIZED_ARB : constant := 16#886A#;  --  ../include/SDL/SDL_opengl.h:757
   GL_PROGRAM_ERROR_STRING_ARB : constant := 16#8874#;  --  ../include/SDL/SDL_opengl.h:758
   GL_PROGRAM_FORMAT_ASCII_ARB : constant := 16#8875#;  --  ../include/SDL/SDL_opengl.h:759
   GL_PROGRAM_FORMAT_ARB : constant := 16#8876#;  --  ../include/SDL/SDL_opengl.h:760
   GL_PROGRAM_INSTRUCTIONS_ARB : constant := 16#88A0#;  --  ../include/SDL/SDL_opengl.h:761
   GL_MAX_PROGRAM_INSTRUCTIONS_ARB : constant := 16#88A1#;  --  ../include/SDL/SDL_opengl.h:762
   GL_PROGRAM_NATIVE_INSTRUCTIONS_ARB : constant := 16#88A2#;  --  ../include/SDL/SDL_opengl.h:763
   GL_MAX_PROGRAM_NATIVE_INSTRUCTIONS_ARB : constant := 16#88A3#;  --  ../include/SDL/SDL_opengl.h:764
   GL_PROGRAM_TEMPORARIES_ARB : constant := 16#88A4#;  --  ../include/SDL/SDL_opengl.h:765
   GL_MAX_PROGRAM_TEMPORARIES_ARB : constant := 16#88A5#;  --  ../include/SDL/SDL_opengl.h:766
   GL_PROGRAM_NATIVE_TEMPORARIES_ARB : constant := 16#88A6#;  --  ../include/SDL/SDL_opengl.h:767
   GL_MAX_PROGRAM_NATIVE_TEMPORARIES_ARB : constant := 16#88A7#;  --  ../include/SDL/SDL_opengl.h:768
   GL_PROGRAM_PARAMETERS_ARB : constant := 16#88A8#;  --  ../include/SDL/SDL_opengl.h:769
   GL_MAX_PROGRAM_PARAMETERS_ARB : constant := 16#88A9#;  --  ../include/SDL/SDL_opengl.h:770
   GL_PROGRAM_NATIVE_PARAMETERS_ARB : constant := 16#88AA#;  --  ../include/SDL/SDL_opengl.h:771
   GL_MAX_PROGRAM_NATIVE_PARAMETERS_ARB : constant := 16#88AB#;  --  ../include/SDL/SDL_opengl.h:772
   GL_PROGRAM_ATTRIBS_ARB : constant := 16#88AC#;  --  ../include/SDL/SDL_opengl.h:773
   GL_MAX_PROGRAM_ATTRIBS_ARB : constant := 16#88AD#;  --  ../include/SDL/SDL_opengl.h:774
   GL_PROGRAM_NATIVE_ATTRIBS_ARB : constant := 16#88AE#;  --  ../include/SDL/SDL_opengl.h:775
   GL_MAX_PROGRAM_NATIVE_ATTRIBS_ARB : constant := 16#88AF#;  --  ../include/SDL/SDL_opengl.h:776
   GL_PROGRAM_ADDRESS_REGISTERS_ARB : constant := 16#88B0#;  --  ../include/SDL/SDL_opengl.h:777
   GL_MAX_PROGRAM_ADDRESS_REGISTERS_ARB : constant := 16#88B1#;  --  ../include/SDL/SDL_opengl.h:778
   GL_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB : constant := 16#88B2#;  --  ../include/SDL/SDL_opengl.h:779
   GL_MAX_PROGRAM_NATIVE_ADDRESS_REGISTERS_ARB : constant := 16#88B3#;  --  ../include/SDL/SDL_opengl.h:780
   GL_MAX_PROGRAM_LOCAL_PARAMETERS_ARB : constant := 16#88B4#;  --  ../include/SDL/SDL_opengl.h:781
   GL_MAX_PROGRAM_ENV_PARAMETERS_ARB : constant := 16#88B5#;  --  ../include/SDL/SDL_opengl.h:782
   GL_PROGRAM_UNDER_NATIVE_LIMITS_ARB : constant := 16#88B6#;  --  ../include/SDL/SDL_opengl.h:783
   GL_TRANSPOSE_CURRENT_MATRIX_ARB : constant := 16#88B7#;  --  ../include/SDL/SDL_opengl.h:784
   GL_MATRIX0_ARB : constant := 16#88C0#;  --  ../include/SDL/SDL_opengl.h:785
   GL_MATRIX1_ARB : constant := 16#88C1#;  --  ../include/SDL/SDL_opengl.h:786
   GL_MATRIX2_ARB : constant := 16#88C2#;  --  ../include/SDL/SDL_opengl.h:787
   GL_MATRIX3_ARB : constant := 16#88C3#;  --  ../include/SDL/SDL_opengl.h:788
   GL_MATRIX4_ARB : constant := 16#88C4#;  --  ../include/SDL/SDL_opengl.h:789
   GL_MATRIX5_ARB : constant := 16#88C5#;  --  ../include/SDL/SDL_opengl.h:790
   GL_MATRIX6_ARB : constant := 16#88C6#;  --  ../include/SDL/SDL_opengl.h:791
   GL_MATRIX7_ARB : constant := 16#88C7#;  --  ../include/SDL/SDL_opengl.h:792
   GL_MATRIX8_ARB : constant := 16#88C8#;  --  ../include/SDL/SDL_opengl.h:793
   GL_MATRIX9_ARB : constant := 16#88C9#;  --  ../include/SDL/SDL_opengl.h:794
   GL_MATRIX10_ARB : constant := 16#88CA#;  --  ../include/SDL/SDL_opengl.h:795
   GL_MATRIX11_ARB : constant := 16#88CB#;  --  ../include/SDL/SDL_opengl.h:796
   GL_MATRIX12_ARB : constant := 16#88CC#;  --  ../include/SDL/SDL_opengl.h:797
   GL_MATRIX13_ARB : constant := 16#88CD#;  --  ../include/SDL/SDL_opengl.h:798
   GL_MATRIX14_ARB : constant := 16#88CE#;  --  ../include/SDL/SDL_opengl.h:799
   GL_MATRIX15_ARB : constant := 16#88CF#;  --  ../include/SDL/SDL_opengl.h:800
   GL_MATRIX16_ARB : constant := 16#88D0#;  --  ../include/SDL/SDL_opengl.h:801
   GL_MATRIX17_ARB : constant := 16#88D1#;  --  ../include/SDL/SDL_opengl.h:802
   GL_MATRIX18_ARB : constant := 16#88D2#;  --  ../include/SDL/SDL_opengl.h:803
   GL_MATRIX19_ARB : constant := 16#88D3#;  --  ../include/SDL/SDL_opengl.h:804
   GL_MATRIX20_ARB : constant := 16#88D4#;  --  ../include/SDL/SDL_opengl.h:805
   GL_MATRIX21_ARB : constant := 16#88D5#;  --  ../include/SDL/SDL_opengl.h:806
   GL_MATRIX22_ARB : constant := 16#88D6#;  --  ../include/SDL/SDL_opengl.h:807
   GL_MATRIX23_ARB : constant := 16#88D7#;  --  ../include/SDL/SDL_opengl.h:808
   GL_MATRIX24_ARB : constant := 16#88D8#;  --  ../include/SDL/SDL_opengl.h:809
   GL_MATRIX25_ARB : constant := 16#88D9#;  --  ../include/SDL/SDL_opengl.h:810
   GL_MATRIX26_ARB : constant := 16#88DA#;  --  ../include/SDL/SDL_opengl.h:811
   GL_MATRIX27_ARB : constant := 16#88DB#;  --  ../include/SDL/SDL_opengl.h:812
   GL_MATRIX28_ARB : constant := 16#88DC#;  --  ../include/SDL/SDL_opengl.h:813
   GL_MATRIX29_ARB : constant := 16#88DD#;  --  ../include/SDL/SDL_opengl.h:814
   GL_MATRIX30_ARB : constant := 16#88DE#;  --  ../include/SDL/SDL_opengl.h:815
   GL_MATRIX31_ARB : constant := 16#88DF#;  --  ../include/SDL/SDL_opengl.h:816

   GL_FRAGMENT_PROGRAM_ARB : constant := 16#8804#;  --  ../include/SDL/SDL_opengl.h:820
   GL_PROGRAM_ALU_INSTRUCTIONS_ARB : constant := 16#8805#;  --  ../include/SDL/SDL_opengl.h:821
   GL_PROGRAM_TEX_INSTRUCTIONS_ARB : constant := 16#8806#;  --  ../include/SDL/SDL_opengl.h:822
   GL_PROGRAM_TEX_INDIRECTIONS_ARB : constant := 16#8807#;  --  ../include/SDL/SDL_opengl.h:823
   GL_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB : constant := 16#8808#;  --  ../include/SDL/SDL_opengl.h:824
   GL_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB : constant := 16#8809#;  --  ../include/SDL/SDL_opengl.h:825
   GL_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB : constant := 16#880A#;  --  ../include/SDL/SDL_opengl.h:826
   GL_MAX_PROGRAM_ALU_INSTRUCTIONS_ARB : constant := 16#880B#;  --  ../include/SDL/SDL_opengl.h:827
   GL_MAX_PROGRAM_TEX_INSTRUCTIONS_ARB : constant := 16#880C#;  --  ../include/SDL/SDL_opengl.h:828
   GL_MAX_PROGRAM_TEX_INDIRECTIONS_ARB : constant := 16#880D#;  --  ../include/SDL/SDL_opengl.h:829
   GL_MAX_PROGRAM_NATIVE_ALU_INSTRUCTIONS_ARB : constant := 16#880E#;  --  ../include/SDL/SDL_opengl.h:830
   GL_MAX_PROGRAM_NATIVE_TEX_INSTRUCTIONS_ARB : constant := 16#880F#;  --  ../include/SDL/SDL_opengl.h:831
   GL_MAX_PROGRAM_NATIVE_TEX_INDIRECTIONS_ARB : constant := 16#8810#;  --  ../include/SDL/SDL_opengl.h:832
   GL_MAX_TEXTURE_COORDS_ARB : constant := 16#8871#;  --  ../include/SDL/SDL_opengl.h:833
   GL_MAX_TEXTURE_IMAGE_UNITS_ARB : constant := 16#8872#;  --  ../include/SDL/SDL_opengl.h:834

   GL_BUFFER_SIZE_ARB : constant := 16#8764#;  --  ../include/SDL/SDL_opengl.h:838
   GL_BUFFER_USAGE_ARB : constant := 16#8765#;  --  ../include/SDL/SDL_opengl.h:839
   GL_ARRAY_BUFFER_ARB : constant := 16#8892#;  --  ../include/SDL/SDL_opengl.h:840
   GL_ELEMENT_ARRAY_BUFFER_ARB : constant := 16#8893#;  --  ../include/SDL/SDL_opengl.h:841
   GL_ARRAY_BUFFER_BINDING_ARB : constant := 16#8894#;  --  ../include/SDL/SDL_opengl.h:842
   GL_ELEMENT_ARRAY_BUFFER_BINDING_ARB : constant := 16#8895#;  --  ../include/SDL/SDL_opengl.h:843
   GL_VERTEX_ARRAY_BUFFER_BINDING_ARB : constant := 16#8896#;  --  ../include/SDL/SDL_opengl.h:844
   GL_NORMAL_ARRAY_BUFFER_BINDING_ARB : constant := 16#8897#;  --  ../include/SDL/SDL_opengl.h:845
   GL_COLOR_ARRAY_BUFFER_BINDING_ARB : constant := 16#8898#;  --  ../include/SDL/SDL_opengl.h:846
   GL_INDEX_ARRAY_BUFFER_BINDING_ARB : constant := 16#8899#;  --  ../include/SDL/SDL_opengl.h:847
   GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING_ARB : constant := 16#889A#;  --  ../include/SDL/SDL_opengl.h:848
   GL_EDGE_FLAG_ARRAY_BUFFER_BINDING_ARB : constant := 16#889B#;  --  ../include/SDL/SDL_opengl.h:849
   GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING_ARB : constant := 16#889C#;  --  ../include/SDL/SDL_opengl.h:850
   GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING_ARB : constant := 16#889D#;  --  ../include/SDL/SDL_opengl.h:851
   GL_WEIGHT_ARRAY_BUFFER_BINDING_ARB : constant := 16#889E#;  --  ../include/SDL/SDL_opengl.h:852
   GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING_ARB : constant := 16#889F#;  --  ../include/SDL/SDL_opengl.h:853
   GL_READ_ONLY_ARB : constant := 16#88B8#;  --  ../include/SDL/SDL_opengl.h:854
   GL_WRITE_ONLY_ARB : constant := 16#88B9#;  --  ../include/SDL/SDL_opengl.h:855
   GL_READ_WRITE_ARB : constant := 16#88BA#;  --  ../include/SDL/SDL_opengl.h:856
   GL_BUFFER_ACCESS_ARB : constant := 16#88BB#;  --  ../include/SDL/SDL_opengl.h:857
   GL_BUFFER_MAPPED_ARB : constant := 16#88BC#;  --  ../include/SDL/SDL_opengl.h:858
   GL_BUFFER_MAP_POINTER_ARB : constant := 16#88BD#;  --  ../include/SDL/SDL_opengl.h:859
   GL_STREAM_DRAW_ARB : constant := 16#88E0#;  --  ../include/SDL/SDL_opengl.h:860
   GL_STREAM_READ_ARB : constant := 16#88E1#;  --  ../include/SDL/SDL_opengl.h:861
   GL_STREAM_COPY_ARB : constant := 16#88E2#;  --  ../include/SDL/SDL_opengl.h:862
   GL_STATIC_DRAW_ARB : constant := 16#88E4#;  --  ../include/SDL/SDL_opengl.h:863
   GL_STATIC_READ_ARB : constant := 16#88E5#;  --  ../include/SDL/SDL_opengl.h:864
   GL_STATIC_COPY_ARB : constant := 16#88E6#;  --  ../include/SDL/SDL_opengl.h:865
   GL_DYNAMIC_DRAW_ARB : constant := 16#88E8#;  --  ../include/SDL/SDL_opengl.h:866
   GL_DYNAMIC_READ_ARB : constant := 16#88E9#;  --  ../include/SDL/SDL_opengl.h:867
   GL_DYNAMIC_COPY_ARB : constant := 16#88EA#;  --  ../include/SDL/SDL_opengl.h:868

   GL_QUERY_COUNTER_BITS_ARB : constant := 16#8864#;  --  ../include/SDL/SDL_opengl.h:872
   GL_CURRENT_QUERY_ARB : constant := 16#8865#;  --  ../include/SDL/SDL_opengl.h:873
   GL_QUERY_RESULT_ARB : constant := 16#8866#;  --  ../include/SDL/SDL_opengl.h:874
   GL_QUERY_RESULT_AVAILABLE_ARB : constant := 16#8867#;  --  ../include/SDL/SDL_opengl.h:875
   GL_SAMPLES_PASSED_ARB : constant := 16#8914#;  --  ../include/SDL/SDL_opengl.h:876

   GL_PROGRAM_OBJECT_ARB : constant := 16#8B40#;  --  ../include/SDL/SDL_opengl.h:880
   GL_SHADER_OBJECT_ARB : constant := 16#8B48#;  --  ../include/SDL/SDL_opengl.h:881
   GL_OBJECT_TYPE_ARB : constant := 16#8B4E#;  --  ../include/SDL/SDL_opengl.h:882
   GL_OBJECT_SUBTYPE_ARB : constant := 16#8B4F#;  --  ../include/SDL/SDL_opengl.h:883
   GL_FLOAT_VEC2_ARB : constant := 16#8B50#;  --  ../include/SDL/SDL_opengl.h:884
   GL_FLOAT_VEC3_ARB : constant := 16#8B51#;  --  ../include/SDL/SDL_opengl.h:885
   GL_FLOAT_VEC4_ARB : constant := 16#8B52#;  --  ../include/SDL/SDL_opengl.h:886
   GL_INT_VEC2_ARB : constant := 16#8B53#;  --  ../include/SDL/SDL_opengl.h:887
   GL_INT_VEC3_ARB : constant := 16#8B54#;  --  ../include/SDL/SDL_opengl.h:888
   GL_INT_VEC4_ARB : constant := 16#8B55#;  --  ../include/SDL/SDL_opengl.h:889
   GL_BOOL_ARB : constant := 16#8B56#;  --  ../include/SDL/SDL_opengl.h:890
   GL_BOOL_VEC2_ARB : constant := 16#8B57#;  --  ../include/SDL/SDL_opengl.h:891
   GL_BOOL_VEC3_ARB : constant := 16#8B58#;  --  ../include/SDL/SDL_opengl.h:892
   GL_BOOL_VEC4_ARB : constant := 16#8B59#;  --  ../include/SDL/SDL_opengl.h:893
   GL_FLOAT_MAT2_ARB : constant := 16#8B5A#;  --  ../include/SDL/SDL_opengl.h:894
   GL_FLOAT_MAT3_ARB : constant := 16#8B5B#;  --  ../include/SDL/SDL_opengl.h:895
   GL_FLOAT_MAT4_ARB : constant := 16#8B5C#;  --  ../include/SDL/SDL_opengl.h:896
   GL_SAMPLER_1D_ARB : constant := 16#8B5D#;  --  ../include/SDL/SDL_opengl.h:897
   GL_SAMPLER_2D_ARB : constant := 16#8B5E#;  --  ../include/SDL/SDL_opengl.h:898
   GL_SAMPLER_3D_ARB : constant := 16#8B5F#;  --  ../include/SDL/SDL_opengl.h:899
   GL_SAMPLER_CUBE_ARB : constant := 16#8B60#;  --  ../include/SDL/SDL_opengl.h:900
   GL_SAMPLER_1D_SHADOW_ARB : constant := 16#8B61#;  --  ../include/SDL/SDL_opengl.h:901
   GL_SAMPLER_2D_SHADOW_ARB : constant := 16#8B62#;  --  ../include/SDL/SDL_opengl.h:902
   GL_SAMPLER_2D_RECT_ARB : constant := 16#8B63#;  --  ../include/SDL/SDL_opengl.h:903
   GL_SAMPLER_2D_RECT_SHADOW_ARB : constant := 16#8B64#;  --  ../include/SDL/SDL_opengl.h:904
   GL_OBJECT_DELETE_STATUS_ARB : constant := 16#8B80#;  --  ../include/SDL/SDL_opengl.h:905
   GL_OBJECT_COMPILE_STATUS_ARB : constant := 16#8B81#;  --  ../include/SDL/SDL_opengl.h:906
   GL_OBJECT_LINK_STATUS_ARB : constant := 16#8B82#;  --  ../include/SDL/SDL_opengl.h:907
   GL_OBJECT_VALIDATE_STATUS_ARB : constant := 16#8B83#;  --  ../include/SDL/SDL_opengl.h:908
   GL_OBJECT_INFO_LOG_LENGTH_ARB : constant := 16#8B84#;  --  ../include/SDL/SDL_opengl.h:909
   GL_OBJECT_ATTACHED_OBJECTS_ARB : constant := 16#8B85#;  --  ../include/SDL/SDL_opengl.h:910
   GL_OBJECT_ACTIVE_UNIFORMS_ARB : constant := 16#8B86#;  --  ../include/SDL/SDL_opengl.h:911
   GL_OBJECT_ACTIVE_UNIFORM_MAX_LENGTH_ARB : constant := 16#8B87#;  --  ../include/SDL/SDL_opengl.h:912
   GL_OBJECT_SHADER_SOURCE_LENGTH_ARB : constant := 16#8B88#;  --  ../include/SDL/SDL_opengl.h:913

   GL_VERTEX_SHADER_ARB : constant := 16#8B31#;  --  ../include/SDL/SDL_opengl.h:917
   GL_MAX_VERTEX_UNIFORM_COMPONENTS_ARB : constant := 16#8B4A#;  --  ../include/SDL/SDL_opengl.h:918
   GL_MAX_VARYING_FLOATS_ARB : constant := 16#8B4B#;  --  ../include/SDL/SDL_opengl.h:919
   GL_MAX_VERTEX_TEXTURE_IMAGE_UNITS_ARB : constant := 16#8B4C#;  --  ../include/SDL/SDL_opengl.h:920
   GL_MAX_COMBINED_TEXTURE_IMAGE_UNITS_ARB : constant := 16#8B4D#;  --  ../include/SDL/SDL_opengl.h:921
   GL_OBJECT_ACTIVE_ATTRIBUTES_ARB : constant := 16#8B89#;  --  ../include/SDL/SDL_opengl.h:922
   GL_OBJECT_ACTIVE_ATTRIBUTE_MAX_LENGTH_ARB : constant := 16#8B8A#;  --  ../include/SDL/SDL_opengl.h:923

   GL_FRAGMENT_SHADER_ARB : constant := 16#8B30#;  --  ../include/SDL/SDL_opengl.h:927
   GL_MAX_FRAGMENT_UNIFORM_COMPONENTS_ARB : constant := 16#8B49#;  --  ../include/SDL/SDL_opengl.h:928
   GL_FRAGMENT_SHADER_DERIVATIVE_HINT_ARB : constant := 16#8B8B#;  --  ../include/SDL/SDL_opengl.h:929

   GL_SHADING_LANGUAGE_VERSION_ARB : constant := 16#8B8C#;  --  ../include/SDL/SDL_opengl.h:933

   GL_POINT_SPRITE_ARB : constant := 16#8861#;  --  ../include/SDL/SDL_opengl.h:940
   GL_COORD_REPLACE_ARB : constant := 16#8862#;  --  ../include/SDL/SDL_opengl.h:941

   GL_MAX_DRAW_BUFFERS_ARB : constant := 16#8824#;  --  ../include/SDL/SDL_opengl.h:948
   GL_DRAW_BUFFER0_ARB : constant := 16#8825#;  --  ../include/SDL/SDL_opengl.h:949
   GL_DRAW_BUFFER1_ARB : constant := 16#8826#;  --  ../include/SDL/SDL_opengl.h:950
   GL_DRAW_BUFFER2_ARB : constant := 16#8827#;  --  ../include/SDL/SDL_opengl.h:951
   GL_DRAW_BUFFER3_ARB : constant := 16#8828#;  --  ../include/SDL/SDL_opengl.h:952
   GL_DRAW_BUFFER4_ARB : constant := 16#8829#;  --  ../include/SDL/SDL_opengl.h:953
   GL_DRAW_BUFFER5_ARB : constant := 16#882A#;  --  ../include/SDL/SDL_opengl.h:954
   GL_DRAW_BUFFER6_ARB : constant := 16#882B#;  --  ../include/SDL/SDL_opengl.h:955
   GL_DRAW_BUFFER7_ARB : constant := 16#882C#;  --  ../include/SDL/SDL_opengl.h:956
   GL_DRAW_BUFFER8_ARB : constant := 16#882D#;  --  ../include/SDL/SDL_opengl.h:957
   GL_DRAW_BUFFER9_ARB : constant := 16#882E#;  --  ../include/SDL/SDL_opengl.h:958
   GL_DRAW_BUFFER10_ARB : constant := 16#882F#;  --  ../include/SDL/SDL_opengl.h:959
   GL_DRAW_BUFFER11_ARB : constant := 16#8830#;  --  ../include/SDL/SDL_opengl.h:960
   GL_DRAW_BUFFER12_ARB : constant := 16#8831#;  --  ../include/SDL/SDL_opengl.h:961
   GL_DRAW_BUFFER13_ARB : constant := 16#8832#;  --  ../include/SDL/SDL_opengl.h:962
   GL_DRAW_BUFFER14_ARB : constant := 16#8833#;  --  ../include/SDL/SDL_opengl.h:963
   GL_DRAW_BUFFER15_ARB : constant := 16#8834#;  --  ../include/SDL/SDL_opengl.h:964

   GL_TEXTURE_RECTANGLE_ARB : constant := 16#84F5#;  --  ../include/SDL/SDL_opengl.h:968
   GL_TEXTURE_BINDING_RECTANGLE_ARB : constant := 16#84F6#;  --  ../include/SDL/SDL_opengl.h:969
   GL_PROXY_TEXTURE_RECTANGLE_ARB : constant := 16#84F7#;  --  ../include/SDL/SDL_opengl.h:970
   GL_MAX_RECTANGLE_TEXTURE_SIZE_ARB : constant := 16#84F8#;  --  ../include/SDL/SDL_opengl.h:971

   GL_RGBA_FLOAT_MODE_ARB : constant := 16#8820#;  --  ../include/SDL/SDL_opengl.h:975
   GL_CLAMP_VERTEX_COLOR_ARB : constant := 16#891A#;  --  ../include/SDL/SDL_opengl.h:976
   GL_CLAMP_FRAGMENT_COLOR_ARB : constant := 16#891B#;  --  ../include/SDL/SDL_opengl.h:977
   GL_CLAMP_READ_COLOR_ARB : constant := 16#891C#;  --  ../include/SDL/SDL_opengl.h:978
   GL_FIXED_ONLY_ARB : constant := 16#891D#;  --  ../include/SDL/SDL_opengl.h:979

   GL_HALF_FLOAT_ARB : constant := 16#140B#;  --  ../include/SDL/SDL_opengl.h:983

   GL_TEXTURE_RED_TYPE_ARB : constant := 16#8C10#;  --  ../include/SDL/SDL_opengl.h:987
   GL_TEXTURE_GREEN_TYPE_ARB : constant := 16#8C11#;  --  ../include/SDL/SDL_opengl.h:988
   GL_TEXTURE_BLUE_TYPE_ARB : constant := 16#8C12#;  --  ../include/SDL/SDL_opengl.h:989
   GL_TEXTURE_ALPHA_TYPE_ARB : constant := 16#8C13#;  --  ../include/SDL/SDL_opengl.h:990
   GL_TEXTURE_LUMINANCE_TYPE_ARB : constant := 16#8C14#;  --  ../include/SDL/SDL_opengl.h:991
   GL_TEXTURE_INTENSITY_TYPE_ARB : constant := 16#8C15#;  --  ../include/SDL/SDL_opengl.h:992
   GL_TEXTURE_DEPTH_TYPE_ARB : constant := 16#8C16#;  --  ../include/SDL/SDL_opengl.h:993
   GL_UNSIGNED_NORMALIZED_ARB : constant := 16#8C17#;  --  ../include/SDL/SDL_opengl.h:994
   GL_RGBA32F_ARB : constant := 16#8814#;  --  ../include/SDL/SDL_opengl.h:995
   GL_RGB32F_ARB : constant := 16#8815#;  --  ../include/SDL/SDL_opengl.h:996
   GL_ALPHA32F_ARB : constant := 16#8816#;  --  ../include/SDL/SDL_opengl.h:997
   GL_INTENSITY32F_ARB : constant := 16#8817#;  --  ../include/SDL/SDL_opengl.h:998
   GL_LUMINANCE32F_ARB : constant := 16#8818#;  --  ../include/SDL/SDL_opengl.h:999
   GL_LUMINANCE_ALPHA32F_ARB : constant := 16#8819#;  --  ../include/SDL/SDL_opengl.h:1000
   GL_RGBA16F_ARB : constant := 16#881A#;  --  ../include/SDL/SDL_opengl.h:1001
   GL_RGB16F_ARB : constant := 16#881B#;  --  ../include/SDL/SDL_opengl.h:1002
   GL_ALPHA16F_ARB : constant := 16#881C#;  --  ../include/SDL/SDL_opengl.h:1003
   GL_INTENSITY16F_ARB : constant := 16#881D#;  --  ../include/SDL/SDL_opengl.h:1004
   GL_LUMINANCE16F_ARB : constant := 16#881E#;  --  ../include/SDL/SDL_opengl.h:1005
   GL_LUMINANCE_ALPHA16F_ARB : constant := 16#881F#;  --  ../include/SDL/SDL_opengl.h:1006

   GL_PIXEL_PACK_BUFFER_ARB : constant := 16#88EB#;  --  ../include/SDL/SDL_opengl.h:1010
   GL_PIXEL_UNPACK_BUFFER_ARB : constant := 16#88EC#;  --  ../include/SDL/SDL_opengl.h:1011
   GL_PIXEL_PACK_BUFFER_BINDING_ARB : constant := 16#88ED#;  --  ../include/SDL/SDL_opengl.h:1012
   GL_PIXEL_UNPACK_BUFFER_BINDING_ARB : constant := 16#88EF#;  --  ../include/SDL/SDL_opengl.h:1013

   GL_ABGR_EXT : constant := 16#8000#;  --  ../include/SDL/SDL_opengl.h:1017

   GL_CONSTANT_COLOR_EXT : constant := 16#8001#;  --  ../include/SDL/SDL_opengl.h:1021
   GL_ONE_MINUS_CONSTANT_COLOR_EXT : constant := 16#8002#;  --  ../include/SDL/SDL_opengl.h:1022
   GL_CONSTANT_ALPHA_EXT : constant := 16#8003#;  --  ../include/SDL/SDL_opengl.h:1023
   GL_ONE_MINUS_CONSTANT_ALPHA_EXT : constant := 16#8004#;  --  ../include/SDL/SDL_opengl.h:1024
   GL_BLEND_COLOR_EXT : constant := 16#8005#;  --  ../include/SDL/SDL_opengl.h:1025

   GL_POLYGON_OFFSET_EXT : constant := 16#8037#;  --  ../include/SDL/SDL_opengl.h:1029
   GL_POLYGON_OFFSET_FACTOR_EXT : constant := 16#8038#;  --  ../include/SDL/SDL_opengl.h:1030
   GL_POLYGON_OFFSET_BIAS_EXT : constant := 16#8039#;  --  ../include/SDL/SDL_opengl.h:1031

   GL_ALPHA4_EXT : constant := 16#803B#;  --  ../include/SDL/SDL_opengl.h:1035
   GL_ALPHA8_EXT : constant := 16#803C#;  --  ../include/SDL/SDL_opengl.h:1036
   GL_ALPHA12_EXT : constant := 16#803D#;  --  ../include/SDL/SDL_opengl.h:1037
   GL_ALPHA16_EXT : constant := 16#803E#;  --  ../include/SDL/SDL_opengl.h:1038
   GL_LUMINANCE4_EXT : constant := 16#803F#;  --  ../include/SDL/SDL_opengl.h:1039
   GL_LUMINANCE8_EXT : constant := 16#8040#;  --  ../include/SDL/SDL_opengl.h:1040
   GL_LUMINANCE12_EXT : constant := 16#8041#;  --  ../include/SDL/SDL_opengl.h:1041
   GL_LUMINANCE16_EXT : constant := 16#8042#;  --  ../include/SDL/SDL_opengl.h:1042
   GL_LUMINANCE4_ALPHA4_EXT : constant := 16#8043#;  --  ../include/SDL/SDL_opengl.h:1043
   GL_LUMINANCE6_ALPHA2_EXT : constant := 16#8044#;  --  ../include/SDL/SDL_opengl.h:1044
   GL_LUMINANCE8_ALPHA8_EXT : constant := 16#8045#;  --  ../include/SDL/SDL_opengl.h:1045
   GL_LUMINANCE12_ALPHA4_EXT : constant := 16#8046#;  --  ../include/SDL/SDL_opengl.h:1046
   GL_LUMINANCE12_ALPHA12_EXT : constant := 16#8047#;  --  ../include/SDL/SDL_opengl.h:1047
   GL_LUMINANCE16_ALPHA16_EXT : constant := 16#8048#;  --  ../include/SDL/SDL_opengl.h:1048
   GL_INTENSITY_EXT : constant := 16#8049#;  --  ../include/SDL/SDL_opengl.h:1049
   GL_INTENSITY4_EXT : constant := 16#804A#;  --  ../include/SDL/SDL_opengl.h:1050
   GL_INTENSITY8_EXT : constant := 16#804B#;  --  ../include/SDL/SDL_opengl.h:1051
   GL_INTENSITY12_EXT : constant := 16#804C#;  --  ../include/SDL/SDL_opengl.h:1052
   GL_INTENSITY16_EXT : constant := 16#804D#;  --  ../include/SDL/SDL_opengl.h:1053
   GL_RGB2_EXT : constant := 16#804E#;  --  ../include/SDL/SDL_opengl.h:1054
   GL_RGB4_EXT : constant := 16#804F#;  --  ../include/SDL/SDL_opengl.h:1055
   GL_RGB5_EXT : constant := 16#8050#;  --  ../include/SDL/SDL_opengl.h:1056
   GL_RGB8_EXT : constant := 16#8051#;  --  ../include/SDL/SDL_opengl.h:1057
   GL_RGB10_EXT : constant := 16#8052#;  --  ../include/SDL/SDL_opengl.h:1058
   GL_RGB12_EXT : constant := 16#8053#;  --  ../include/SDL/SDL_opengl.h:1059
   GL_RGB16_EXT : constant := 16#8054#;  --  ../include/SDL/SDL_opengl.h:1060
   GL_RGBA2_EXT : constant := 16#8055#;  --  ../include/SDL/SDL_opengl.h:1061
   GL_RGBA4_EXT : constant := 16#8056#;  --  ../include/SDL/SDL_opengl.h:1062
   GL_RGB5_A1_EXT : constant := 16#8057#;  --  ../include/SDL/SDL_opengl.h:1063
   GL_RGBA8_EXT : constant := 16#8058#;  --  ../include/SDL/SDL_opengl.h:1064
   GL_RGB10_A2_EXT : constant := 16#8059#;  --  ../include/SDL/SDL_opengl.h:1065
   GL_RGBA12_EXT : constant := 16#805A#;  --  ../include/SDL/SDL_opengl.h:1066
   GL_RGBA16_EXT : constant := 16#805B#;  --  ../include/SDL/SDL_opengl.h:1067
   GL_TEXTURE_RED_SIZE_EXT : constant := 16#805C#;  --  ../include/SDL/SDL_opengl.h:1068
   GL_TEXTURE_GREEN_SIZE_EXT : constant := 16#805D#;  --  ../include/SDL/SDL_opengl.h:1069
   GL_TEXTURE_BLUE_SIZE_EXT : constant := 16#805E#;  --  ../include/SDL/SDL_opengl.h:1070
   GL_TEXTURE_ALPHA_SIZE_EXT : constant := 16#805F#;  --  ../include/SDL/SDL_opengl.h:1071
   GL_TEXTURE_LUMINANCE_SIZE_EXT : constant := 16#8060#;  --  ../include/SDL/SDL_opengl.h:1072
   GL_TEXTURE_INTENSITY_SIZE_EXT : constant := 16#8061#;  --  ../include/SDL/SDL_opengl.h:1073
   GL_REPLACE_EXT : constant := 16#8062#;  --  ../include/SDL/SDL_opengl.h:1074
   GL_PROXY_TEXTURE_1D_EXT : constant := 16#8063#;  --  ../include/SDL/SDL_opengl.h:1075
   GL_PROXY_TEXTURE_2D_EXT : constant := 16#8064#;  --  ../include/SDL/SDL_opengl.h:1076
   GL_TEXTURE_TOO_LARGE_EXT : constant := 16#8065#;  --  ../include/SDL/SDL_opengl.h:1077

   GL_PACK_SKIP_IMAGES_EXT : constant := 16#806B#;  --  ../include/SDL/SDL_opengl.h:1081
   GL_PACK_IMAGE_HEIGHT_EXT : constant := 16#806C#;  --  ../include/SDL/SDL_opengl.h:1082
   GL_UNPACK_SKIP_IMAGES_EXT : constant := 16#806D#;  --  ../include/SDL/SDL_opengl.h:1083
   GL_UNPACK_IMAGE_HEIGHT_EXT : constant := 16#806E#;  --  ../include/SDL/SDL_opengl.h:1084
   GL_TEXTURE_3D_EXT : constant := 16#806F#;  --  ../include/SDL/SDL_opengl.h:1085
   GL_PROXY_TEXTURE_3D_EXT : constant := 16#8070#;  --  ../include/SDL/SDL_opengl.h:1086
   GL_TEXTURE_DEPTH_EXT : constant := 16#8071#;  --  ../include/SDL/SDL_opengl.h:1087
   GL_TEXTURE_WRAP_R_EXT : constant := 16#8072#;  --  ../include/SDL/SDL_opengl.h:1088
   GL_MAX_3D_TEXTURE_SIZE_EXT : constant := 16#8073#;  --  ../include/SDL/SDL_opengl.h:1089

   GL_FILTER4_SGIS : constant := 16#8146#;  --  ../include/SDL/SDL_opengl.h:1093
   GL_TEXTURE_FILTER4_SIZE_SGIS : constant := 16#8147#;  --  ../include/SDL/SDL_opengl.h:1094

   GL_HISTOGRAM_EXT : constant := 16#8024#;  --  ../include/SDL/SDL_opengl.h:1104
   GL_PROXY_HISTOGRAM_EXT : constant := 16#8025#;  --  ../include/SDL/SDL_opengl.h:1105
   GL_HISTOGRAM_WIDTH_EXT : constant := 16#8026#;  --  ../include/SDL/SDL_opengl.h:1106
   GL_HISTOGRAM_FORMAT_EXT : constant := 16#8027#;  --  ../include/SDL/SDL_opengl.h:1107
   GL_HISTOGRAM_RED_SIZE_EXT : constant := 16#8028#;  --  ../include/SDL/SDL_opengl.h:1108
   GL_HISTOGRAM_GREEN_SIZE_EXT : constant := 16#8029#;  --  ../include/SDL/SDL_opengl.h:1109
   GL_HISTOGRAM_BLUE_SIZE_EXT : constant := 16#802A#;  --  ../include/SDL/SDL_opengl.h:1110
   GL_HISTOGRAM_ALPHA_SIZE_EXT : constant := 16#802B#;  --  ../include/SDL/SDL_opengl.h:1111
   GL_HISTOGRAM_LUMINANCE_SIZE_EXT : constant := 16#802C#;  --  ../include/SDL/SDL_opengl.h:1112
   GL_HISTOGRAM_SINK_EXT : constant := 16#802D#;  --  ../include/SDL/SDL_opengl.h:1113
   GL_MINMAX_EXT : constant := 16#802E#;  --  ../include/SDL/SDL_opengl.h:1114
   GL_MINMAX_FORMAT_EXT : constant := 16#802F#;  --  ../include/SDL/SDL_opengl.h:1115
   GL_MINMAX_SINK_EXT : constant := 16#8030#;  --  ../include/SDL/SDL_opengl.h:1116
   GL_TABLE_TOO_LARGE_EXT : constant := 16#8031#;  --  ../include/SDL/SDL_opengl.h:1117

   GL_CONVOLUTION_1D_EXT : constant := 16#8010#;  --  ../include/SDL/SDL_opengl.h:1121
   GL_CONVOLUTION_2D_EXT : constant := 16#8011#;  --  ../include/SDL/SDL_opengl.h:1122
   GL_SEPARABLE_2D_EXT : constant := 16#8012#;  --  ../include/SDL/SDL_opengl.h:1123
   GL_CONVOLUTION_BORDER_MODE_EXT : constant := 16#8013#;  --  ../include/SDL/SDL_opengl.h:1124
   GL_CONVOLUTION_FILTER_SCALE_EXT : constant := 16#8014#;  --  ../include/SDL/SDL_opengl.h:1125
   GL_CONVOLUTION_FILTER_BIAS_EXT : constant := 16#8015#;  --  ../include/SDL/SDL_opengl.h:1126
   GL_REDUCE_EXT : constant := 16#8016#;  --  ../include/SDL/SDL_opengl.h:1127
   GL_CONVOLUTION_FORMAT_EXT : constant := 16#8017#;  --  ../include/SDL/SDL_opengl.h:1128
   GL_CONVOLUTION_WIDTH_EXT : constant := 16#8018#;  --  ../include/SDL/SDL_opengl.h:1129
   GL_CONVOLUTION_HEIGHT_EXT : constant := 16#8019#;  --  ../include/SDL/SDL_opengl.h:1130
   GL_MAX_CONVOLUTION_WIDTH_EXT : constant := 16#801A#;  --  ../include/SDL/SDL_opengl.h:1131
   GL_MAX_CONVOLUTION_HEIGHT_EXT : constant := 16#801B#;  --  ../include/SDL/SDL_opengl.h:1132
   GL_POST_CONVOLUTION_RED_SCALE_EXT : constant := 16#801C#;  --  ../include/SDL/SDL_opengl.h:1133
   GL_POST_CONVOLUTION_GREEN_SCALE_EXT : constant := 16#801D#;  --  ../include/SDL/SDL_opengl.h:1134
   GL_POST_CONVOLUTION_BLUE_SCALE_EXT : constant := 16#801E#;  --  ../include/SDL/SDL_opengl.h:1135
   GL_POST_CONVOLUTION_ALPHA_SCALE_EXT : constant := 16#801F#;  --  ../include/SDL/SDL_opengl.h:1136
   GL_POST_CONVOLUTION_RED_BIAS_EXT : constant := 16#8020#;  --  ../include/SDL/SDL_opengl.h:1137
   GL_POST_CONVOLUTION_GREEN_BIAS_EXT : constant := 16#8021#;  --  ../include/SDL/SDL_opengl.h:1138
   GL_POST_CONVOLUTION_BLUE_BIAS_EXT : constant := 16#8022#;  --  ../include/SDL/SDL_opengl.h:1139
   GL_POST_CONVOLUTION_ALPHA_BIAS_EXT : constant := 16#8023#;  --  ../include/SDL/SDL_opengl.h:1140

   GL_COLOR_MATRIX_SGI : constant := 16#80B1#;  --  ../include/SDL/SDL_opengl.h:1144
   GL_COLOR_MATRIX_STACK_DEPTH_SGI : constant := 16#80B2#;  --  ../include/SDL/SDL_opengl.h:1145
   GL_MAX_COLOR_MATRIX_STACK_DEPTH_SGI : constant := 16#80B3#;  --  ../include/SDL/SDL_opengl.h:1146
   GL_POST_COLOR_MATRIX_RED_SCALE_SGI : constant := 16#80B4#;  --  ../include/SDL/SDL_opengl.h:1147
   GL_POST_COLOR_MATRIX_GREEN_SCALE_SGI : constant := 16#80B5#;  --  ../include/SDL/SDL_opengl.h:1148
   GL_POST_COLOR_MATRIX_BLUE_SCALE_SGI : constant := 16#80B6#;  --  ../include/SDL/SDL_opengl.h:1149
   GL_POST_COLOR_MATRIX_ALPHA_SCALE_SGI : constant := 16#80B7#;  --  ../include/SDL/SDL_opengl.h:1150
   GL_POST_COLOR_MATRIX_RED_BIAS_SGI : constant := 16#80B8#;  --  ../include/SDL/SDL_opengl.h:1151
   GL_POST_COLOR_MATRIX_GREEN_BIAS_SGI : constant := 16#80B9#;  --  ../include/SDL/SDL_opengl.h:1152
   GL_POST_COLOR_MATRIX_BLUE_BIAS_SGI : constant := 16#80BA#;  --  ../include/SDL/SDL_opengl.h:1153
   GL_POST_COLOR_MATRIX_ALPHA_BIAS_SGI : constant := 16#80BB#;  --  ../include/SDL/SDL_opengl.h:1154

   GL_COLOR_TABLE_SGI : constant := 16#80D0#;  --  ../include/SDL/SDL_opengl.h:1158
   GL_POST_CONVOLUTION_COLOR_TABLE_SGI : constant := 16#80D1#;  --  ../include/SDL/SDL_opengl.h:1159
   GL_POST_COLOR_MATRIX_COLOR_TABLE_SGI : constant := 16#80D2#;  --  ../include/SDL/SDL_opengl.h:1160
   GL_PROXY_COLOR_TABLE_SGI : constant := 16#80D3#;  --  ../include/SDL/SDL_opengl.h:1161
   GL_PROXY_POST_CONVOLUTION_COLOR_TABLE_SGI : constant := 16#80D4#;  --  ../include/SDL/SDL_opengl.h:1162
   GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE_SGI : constant := 16#80D5#;  --  ../include/SDL/SDL_opengl.h:1163
   GL_COLOR_TABLE_SCALE_SGI : constant := 16#80D6#;  --  ../include/SDL/SDL_opengl.h:1164
   GL_COLOR_TABLE_BIAS_SGI : constant := 16#80D7#;  --  ../include/SDL/SDL_opengl.h:1165
   GL_COLOR_TABLE_FORMAT_SGI : constant := 16#80D8#;  --  ../include/SDL/SDL_opengl.h:1166
   GL_COLOR_TABLE_WIDTH_SGI : constant := 16#80D9#;  --  ../include/SDL/SDL_opengl.h:1167
   GL_COLOR_TABLE_RED_SIZE_SGI : constant := 16#80DA#;  --  ../include/SDL/SDL_opengl.h:1168
   GL_COLOR_TABLE_GREEN_SIZE_SGI : constant := 16#80DB#;  --  ../include/SDL/SDL_opengl.h:1169
   GL_COLOR_TABLE_BLUE_SIZE_SGI : constant := 16#80DC#;  --  ../include/SDL/SDL_opengl.h:1170
   GL_COLOR_TABLE_ALPHA_SIZE_SGI : constant := 16#80DD#;  --  ../include/SDL/SDL_opengl.h:1171
   GL_COLOR_TABLE_LUMINANCE_SIZE_SGI : constant := 16#80DE#;  --  ../include/SDL/SDL_opengl.h:1172
   GL_COLOR_TABLE_INTENSITY_SIZE_SGI : constant := 16#80DF#;  --  ../include/SDL/SDL_opengl.h:1173

   GL_PIXEL_TEXTURE_SGIS : constant := 16#8353#;  --  ../include/SDL/SDL_opengl.h:1177
   GL_PIXEL_FRAGMENT_RGB_SOURCE_SGIS : constant := 16#8354#;  --  ../include/SDL/SDL_opengl.h:1178
   GL_PIXEL_FRAGMENT_ALPHA_SOURCE_SGIS : constant := 16#8355#;  --  ../include/SDL/SDL_opengl.h:1179
   GL_PIXEL_GROUP_COLOR_SGIS : constant := 16#8356#;  --  ../include/SDL/SDL_opengl.h:1180

   GL_PIXEL_TEX_GEN_SGIX : constant := 16#8139#;  --  ../include/SDL/SDL_opengl.h:1184
   GL_PIXEL_TEX_GEN_MODE_SGIX : constant := 16#832B#;  --  ../include/SDL/SDL_opengl.h:1185

   GL_PACK_SKIP_VOLUMES_SGIS : constant := 16#8130#;  --  ../include/SDL/SDL_opengl.h:1189
   GL_PACK_IMAGE_DEPTH_SGIS : constant := 16#8131#;  --  ../include/SDL/SDL_opengl.h:1190
   GL_UNPACK_SKIP_VOLUMES_SGIS : constant := 16#8132#;  --  ../include/SDL/SDL_opengl.h:1191
   GL_UNPACK_IMAGE_DEPTH_SGIS : constant := 16#8133#;  --  ../include/SDL/SDL_opengl.h:1192
   GL_TEXTURE_4D_SGIS : constant := 16#8134#;  --  ../include/SDL/SDL_opengl.h:1193
   GL_PROXY_TEXTURE_4D_SGIS : constant := 16#8135#;  --  ../include/SDL/SDL_opengl.h:1194
   GL_TEXTURE_4DSIZE_SGIS : constant := 16#8136#;  --  ../include/SDL/SDL_opengl.h:1195
   GL_TEXTURE_WRAP_Q_SGIS : constant := 16#8137#;  --  ../include/SDL/SDL_opengl.h:1196
   GL_MAX_4D_TEXTURE_SIZE_SGIS : constant := 16#8138#;  --  ../include/SDL/SDL_opengl.h:1197
   GL_TEXTURE_4D_BINDING_SGIS : constant := 16#814F#;  --  ../include/SDL/SDL_opengl.h:1198

   GL_TEXTURE_COLOR_TABLE_SGI : constant := 16#80BC#;  --  ../include/SDL/SDL_opengl.h:1202
   GL_PROXY_TEXTURE_COLOR_TABLE_SGI : constant := 16#80BD#;  --  ../include/SDL/SDL_opengl.h:1203

   GL_CMYK_EXT : constant := 16#800C#;  --  ../include/SDL/SDL_opengl.h:1207
   GL_CMYKA_EXT : constant := 16#800D#;  --  ../include/SDL/SDL_opengl.h:1208
   GL_PACK_CMYK_HINT_EXT : constant := 16#800E#;  --  ../include/SDL/SDL_opengl.h:1209
   GL_UNPACK_CMYK_HINT_EXT : constant := 16#800F#;  --  ../include/SDL/SDL_opengl.h:1210

   GL_TEXTURE_PRIORITY_EXT : constant := 16#8066#;  --  ../include/SDL/SDL_opengl.h:1214
   GL_TEXTURE_RESIDENT_EXT : constant := 16#8067#;  --  ../include/SDL/SDL_opengl.h:1215
   GL_TEXTURE_1D_BINDING_EXT : constant := 16#8068#;  --  ../include/SDL/SDL_opengl.h:1216
   GL_TEXTURE_2D_BINDING_EXT : constant := 16#8069#;  --  ../include/SDL/SDL_opengl.h:1217
   GL_TEXTURE_3D_BINDING_EXT : constant := 16#806A#;  --  ../include/SDL/SDL_opengl.h:1218

   GL_DETAIL_TEXTURE_2D_SGIS : constant := 16#8095#;  --  ../include/SDL/SDL_opengl.h:1222
   GL_DETAIL_TEXTURE_2D_BINDING_SGIS : constant := 16#8096#;  --  ../include/SDL/SDL_opengl.h:1223
   GL_LINEAR_DETAIL_SGIS : constant := 16#8097#;  --  ../include/SDL/SDL_opengl.h:1224
   GL_LINEAR_DETAIL_ALPHA_SGIS : constant := 16#8098#;  --  ../include/SDL/SDL_opengl.h:1225
   GL_LINEAR_DETAIL_COLOR_SGIS : constant := 16#8099#;  --  ../include/SDL/SDL_opengl.h:1226
   GL_DETAIL_TEXTURE_LEVEL_SGIS : constant := 16#809A#;  --  ../include/SDL/SDL_opengl.h:1227
   GL_DETAIL_TEXTURE_MODE_SGIS : constant := 16#809B#;  --  ../include/SDL/SDL_opengl.h:1228
   GL_DETAIL_TEXTURE_FUNC_POINTS_SGIS : constant := 16#809C#;  --  ../include/SDL/SDL_opengl.h:1229

   GL_LINEAR_SHARPEN_SGIS : constant := 16#80AD#;  --  ../include/SDL/SDL_opengl.h:1233
   GL_LINEAR_SHARPEN_ALPHA_SGIS : constant := 16#80AE#;  --  ../include/SDL/SDL_opengl.h:1234
   GL_LINEAR_SHARPEN_COLOR_SGIS : constant := 16#80AF#;  --  ../include/SDL/SDL_opengl.h:1235
   GL_SHARPEN_TEXTURE_FUNC_POINTS_SGIS : constant := 16#80B0#;  --  ../include/SDL/SDL_opengl.h:1236

   GL_UNSIGNED_BYTE_3_3_2_EXT : constant := 16#8032#;  --  ../include/SDL/SDL_opengl.h:1240
   GL_UNSIGNED_SHORT_4_4_4_4_EXT : constant := 16#8033#;  --  ../include/SDL/SDL_opengl.h:1241
   GL_UNSIGNED_SHORT_5_5_5_1_EXT : constant := 16#8034#;  --  ../include/SDL/SDL_opengl.h:1242
   GL_UNSIGNED_INT_8_8_8_8_EXT : constant := 16#8035#;  --  ../include/SDL/SDL_opengl.h:1243
   GL_UNSIGNED_INT_10_10_10_2_EXT : constant := 16#8036#;  --  ../include/SDL/SDL_opengl.h:1244

   GL_TEXTURE_MIN_LOD_SGIS : constant := 16#813A#;  --  ../include/SDL/SDL_opengl.h:1248
   GL_TEXTURE_MAX_LOD_SGIS : constant := 16#813B#;  --  ../include/SDL/SDL_opengl.h:1249
   GL_TEXTURE_BASE_LEVEL_SGIS : constant := 16#813C#;  --  ../include/SDL/SDL_opengl.h:1250
   GL_TEXTURE_MAX_LEVEL_SGIS : constant := 16#813D#;  --  ../include/SDL/SDL_opengl.h:1251

   GL_MULTISAMPLE_SGIS : constant := 16#809D#;  --  ../include/SDL/SDL_opengl.h:1255
   GL_SAMPLE_ALPHA_TO_MASK_SGIS : constant := 16#809E#;  --  ../include/SDL/SDL_opengl.h:1256
   GL_SAMPLE_ALPHA_TO_ONE_SGIS : constant := 16#809F#;  --  ../include/SDL/SDL_opengl.h:1257
   GL_SAMPLE_MASK_SGIS : constant := 16#80A0#;  --  ../include/SDL/SDL_opengl.h:1258
   GL_1PASS_SGIS : constant := 16#80A1#;  --  ../include/SDL/SDL_opengl.h:1259
   GL_2PASS_0_SGIS : constant := 16#80A2#;  --  ../include/SDL/SDL_opengl.h:1260
   GL_2PASS_1_SGIS : constant := 16#80A3#;  --  ../include/SDL/SDL_opengl.h:1261
   GL_4PASS_0_SGIS : constant := 16#80A4#;  --  ../include/SDL/SDL_opengl.h:1262
   GL_4PASS_1_SGIS : constant := 16#80A5#;  --  ../include/SDL/SDL_opengl.h:1263
   GL_4PASS_2_SGIS : constant := 16#80A6#;  --  ../include/SDL/SDL_opengl.h:1264
   GL_4PASS_3_SGIS : constant := 16#80A7#;  --  ../include/SDL/SDL_opengl.h:1265
   GL_SAMPLE_BUFFERS_SGIS : constant := 16#80A8#;  --  ../include/SDL/SDL_opengl.h:1266
   GL_SAMPLES_SGIS : constant := 16#80A9#;  --  ../include/SDL/SDL_opengl.h:1267
   GL_SAMPLE_MASK_VALUE_SGIS : constant := 16#80AA#;  --  ../include/SDL/SDL_opengl.h:1268
   GL_SAMPLE_MASK_INVERT_SGIS : constant := 16#80AB#;  --  ../include/SDL/SDL_opengl.h:1269
   GL_SAMPLE_PATTERN_SGIS : constant := 16#80AC#;  --  ../include/SDL/SDL_opengl.h:1270

   GL_RESCALE_NORMAL_EXT : constant := 16#803A#;  --  ../include/SDL/SDL_opengl.h:1274

   GL_GENERATE_MIPMAP_SGIS : constant := 16#8191#;  --  ../include/SDL/SDL_opengl.h:1316
   GL_GENERATE_MIPMAP_HINT_SGIS : constant := 16#8192#;  --  ../include/SDL/SDL_opengl.h:1317

   GL_LINEAR_CLIPMAP_LINEAR_SGIX : constant := 16#8170#;  --  ../include/SDL/SDL_opengl.h:1321
   GL_TEXTURE_CLIPMAP_CENTER_SGIX : constant := 16#8171#;  --  ../include/SDL/SDL_opengl.h:1322
   GL_TEXTURE_CLIPMAP_FRAME_SGIX : constant := 16#8172#;  --  ../include/SDL/SDL_opengl.h:1323
   GL_TEXTURE_CLIPMAP_OFFSET_SGIX : constant := 16#8173#;  --  ../include/SDL/SDL_opengl.h:1324
   GL_TEXTURE_CLIPMAP_VIRTUAL_DEPTH_SGIX : constant := 16#8174#;  --  ../include/SDL/SDL_opengl.h:1325
   GL_TEXTURE_CLIPMAP_LOD_OFFSET_SGIX : constant := 16#8175#;  --  ../include/SDL/SDL_opengl.h:1326
   GL_TEXTURE_CLIPMAP_DEPTH_SGIX : constant := 16#8176#;  --  ../include/SDL/SDL_opengl.h:1327
   GL_MAX_CLIPMAP_DEPTH_SGIX : constant := 16#8177#;  --  ../include/SDL/SDL_opengl.h:1328
   GL_MAX_CLIPMAP_VIRTUAL_DEPTH_SGIX : constant := 16#8178#;  --  ../include/SDL/SDL_opengl.h:1329
   GL_NEAREST_CLIPMAP_NEAREST_SGIX : constant := 16#844D#;  --  ../include/SDL/SDL_opengl.h:1330
   GL_NEAREST_CLIPMAP_LINEAR_SGIX : constant := 16#844E#;  --  ../include/SDL/SDL_opengl.h:1331
   GL_LINEAR_CLIPMAP_NEAREST_SGIX : constant := 16#844F#;  --  ../include/SDL/SDL_opengl.h:1332

   GL_TEXTURE_COMPARE_SGIX : constant := 16#819A#;  --  ../include/SDL/SDL_opengl.h:1336
   GL_TEXTURE_COMPARE_OPERATOR_SGIX : constant := 16#819B#;  --  ../include/SDL/SDL_opengl.h:1337
   GL_TEXTURE_LEQUAL_R_SGIX : constant := 16#819C#;  --  ../include/SDL/SDL_opengl.h:1338
   GL_TEXTURE_GEQUAL_R_SGIX : constant := 16#819D#;  --  ../include/SDL/SDL_opengl.h:1339

   GL_CLAMP_TO_EDGE_SGIS : constant := 16#812F#;  --  ../include/SDL/SDL_opengl.h:1343

   GL_CLAMP_TO_BORDER_SGIS : constant := 16#812D#;  --  ../include/SDL/SDL_opengl.h:1347

   GL_FUNC_ADD_EXT : constant := 16#8006#;  --  ../include/SDL/SDL_opengl.h:1351
   GL_MIN_EXT : constant := 16#8007#;  --  ../include/SDL/SDL_opengl.h:1352
   GL_MAX_EXT : constant := 16#8008#;  --  ../include/SDL/SDL_opengl.h:1353
   GL_BLEND_EQUATION_EXT : constant := 16#8009#;  --  ../include/SDL/SDL_opengl.h:1354

   GL_FUNC_SUBTRACT_EXT : constant := 16#800A#;  --  ../include/SDL/SDL_opengl.h:1358
   GL_FUNC_REVERSE_SUBTRACT_EXT : constant := 16#800B#;  --  ../include/SDL/SDL_opengl.h:1359

   GL_INTERLACE_SGIX : constant := 16#8094#;  --  ../include/SDL/SDL_opengl.h:1366

   GL_PIXEL_TILE_BEST_ALIGNMENT_SGIX : constant := 16#813E#;  --  ../include/SDL/SDL_opengl.h:1370
   GL_PIXEL_TILE_CACHE_INCREMENT_SGIX : constant := 16#813F#;  --  ../include/SDL/SDL_opengl.h:1371
   GL_PIXEL_TILE_WIDTH_SGIX : constant := 16#8140#;  --  ../include/SDL/SDL_opengl.h:1372
   GL_PIXEL_TILE_HEIGHT_SGIX : constant := 16#8141#;  --  ../include/SDL/SDL_opengl.h:1373
   GL_PIXEL_TILE_GRID_WIDTH_SGIX : constant := 16#8142#;  --  ../include/SDL/SDL_opengl.h:1374
   GL_PIXEL_TILE_GRID_HEIGHT_SGIX : constant := 16#8143#;  --  ../include/SDL/SDL_opengl.h:1375
   GL_PIXEL_TILE_GRID_DEPTH_SGIX : constant := 16#8144#;  --  ../include/SDL/SDL_opengl.h:1376
   GL_PIXEL_TILE_CACHE_SIZE_SGIX : constant := 16#8145#;  --  ../include/SDL/SDL_opengl.h:1377

   GL_DUAL_ALPHA4_SGIS : constant := 16#8110#;  --  ../include/SDL/SDL_opengl.h:1381
   GL_DUAL_ALPHA8_SGIS : constant := 16#8111#;  --  ../include/SDL/SDL_opengl.h:1382
   GL_DUAL_ALPHA12_SGIS : constant := 16#8112#;  --  ../include/SDL/SDL_opengl.h:1383
   GL_DUAL_ALPHA16_SGIS : constant := 16#8113#;  --  ../include/SDL/SDL_opengl.h:1384
   GL_DUAL_LUMINANCE4_SGIS : constant := 16#8114#;  --  ../include/SDL/SDL_opengl.h:1385
   GL_DUAL_LUMINANCE8_SGIS : constant := 16#8115#;  --  ../include/SDL/SDL_opengl.h:1386
   GL_DUAL_LUMINANCE12_SGIS : constant := 16#8116#;  --  ../include/SDL/SDL_opengl.h:1387
   GL_DUAL_LUMINANCE16_SGIS : constant := 16#8117#;  --  ../include/SDL/SDL_opengl.h:1388
   GL_DUAL_INTENSITY4_SGIS : constant := 16#8118#;  --  ../include/SDL/SDL_opengl.h:1389
   GL_DUAL_INTENSITY8_SGIS : constant := 16#8119#;  --  ../include/SDL/SDL_opengl.h:1390
   GL_DUAL_INTENSITY12_SGIS : constant := 16#811A#;  --  ../include/SDL/SDL_opengl.h:1391
   GL_DUAL_INTENSITY16_SGIS : constant := 16#811B#;  --  ../include/SDL/SDL_opengl.h:1392
   GL_DUAL_LUMINANCE_ALPHA4_SGIS : constant := 16#811C#;  --  ../include/SDL/SDL_opengl.h:1393
   GL_DUAL_LUMINANCE_ALPHA8_SGIS : constant := 16#811D#;  --  ../include/SDL/SDL_opengl.h:1394
   GL_QUAD_ALPHA4_SGIS : constant := 16#811E#;  --  ../include/SDL/SDL_opengl.h:1395
   GL_QUAD_ALPHA8_SGIS : constant := 16#811F#;  --  ../include/SDL/SDL_opengl.h:1396
   GL_QUAD_LUMINANCE4_SGIS : constant := 16#8120#;  --  ../include/SDL/SDL_opengl.h:1397
   GL_QUAD_LUMINANCE8_SGIS : constant := 16#8121#;  --  ../include/SDL/SDL_opengl.h:1398
   GL_QUAD_INTENSITY4_SGIS : constant := 16#8122#;  --  ../include/SDL/SDL_opengl.h:1399
   GL_QUAD_INTENSITY8_SGIS : constant := 16#8123#;  --  ../include/SDL/SDL_opengl.h:1400
   GL_DUAL_TEXTURE_SELECT_SGIS : constant := 16#8124#;  --  ../include/SDL/SDL_opengl.h:1401
   GL_QUAD_TEXTURE_SELECT_SGIS : constant := 16#8125#;  --  ../include/SDL/SDL_opengl.h:1402

   GL_SPRITE_SGIX : constant := 16#8148#;  --  ../include/SDL/SDL_opengl.h:1406
   GL_SPRITE_MODE_SGIX : constant := 16#8149#;  --  ../include/SDL/SDL_opengl.h:1407
   GL_SPRITE_AXIS_SGIX : constant := 16#814A#;  --  ../include/SDL/SDL_opengl.h:1408
   GL_SPRITE_TRANSLATION_SGIX : constant := 16#814B#;  --  ../include/SDL/SDL_opengl.h:1409
   GL_SPRITE_AXIAL_SGIX : constant := 16#814C#;  --  ../include/SDL/SDL_opengl.h:1410
   GL_SPRITE_OBJECT_ALIGNED_SGIX : constant := 16#814D#;  --  ../include/SDL/SDL_opengl.h:1411
   GL_SPRITE_EYE_ALIGNED_SGIX : constant := 16#814E#;  --  ../include/SDL/SDL_opengl.h:1412

   GL_TEXTURE_MULTI_BUFFER_HINT_SGIX : constant := 16#812E#;  --  ../include/SDL/SDL_opengl.h:1416

   GL_POINT_SIZE_MIN_EXT : constant := 16#8126#;  --  ../include/SDL/SDL_opengl.h:1420
   GL_POINT_SIZE_MAX_EXT : constant := 16#8127#;  --  ../include/SDL/SDL_opengl.h:1421
   GL_POINT_FADE_THRESHOLD_SIZE_EXT : constant := 16#8128#;  --  ../include/SDL/SDL_opengl.h:1422
   GL_DISTANCE_ATTENUATION_EXT : constant := 16#8129#;  --  ../include/SDL/SDL_opengl.h:1423

   GL_POINT_SIZE_MIN_SGIS : constant := 16#8126#;  --  ../include/SDL/SDL_opengl.h:1427
   GL_POINT_SIZE_MAX_SGIS : constant := 16#8127#;  --  ../include/SDL/SDL_opengl.h:1428
   GL_POINT_FADE_THRESHOLD_SIZE_SGIS : constant := 16#8128#;  --  ../include/SDL/SDL_opengl.h:1429
   GL_DISTANCE_ATTENUATION_SGIS : constant := 16#8129#;  --  ../include/SDL/SDL_opengl.h:1430

   GL_INSTRUMENT_BUFFER_POINTER_SGIX : constant := 16#8180#;  --  ../include/SDL/SDL_opengl.h:1434
   GL_INSTRUMENT_MEASUREMENTS_SGIX : constant := 16#8181#;  --  ../include/SDL/SDL_opengl.h:1435

   GL_POST_TEXTURE_FILTER_BIAS_SGIX : constant := 16#8179#;  --  ../include/SDL/SDL_opengl.h:1439
   GL_POST_TEXTURE_FILTER_SCALE_SGIX : constant := 16#817A#;  --  ../include/SDL/SDL_opengl.h:1440
   GL_POST_TEXTURE_FILTER_BIAS_RANGE_SGIX : constant := 16#817B#;  --  ../include/SDL/SDL_opengl.h:1441
   GL_POST_TEXTURE_FILTER_SCALE_RANGE_SGIX : constant := 16#817C#;  --  ../include/SDL/SDL_opengl.h:1442

   GL_FRAMEZOOM_SGIX : constant := 16#818B#;  --  ../include/SDL/SDL_opengl.h:1446
   GL_FRAMEZOOM_FACTOR_SGIX : constant := 16#818C#;  --  ../include/SDL/SDL_opengl.h:1447
   GL_MAX_FRAMEZOOM_FACTOR_SGIX : constant := 16#818D#;  --  ../include/SDL/SDL_opengl.h:1448

   GL_TEXTURE_DEFORMATION_BIT_SGIX : constant := 16#00000001#;  --  ../include/SDL/SDL_opengl.h:1455
   GL_GEOMETRY_DEFORMATION_BIT_SGIX : constant := 16#00000002#;  --  ../include/SDL/SDL_opengl.h:1456

   GL_GEOMETRY_DEFORMATION_SGIX : constant := 16#8194#;  --  ../include/SDL/SDL_opengl.h:1460
   GL_TEXTURE_DEFORMATION_SGIX : constant := 16#8195#;  --  ../include/SDL/SDL_opengl.h:1461
   GL_DEFORMATIONS_MASK_SGIX : constant := 16#8196#;  --  ../include/SDL/SDL_opengl.h:1462
   GL_MAX_DEFORMATION_ORDER_SGIX : constant := 16#8197#;  --  ../include/SDL/SDL_opengl.h:1463

   GL_REFERENCE_PLANE_SGIX : constant := 16#817D#;  --  ../include/SDL/SDL_opengl.h:1467
   GL_REFERENCE_PLANE_EQUATION_SGIX : constant := 16#817E#;  --  ../include/SDL/SDL_opengl.h:1468

   GL_DEPTH_COMPONENT16_SGIX : constant := 16#81A5#;  --  ../include/SDL/SDL_opengl.h:1475
   GL_DEPTH_COMPONENT24_SGIX : constant := 16#81A6#;  --  ../include/SDL/SDL_opengl.h:1476
   GL_DEPTH_COMPONENT32_SGIX : constant := 16#81A7#;  --  ../include/SDL/SDL_opengl.h:1477

   GL_FOG_FUNC_SGIS : constant := 16#812A#;  --  ../include/SDL/SDL_opengl.h:1481
   GL_FOG_FUNC_POINTS_SGIS : constant := 16#812B#;  --  ../include/SDL/SDL_opengl.h:1482
   GL_MAX_FOG_FUNC_POINTS_SGIS : constant := 16#812C#;  --  ../include/SDL/SDL_opengl.h:1483

   GL_FOG_OFFSET_SGIX : constant := 16#8198#;  --  ../include/SDL/SDL_opengl.h:1487
   GL_FOG_OFFSET_VALUE_SGIX : constant := 16#8199#;  --  ../include/SDL/SDL_opengl.h:1488

   GL_IMAGE_SCALE_X_HP : constant := 16#8155#;  --  ../include/SDL/SDL_opengl.h:1492
   GL_IMAGE_SCALE_Y_HP : constant := 16#8156#;  --  ../include/SDL/SDL_opengl.h:1493
   GL_IMAGE_TRANSLATE_X_HP : constant := 16#8157#;  --  ../include/SDL/SDL_opengl.h:1494
   GL_IMAGE_TRANSLATE_Y_HP : constant := 16#8158#;  --  ../include/SDL/SDL_opengl.h:1495
   GL_IMAGE_ROTATE_ANGLE_HP : constant := 16#8159#;  --  ../include/SDL/SDL_opengl.h:1496
   GL_IMAGE_ROTATE_ORIGIN_X_HP : constant := 16#815A#;  --  ../include/SDL/SDL_opengl.h:1497
   GL_IMAGE_ROTATE_ORIGIN_Y_HP : constant := 16#815B#;  --  ../include/SDL/SDL_opengl.h:1498
   GL_IMAGE_MAG_FILTER_HP : constant := 16#815C#;  --  ../include/SDL/SDL_opengl.h:1499
   GL_IMAGE_MIN_FILTER_HP : constant := 16#815D#;  --  ../include/SDL/SDL_opengl.h:1500
   GL_IMAGE_CUBIC_WEIGHT_HP : constant := 16#815E#;  --  ../include/SDL/SDL_opengl.h:1501
   GL_CUBIC_HP : constant := 16#815F#;  --  ../include/SDL/SDL_opengl.h:1502
   GL_AVERAGE_HP : constant := 16#8160#;  --  ../include/SDL/SDL_opengl.h:1503
   GL_IMAGE_TRANSFORM_2D_HP : constant := 16#8161#;  --  ../include/SDL/SDL_opengl.h:1504
   GL_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP : constant := 16#8162#;  --  ../include/SDL/SDL_opengl.h:1505
   GL_PROXY_POST_IMAGE_TRANSFORM_COLOR_TABLE_HP : constant := 16#8163#;  --  ../include/SDL/SDL_opengl.h:1506

   GL_IGNORE_BORDER_HP : constant := 16#8150#;  --  ../include/SDL/SDL_opengl.h:1510
   GL_CONSTANT_BORDER_HP : constant := 16#8151#;  --  ../include/SDL/SDL_opengl.h:1511
   GL_REPLICATE_BORDER_HP : constant := 16#8153#;  --  ../include/SDL/SDL_opengl.h:1512
   GL_CONVOLUTION_BORDER_COLOR_HP : constant := 16#8154#;  --  ../include/SDL/SDL_opengl.h:1513

   GL_TEXTURE_ENV_BIAS_SGIX : constant := 16#80BE#;  --  ../include/SDL/SDL_opengl.h:1520

   GL_VERTEX_DATA_HINT_PGI : constant := 16#1A22A#;  --  ../include/SDL/SDL_opengl.h:1527
   GL_VERTEX_CONSISTENT_HINT_PGI : constant := 16#1A22B#;  --  ../include/SDL/SDL_opengl.h:1528
   GL_MATERIAL_SIDE_HINT_PGI : constant := 16#1A22C#;  --  ../include/SDL/SDL_opengl.h:1529
   GL_MAX_VERTEX_HINT_PGI : constant := 16#1A22D#;  --  ../include/SDL/SDL_opengl.h:1530
   GL_COLOR3_BIT_PGI : constant := 16#00010000#;  --  ../include/SDL/SDL_opengl.h:1531
   GL_COLOR4_BIT_PGI : constant := 16#00020000#;  --  ../include/SDL/SDL_opengl.h:1532
   GL_EDGEFLAG_BIT_PGI : constant := 16#00040000#;  --  ../include/SDL/SDL_opengl.h:1533
   GL_INDEX_BIT_PGI : constant := 16#00080000#;  --  ../include/SDL/SDL_opengl.h:1534
   GL_MAT_AMBIENT_BIT_PGI : constant := 16#00100000#;  --  ../include/SDL/SDL_opengl.h:1535
   GL_MAT_AMBIENT_AND_DIFFUSE_BIT_PGI : constant := 16#00200000#;  --  ../include/SDL/SDL_opengl.h:1536
   GL_MAT_DIFFUSE_BIT_PGI : constant := 16#00400000#;  --  ../include/SDL/SDL_opengl.h:1537
   GL_MAT_EMISSION_BIT_PGI : constant := 16#00800000#;  --  ../include/SDL/SDL_opengl.h:1538
   GL_MAT_COLOR_INDEXES_BIT_PGI : constant := 16#01000000#;  --  ../include/SDL/SDL_opengl.h:1539
   GL_MAT_SHININESS_BIT_PGI : constant := 16#02000000#;  --  ../include/SDL/SDL_opengl.h:1540
   GL_MAT_SPECULAR_BIT_PGI : constant := 16#04000000#;  --  ../include/SDL/SDL_opengl.h:1541
   GL_NORMAL_BIT_PGI : constant := 16#08000000#;  --  ../include/SDL/SDL_opengl.h:1542
   GL_TEXCOORD1_BIT_PGI : constant := 16#10000000#;  --  ../include/SDL/SDL_opengl.h:1543
   GL_TEXCOORD2_BIT_PGI : constant := 16#20000000#;  --  ../include/SDL/SDL_opengl.h:1544
   GL_TEXCOORD3_BIT_PGI : constant := 16#40000000#;  --  ../include/SDL/SDL_opengl.h:1545
   GL_TEXCOORD4_BIT_PGI : constant := 16#80000000#;  --  ../include/SDL/SDL_opengl.h:1546
   GL_VERTEX23_BIT_PGI : constant := 16#00000004#;  --  ../include/SDL/SDL_opengl.h:1547
   GL_VERTEX4_BIT_PGI : constant := 16#00000008#;  --  ../include/SDL/SDL_opengl.h:1548

   GL_PREFER_DOUBLEBUFFER_HINT_PGI : constant := 16#1A1F8#;  --  ../include/SDL/SDL_opengl.h:1552
   GL_CONSERVE_MEMORY_HINT_PGI : constant := 16#1A1FD#;  --  ../include/SDL/SDL_opengl.h:1553
   GL_RECLAIM_MEMORY_HINT_PGI : constant := 16#1A1FE#;  --  ../include/SDL/SDL_opengl.h:1554
   GL_NATIVE_GRAPHICS_HANDLE_PGI : constant := 16#1A202#;  --  ../include/SDL/SDL_opengl.h:1555
   GL_NATIVE_GRAPHICS_BEGIN_HINT_PGI : constant := 16#1A203#;  --  ../include/SDL/SDL_opengl.h:1556
   GL_NATIVE_GRAPHICS_END_HINT_PGI : constant := 16#1A204#;  --  ../include/SDL/SDL_opengl.h:1557
   GL_ALWAYS_FAST_HINT_PGI : constant := 16#1A20C#;  --  ../include/SDL/SDL_opengl.h:1558
   GL_ALWAYS_SOFT_HINT_PGI : constant := 16#1A20D#;  --  ../include/SDL/SDL_opengl.h:1559
   GL_ALLOW_DRAW_OBJ_HINT_PGI : constant := 16#1A20E#;  --  ../include/SDL/SDL_opengl.h:1560
   GL_ALLOW_DRAW_WIN_HINT_PGI : constant := 16#1A20F#;  --  ../include/SDL/SDL_opengl.h:1561
   GL_ALLOW_DRAW_FRG_HINT_PGI : constant := 16#1A210#;  --  ../include/SDL/SDL_opengl.h:1562
   GL_ALLOW_DRAW_MEM_HINT_PGI : constant := 16#1A211#;  --  ../include/SDL/SDL_opengl.h:1563
   GL_STRICT_DEPTHFUNC_HINT_PGI : constant := 16#1A216#;  --  ../include/SDL/SDL_opengl.h:1564
   GL_STRICT_LIGHTING_HINT_PGI : constant := 16#1A217#;  --  ../include/SDL/SDL_opengl.h:1565
   GL_STRICT_SCISSOR_HINT_PGI : constant := 16#1A218#;  --  ../include/SDL/SDL_opengl.h:1566
   GL_FULL_STIPPLE_HINT_PGI : constant := 16#1A219#;  --  ../include/SDL/SDL_opengl.h:1567
   GL_CLIP_NEAR_HINT_PGI : constant := 16#1A220#;  --  ../include/SDL/SDL_opengl.h:1568
   GL_CLIP_FAR_HINT_PGI : constant := 16#1A221#;  --  ../include/SDL/SDL_opengl.h:1569
   GL_WIDE_LINE_HINT_PGI : constant := 16#1A222#;  --  ../include/SDL/SDL_opengl.h:1570
   GL_BACK_NORMALS_HINT_PGI : constant := 16#1A223#;  --  ../include/SDL/SDL_opengl.h:1571

   GL_CLIP_VOLUME_CLIPPING_HINT_EXT : constant := 16#80F0#;  --  ../include/SDL/SDL_opengl.h:1585

   GL_LIST_PRIORITY_SGIX : constant := 16#8182#;  --  ../include/SDL/SDL_opengl.h:1589

   GL_IR_INSTRUMENT1_SGIX : constant := 16#817F#;  --  ../include/SDL/SDL_opengl.h:1593

   GL_CALLIGRAPHIC_FRAGMENT_SGIX : constant := 16#8183#;  --  ../include/SDL/SDL_opengl.h:1597

   GL_TEXTURE_LOD_BIAS_S_SGIX : constant := 16#818E#;  --  ../include/SDL/SDL_opengl.h:1601
   GL_TEXTURE_LOD_BIAS_T_SGIX : constant := 16#818F#;  --  ../include/SDL/SDL_opengl.h:1602
   GL_TEXTURE_LOD_BIAS_R_SGIX : constant := 16#8190#;  --  ../include/SDL/SDL_opengl.h:1603

   GL_SHADOW_AMBIENT_SGIX : constant := 16#80BF#;  --  ../include/SDL/SDL_opengl.h:1607

   GL_INDEX_MATERIAL_EXT : constant := 16#81B8#;  --  ../include/SDL/SDL_opengl.h:1614
   GL_INDEX_MATERIAL_PARAMETER_EXT : constant := 16#81B9#;  --  ../include/SDL/SDL_opengl.h:1615
   GL_INDEX_MATERIAL_FACE_EXT : constant := 16#81BA#;  --  ../include/SDL/SDL_opengl.h:1616

   GL_INDEX_TEST_EXT : constant := 16#81B5#;  --  ../include/SDL/SDL_opengl.h:1620
   GL_INDEX_TEST_FUNC_EXT : constant := 16#81B6#;  --  ../include/SDL/SDL_opengl.h:1621
   GL_INDEX_TEST_REF_EXT : constant := 16#81B7#;  --  ../include/SDL/SDL_opengl.h:1622

   GL_IUI_V2F_EXT : constant := 16#81AD#;  --  ../include/SDL/SDL_opengl.h:1626
   GL_IUI_V3F_EXT : constant := 16#81AE#;  --  ../include/SDL/SDL_opengl.h:1627
   GL_IUI_N3F_V2F_EXT : constant := 16#81AF#;  --  ../include/SDL/SDL_opengl.h:1628
   GL_IUI_N3F_V3F_EXT : constant := 16#81B0#;  --  ../include/SDL/SDL_opengl.h:1629
   GL_T2F_IUI_V2F_EXT : constant := 16#81B1#;  --  ../include/SDL/SDL_opengl.h:1630
   GL_T2F_IUI_V3F_EXT : constant := 16#81B2#;  --  ../include/SDL/SDL_opengl.h:1631
   GL_T2F_IUI_N3F_V2F_EXT : constant := 16#81B3#;  --  ../include/SDL/SDL_opengl.h:1632
   GL_T2F_IUI_N3F_V3F_EXT : constant := 16#81B4#;  --  ../include/SDL/SDL_opengl.h:1633

   GL_ARRAY_ELEMENT_LOCK_FIRST_EXT : constant := 16#81A8#;  --  ../include/SDL/SDL_opengl.h:1637
   GL_ARRAY_ELEMENT_LOCK_COUNT_EXT : constant := 16#81A9#;  --  ../include/SDL/SDL_opengl.h:1638

   GL_CULL_VERTEX_EXT : constant := 16#81AA#;  --  ../include/SDL/SDL_opengl.h:1642
   GL_CULL_VERTEX_EYE_POSITION_EXT : constant := 16#81AB#;  --  ../include/SDL/SDL_opengl.h:1643
   GL_CULL_VERTEX_OBJECT_POSITION_EXT : constant := 16#81AC#;  --  ../include/SDL/SDL_opengl.h:1644

   GL_YCRCB_422_SGIX : constant := 16#81BB#;  --  ../include/SDL/SDL_opengl.h:1648
   GL_YCRCB_444_SGIX : constant := 16#81BC#;  --  ../include/SDL/SDL_opengl.h:1649

   GL_FRAGMENT_LIGHTING_SGIX : constant := 16#8400#;  --  ../include/SDL/SDL_opengl.h:1653
   GL_FRAGMENT_COLOR_MATERIAL_SGIX : constant := 16#8401#;  --  ../include/SDL/SDL_opengl.h:1654
   GL_FRAGMENT_COLOR_MATERIAL_FACE_SGIX : constant := 16#8402#;  --  ../include/SDL/SDL_opengl.h:1655
   GL_FRAGMENT_COLOR_MATERIAL_PARAMETER_SGIX : constant := 16#8403#;  --  ../include/SDL/SDL_opengl.h:1656
   GL_MAX_FRAGMENT_LIGHTS_SGIX : constant := 16#8404#;  --  ../include/SDL/SDL_opengl.h:1657
   GL_MAX_ACTIVE_LIGHTS_SGIX : constant := 16#8405#;  --  ../include/SDL/SDL_opengl.h:1658
   GL_CURRENT_RASTER_NORMAL_SGIX : constant := 16#8406#;  --  ../include/SDL/SDL_opengl.h:1659
   GL_LIGHT_ENV_MODE_SGIX : constant := 16#8407#;  --  ../include/SDL/SDL_opengl.h:1660
   GL_FRAGMENT_LIGHT_MODEL_LOCAL_VIEWER_SGIX : constant := 16#8408#;  --  ../include/SDL/SDL_opengl.h:1661
   GL_FRAGMENT_LIGHT_MODEL_TWO_SIDE_SGIX : constant := 16#8409#;  --  ../include/SDL/SDL_opengl.h:1662
   GL_FRAGMENT_LIGHT_MODEL_AMBIENT_SGIX : constant := 16#840A#;  --  ../include/SDL/SDL_opengl.h:1663
   GL_FRAGMENT_LIGHT_MODEL_NORMAL_INTERPOLATION_SGIX : constant := 16#840B#;  --  ../include/SDL/SDL_opengl.h:1664
   GL_FRAGMENT_LIGHT0_SGIX : constant := 16#840C#;  --  ../include/SDL/SDL_opengl.h:1665
   GL_FRAGMENT_LIGHT1_SGIX : constant := 16#840D#;  --  ../include/SDL/SDL_opengl.h:1666
   GL_FRAGMENT_LIGHT2_SGIX : constant := 16#840E#;  --  ../include/SDL/SDL_opengl.h:1667
   GL_FRAGMENT_LIGHT3_SGIX : constant := 16#840F#;  --  ../include/SDL/SDL_opengl.h:1668
   GL_FRAGMENT_LIGHT4_SGIX : constant := 16#8410#;  --  ../include/SDL/SDL_opengl.h:1669
   GL_FRAGMENT_LIGHT5_SGIX : constant := 16#8411#;  --  ../include/SDL/SDL_opengl.h:1670
   GL_FRAGMENT_LIGHT6_SGIX : constant := 16#8412#;  --  ../include/SDL/SDL_opengl.h:1671
   GL_FRAGMENT_LIGHT7_SGIX : constant := 16#8413#;  --  ../include/SDL/SDL_opengl.h:1672

   GL_RASTER_POSITION_UNCLIPPED_IBM : constant := 16#19262#;  --  ../include/SDL/SDL_opengl.h:1676

   GL_TEXTURE_LIGHTING_MODE_HP : constant := 16#8167#;  --  ../include/SDL/SDL_opengl.h:1680
   GL_TEXTURE_POST_SPECULAR_HP : constant := 16#8168#;  --  ../include/SDL/SDL_opengl.h:1681
   GL_TEXTURE_PRE_SPECULAR_HP : constant := 16#8169#;  --  ../include/SDL/SDL_opengl.h:1682

   GL_MAX_ELEMENTS_VERTICES_EXT : constant := 16#80E8#;  --  ../include/SDL/SDL_opengl.h:1686
   GL_MAX_ELEMENTS_INDICES_EXT : constant := 16#80E9#;  --  ../include/SDL/SDL_opengl.h:1687

   GL_PHONG_WIN : constant := 16#80EA#;  --  ../include/SDL/SDL_opengl.h:1691
   GL_PHONG_HINT_WIN : constant := 16#80EB#;  --  ../include/SDL/SDL_opengl.h:1692

   GL_FOG_SPECULAR_TEXTURE_WIN : constant := 16#80EC#;  --  ../include/SDL/SDL_opengl.h:1696

   GL_FRAGMENT_MATERIAL_EXT : constant := 16#8349#;  --  ../include/SDL/SDL_opengl.h:1700
   GL_FRAGMENT_NORMAL_EXT : constant := 16#834A#;  --  ../include/SDL/SDL_opengl.h:1701
   GL_FRAGMENT_COLOR_EXT : constant := 16#834C#;  --  ../include/SDL/SDL_opengl.h:1702
   GL_ATTENUATION_EXT : constant := 16#834D#;  --  ../include/SDL/SDL_opengl.h:1703
   GL_SHADOW_ATTENUATION_EXT : constant := 16#834E#;  --  ../include/SDL/SDL_opengl.h:1704
   GL_TEXTURE_APPLICATION_MODE_EXT : constant := 16#834F#;  --  ../include/SDL/SDL_opengl.h:1705
   GL_TEXTURE_LIGHT_EXT : constant := 16#8350#;  --  ../include/SDL/SDL_opengl.h:1706
   GL_TEXTURE_MATERIAL_FACE_EXT : constant := 16#8351#;  --  ../include/SDL/SDL_opengl.h:1707
   GL_TEXTURE_MATERIAL_PARAMETER_EXT : constant := 16#8352#;  --  ../include/SDL/SDL_opengl.h:1708

   GL_ALPHA_MIN_SGIX : constant := 16#8320#;  --  ../include/SDL/SDL_opengl.h:1713
   GL_ALPHA_MAX_SGIX : constant := 16#8321#;  --  ../include/SDL/SDL_opengl.h:1714

   GL_PIXEL_TEX_GEN_Q_CEILING_SGIX : constant := 16#8184#;  --  ../include/SDL/SDL_opengl.h:1718
   GL_PIXEL_TEX_GEN_Q_ROUND_SGIX : constant := 16#8185#;  --  ../include/SDL/SDL_opengl.h:1719
   GL_PIXEL_TEX_GEN_Q_FLOOR_SGIX : constant := 16#8186#;  --  ../include/SDL/SDL_opengl.h:1720
   GL_PIXEL_TEX_GEN_ALPHA_REPLACE_SGIX : constant := 16#8187#;  --  ../include/SDL/SDL_opengl.h:1721
   GL_PIXEL_TEX_GEN_ALPHA_NO_REPLACE_SGIX : constant := 16#8188#;  --  ../include/SDL/SDL_opengl.h:1722
   GL_PIXEL_TEX_GEN_ALPHA_LS_SGIX : constant := 16#8189#;  --  ../include/SDL/SDL_opengl.h:1723
   GL_PIXEL_TEX_GEN_ALPHA_MS_SGIX : constant := 16#818A#;  --  ../include/SDL/SDL_opengl.h:1724

   GL_ASYNC_MARKER_SGIX : constant := 16#8329#;  --  ../include/SDL/SDL_opengl.h:1733

   GL_ASYNC_TEX_IMAGE_SGIX : constant := 16#835C#;  --  ../include/SDL/SDL_opengl.h:1737
   GL_ASYNC_DRAW_PIXELS_SGIX : constant := 16#835D#;  --  ../include/SDL/SDL_opengl.h:1738
   GL_ASYNC_READ_PIXELS_SGIX : constant := 16#835E#;  --  ../include/SDL/SDL_opengl.h:1739
   GL_MAX_ASYNC_TEX_IMAGE_SGIX : constant := 16#835F#;  --  ../include/SDL/SDL_opengl.h:1740
   GL_MAX_ASYNC_DRAW_PIXELS_SGIX : constant := 16#8360#;  --  ../include/SDL/SDL_opengl.h:1741
   GL_MAX_ASYNC_READ_PIXELS_SGIX : constant := 16#8361#;  --  ../include/SDL/SDL_opengl.h:1742

   GL_ASYNC_HISTOGRAM_SGIX : constant := 16#832C#;  --  ../include/SDL/SDL_opengl.h:1746
   GL_MAX_ASYNC_HISTOGRAM_SGIX : constant := 16#832D#;  --  ../include/SDL/SDL_opengl.h:1747

   GL_PARALLEL_ARRAYS_INTEL : constant := 16#83F4#;  --  ../include/SDL/SDL_opengl.h:1754
   GL_VERTEX_ARRAY_PARALLEL_POINTERS_INTEL : constant := 16#83F5#;  --  ../include/SDL/SDL_opengl.h:1755
   GL_NORMAL_ARRAY_PARALLEL_POINTERS_INTEL : constant := 16#83F6#;  --  ../include/SDL/SDL_opengl.h:1756
   GL_COLOR_ARRAY_PARALLEL_POINTERS_INTEL : constant := 16#83F7#;  --  ../include/SDL/SDL_opengl.h:1757
   GL_TEXTURE_COORD_ARRAY_PARALLEL_POINTERS_INTEL : constant := 16#83F8#;  --  ../include/SDL/SDL_opengl.h:1758

   GL_OCCLUSION_TEST_HP : constant := 16#8165#;  --  ../include/SDL/SDL_opengl.h:1762
   GL_OCCLUSION_TEST_RESULT_HP : constant := 16#8166#;  --  ../include/SDL/SDL_opengl.h:1763

   GL_PIXEL_TRANSFORM_2D_EXT : constant := 16#8330#;  --  ../include/SDL/SDL_opengl.h:1767
   GL_PIXEL_MAG_FILTER_EXT : constant := 16#8331#;  --  ../include/SDL/SDL_opengl.h:1768
   GL_PIXEL_MIN_FILTER_EXT : constant := 16#8332#;  --  ../include/SDL/SDL_opengl.h:1769
   GL_PIXEL_CUBIC_WEIGHT_EXT : constant := 16#8333#;  --  ../include/SDL/SDL_opengl.h:1770
   GL_CUBIC_EXT : constant := 16#8334#;  --  ../include/SDL/SDL_opengl.h:1771
   GL_AVERAGE_EXT : constant := 16#8335#;  --  ../include/SDL/SDL_opengl.h:1772
   GL_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT : constant := 16#8336#;  --  ../include/SDL/SDL_opengl.h:1773
   GL_MAX_PIXEL_TRANSFORM_2D_STACK_DEPTH_EXT : constant := 16#8337#;  --  ../include/SDL/SDL_opengl.h:1774
   GL_PIXEL_TRANSFORM_2D_MATRIX_EXT : constant := 16#8338#;  --  ../include/SDL/SDL_opengl.h:1775

   GL_SHARED_TEXTURE_PALETTE_EXT : constant := 16#81FB#;  --  ../include/SDL/SDL_opengl.h:1782

   GL_LIGHT_MODEL_COLOR_CONTROL_EXT : constant := 16#81F8#;  --  ../include/SDL/SDL_opengl.h:1786
   GL_SINGLE_COLOR_EXT : constant := 16#81F9#;  --  ../include/SDL/SDL_opengl.h:1787
   GL_SEPARATE_SPECULAR_COLOR_EXT : constant := 16#81FA#;  --  ../include/SDL/SDL_opengl.h:1788

   GL_COLOR_SUM_EXT : constant := 16#8458#;  --  ../include/SDL/SDL_opengl.h:1792
   GL_CURRENT_SECONDARY_COLOR_EXT : constant := 16#8459#;  --  ../include/SDL/SDL_opengl.h:1793
   GL_SECONDARY_COLOR_ARRAY_SIZE_EXT : constant := 16#845A#;  --  ../include/SDL/SDL_opengl.h:1794
   GL_SECONDARY_COLOR_ARRAY_TYPE_EXT : constant := 16#845B#;  --  ../include/SDL/SDL_opengl.h:1795
   GL_SECONDARY_COLOR_ARRAY_STRIDE_EXT : constant := 16#845C#;  --  ../include/SDL/SDL_opengl.h:1796
   GL_SECONDARY_COLOR_ARRAY_POINTER_EXT : constant := 16#845D#;  --  ../include/SDL/SDL_opengl.h:1797
   GL_SECONDARY_COLOR_ARRAY_EXT : constant := 16#845E#;  --  ../include/SDL/SDL_opengl.h:1798

   GL_PERTURB_EXT : constant := 16#85AE#;  --  ../include/SDL/SDL_opengl.h:1802
   GL_TEXTURE_NORMAL_EXT : constant := 16#85AF#;  --  ../include/SDL/SDL_opengl.h:1803

   GL_FOG_COORDINATE_SOURCE_EXT : constant := 16#8450#;  --  ../include/SDL/SDL_opengl.h:1810
   GL_FOG_COORDINATE_EXT : constant := 16#8451#;  --  ../include/SDL/SDL_opengl.h:1811
   GL_FRAGMENT_DEPTH_EXT : constant := 16#8452#;  --  ../include/SDL/SDL_opengl.h:1812
   GL_CURRENT_FOG_COORDINATE_EXT : constant := 16#8453#;  --  ../include/SDL/SDL_opengl.h:1813
   GL_FOG_COORDINATE_ARRAY_TYPE_EXT : constant := 16#8454#;  --  ../include/SDL/SDL_opengl.h:1814
   GL_FOG_COORDINATE_ARRAY_STRIDE_EXT : constant := 16#8455#;  --  ../include/SDL/SDL_opengl.h:1815
   GL_FOG_COORDINATE_ARRAY_POINTER_EXT : constant := 16#8456#;  --  ../include/SDL/SDL_opengl.h:1816
   GL_FOG_COORDINATE_ARRAY_EXT : constant := 16#8457#;  --  ../include/SDL/SDL_opengl.h:1817

   GL_SCREEN_COORDINATES_REND : constant := 16#8490#;  --  ../include/SDL/SDL_opengl.h:1821
   GL_INVERTED_SCREEN_W_REND : constant := 16#8491#;  --  ../include/SDL/SDL_opengl.h:1822

   GL_TANGENT_ARRAY_EXT : constant := 16#8439#;  --  ../include/SDL/SDL_opengl.h:1826
   GL_BINORMAL_ARRAY_EXT : constant := 16#843A#;  --  ../include/SDL/SDL_opengl.h:1827
   GL_CURRENT_TANGENT_EXT : constant := 16#843B#;  --  ../include/SDL/SDL_opengl.h:1828
   GL_CURRENT_BINORMAL_EXT : constant := 16#843C#;  --  ../include/SDL/SDL_opengl.h:1829
   GL_TANGENT_ARRAY_TYPE_EXT : constant := 16#843E#;  --  ../include/SDL/SDL_opengl.h:1830
   GL_TANGENT_ARRAY_STRIDE_EXT : constant := 16#843F#;  --  ../include/SDL/SDL_opengl.h:1831
   GL_BINORMAL_ARRAY_TYPE_EXT : constant := 16#8440#;  --  ../include/SDL/SDL_opengl.h:1832
   GL_BINORMAL_ARRAY_STRIDE_EXT : constant := 16#8441#;  --  ../include/SDL/SDL_opengl.h:1833
   GL_TANGENT_ARRAY_POINTER_EXT : constant := 16#8442#;  --  ../include/SDL/SDL_opengl.h:1834
   GL_BINORMAL_ARRAY_POINTER_EXT : constant := 16#8443#;  --  ../include/SDL/SDL_opengl.h:1835
   GL_MAP1_TANGENT_EXT : constant := 16#8444#;  --  ../include/SDL/SDL_opengl.h:1836
   GL_MAP2_TANGENT_EXT : constant := 16#8445#;  --  ../include/SDL/SDL_opengl.h:1837
   GL_MAP1_BINORMAL_EXT : constant := 16#8446#;  --  ../include/SDL/SDL_opengl.h:1838
   GL_MAP2_BINORMAL_EXT : constant := 16#8447#;  --  ../include/SDL/SDL_opengl.h:1839

   GL_COMBINE_EXT : constant := 16#8570#;  --  ../include/SDL/SDL_opengl.h:1843
   GL_COMBINE_RGB_EXT : constant := 16#8571#;  --  ../include/SDL/SDL_opengl.h:1844
   GL_COMBINE_ALPHA_EXT : constant := 16#8572#;  --  ../include/SDL/SDL_opengl.h:1845
   GL_RGB_SCALE_EXT : constant := 16#8573#;  --  ../include/SDL/SDL_opengl.h:1846
   GL_ADD_SIGNED_EXT : constant := 16#8574#;  --  ../include/SDL/SDL_opengl.h:1847
   GL_INTERPOLATE_EXT : constant := 16#8575#;  --  ../include/SDL/SDL_opengl.h:1848
   GL_CONSTANT_EXT : constant := 16#8576#;  --  ../include/SDL/SDL_opengl.h:1849
   GL_PRIMARY_COLOR_EXT : constant := 16#8577#;  --  ../include/SDL/SDL_opengl.h:1850
   GL_PREVIOUS_EXT : constant := 16#8578#;  --  ../include/SDL/SDL_opengl.h:1851
   GL_SOURCE0_RGB_EXT : constant := 16#8580#;  --  ../include/SDL/SDL_opengl.h:1852
   GL_SOURCE1_RGB_EXT : constant := 16#8581#;  --  ../include/SDL/SDL_opengl.h:1853
   GL_SOURCE2_RGB_EXT : constant := 16#8582#;  --  ../include/SDL/SDL_opengl.h:1854
   GL_SOURCE0_ALPHA_EXT : constant := 16#8588#;  --  ../include/SDL/SDL_opengl.h:1855
   GL_SOURCE1_ALPHA_EXT : constant := 16#8589#;  --  ../include/SDL/SDL_opengl.h:1856
   GL_SOURCE2_ALPHA_EXT : constant := 16#858A#;  --  ../include/SDL/SDL_opengl.h:1857
   GL_OPERAND0_RGB_EXT : constant := 16#8590#;  --  ../include/SDL/SDL_opengl.h:1858
   GL_OPERAND1_RGB_EXT : constant := 16#8591#;  --  ../include/SDL/SDL_opengl.h:1859
   GL_OPERAND2_RGB_EXT : constant := 16#8592#;  --  ../include/SDL/SDL_opengl.h:1860
   GL_OPERAND0_ALPHA_EXT : constant := 16#8598#;  --  ../include/SDL/SDL_opengl.h:1861
   GL_OPERAND1_ALPHA_EXT : constant := 16#8599#;  --  ../include/SDL/SDL_opengl.h:1862
   GL_OPERAND2_ALPHA_EXT : constant := 16#859A#;  --  ../include/SDL/SDL_opengl.h:1863

   GL_LIGHT_MODEL_SPECULAR_VECTOR_APPLE : constant := 16#85B0#;  --  ../include/SDL/SDL_opengl.h:1867

   GL_TRANSFORM_HINT_APPLE : constant := 16#85B1#;  --  ../include/SDL/SDL_opengl.h:1871

   GL_FOG_SCALE_SGIX : constant := 16#81FC#;  --  ../include/SDL/SDL_opengl.h:1875
   GL_FOG_SCALE_VALUE_SGIX : constant := 16#81FD#;  --  ../include/SDL/SDL_opengl.h:1876

   GL_UNPACK_CONSTANT_DATA_SUNX : constant := 16#81D5#;  --  ../include/SDL/SDL_opengl.h:1880
   GL_TEXTURE_CONSTANT_DATA_SUNX : constant := 16#81D6#;  --  ../include/SDL/SDL_opengl.h:1881

   GL_GLOBAL_ALPHA_SUN : constant := 16#81D9#;  --  ../include/SDL/SDL_opengl.h:1885
   GL_GLOBAL_ALPHA_FACTOR_SUN : constant := 16#81DA#;  --  ../include/SDL/SDL_opengl.h:1886

   GL_RESTART_SUN : constant := 16#0001#;  --  ../include/SDL/SDL_opengl.h:1890
   GL_REPLACE_MIDDLE_SUN : constant := 16#0002#;  --  ../include/SDL/SDL_opengl.h:1891
   GL_REPLACE_OLDEST_SUN : constant := 16#0003#;  --  ../include/SDL/SDL_opengl.h:1892
   GL_TRIANGLE_LIST_SUN : constant := 16#81D7#;  --  ../include/SDL/SDL_opengl.h:1893
   GL_REPLACEMENT_CODE_SUN : constant := 16#81D8#;  --  ../include/SDL/SDL_opengl.h:1894
   GL_REPLACEMENT_CODE_ARRAY_SUN : constant := 16#85C0#;  --  ../include/SDL/SDL_opengl.h:1895
   GL_REPLACEMENT_CODE_ARRAY_TYPE_SUN : constant := 16#85C1#;  --  ../include/SDL/SDL_opengl.h:1896
   GL_REPLACEMENT_CODE_ARRAY_STRIDE_SUN : constant := 16#85C2#;  --  ../include/SDL/SDL_opengl.h:1897
   GL_REPLACEMENT_CODE_ARRAY_POINTER_SUN : constant := 16#85C3#;  --  ../include/SDL/SDL_opengl.h:1898
   GL_R1UI_V3F_SUN : constant := 16#85C4#;  --  ../include/SDL/SDL_opengl.h:1899
   GL_R1UI_C4UB_V3F_SUN : constant := 16#85C5#;  --  ../include/SDL/SDL_opengl.h:1900
   GL_R1UI_C3F_V3F_SUN : constant := 16#85C6#;  --  ../include/SDL/SDL_opengl.h:1901
   GL_R1UI_N3F_V3F_SUN : constant := 16#85C7#;  --  ../include/SDL/SDL_opengl.h:1902
   GL_R1UI_C4F_N3F_V3F_SUN : constant := 16#85C8#;  --  ../include/SDL/SDL_opengl.h:1903
   GL_R1UI_T2F_V3F_SUN : constant := 16#85C9#;  --  ../include/SDL/SDL_opengl.h:1904
   GL_R1UI_T2F_N3F_V3F_SUN : constant := 16#85CA#;  --  ../include/SDL/SDL_opengl.h:1905
   GL_R1UI_T2F_C4F_N3F_V3F_SUN : constant := 16#85CB#;  --  ../include/SDL/SDL_opengl.h:1906

   GL_BLEND_DST_RGB_EXT : constant := 16#80C8#;  --  ../include/SDL/SDL_opengl.h:1913
   GL_BLEND_SRC_RGB_EXT : constant := 16#80C9#;  --  ../include/SDL/SDL_opengl.h:1914
   GL_BLEND_DST_ALPHA_EXT : constant := 16#80CA#;  --  ../include/SDL/SDL_opengl.h:1915
   GL_BLEND_SRC_ALPHA_EXT : constant := 16#80CB#;  --  ../include/SDL/SDL_opengl.h:1916

   GL_RED_MIN_CLAMP_INGR : constant := 16#8560#;  --  ../include/SDL/SDL_opengl.h:1920
   GL_GREEN_MIN_CLAMP_INGR : constant := 16#8561#;  --  ../include/SDL/SDL_opengl.h:1921
   GL_BLUE_MIN_CLAMP_INGR : constant := 16#8562#;  --  ../include/SDL/SDL_opengl.h:1922
   GL_ALPHA_MIN_CLAMP_INGR : constant := 16#8563#;  --  ../include/SDL/SDL_opengl.h:1923
   GL_RED_MAX_CLAMP_INGR : constant := 16#8564#;  --  ../include/SDL/SDL_opengl.h:1924
   GL_GREEN_MAX_CLAMP_INGR : constant := 16#8565#;  --  ../include/SDL/SDL_opengl.h:1925
   GL_BLUE_MAX_CLAMP_INGR : constant := 16#8566#;  --  ../include/SDL/SDL_opengl.h:1926
   GL_ALPHA_MAX_CLAMP_INGR : constant := 16#8567#;  --  ../include/SDL/SDL_opengl.h:1927

   GL_INTERLACE_READ_INGR : constant := 16#8568#;  --  ../include/SDL/SDL_opengl.h:1931

   GL_INCR_WRAP_EXT : constant := 16#8507#;  --  ../include/SDL/SDL_opengl.h:1935
   GL_DECR_WRAP_EXT : constant := 16#8508#;  --  ../include/SDL/SDL_opengl.h:1936

   GL_422_EXT : constant := 16#80CC#;  --  ../include/SDL/SDL_opengl.h:1940
   GL_422_REV_EXT : constant := 16#80CD#;  --  ../include/SDL/SDL_opengl.h:1941
   GL_422_AVERAGE_EXT : constant := 16#80CE#;  --  ../include/SDL/SDL_opengl.h:1942
   GL_422_REV_AVERAGE_EXT : constant := 16#80CF#;  --  ../include/SDL/SDL_opengl.h:1943

   GL_NORMAL_MAP_NV : constant := 16#8511#;  --  ../include/SDL/SDL_opengl.h:1947
   GL_REFLECTION_MAP_NV : constant := 16#8512#;  --  ../include/SDL/SDL_opengl.h:1948

   GL_NORMAL_MAP_EXT : constant := 16#8511#;  --  ../include/SDL/SDL_opengl.h:1952
   GL_REFLECTION_MAP_EXT : constant := 16#8512#;  --  ../include/SDL/SDL_opengl.h:1953
   GL_TEXTURE_CUBE_MAP_EXT : constant := 16#8513#;  --  ../include/SDL/SDL_opengl.h:1954
   GL_TEXTURE_BINDING_CUBE_MAP_EXT : constant := 16#8514#;  --  ../include/SDL/SDL_opengl.h:1955
   GL_TEXTURE_CUBE_MAP_POSITIVE_X_EXT : constant := 16#8515#;  --  ../include/SDL/SDL_opengl.h:1956
   GL_TEXTURE_CUBE_MAP_NEGATIVE_X_EXT : constant := 16#8516#;  --  ../include/SDL/SDL_opengl.h:1957
   GL_TEXTURE_CUBE_MAP_POSITIVE_Y_EXT : constant := 16#8517#;  --  ../include/SDL/SDL_opengl.h:1958
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Y_EXT : constant := 16#8518#;  --  ../include/SDL/SDL_opengl.h:1959
   GL_TEXTURE_CUBE_MAP_POSITIVE_Z_EXT : constant := 16#8519#;  --  ../include/SDL/SDL_opengl.h:1960
   GL_TEXTURE_CUBE_MAP_NEGATIVE_Z_EXT : constant := 16#851A#;  --  ../include/SDL/SDL_opengl.h:1961
   GL_PROXY_TEXTURE_CUBE_MAP_EXT : constant := 16#851B#;  --  ../include/SDL/SDL_opengl.h:1962
   GL_MAX_CUBE_MAP_TEXTURE_SIZE_EXT : constant := 16#851C#;  --  ../include/SDL/SDL_opengl.h:1963

   GL_WRAP_BORDER_SUN : constant := 16#81D4#;  --  ../include/SDL/SDL_opengl.h:1967

   GL_MAX_TEXTURE_LOD_BIAS_EXT : constant := 16#84FD#;  --  ../include/SDL/SDL_opengl.h:1974
   GL_TEXTURE_FILTER_CONTROL_EXT : constant := 16#8500#;  --  ../include/SDL/SDL_opengl.h:1975
   GL_TEXTURE_LOD_BIAS_EXT : constant := 16#8501#;  --  ../include/SDL/SDL_opengl.h:1976

   GL_TEXTURE_MAX_ANISOTROPY_EXT : constant := 16#84FE#;  --  ../include/SDL/SDL_opengl.h:1980
   GL_MAX_TEXTURE_MAX_ANISOTROPY_EXT : constant := 16#84FF#;  --  ../include/SDL/SDL_opengl.h:1981
   --  unsupported macro: GL_MODELVIEW0_STACK_DEPTH_EXT GL_MODELVIEW_STACK_DEPTH

   GL_MODELVIEW1_STACK_DEPTH_EXT : constant := 16#8502#;  --  ../include/SDL/SDL_opengl.h:1986
   --  unsupported macro: GL_MODELVIEW0_MATRIX_EXT GL_MODELVIEW_MATRIX

   GL_MODELVIEW1_MATRIX_EXT : constant := 16#8506#;  --  ../include/SDL/SDL_opengl.h:1988
   GL_VERTEX_WEIGHTING_EXT : constant := 16#8509#;  --  ../include/SDL/SDL_opengl.h:1989
   --  unsupported macro: GL_MODELVIEW0_EXT GL_MODELVIEW

   GL_MODELVIEW1_EXT : constant := 16#850A#;  --  ../include/SDL/SDL_opengl.h:1991
   GL_CURRENT_VERTEX_WEIGHT_EXT : constant := 16#850B#;  --  ../include/SDL/SDL_opengl.h:1992
   GL_VERTEX_WEIGHT_ARRAY_EXT : constant := 16#850C#;  --  ../include/SDL/SDL_opengl.h:1993
   GL_VERTEX_WEIGHT_ARRAY_SIZE_EXT : constant := 16#850D#;  --  ../include/SDL/SDL_opengl.h:1994
   GL_VERTEX_WEIGHT_ARRAY_TYPE_EXT : constant := 16#850E#;  --  ../include/SDL/SDL_opengl.h:1995
   GL_VERTEX_WEIGHT_ARRAY_STRIDE_EXT : constant := 16#850F#;  --  ../include/SDL/SDL_opengl.h:1996
   GL_VERTEX_WEIGHT_ARRAY_POINTER_EXT : constant := 16#8510#;  --  ../include/SDL/SDL_opengl.h:1997

   GL_MAX_SHININESS_NV : constant := 16#8504#;  --  ../include/SDL/SDL_opengl.h:2001
   GL_MAX_SPOT_EXPONENT_NV : constant := 16#8505#;  --  ../include/SDL/SDL_opengl.h:2002

   GL_VERTEX_ARRAY_RANGE_NV : constant := 16#851D#;  --  ../include/SDL/SDL_opengl.h:2006
   GL_VERTEX_ARRAY_RANGE_LENGTH_NV : constant := 16#851E#;  --  ../include/SDL/SDL_opengl.h:2007
   GL_VERTEX_ARRAY_RANGE_VALID_NV : constant := 16#851F#;  --  ../include/SDL/SDL_opengl.h:2008
   GL_MAX_VERTEX_ARRAY_RANGE_ELEMENT_NV : constant := 16#8520#;  --  ../include/SDL/SDL_opengl.h:2009
   GL_VERTEX_ARRAY_RANGE_POINTER_NV : constant := 16#8521#;  --  ../include/SDL/SDL_opengl.h:2010

   GL_REGISTER_COMBINERS_NV : constant := 16#8522#;  --  ../include/SDL/SDL_opengl.h:2014
   GL_VARIABLE_A_NV : constant := 16#8523#;  --  ../include/SDL/SDL_opengl.h:2015
   GL_VARIABLE_B_NV : constant := 16#8524#;  --  ../include/SDL/SDL_opengl.h:2016
   GL_VARIABLE_C_NV : constant := 16#8525#;  --  ../include/SDL/SDL_opengl.h:2017
   GL_VARIABLE_D_NV : constant := 16#8526#;  --  ../include/SDL/SDL_opengl.h:2018
   GL_VARIABLE_E_NV : constant := 16#8527#;  --  ../include/SDL/SDL_opengl.h:2019
   GL_VARIABLE_F_NV : constant := 16#8528#;  --  ../include/SDL/SDL_opengl.h:2020
   GL_VARIABLE_G_NV : constant := 16#8529#;  --  ../include/SDL/SDL_opengl.h:2021
   GL_CONSTANT_COLOR0_NV : constant := 16#852A#;  --  ../include/SDL/SDL_opengl.h:2022
   GL_CONSTANT_COLOR1_NV : constant := 16#852B#;  --  ../include/SDL/SDL_opengl.h:2023
   GL_PRIMARY_COLOR_NV : constant := 16#852C#;  --  ../include/SDL/SDL_opengl.h:2024
   GL_SECONDARY_COLOR_NV : constant := 16#852D#;  --  ../include/SDL/SDL_opengl.h:2025
   GL_SPARE0_NV : constant := 16#852E#;  --  ../include/SDL/SDL_opengl.h:2026
   GL_SPARE1_NV : constant := 16#852F#;  --  ../include/SDL/SDL_opengl.h:2027
   GL_DISCARD_NV : constant := 16#8530#;  --  ../include/SDL/SDL_opengl.h:2028
   GL_E_TIMES_F_NV : constant := 16#8531#;  --  ../include/SDL/SDL_opengl.h:2029
   GL_SPARE0_PLUS_SECONDARY_COLOR_NV : constant := 16#8532#;  --  ../include/SDL/SDL_opengl.h:2030
   GL_UNSIGNED_IDENTITY_NV : constant := 16#8536#;  --  ../include/SDL/SDL_opengl.h:2031
   GL_UNSIGNED_INVERT_NV : constant := 16#8537#;  --  ../include/SDL/SDL_opengl.h:2032
   GL_EXPAND_NORMAL_NV : constant := 16#8538#;  --  ../include/SDL/SDL_opengl.h:2033
   GL_EXPAND_NEGATE_NV : constant := 16#8539#;  --  ../include/SDL/SDL_opengl.h:2034
   GL_HALF_BIAS_NORMAL_NV : constant := 16#853A#;  --  ../include/SDL/SDL_opengl.h:2035
   GL_HALF_BIAS_NEGATE_NV : constant := 16#853B#;  --  ../include/SDL/SDL_opengl.h:2036
   GL_SIGNED_IDENTITY_NV : constant := 16#853C#;  --  ../include/SDL/SDL_opengl.h:2037
   GL_SIGNED_NEGATE_NV : constant := 16#853D#;  --  ../include/SDL/SDL_opengl.h:2038
   GL_SCALE_BY_TWO_NV : constant := 16#853E#;  --  ../include/SDL/SDL_opengl.h:2039
   GL_SCALE_BY_FOUR_NV : constant := 16#853F#;  --  ../include/SDL/SDL_opengl.h:2040
   GL_SCALE_BY_ONE_HALF_NV : constant := 16#8540#;  --  ../include/SDL/SDL_opengl.h:2041
   GL_BIAS_BY_NEGATIVE_ONE_HALF_NV : constant := 16#8541#;  --  ../include/SDL/SDL_opengl.h:2042
   GL_COMBINER_INPUT_NV : constant := 16#8542#;  --  ../include/SDL/SDL_opengl.h:2043
   GL_COMBINER_MAPPING_NV : constant := 16#8543#;  --  ../include/SDL/SDL_opengl.h:2044
   GL_COMBINER_COMPONENT_USAGE_NV : constant := 16#8544#;  --  ../include/SDL/SDL_opengl.h:2045
   GL_COMBINER_AB_DOT_PRODUCT_NV : constant := 16#8545#;  --  ../include/SDL/SDL_opengl.h:2046
   GL_COMBINER_CD_DOT_PRODUCT_NV : constant := 16#8546#;  --  ../include/SDL/SDL_opengl.h:2047
   GL_COMBINER_MUX_SUM_NV : constant := 16#8547#;  --  ../include/SDL/SDL_opengl.h:2048
   GL_COMBINER_SCALE_NV : constant := 16#8548#;  --  ../include/SDL/SDL_opengl.h:2049
   GL_COMBINER_BIAS_NV : constant := 16#8549#;  --  ../include/SDL/SDL_opengl.h:2050
   GL_COMBINER_AB_OUTPUT_NV : constant := 16#854A#;  --  ../include/SDL/SDL_opengl.h:2051
   GL_COMBINER_CD_OUTPUT_NV : constant := 16#854B#;  --  ../include/SDL/SDL_opengl.h:2052
   GL_COMBINER_SUM_OUTPUT_NV : constant := 16#854C#;  --  ../include/SDL/SDL_opengl.h:2053
   GL_MAX_GENERAL_COMBINERS_NV : constant := 16#854D#;  --  ../include/SDL/SDL_opengl.h:2054
   GL_NUM_GENERAL_COMBINERS_NV : constant := 16#854E#;  --  ../include/SDL/SDL_opengl.h:2055
   GL_COLOR_SUM_CLAMP_NV : constant := 16#854F#;  --  ../include/SDL/SDL_opengl.h:2056
   GL_COMBINER0_NV : constant := 16#8550#;  --  ../include/SDL/SDL_opengl.h:2057
   GL_COMBINER1_NV : constant := 16#8551#;  --  ../include/SDL/SDL_opengl.h:2058
   GL_COMBINER2_NV : constant := 16#8552#;  --  ../include/SDL/SDL_opengl.h:2059
   GL_COMBINER3_NV : constant := 16#8553#;  --  ../include/SDL/SDL_opengl.h:2060
   GL_COMBINER4_NV : constant := 16#8554#;  --  ../include/SDL/SDL_opengl.h:2061
   GL_COMBINER5_NV : constant := 16#8555#;  --  ../include/SDL/SDL_opengl.h:2062
   GL_COMBINER6_NV : constant := 16#8556#;  --  ../include/SDL/SDL_opengl.h:2063
   GL_COMBINER7_NV : constant := 16#8557#;  --  ../include/SDL/SDL_opengl.h:2064

   GL_FOG_DISTANCE_MODE_NV : constant := 16#855A#;  --  ../include/SDL/SDL_opengl.h:2073
   GL_EYE_RADIAL_NV : constant := 16#855B#;  --  ../include/SDL/SDL_opengl.h:2074
   GL_EYE_PLANE_ABSOLUTE_NV : constant := 16#855C#;  --  ../include/SDL/SDL_opengl.h:2075

   GL_EMBOSS_LIGHT_NV : constant := 16#855D#;  --  ../include/SDL/SDL_opengl.h:2080
   GL_EMBOSS_CONSTANT_NV : constant := 16#855E#;  --  ../include/SDL/SDL_opengl.h:2081
   GL_EMBOSS_MAP_NV : constant := 16#855F#;  --  ../include/SDL/SDL_opengl.h:2082

   GL_COMBINE4_NV : constant := 16#8503#;  --  ../include/SDL/SDL_opengl.h:2089
   GL_SOURCE3_RGB_NV : constant := 16#8583#;  --  ../include/SDL/SDL_opengl.h:2090
   GL_SOURCE3_ALPHA_NV : constant := 16#858B#;  --  ../include/SDL/SDL_opengl.h:2091
   GL_OPERAND3_RGB_NV : constant := 16#8593#;  --  ../include/SDL/SDL_opengl.h:2092
   GL_OPERAND3_ALPHA_NV : constant := 16#859B#;  --  ../include/SDL/SDL_opengl.h:2093

   GL_COMPRESSED_RGB_S3TC_DXT1_EXT : constant := 16#83F0#;  --  ../include/SDL/SDL_opengl.h:2103
   GL_COMPRESSED_RGBA_S3TC_DXT1_EXT : constant := 16#83F1#;  --  ../include/SDL/SDL_opengl.h:2104
   GL_COMPRESSED_RGBA_S3TC_DXT3_EXT : constant := 16#83F2#;  --  ../include/SDL/SDL_opengl.h:2105
   GL_COMPRESSED_RGBA_S3TC_DXT5_EXT : constant := 16#83F3#;  --  ../include/SDL/SDL_opengl.h:2106

   GL_CULL_VERTEX_IBM : constant := 103050;  --  ../include/SDL/SDL_opengl.h:2110

   GL_VERTEX_ARRAY_LIST_IBM : constant := 103070;  --  ../include/SDL/SDL_opengl.h:2117
   GL_NORMAL_ARRAY_LIST_IBM : constant := 103071;  --  ../include/SDL/SDL_opengl.h:2118
   GL_COLOR_ARRAY_LIST_IBM : constant := 103072;  --  ../include/SDL/SDL_opengl.h:2119
   GL_INDEX_ARRAY_LIST_IBM : constant := 103073;  --  ../include/SDL/SDL_opengl.h:2120
   GL_TEXTURE_COORD_ARRAY_LIST_IBM : constant := 103074;  --  ../include/SDL/SDL_opengl.h:2121
   GL_EDGE_FLAG_ARRAY_LIST_IBM : constant := 103075;  --  ../include/SDL/SDL_opengl.h:2122
   GL_FOG_COORDINATE_ARRAY_LIST_IBM : constant := 103076;  --  ../include/SDL/SDL_opengl.h:2123
   GL_SECONDARY_COLOR_ARRAY_LIST_IBM : constant := 103077;  --  ../include/SDL/SDL_opengl.h:2124
   GL_VERTEX_ARRAY_LIST_STRIDE_IBM : constant := 103080;  --  ../include/SDL/SDL_opengl.h:2125
   GL_NORMAL_ARRAY_LIST_STRIDE_IBM : constant := 103081;  --  ../include/SDL/SDL_opengl.h:2126
   GL_COLOR_ARRAY_LIST_STRIDE_IBM : constant := 103082;  --  ../include/SDL/SDL_opengl.h:2127
   GL_INDEX_ARRAY_LIST_STRIDE_IBM : constant := 103083;  --  ../include/SDL/SDL_opengl.h:2128
   GL_TEXTURE_COORD_ARRAY_LIST_STRIDE_IBM : constant := 103084;  --  ../include/SDL/SDL_opengl.h:2129
   GL_EDGE_FLAG_ARRAY_LIST_STRIDE_IBM : constant := 103085;  --  ../include/SDL/SDL_opengl.h:2130
   GL_FOG_COORDINATE_ARRAY_LIST_STRIDE_IBM : constant := 103086;  --  ../include/SDL/SDL_opengl.h:2131
   GL_SECONDARY_COLOR_ARRAY_LIST_STRIDE_IBM : constant := 103087;  --  ../include/SDL/SDL_opengl.h:2132

   GL_PACK_SUBSAMPLE_RATE_SGIX : constant := 16#85A0#;  --  ../include/SDL/SDL_opengl.h:2136
   GL_UNPACK_SUBSAMPLE_RATE_SGIX : constant := 16#85A1#;  --  ../include/SDL/SDL_opengl.h:2137
   GL_PIXEL_SUBSAMPLE_4444_SGIX : constant := 16#85A2#;  --  ../include/SDL/SDL_opengl.h:2138
   GL_PIXEL_SUBSAMPLE_2424_SGIX : constant := 16#85A3#;  --  ../include/SDL/SDL_opengl.h:2139
   GL_PIXEL_SUBSAMPLE_4242_SGIX : constant := 16#85A4#;  --  ../include/SDL/SDL_opengl.h:2140

   GL_YCRCB_SGIX : constant := 16#8318#;  --  ../include/SDL/SDL_opengl.h:2147
   GL_YCRCBA_SGIX : constant := 16#8319#;  --  ../include/SDL/SDL_opengl.h:2148

   GL_DEPTH_PASS_INSTRUMENT_SGIX : constant := 16#8310#;  --  ../include/SDL/SDL_opengl.h:2152
   GL_DEPTH_PASS_INSTRUMENT_COUNTERS_SGIX : constant := 16#8311#;  --  ../include/SDL/SDL_opengl.h:2153
   GL_DEPTH_PASS_INSTRUMENT_MAX_SGIX : constant := 16#8312#;  --  ../include/SDL/SDL_opengl.h:2154

   GL_COMPRESSED_RGB_FXT1_3DFX : constant := 16#86B0#;  --  ../include/SDL/SDL_opengl.h:2158
   GL_COMPRESSED_RGBA_FXT1_3DFX : constant := 16#86B1#;  --  ../include/SDL/SDL_opengl.h:2159

   GL_MULTISAMPLE_3DFX : constant := 16#86B2#;  --  ../include/SDL/SDL_opengl.h:2163
   GL_SAMPLE_BUFFERS_3DFX : constant := 16#86B3#;  --  ../include/SDL/SDL_opengl.h:2164
   GL_SAMPLES_3DFX : constant := 16#86B4#;  --  ../include/SDL/SDL_opengl.h:2165
   GL_MULTISAMPLE_BIT_3DFX : constant := 16#20000000#;  --  ../include/SDL/SDL_opengl.h:2166

   GL_MULTISAMPLE_EXT : constant := 16#809D#;  --  ../include/SDL/SDL_opengl.h:2173
   GL_SAMPLE_ALPHA_TO_MASK_EXT : constant := 16#809E#;  --  ../include/SDL/SDL_opengl.h:2174
   GL_SAMPLE_ALPHA_TO_ONE_EXT : constant := 16#809F#;  --  ../include/SDL/SDL_opengl.h:2175
   GL_SAMPLE_MASK_EXT : constant := 16#80A0#;  --  ../include/SDL/SDL_opengl.h:2176
   GL_1PASS_EXT : constant := 16#80A1#;  --  ../include/SDL/SDL_opengl.h:2177
   GL_2PASS_0_EXT : constant := 16#80A2#;  --  ../include/SDL/SDL_opengl.h:2178
   GL_2PASS_1_EXT : constant := 16#80A3#;  --  ../include/SDL/SDL_opengl.h:2179
   GL_4PASS_0_EXT : constant := 16#80A4#;  --  ../include/SDL/SDL_opengl.h:2180
   GL_4PASS_1_EXT : constant := 16#80A5#;  --  ../include/SDL/SDL_opengl.h:2181
   GL_4PASS_2_EXT : constant := 16#80A6#;  --  ../include/SDL/SDL_opengl.h:2182
   GL_4PASS_3_EXT : constant := 16#80A7#;  --  ../include/SDL/SDL_opengl.h:2183
   GL_SAMPLE_BUFFERS_EXT : constant := 16#80A8#;  --  ../include/SDL/SDL_opengl.h:2184
   GL_SAMPLES_EXT : constant := 16#80A9#;  --  ../include/SDL/SDL_opengl.h:2185
   GL_SAMPLE_MASK_VALUE_EXT : constant := 16#80AA#;  --  ../include/SDL/SDL_opengl.h:2186
   GL_SAMPLE_MASK_INVERT_EXT : constant := 16#80AB#;  --  ../include/SDL/SDL_opengl.h:2187
   GL_SAMPLE_PATTERN_EXT : constant := 16#80AC#;  --  ../include/SDL/SDL_opengl.h:2188
   GL_MULTISAMPLE_BIT_EXT : constant := 16#20000000#;  --  ../include/SDL/SDL_opengl.h:2189

   GL_VERTEX_PRECLIP_SGIX : constant := 16#83EE#;  --  ../include/SDL/SDL_opengl.h:2193
   GL_VERTEX_PRECLIP_HINT_SGIX : constant := 16#83EF#;  --  ../include/SDL/SDL_opengl.h:2194

   GL_CONVOLUTION_HINT_SGIX : constant := 16#8316#;  --  ../include/SDL/SDL_opengl.h:2198

   GL_PACK_RESAMPLE_SGIX : constant := 16#842C#;  --  ../include/SDL/SDL_opengl.h:2202
   GL_UNPACK_RESAMPLE_SGIX : constant := 16#842D#;  --  ../include/SDL/SDL_opengl.h:2203
   GL_RESAMPLE_REPLICATE_SGIX : constant := 16#842E#;  --  ../include/SDL/SDL_opengl.h:2204
   GL_RESAMPLE_ZERO_FILL_SGIX : constant := 16#842F#;  --  ../include/SDL/SDL_opengl.h:2205
   GL_RESAMPLE_DECIMATE_SGIX : constant := 16#8430#;  --  ../include/SDL/SDL_opengl.h:2206

   GL_EYE_DISTANCE_TO_POINT_SGIS : constant := 16#81F0#;  --  ../include/SDL/SDL_opengl.h:2210
   GL_OBJECT_DISTANCE_TO_POINT_SGIS : constant := 16#81F1#;  --  ../include/SDL/SDL_opengl.h:2211
   GL_EYE_DISTANCE_TO_LINE_SGIS : constant := 16#81F2#;  --  ../include/SDL/SDL_opengl.h:2212
   GL_OBJECT_DISTANCE_TO_LINE_SGIS : constant := 16#81F3#;  --  ../include/SDL/SDL_opengl.h:2213
   GL_EYE_POINT_SGIS : constant := 16#81F4#;  --  ../include/SDL/SDL_opengl.h:2214
   GL_OBJECT_POINT_SGIS : constant := 16#81F5#;  --  ../include/SDL/SDL_opengl.h:2215
   GL_EYE_LINE_SGIS : constant := 16#81F6#;  --  ../include/SDL/SDL_opengl.h:2216
   GL_OBJECT_LINE_SGIS : constant := 16#81F7#;  --  ../include/SDL/SDL_opengl.h:2217

   GL_TEXTURE_COLOR_WRITEMASK_SGIS : constant := 16#81EF#;  --  ../include/SDL/SDL_opengl.h:2221

   GL_DOT3_RGB_EXT : constant := 16#8740#;  --  ../include/SDL/SDL_opengl.h:2225
   GL_DOT3_RGBA_EXT : constant := 16#8741#;  --  ../include/SDL/SDL_opengl.h:2226

   GL_MIRROR_CLAMP_ATI : constant := 16#8742#;  --  ../include/SDL/SDL_opengl.h:2230
   GL_MIRROR_CLAMP_TO_EDGE_ATI : constant := 16#8743#;  --  ../include/SDL/SDL_opengl.h:2231

   GL_ALL_COMPLETED_NV : constant := 16#84F2#;  --  ../include/SDL/SDL_opengl.h:2235
   GL_FENCE_STATUS_NV : constant := 16#84F3#;  --  ../include/SDL/SDL_opengl.h:2236
   GL_FENCE_CONDITION_NV : constant := 16#84F4#;  --  ../include/SDL/SDL_opengl.h:2237

   GL_MIRRORED_REPEAT_IBM : constant := 16#8370#;  --  ../include/SDL/SDL_opengl.h:2241

   GL_EVAL_2D_NV : constant := 16#86C0#;  --  ../include/SDL/SDL_opengl.h:2245
   GL_EVAL_TRIANGULAR_2D_NV : constant := 16#86C1#;  --  ../include/SDL/SDL_opengl.h:2246
   GL_MAP_TESSELLATION_NV : constant := 16#86C2#;  --  ../include/SDL/SDL_opengl.h:2247
   GL_MAP_ATTRIB_U_ORDER_NV : constant := 16#86C3#;  --  ../include/SDL/SDL_opengl.h:2248
   GL_MAP_ATTRIB_V_ORDER_NV : constant := 16#86C4#;  --  ../include/SDL/SDL_opengl.h:2249
   GL_EVAL_FRACTIONAL_TESSELLATION_NV : constant := 16#86C5#;  --  ../include/SDL/SDL_opengl.h:2250
   GL_EVAL_VERTEX_ATTRIB0_NV : constant := 16#86C6#;  --  ../include/SDL/SDL_opengl.h:2251
   GL_EVAL_VERTEX_ATTRIB1_NV : constant := 16#86C7#;  --  ../include/SDL/SDL_opengl.h:2252
   GL_EVAL_VERTEX_ATTRIB2_NV : constant := 16#86C8#;  --  ../include/SDL/SDL_opengl.h:2253
   GL_EVAL_VERTEX_ATTRIB3_NV : constant := 16#86C9#;  --  ../include/SDL/SDL_opengl.h:2254
   GL_EVAL_VERTEX_ATTRIB4_NV : constant := 16#86CA#;  --  ../include/SDL/SDL_opengl.h:2255
   GL_EVAL_VERTEX_ATTRIB5_NV : constant := 16#86CB#;  --  ../include/SDL/SDL_opengl.h:2256
   GL_EVAL_VERTEX_ATTRIB6_NV : constant := 16#86CC#;  --  ../include/SDL/SDL_opengl.h:2257
   GL_EVAL_VERTEX_ATTRIB7_NV : constant := 16#86CD#;  --  ../include/SDL/SDL_opengl.h:2258
   GL_EVAL_VERTEX_ATTRIB8_NV : constant := 16#86CE#;  --  ../include/SDL/SDL_opengl.h:2259
   GL_EVAL_VERTEX_ATTRIB9_NV : constant := 16#86CF#;  --  ../include/SDL/SDL_opengl.h:2260
   GL_EVAL_VERTEX_ATTRIB10_NV : constant := 16#86D0#;  --  ../include/SDL/SDL_opengl.h:2261
   GL_EVAL_VERTEX_ATTRIB11_NV : constant := 16#86D1#;  --  ../include/SDL/SDL_opengl.h:2262
   GL_EVAL_VERTEX_ATTRIB12_NV : constant := 16#86D2#;  --  ../include/SDL/SDL_opengl.h:2263
   GL_EVAL_VERTEX_ATTRIB13_NV : constant := 16#86D3#;  --  ../include/SDL/SDL_opengl.h:2264
   GL_EVAL_VERTEX_ATTRIB14_NV : constant := 16#86D4#;  --  ../include/SDL/SDL_opengl.h:2265
   GL_EVAL_VERTEX_ATTRIB15_NV : constant := 16#86D5#;  --  ../include/SDL/SDL_opengl.h:2266
   GL_MAX_MAP_TESSELLATION_NV : constant := 16#86D6#;  --  ../include/SDL/SDL_opengl.h:2267
   GL_MAX_RATIONAL_EVAL_ORDER_NV : constant := 16#86D7#;  --  ../include/SDL/SDL_opengl.h:2268

   GL_DEPTH_STENCIL_NV : constant := 16#84F9#;  --  ../include/SDL/SDL_opengl.h:2272
   GL_UNSIGNED_INT_24_8_NV : constant := 16#84FA#;  --  ../include/SDL/SDL_opengl.h:2273

   GL_PER_STAGE_CONSTANTS_NV : constant := 16#8535#;  --  ../include/SDL/SDL_opengl.h:2277

   GL_TEXTURE_RECTANGLE_NV : constant := 16#84F5#;  --  ../include/SDL/SDL_opengl.h:2284
   GL_TEXTURE_BINDING_RECTANGLE_NV : constant := 16#84F6#;  --  ../include/SDL/SDL_opengl.h:2285
   GL_PROXY_TEXTURE_RECTANGLE_NV : constant := 16#84F7#;  --  ../include/SDL/SDL_opengl.h:2286
   GL_MAX_RECTANGLE_TEXTURE_SIZE_NV : constant := 16#84F8#;  --  ../include/SDL/SDL_opengl.h:2287

   GL_OFFSET_TEXTURE_RECTANGLE_NV : constant := 16#864C#;  --  ../include/SDL/SDL_opengl.h:2291
   GL_OFFSET_TEXTURE_RECTANGLE_SCALE_NV : constant := 16#864D#;  --  ../include/SDL/SDL_opengl.h:2292
   GL_DOT_PRODUCT_TEXTURE_RECTANGLE_NV : constant := 16#864E#;  --  ../include/SDL/SDL_opengl.h:2293
   GL_RGBA_UNSIGNED_DOT_PRODUCT_MAPPING_NV : constant := 16#86D9#;  --  ../include/SDL/SDL_opengl.h:2294
   GL_UNSIGNED_INT_S8_S8_8_8_NV : constant := 16#86DA#;  --  ../include/SDL/SDL_opengl.h:2295
   GL_UNSIGNED_INT_8_8_S8_S8_REV_NV : constant := 16#86DB#;  --  ../include/SDL/SDL_opengl.h:2296
   GL_DSDT_MAG_INTENSITY_NV : constant := 16#86DC#;  --  ../include/SDL/SDL_opengl.h:2297
   GL_SHADER_CONSISTENT_NV : constant := 16#86DD#;  --  ../include/SDL/SDL_opengl.h:2298
   GL_TEXTURE_SHADER_NV : constant := 16#86DE#;  --  ../include/SDL/SDL_opengl.h:2299
   GL_SHADER_OPERATION_NV : constant := 16#86DF#;  --  ../include/SDL/SDL_opengl.h:2300
   GL_CULL_MODES_NV : constant := 16#86E0#;  --  ../include/SDL/SDL_opengl.h:2301
   GL_OFFSET_TEXTURE_MATRIX_NV : constant := 16#86E1#;  --  ../include/SDL/SDL_opengl.h:2302
   GL_OFFSET_TEXTURE_SCALE_NV : constant := 16#86E2#;  --  ../include/SDL/SDL_opengl.h:2303
   GL_OFFSET_TEXTURE_BIAS_NV : constant := 16#86E3#;  --  ../include/SDL/SDL_opengl.h:2304
   --  unsupported macro: GL_OFFSET_TEXTURE_2D_MATRIX_NV GL_OFFSET_TEXTURE_MATRIX_NV
   --  unsupported macro: GL_OFFSET_TEXTURE_2D_SCALE_NV GL_OFFSET_TEXTURE_SCALE_NV
   --  unsupported macro: GL_OFFSET_TEXTURE_2D_BIAS_NV GL_OFFSET_TEXTURE_BIAS_NV

   GL_PREVIOUS_TEXTURE_INPUT_NV : constant := 16#86E4#;  --  ../include/SDL/SDL_opengl.h:2308
   GL_CONST_EYE_NV : constant := 16#86E5#;  --  ../include/SDL/SDL_opengl.h:2309
   GL_PASS_THROUGH_NV : constant := 16#86E6#;  --  ../include/SDL/SDL_opengl.h:2310
   GL_CULL_FRAGMENT_NV : constant := 16#86E7#;  --  ../include/SDL/SDL_opengl.h:2311
   GL_OFFSET_TEXTURE_2D_NV : constant := 16#86E8#;  --  ../include/SDL/SDL_opengl.h:2312
   GL_DEPENDENT_AR_TEXTURE_2D_NV : constant := 16#86E9#;  --  ../include/SDL/SDL_opengl.h:2313
   GL_DEPENDENT_GB_TEXTURE_2D_NV : constant := 16#86EA#;  --  ../include/SDL/SDL_opengl.h:2314
   GL_DOT_PRODUCT_NV : constant := 16#86EC#;  --  ../include/SDL/SDL_opengl.h:2315
   GL_DOT_PRODUCT_DEPTH_REPLACE_NV : constant := 16#86ED#;  --  ../include/SDL/SDL_opengl.h:2316
   GL_DOT_PRODUCT_TEXTURE_2D_NV : constant := 16#86EE#;  --  ../include/SDL/SDL_opengl.h:2317
   GL_DOT_PRODUCT_TEXTURE_CUBE_MAP_NV : constant := 16#86F0#;  --  ../include/SDL/SDL_opengl.h:2318
   GL_DOT_PRODUCT_DIFFUSE_CUBE_MAP_NV : constant := 16#86F1#;  --  ../include/SDL/SDL_opengl.h:2319
   GL_DOT_PRODUCT_REFLECT_CUBE_MAP_NV : constant := 16#86F2#;  --  ../include/SDL/SDL_opengl.h:2320
   GL_DOT_PRODUCT_CONST_EYE_REFLECT_CUBE_MAP_NV : constant := 16#86F3#;  --  ../include/SDL/SDL_opengl.h:2321
   GL_HILO_NV : constant := 16#86F4#;  --  ../include/SDL/SDL_opengl.h:2322
   GL_DSDT_NV : constant := 16#86F5#;  --  ../include/SDL/SDL_opengl.h:2323
   GL_DSDT_MAG_NV : constant := 16#86F6#;  --  ../include/SDL/SDL_opengl.h:2324
   GL_DSDT_MAG_VIB_NV : constant := 16#86F7#;  --  ../include/SDL/SDL_opengl.h:2325
   GL_HILO16_NV : constant := 16#86F8#;  --  ../include/SDL/SDL_opengl.h:2326
   GL_SIGNED_HILO_NV : constant := 16#86F9#;  --  ../include/SDL/SDL_opengl.h:2327
   GL_SIGNED_HILO16_NV : constant := 16#86FA#;  --  ../include/SDL/SDL_opengl.h:2328
   GL_SIGNED_RGBA_NV : constant := 16#86FB#;  --  ../include/SDL/SDL_opengl.h:2329
   GL_SIGNED_RGBA8_NV : constant := 16#86FC#;  --  ../include/SDL/SDL_opengl.h:2330
   GL_SIGNED_RGB_NV : constant := 16#86FE#;  --  ../include/SDL/SDL_opengl.h:2331
   GL_SIGNED_RGB8_NV : constant := 16#86FF#;  --  ../include/SDL/SDL_opengl.h:2332
   GL_SIGNED_LUMINANCE_NV : constant := 16#8701#;  --  ../include/SDL/SDL_opengl.h:2333
   GL_SIGNED_LUMINANCE8_NV : constant := 16#8702#;  --  ../include/SDL/SDL_opengl.h:2334
   GL_SIGNED_LUMINANCE_ALPHA_NV : constant := 16#8703#;  --  ../include/SDL/SDL_opengl.h:2335
   GL_SIGNED_LUMINANCE8_ALPHA8_NV : constant := 16#8704#;  --  ../include/SDL/SDL_opengl.h:2336
   GL_SIGNED_ALPHA_NV : constant := 16#8705#;  --  ../include/SDL/SDL_opengl.h:2337
   GL_SIGNED_ALPHA8_NV : constant := 16#8706#;  --  ../include/SDL/SDL_opengl.h:2338
   GL_SIGNED_INTENSITY_NV : constant := 16#8707#;  --  ../include/SDL/SDL_opengl.h:2339
   GL_SIGNED_INTENSITY8_NV : constant := 16#8708#;  --  ../include/SDL/SDL_opengl.h:2340
   GL_DSDT8_NV : constant := 16#8709#;  --  ../include/SDL/SDL_opengl.h:2341
   GL_DSDT8_MAG8_NV : constant := 16#870A#;  --  ../include/SDL/SDL_opengl.h:2342
   GL_DSDT8_MAG8_INTENSITY8_NV : constant := 16#870B#;  --  ../include/SDL/SDL_opengl.h:2343
   GL_SIGNED_RGB_UNSIGNED_ALPHA_NV : constant := 16#870C#;  --  ../include/SDL/SDL_opengl.h:2344
   GL_SIGNED_RGB8_UNSIGNED_ALPHA8_NV : constant := 16#870D#;  --  ../include/SDL/SDL_opengl.h:2345
   GL_HI_SCALE_NV : constant := 16#870E#;  --  ../include/SDL/SDL_opengl.h:2346
   GL_LO_SCALE_NV : constant := 16#870F#;  --  ../include/SDL/SDL_opengl.h:2347
   GL_DS_SCALE_NV : constant := 16#8710#;  --  ../include/SDL/SDL_opengl.h:2348
   GL_DT_SCALE_NV : constant := 16#8711#;  --  ../include/SDL/SDL_opengl.h:2349
   GL_MAGNITUDE_SCALE_NV : constant := 16#8712#;  --  ../include/SDL/SDL_opengl.h:2350
   GL_VIBRANCE_SCALE_NV : constant := 16#8713#;  --  ../include/SDL/SDL_opengl.h:2351
   GL_HI_BIAS_NV : constant := 16#8714#;  --  ../include/SDL/SDL_opengl.h:2352
   GL_LO_BIAS_NV : constant := 16#8715#;  --  ../include/SDL/SDL_opengl.h:2353
   GL_DS_BIAS_NV : constant := 16#8716#;  --  ../include/SDL/SDL_opengl.h:2354
   GL_DT_BIAS_NV : constant := 16#8717#;  --  ../include/SDL/SDL_opengl.h:2355
   GL_MAGNITUDE_BIAS_NV : constant := 16#8718#;  --  ../include/SDL/SDL_opengl.h:2356
   GL_VIBRANCE_BIAS_NV : constant := 16#8719#;  --  ../include/SDL/SDL_opengl.h:2357
   GL_TEXTURE_BORDER_VALUES_NV : constant := 16#871A#;  --  ../include/SDL/SDL_opengl.h:2358
   GL_TEXTURE_HI_SIZE_NV : constant := 16#871B#;  --  ../include/SDL/SDL_opengl.h:2359
   GL_TEXTURE_LO_SIZE_NV : constant := 16#871C#;  --  ../include/SDL/SDL_opengl.h:2360
   GL_TEXTURE_DS_SIZE_NV : constant := 16#871D#;  --  ../include/SDL/SDL_opengl.h:2361
   GL_TEXTURE_DT_SIZE_NV : constant := 16#871E#;  --  ../include/SDL/SDL_opengl.h:2362
   GL_TEXTURE_MAG_SIZE_NV : constant := 16#871F#;  --  ../include/SDL/SDL_opengl.h:2363

   GL_DOT_PRODUCT_TEXTURE_3D_NV : constant := 16#86EF#;  --  ../include/SDL/SDL_opengl.h:2367

   GL_VERTEX_ARRAY_RANGE_WITHOUT_FLUSH_NV : constant := 16#8533#;  --  ../include/SDL/SDL_opengl.h:2371

   GL_VERTEX_PROGRAM_NV : constant := 16#8620#;  --  ../include/SDL/SDL_opengl.h:2375
   GL_VERTEX_STATE_PROGRAM_NV : constant := 16#8621#;  --  ../include/SDL/SDL_opengl.h:2376
   GL_ATTRIB_ARRAY_SIZE_NV : constant := 16#8623#;  --  ../include/SDL/SDL_opengl.h:2377
   GL_ATTRIB_ARRAY_STRIDE_NV : constant := 16#8624#;  --  ../include/SDL/SDL_opengl.h:2378
   GL_ATTRIB_ARRAY_TYPE_NV : constant := 16#8625#;  --  ../include/SDL/SDL_opengl.h:2379
   GL_CURRENT_ATTRIB_NV : constant := 16#8626#;  --  ../include/SDL/SDL_opengl.h:2380
   GL_PROGRAM_LENGTH_NV : constant := 16#8627#;  --  ../include/SDL/SDL_opengl.h:2381
   GL_PROGRAM_STRING_NV : constant := 16#8628#;  --  ../include/SDL/SDL_opengl.h:2382
   GL_MODELVIEW_PROJECTION_NV : constant := 16#8629#;  --  ../include/SDL/SDL_opengl.h:2383
   GL_IDENTITY_NV : constant := 16#862A#;  --  ../include/SDL/SDL_opengl.h:2384
   GL_INVERSE_NV : constant := 16#862B#;  --  ../include/SDL/SDL_opengl.h:2385
   GL_TRANSPOSE_NV : constant := 16#862C#;  --  ../include/SDL/SDL_opengl.h:2386
   GL_INVERSE_TRANSPOSE_NV : constant := 16#862D#;  --  ../include/SDL/SDL_opengl.h:2387
   GL_MAX_TRACK_MATRIX_STACK_DEPTH_NV : constant := 16#862E#;  --  ../include/SDL/SDL_opengl.h:2388
   GL_MAX_TRACK_MATRICES_NV : constant := 16#862F#;  --  ../include/SDL/SDL_opengl.h:2389
   GL_MATRIX0_NV : constant := 16#8630#;  --  ../include/SDL/SDL_opengl.h:2390
   GL_MATRIX1_NV : constant := 16#8631#;  --  ../include/SDL/SDL_opengl.h:2391
   GL_MATRIX2_NV : constant := 16#8632#;  --  ../include/SDL/SDL_opengl.h:2392
   GL_MATRIX3_NV : constant := 16#8633#;  --  ../include/SDL/SDL_opengl.h:2393
   GL_MATRIX4_NV : constant := 16#8634#;  --  ../include/SDL/SDL_opengl.h:2394
   GL_MATRIX5_NV : constant := 16#8635#;  --  ../include/SDL/SDL_opengl.h:2395
   GL_MATRIX6_NV : constant := 16#8636#;  --  ../include/SDL/SDL_opengl.h:2396
   GL_MATRIX7_NV : constant := 16#8637#;  --  ../include/SDL/SDL_opengl.h:2397
   GL_CURRENT_MATRIX_STACK_DEPTH_NV : constant := 16#8640#;  --  ../include/SDL/SDL_opengl.h:2398
   GL_CURRENT_MATRIX_NV : constant := 16#8641#;  --  ../include/SDL/SDL_opengl.h:2399
   GL_VERTEX_PROGRAM_POINT_SIZE_NV : constant := 16#8642#;  --  ../include/SDL/SDL_opengl.h:2400
   GL_VERTEX_PROGRAM_TWO_SIDE_NV : constant := 16#8643#;  --  ../include/SDL/SDL_opengl.h:2401
   GL_PROGRAM_PARAMETER_NV : constant := 16#8644#;  --  ../include/SDL/SDL_opengl.h:2402
   GL_ATTRIB_ARRAY_POINTER_NV : constant := 16#8645#;  --  ../include/SDL/SDL_opengl.h:2403
   GL_PROGRAM_TARGET_NV : constant := 16#8646#;  --  ../include/SDL/SDL_opengl.h:2404
   GL_PROGRAM_RESIDENT_NV : constant := 16#8647#;  --  ../include/SDL/SDL_opengl.h:2405
   GL_TRACK_MATRIX_NV : constant := 16#8648#;  --  ../include/SDL/SDL_opengl.h:2406
   GL_TRACK_MATRIX_TRANSFORM_NV : constant := 16#8649#;  --  ../include/SDL/SDL_opengl.h:2407
   GL_VERTEX_PROGRAM_BINDING_NV : constant := 16#864A#;  --  ../include/SDL/SDL_opengl.h:2408
   GL_PROGRAM_ERROR_POSITION_NV : constant := 16#864B#;  --  ../include/SDL/SDL_opengl.h:2409
   GL_VERTEX_ATTRIB_ARRAY0_NV : constant := 16#8650#;  --  ../include/SDL/SDL_opengl.h:2410
   GL_VERTEX_ATTRIB_ARRAY1_NV : constant := 16#8651#;  --  ../include/SDL/SDL_opengl.h:2411
   GL_VERTEX_ATTRIB_ARRAY2_NV : constant := 16#8652#;  --  ../include/SDL/SDL_opengl.h:2412
   GL_VERTEX_ATTRIB_ARRAY3_NV : constant := 16#8653#;  --  ../include/SDL/SDL_opengl.h:2413
   GL_VERTEX_ATTRIB_ARRAY4_NV : constant := 16#8654#;  --  ../include/SDL/SDL_opengl.h:2414
   GL_VERTEX_ATTRIB_ARRAY5_NV : constant := 16#8655#;  --  ../include/SDL/SDL_opengl.h:2415
   GL_VERTEX_ATTRIB_ARRAY6_NV : constant := 16#8656#;  --  ../include/SDL/SDL_opengl.h:2416
   GL_VERTEX_ATTRIB_ARRAY7_NV : constant := 16#8657#;  --  ../include/SDL/SDL_opengl.h:2417
   GL_VERTEX_ATTRIB_ARRAY8_NV : constant := 16#8658#;  --  ../include/SDL/SDL_opengl.h:2418
   GL_VERTEX_ATTRIB_ARRAY9_NV : constant := 16#8659#;  --  ../include/SDL/SDL_opengl.h:2419
   GL_VERTEX_ATTRIB_ARRAY10_NV : constant := 16#865A#;  --  ../include/SDL/SDL_opengl.h:2420
   GL_VERTEX_ATTRIB_ARRAY11_NV : constant := 16#865B#;  --  ../include/SDL/SDL_opengl.h:2421
   GL_VERTEX_ATTRIB_ARRAY12_NV : constant := 16#865C#;  --  ../include/SDL/SDL_opengl.h:2422
   GL_VERTEX_ATTRIB_ARRAY13_NV : constant := 16#865D#;  --  ../include/SDL/SDL_opengl.h:2423
   GL_VERTEX_ATTRIB_ARRAY14_NV : constant := 16#865E#;  --  ../include/SDL/SDL_opengl.h:2424
   GL_VERTEX_ATTRIB_ARRAY15_NV : constant := 16#865F#;  --  ../include/SDL/SDL_opengl.h:2425
   GL_MAP1_VERTEX_ATTRIB0_4_NV : constant := 16#8660#;  --  ../include/SDL/SDL_opengl.h:2426
   GL_MAP1_VERTEX_ATTRIB1_4_NV : constant := 16#8661#;  --  ../include/SDL/SDL_opengl.h:2427
   GL_MAP1_VERTEX_ATTRIB2_4_NV : constant := 16#8662#;  --  ../include/SDL/SDL_opengl.h:2428
   GL_MAP1_VERTEX_ATTRIB3_4_NV : constant := 16#8663#;  --  ../include/SDL/SDL_opengl.h:2429
   GL_MAP1_VERTEX_ATTRIB4_4_NV : constant := 16#8664#;  --  ../include/SDL/SDL_opengl.h:2430
   GL_MAP1_VERTEX_ATTRIB5_4_NV : constant := 16#8665#;  --  ../include/SDL/SDL_opengl.h:2431
   GL_MAP1_VERTEX_ATTRIB6_4_NV : constant := 16#8666#;  --  ../include/SDL/SDL_opengl.h:2432
   GL_MAP1_VERTEX_ATTRIB7_4_NV : constant := 16#8667#;  --  ../include/SDL/SDL_opengl.h:2433
   GL_MAP1_VERTEX_ATTRIB8_4_NV : constant := 16#8668#;  --  ../include/SDL/SDL_opengl.h:2434
   GL_MAP1_VERTEX_ATTRIB9_4_NV : constant := 16#8669#;  --  ../include/SDL/SDL_opengl.h:2435
   GL_MAP1_VERTEX_ATTRIB10_4_NV : constant := 16#866A#;  --  ../include/SDL/SDL_opengl.h:2436
   GL_MAP1_VERTEX_ATTRIB11_4_NV : constant := 16#866B#;  --  ../include/SDL/SDL_opengl.h:2437
   GL_MAP1_VERTEX_ATTRIB12_4_NV : constant := 16#866C#;  --  ../include/SDL/SDL_opengl.h:2438
   GL_MAP1_VERTEX_ATTRIB13_4_NV : constant := 16#866D#;  --  ../include/SDL/SDL_opengl.h:2439
   GL_MAP1_VERTEX_ATTRIB14_4_NV : constant := 16#866E#;  --  ../include/SDL/SDL_opengl.h:2440
   GL_MAP1_VERTEX_ATTRIB15_4_NV : constant := 16#866F#;  --  ../include/SDL/SDL_opengl.h:2441
   GL_MAP2_VERTEX_ATTRIB0_4_NV : constant := 16#8670#;  --  ../include/SDL/SDL_opengl.h:2442
   GL_MAP2_VERTEX_ATTRIB1_4_NV : constant := 16#8671#;  --  ../include/SDL/SDL_opengl.h:2443
   GL_MAP2_VERTEX_ATTRIB2_4_NV : constant := 16#8672#;  --  ../include/SDL/SDL_opengl.h:2444
   GL_MAP2_VERTEX_ATTRIB3_4_NV : constant := 16#8673#;  --  ../include/SDL/SDL_opengl.h:2445
   GL_MAP2_VERTEX_ATTRIB4_4_NV : constant := 16#8674#;  --  ../include/SDL/SDL_opengl.h:2446
   GL_MAP2_VERTEX_ATTRIB5_4_NV : constant := 16#8675#;  --  ../include/SDL/SDL_opengl.h:2447
   GL_MAP2_VERTEX_ATTRIB6_4_NV : constant := 16#8676#;  --  ../include/SDL/SDL_opengl.h:2448
   GL_MAP2_VERTEX_ATTRIB7_4_NV : constant := 16#8677#;  --  ../include/SDL/SDL_opengl.h:2449
   GL_MAP2_VERTEX_ATTRIB8_4_NV : constant := 16#8678#;  --  ../include/SDL/SDL_opengl.h:2450
   GL_MAP2_VERTEX_ATTRIB9_4_NV : constant := 16#8679#;  --  ../include/SDL/SDL_opengl.h:2451
   GL_MAP2_VERTEX_ATTRIB10_4_NV : constant := 16#867A#;  --  ../include/SDL/SDL_opengl.h:2452
   GL_MAP2_VERTEX_ATTRIB11_4_NV : constant := 16#867B#;  --  ../include/SDL/SDL_opengl.h:2453
   GL_MAP2_VERTEX_ATTRIB12_4_NV : constant := 16#867C#;  --  ../include/SDL/SDL_opengl.h:2454
   GL_MAP2_VERTEX_ATTRIB13_4_NV : constant := 16#867D#;  --  ../include/SDL/SDL_opengl.h:2455
   GL_MAP2_VERTEX_ATTRIB14_4_NV : constant := 16#867E#;  --  ../include/SDL/SDL_opengl.h:2456
   GL_MAP2_VERTEX_ATTRIB15_4_NV : constant := 16#867F#;  --  ../include/SDL/SDL_opengl.h:2457

   GL_TEXTURE_MAX_CLAMP_S_SGIX : constant := 16#8369#;  --  ../include/SDL/SDL_opengl.h:2461
   GL_TEXTURE_MAX_CLAMP_T_SGIX : constant := 16#836A#;  --  ../include/SDL/SDL_opengl.h:2462
   GL_TEXTURE_MAX_CLAMP_R_SGIX : constant := 16#836B#;  --  ../include/SDL/SDL_opengl.h:2463

   GL_SCALEBIAS_HINT_SGIX : constant := 16#8322#;  --  ../include/SDL/SDL_opengl.h:2467

   GL_INTERLACE_OML : constant := 16#8980#;  --  ../include/SDL/SDL_opengl.h:2471
   GL_INTERLACE_READ_OML : constant := 16#8981#;  --  ../include/SDL/SDL_opengl.h:2472

   GL_FORMAT_SUBSAMPLE_24_24_OML : constant := 16#8982#;  --  ../include/SDL/SDL_opengl.h:2476
   GL_FORMAT_SUBSAMPLE_244_244_OML : constant := 16#8983#;  --  ../include/SDL/SDL_opengl.h:2477

   GL_PACK_RESAMPLE_OML : constant := 16#8984#;  --  ../include/SDL/SDL_opengl.h:2481
   GL_UNPACK_RESAMPLE_OML : constant := 16#8985#;  --  ../include/SDL/SDL_opengl.h:2482
   GL_RESAMPLE_REPLICATE_OML : constant := 16#8986#;  --  ../include/SDL/SDL_opengl.h:2483
   GL_RESAMPLE_ZERO_FILL_OML : constant := 16#8987#;  --  ../include/SDL/SDL_opengl.h:2484
   GL_RESAMPLE_AVERAGE_OML : constant := 16#8988#;  --  ../include/SDL/SDL_opengl.h:2485
   GL_RESAMPLE_DECIMATE_OML : constant := 16#8989#;  --  ../include/SDL/SDL_opengl.h:2486

   GL_DEPTH_STENCIL_TO_RGBA_NV : constant := 16#886E#;  --  ../include/SDL/SDL_opengl.h:2490
   GL_DEPTH_STENCIL_TO_BGRA_NV : constant := 16#886F#;  --  ../include/SDL/SDL_opengl.h:2491

   GL_BUMP_ROT_MATRIX_ATI : constant := 16#8775#;  --  ../include/SDL/SDL_opengl.h:2495
   GL_BUMP_ROT_MATRIX_SIZE_ATI : constant := 16#8776#;  --  ../include/SDL/SDL_opengl.h:2496
   GL_BUMP_NUM_TEX_UNITS_ATI : constant := 16#8777#;  --  ../include/SDL/SDL_opengl.h:2497
   GL_BUMP_TEX_UNITS_ATI : constant := 16#8778#;  --  ../include/SDL/SDL_opengl.h:2498
   GL_DUDV_ATI : constant := 16#8779#;  --  ../include/SDL/SDL_opengl.h:2499
   GL_DU8DV8_ATI : constant := 16#877A#;  --  ../include/SDL/SDL_opengl.h:2500
   GL_BUMP_ENVMAP_ATI : constant := 16#877B#;  --  ../include/SDL/SDL_opengl.h:2501
   GL_BUMP_TARGET_ATI : constant := 16#877C#;  --  ../include/SDL/SDL_opengl.h:2502

   GL_FRAGMENT_SHADER_ATI : constant := 16#8920#;  --  ../include/SDL/SDL_opengl.h:2506
   GL_REG_0_ATI : constant := 16#8921#;  --  ../include/SDL/SDL_opengl.h:2507
   GL_REG_1_ATI : constant := 16#8922#;  --  ../include/SDL/SDL_opengl.h:2508
   GL_REG_2_ATI : constant := 16#8923#;  --  ../include/SDL/SDL_opengl.h:2509
   GL_REG_3_ATI : constant := 16#8924#;  --  ../include/SDL/SDL_opengl.h:2510
   GL_REG_4_ATI : constant := 16#8925#;  --  ../include/SDL/SDL_opengl.h:2511
   GL_REG_5_ATI : constant := 16#8926#;  --  ../include/SDL/SDL_opengl.h:2512
   GL_REG_6_ATI : constant := 16#8927#;  --  ../include/SDL/SDL_opengl.h:2513
   GL_REG_7_ATI : constant := 16#8928#;  --  ../include/SDL/SDL_opengl.h:2514
   GL_REG_8_ATI : constant := 16#8929#;  --  ../include/SDL/SDL_opengl.h:2515
   GL_REG_9_ATI : constant := 16#892A#;  --  ../include/SDL/SDL_opengl.h:2516
   GL_REG_10_ATI : constant := 16#892B#;  --  ../include/SDL/SDL_opengl.h:2517
   GL_REG_11_ATI : constant := 16#892C#;  --  ../include/SDL/SDL_opengl.h:2518
   GL_REG_12_ATI : constant := 16#892D#;  --  ../include/SDL/SDL_opengl.h:2519
   GL_REG_13_ATI : constant := 16#892E#;  --  ../include/SDL/SDL_opengl.h:2520
   GL_REG_14_ATI : constant := 16#892F#;  --  ../include/SDL/SDL_opengl.h:2521
   GL_REG_15_ATI : constant := 16#8930#;  --  ../include/SDL/SDL_opengl.h:2522
   GL_REG_16_ATI : constant := 16#8931#;  --  ../include/SDL/SDL_opengl.h:2523
   GL_REG_17_ATI : constant := 16#8932#;  --  ../include/SDL/SDL_opengl.h:2524
   GL_REG_18_ATI : constant := 16#8933#;  --  ../include/SDL/SDL_opengl.h:2525
   GL_REG_19_ATI : constant := 16#8934#;  --  ../include/SDL/SDL_opengl.h:2526
   GL_REG_20_ATI : constant := 16#8935#;  --  ../include/SDL/SDL_opengl.h:2527
   GL_REG_21_ATI : constant := 16#8936#;  --  ../include/SDL/SDL_opengl.h:2528
   GL_REG_22_ATI : constant := 16#8937#;  --  ../include/SDL/SDL_opengl.h:2529
   GL_REG_23_ATI : constant := 16#8938#;  --  ../include/SDL/SDL_opengl.h:2530
   GL_REG_24_ATI : constant := 16#8939#;  --  ../include/SDL/SDL_opengl.h:2531
   GL_REG_25_ATI : constant := 16#893A#;  --  ../include/SDL/SDL_opengl.h:2532
   GL_REG_26_ATI : constant := 16#893B#;  --  ../include/SDL/SDL_opengl.h:2533
   GL_REG_27_ATI : constant := 16#893C#;  --  ../include/SDL/SDL_opengl.h:2534
   GL_REG_28_ATI : constant := 16#893D#;  --  ../include/SDL/SDL_opengl.h:2535
   GL_REG_29_ATI : constant := 16#893E#;  --  ../include/SDL/SDL_opengl.h:2536
   GL_REG_30_ATI : constant := 16#893F#;  --  ../include/SDL/SDL_opengl.h:2537
   GL_REG_31_ATI : constant := 16#8940#;  --  ../include/SDL/SDL_opengl.h:2538
   GL_CON_0_ATI : constant := 16#8941#;  --  ../include/SDL/SDL_opengl.h:2539
   GL_CON_1_ATI : constant := 16#8942#;  --  ../include/SDL/SDL_opengl.h:2540
   GL_CON_2_ATI : constant := 16#8943#;  --  ../include/SDL/SDL_opengl.h:2541
   GL_CON_3_ATI : constant := 16#8944#;  --  ../include/SDL/SDL_opengl.h:2542
   GL_CON_4_ATI : constant := 16#8945#;  --  ../include/SDL/SDL_opengl.h:2543
   GL_CON_5_ATI : constant := 16#8946#;  --  ../include/SDL/SDL_opengl.h:2544
   GL_CON_6_ATI : constant := 16#8947#;  --  ../include/SDL/SDL_opengl.h:2545
   GL_CON_7_ATI : constant := 16#8948#;  --  ../include/SDL/SDL_opengl.h:2546
   GL_CON_8_ATI : constant := 16#8949#;  --  ../include/SDL/SDL_opengl.h:2547
   GL_CON_9_ATI : constant := 16#894A#;  --  ../include/SDL/SDL_opengl.h:2548
   GL_CON_10_ATI : constant := 16#894B#;  --  ../include/SDL/SDL_opengl.h:2549
   GL_CON_11_ATI : constant := 16#894C#;  --  ../include/SDL/SDL_opengl.h:2550
   GL_CON_12_ATI : constant := 16#894D#;  --  ../include/SDL/SDL_opengl.h:2551
   GL_CON_13_ATI : constant := 16#894E#;  --  ../include/SDL/SDL_opengl.h:2552
   GL_CON_14_ATI : constant := 16#894F#;  --  ../include/SDL/SDL_opengl.h:2553
   GL_CON_15_ATI : constant := 16#8950#;  --  ../include/SDL/SDL_opengl.h:2554
   GL_CON_16_ATI : constant := 16#8951#;  --  ../include/SDL/SDL_opengl.h:2555
   GL_CON_17_ATI : constant := 16#8952#;  --  ../include/SDL/SDL_opengl.h:2556
   GL_CON_18_ATI : constant := 16#8953#;  --  ../include/SDL/SDL_opengl.h:2557
   GL_CON_19_ATI : constant := 16#8954#;  --  ../include/SDL/SDL_opengl.h:2558
   GL_CON_20_ATI : constant := 16#8955#;  --  ../include/SDL/SDL_opengl.h:2559
   GL_CON_21_ATI : constant := 16#8956#;  --  ../include/SDL/SDL_opengl.h:2560
   GL_CON_22_ATI : constant := 16#8957#;  --  ../include/SDL/SDL_opengl.h:2561
   GL_CON_23_ATI : constant := 16#8958#;  --  ../include/SDL/SDL_opengl.h:2562
   GL_CON_24_ATI : constant := 16#8959#;  --  ../include/SDL/SDL_opengl.h:2563
   GL_CON_25_ATI : constant := 16#895A#;  --  ../include/SDL/SDL_opengl.h:2564
   GL_CON_26_ATI : constant := 16#895B#;  --  ../include/SDL/SDL_opengl.h:2565
   GL_CON_27_ATI : constant := 16#895C#;  --  ../include/SDL/SDL_opengl.h:2566
   GL_CON_28_ATI : constant := 16#895D#;  --  ../include/SDL/SDL_opengl.h:2567
   GL_CON_29_ATI : constant := 16#895E#;  --  ../include/SDL/SDL_opengl.h:2568
   GL_CON_30_ATI : constant := 16#895F#;  --  ../include/SDL/SDL_opengl.h:2569
   GL_CON_31_ATI : constant := 16#8960#;  --  ../include/SDL/SDL_opengl.h:2570
   GL_MOV_ATI : constant := 16#8961#;  --  ../include/SDL/SDL_opengl.h:2571
   GL_ADD_ATI : constant := 16#8963#;  --  ../include/SDL/SDL_opengl.h:2572
   GL_MUL_ATI : constant := 16#8964#;  --  ../include/SDL/SDL_opengl.h:2573
   GL_SUB_ATI : constant := 16#8965#;  --  ../include/SDL/SDL_opengl.h:2574
   GL_DOT3_ATI : constant := 16#8966#;  --  ../include/SDL/SDL_opengl.h:2575
   GL_DOT4_ATI : constant := 16#8967#;  --  ../include/SDL/SDL_opengl.h:2576
   GL_MAD_ATI : constant := 16#8968#;  --  ../include/SDL/SDL_opengl.h:2577
   GL_LERP_ATI : constant := 16#8969#;  --  ../include/SDL/SDL_opengl.h:2578
   GL_CND_ATI : constant := 16#896A#;  --  ../include/SDL/SDL_opengl.h:2579
   GL_CND0_ATI : constant := 16#896B#;  --  ../include/SDL/SDL_opengl.h:2580
   GL_DOT2_ADD_ATI : constant := 16#896C#;  --  ../include/SDL/SDL_opengl.h:2581
   GL_SECONDARY_INTERPOLATOR_ATI : constant := 16#896D#;  --  ../include/SDL/SDL_opengl.h:2582
   GL_NUM_FRAGMENT_REGISTERS_ATI : constant := 16#896E#;  --  ../include/SDL/SDL_opengl.h:2583
   GL_NUM_FRAGMENT_CONSTANTS_ATI : constant := 16#896F#;  --  ../include/SDL/SDL_opengl.h:2584
   GL_NUM_PASSES_ATI : constant := 16#8970#;  --  ../include/SDL/SDL_opengl.h:2585
   GL_NUM_INSTRUCTIONS_PER_PASS_ATI : constant := 16#8971#;  --  ../include/SDL/SDL_opengl.h:2586
   GL_NUM_INSTRUCTIONS_TOTAL_ATI : constant := 16#8972#;  --  ../include/SDL/SDL_opengl.h:2587
   GL_NUM_INPUT_INTERPOLATOR_COMPONENTS_ATI : constant := 16#8973#;  --  ../include/SDL/SDL_opengl.h:2588
   GL_NUM_LOOPBACK_COMPONENTS_ATI : constant := 16#8974#;  --  ../include/SDL/SDL_opengl.h:2589
   GL_COLOR_ALPHA_PAIRING_ATI : constant := 16#8975#;  --  ../include/SDL/SDL_opengl.h:2590
   GL_SWIZZLE_STR_ATI : constant := 16#8976#;  --  ../include/SDL/SDL_opengl.h:2591
   GL_SWIZZLE_STQ_ATI : constant := 16#8977#;  --  ../include/SDL/SDL_opengl.h:2592
   GL_SWIZZLE_STR_DR_ATI : constant := 16#8978#;  --  ../include/SDL/SDL_opengl.h:2593
   GL_SWIZZLE_STQ_DQ_ATI : constant := 16#8979#;  --  ../include/SDL/SDL_opengl.h:2594
   GL_SWIZZLE_STRQ_ATI : constant := 16#897A#;  --  ../include/SDL/SDL_opengl.h:2595
   GL_SWIZZLE_STRQ_DQ_ATI : constant := 16#897B#;  --  ../include/SDL/SDL_opengl.h:2596
   GL_RED_BIT_ATI : constant := 16#00000001#;  --  ../include/SDL/SDL_opengl.h:2597
   GL_GREEN_BIT_ATI : constant := 16#00000002#;  --  ../include/SDL/SDL_opengl.h:2598
   GL_BLUE_BIT_ATI : constant := 16#00000004#;  --  ../include/SDL/SDL_opengl.h:2599
   GL_2X_BIT_ATI : constant := 16#00000001#;  --  ../include/SDL/SDL_opengl.h:2600
   GL_4X_BIT_ATI : constant := 16#00000002#;  --  ../include/SDL/SDL_opengl.h:2601
   GL_8X_BIT_ATI : constant := 16#00000004#;  --  ../include/SDL/SDL_opengl.h:2602
   GL_HALF_BIT_ATI : constant := 16#00000008#;  --  ../include/SDL/SDL_opengl.h:2603
   GL_QUARTER_BIT_ATI : constant := 16#00000010#;  --  ../include/SDL/SDL_opengl.h:2604
   GL_EIGHTH_BIT_ATI : constant := 16#00000020#;  --  ../include/SDL/SDL_opengl.h:2605
   GL_SATURATE_BIT_ATI : constant := 16#00000040#;  --  ../include/SDL/SDL_opengl.h:2606
   GL_COMP_BIT_ATI : constant := 16#00000002#;  --  ../include/SDL/SDL_opengl.h:2607
   GL_NEGATE_BIT_ATI : constant := 16#00000004#;  --  ../include/SDL/SDL_opengl.h:2608
   GL_BIAS_BIT_ATI : constant := 16#00000008#;  --  ../include/SDL/SDL_opengl.h:2609

   GL_PN_TRIANGLES_ATI : constant := 16#87F0#;  --  ../include/SDL/SDL_opengl.h:2613
   GL_MAX_PN_TRIANGLES_TESSELATION_LEVEL_ATI : constant := 16#87F1#;  --  ../include/SDL/SDL_opengl.h:2614
   GL_PN_TRIANGLES_POINT_MODE_ATI : constant := 16#87F2#;  --  ../include/SDL/SDL_opengl.h:2615
   GL_PN_TRIANGLES_NORMAL_MODE_ATI : constant := 16#87F3#;  --  ../include/SDL/SDL_opengl.h:2616
   GL_PN_TRIANGLES_TESSELATION_LEVEL_ATI : constant := 16#87F4#;  --  ../include/SDL/SDL_opengl.h:2617
   GL_PN_TRIANGLES_POINT_MODE_LINEAR_ATI : constant := 16#87F5#;  --  ../include/SDL/SDL_opengl.h:2618
   GL_PN_TRIANGLES_POINT_MODE_CUBIC_ATI : constant := 16#87F6#;  --  ../include/SDL/SDL_opengl.h:2619
   GL_PN_TRIANGLES_NORMAL_MODE_LINEAR_ATI : constant := 16#87F7#;  --  ../include/SDL/SDL_opengl.h:2620
   GL_PN_TRIANGLES_NORMAL_MODE_QUADRATIC_ATI : constant := 16#87F8#;  --  ../include/SDL/SDL_opengl.h:2621

   GL_STATIC_ATI : constant := 16#8760#;  --  ../include/SDL/SDL_opengl.h:2625
   GL_DYNAMIC_ATI : constant := 16#8761#;  --  ../include/SDL/SDL_opengl.h:2626
   GL_PRESERVE_ATI : constant := 16#8762#;  --  ../include/SDL/SDL_opengl.h:2627
   GL_DISCARD_ATI : constant := 16#8763#;  --  ../include/SDL/SDL_opengl.h:2628
   GL_OBJECT_BUFFER_SIZE_ATI : constant := 16#8764#;  --  ../include/SDL/SDL_opengl.h:2629
   GL_OBJECT_BUFFER_USAGE_ATI : constant := 16#8765#;  --  ../include/SDL/SDL_opengl.h:2630
   GL_ARRAY_OBJECT_BUFFER_ATI : constant := 16#8766#;  --  ../include/SDL/SDL_opengl.h:2631
   GL_ARRAY_OBJECT_OFFSET_ATI : constant := 16#8767#;  --  ../include/SDL/SDL_opengl.h:2632

   GL_VERTEX_SHADER_EXT : constant := 16#8780#;  --  ../include/SDL/SDL_opengl.h:2636
   GL_VERTEX_SHADER_BINDING_EXT : constant := 16#8781#;  --  ../include/SDL/SDL_opengl.h:2637
   GL_OP_INDEX_EXT : constant := 16#8782#;  --  ../include/SDL/SDL_opengl.h:2638
   GL_OP_NEGATE_EXT : constant := 16#8783#;  --  ../include/SDL/SDL_opengl.h:2639
   GL_OP_DOT3_EXT : constant := 16#8784#;  --  ../include/SDL/SDL_opengl.h:2640
   GL_OP_DOT4_EXT : constant := 16#8785#;  --  ../include/SDL/SDL_opengl.h:2641
   GL_OP_MUL_EXT : constant := 16#8786#;  --  ../include/SDL/SDL_opengl.h:2642
   GL_OP_ADD_EXT : constant := 16#8787#;  --  ../include/SDL/SDL_opengl.h:2643
   GL_OP_MADD_EXT : constant := 16#8788#;  --  ../include/SDL/SDL_opengl.h:2644
   GL_OP_FRAC_EXT : constant := 16#8789#;  --  ../include/SDL/SDL_opengl.h:2645
   GL_OP_MAX_EXT : constant := 16#878A#;  --  ../include/SDL/SDL_opengl.h:2646
   GL_OP_MIN_EXT : constant := 16#878B#;  --  ../include/SDL/SDL_opengl.h:2647
   GL_OP_SET_GE_EXT : constant := 16#878C#;  --  ../include/SDL/SDL_opengl.h:2648
   GL_OP_SET_LT_EXT : constant := 16#878D#;  --  ../include/SDL/SDL_opengl.h:2649
   GL_OP_CLAMP_EXT : constant := 16#878E#;  --  ../include/SDL/SDL_opengl.h:2650
   GL_OP_FLOOR_EXT : constant := 16#878F#;  --  ../include/SDL/SDL_opengl.h:2651
   GL_OP_ROUND_EXT : constant := 16#8790#;  --  ../include/SDL/SDL_opengl.h:2652
   GL_OP_EXP_BASE_2_EXT : constant := 16#8791#;  --  ../include/SDL/SDL_opengl.h:2653
   GL_OP_LOG_BASE_2_EXT : constant := 16#8792#;  --  ../include/SDL/SDL_opengl.h:2654
   GL_OP_POWER_EXT : constant := 16#8793#;  --  ../include/SDL/SDL_opengl.h:2655
   GL_OP_RECIP_EXT : constant := 16#8794#;  --  ../include/SDL/SDL_opengl.h:2656
   GL_OP_RECIP_SQRT_EXT : constant := 16#8795#;  --  ../include/SDL/SDL_opengl.h:2657
   GL_OP_SUB_EXT : constant := 16#8796#;  --  ../include/SDL/SDL_opengl.h:2658
   GL_OP_CROSS_PRODUCT_EXT : constant := 16#8797#;  --  ../include/SDL/SDL_opengl.h:2659
   GL_OP_MULTIPLY_MATRIX_EXT : constant := 16#8798#;  --  ../include/SDL/SDL_opengl.h:2660
   GL_OP_MOV_EXT : constant := 16#8799#;  --  ../include/SDL/SDL_opengl.h:2661
   GL_OUTPUT_VERTEX_EXT : constant := 16#879A#;  --  ../include/SDL/SDL_opengl.h:2662
   GL_OUTPUT_COLOR0_EXT : constant := 16#879B#;  --  ../include/SDL/SDL_opengl.h:2663
   GL_OUTPUT_COLOR1_EXT : constant := 16#879C#;  --  ../include/SDL/SDL_opengl.h:2664
   GL_OUTPUT_TEXTURE_COORD0_EXT : constant := 16#879D#;  --  ../include/SDL/SDL_opengl.h:2665
   GL_OUTPUT_TEXTURE_COORD1_EXT : constant := 16#879E#;  --  ../include/SDL/SDL_opengl.h:2666
   GL_OUTPUT_TEXTURE_COORD2_EXT : constant := 16#879F#;  --  ../include/SDL/SDL_opengl.h:2667
   GL_OUTPUT_TEXTURE_COORD3_EXT : constant := 16#87A0#;  --  ../include/SDL/SDL_opengl.h:2668
   GL_OUTPUT_TEXTURE_COORD4_EXT : constant := 16#87A1#;  --  ../include/SDL/SDL_opengl.h:2669
   GL_OUTPUT_TEXTURE_COORD5_EXT : constant := 16#87A2#;  --  ../include/SDL/SDL_opengl.h:2670
   GL_OUTPUT_TEXTURE_COORD6_EXT : constant := 16#87A3#;  --  ../include/SDL/SDL_opengl.h:2671
   GL_OUTPUT_TEXTURE_COORD7_EXT : constant := 16#87A4#;  --  ../include/SDL/SDL_opengl.h:2672
   GL_OUTPUT_TEXTURE_COORD8_EXT : constant := 16#87A5#;  --  ../include/SDL/SDL_opengl.h:2673
   GL_OUTPUT_TEXTURE_COORD9_EXT : constant := 16#87A6#;  --  ../include/SDL/SDL_opengl.h:2674
   GL_OUTPUT_TEXTURE_COORD10_EXT : constant := 16#87A7#;  --  ../include/SDL/SDL_opengl.h:2675
   GL_OUTPUT_TEXTURE_COORD11_EXT : constant := 16#87A8#;  --  ../include/SDL/SDL_opengl.h:2676
   GL_OUTPUT_TEXTURE_COORD12_EXT : constant := 16#87A9#;  --  ../include/SDL/SDL_opengl.h:2677
   GL_OUTPUT_TEXTURE_COORD13_EXT : constant := 16#87AA#;  --  ../include/SDL/SDL_opengl.h:2678
   GL_OUTPUT_TEXTURE_COORD14_EXT : constant := 16#87AB#;  --  ../include/SDL/SDL_opengl.h:2679
   GL_OUTPUT_TEXTURE_COORD15_EXT : constant := 16#87AC#;  --  ../include/SDL/SDL_opengl.h:2680
   GL_OUTPUT_TEXTURE_COORD16_EXT : constant := 16#87AD#;  --  ../include/SDL/SDL_opengl.h:2681
   GL_OUTPUT_TEXTURE_COORD17_EXT : constant := 16#87AE#;  --  ../include/SDL/SDL_opengl.h:2682
   GL_OUTPUT_TEXTURE_COORD18_EXT : constant := 16#87AF#;  --  ../include/SDL/SDL_opengl.h:2683
   GL_OUTPUT_TEXTURE_COORD19_EXT : constant := 16#87B0#;  --  ../include/SDL/SDL_opengl.h:2684
   GL_OUTPUT_TEXTURE_COORD20_EXT : constant := 16#87B1#;  --  ../include/SDL/SDL_opengl.h:2685
   GL_OUTPUT_TEXTURE_COORD21_EXT : constant := 16#87B2#;  --  ../include/SDL/SDL_opengl.h:2686
   GL_OUTPUT_TEXTURE_COORD22_EXT : constant := 16#87B3#;  --  ../include/SDL/SDL_opengl.h:2687
   GL_OUTPUT_TEXTURE_COORD23_EXT : constant := 16#87B4#;  --  ../include/SDL/SDL_opengl.h:2688
   GL_OUTPUT_TEXTURE_COORD24_EXT : constant := 16#87B5#;  --  ../include/SDL/SDL_opengl.h:2689
   GL_OUTPUT_TEXTURE_COORD25_EXT : constant := 16#87B6#;  --  ../include/SDL/SDL_opengl.h:2690
   GL_OUTPUT_TEXTURE_COORD26_EXT : constant := 16#87B7#;  --  ../include/SDL/SDL_opengl.h:2691
   GL_OUTPUT_TEXTURE_COORD27_EXT : constant := 16#87B8#;  --  ../include/SDL/SDL_opengl.h:2692
   GL_OUTPUT_TEXTURE_COORD28_EXT : constant := 16#87B9#;  --  ../include/SDL/SDL_opengl.h:2693
   GL_OUTPUT_TEXTURE_COORD29_EXT : constant := 16#87BA#;  --  ../include/SDL/SDL_opengl.h:2694
   GL_OUTPUT_TEXTURE_COORD30_EXT : constant := 16#87BB#;  --  ../include/SDL/SDL_opengl.h:2695
   GL_OUTPUT_TEXTURE_COORD31_EXT : constant := 16#87BC#;  --  ../include/SDL/SDL_opengl.h:2696
   GL_OUTPUT_FOG_EXT : constant := 16#87BD#;  --  ../include/SDL/SDL_opengl.h:2697
   GL_SCALAR_EXT : constant := 16#87BE#;  --  ../include/SDL/SDL_opengl.h:2698
   GL_VECTOR_EXT : constant := 16#87BF#;  --  ../include/SDL/SDL_opengl.h:2699
   GL_MATRIX_EXT : constant := 16#87C0#;  --  ../include/SDL/SDL_opengl.h:2700
   GL_VARIANT_EXT : constant := 16#87C1#;  --  ../include/SDL/SDL_opengl.h:2701
   GL_INVARIANT_EXT : constant := 16#87C2#;  --  ../include/SDL/SDL_opengl.h:2702
   GL_LOCAL_CONSTANT_EXT : constant := 16#87C3#;  --  ../include/SDL/SDL_opengl.h:2703
   GL_LOCAL_EXT : constant := 16#87C4#;  --  ../include/SDL/SDL_opengl.h:2704
   GL_MAX_VERTEX_SHADER_INSTRUCTIONS_EXT : constant := 16#87C5#;  --  ../include/SDL/SDL_opengl.h:2705
   GL_MAX_VERTEX_SHADER_VARIANTS_EXT : constant := 16#87C6#;  --  ../include/SDL/SDL_opengl.h:2706
   GL_MAX_VERTEX_SHADER_INVARIANTS_EXT : constant := 16#87C7#;  --  ../include/SDL/SDL_opengl.h:2707
   GL_MAX_VERTEX_SHADER_LOCAL_CONSTANTS_EXT : constant := 16#87C8#;  --  ../include/SDL/SDL_opengl.h:2708
   GL_MAX_VERTEX_SHADER_LOCALS_EXT : constant := 16#87C9#;  --  ../include/SDL/SDL_opengl.h:2709
   GL_MAX_OPTIMIZED_VERTEX_SHADER_INSTRUCTIONS_EXT : constant := 16#87CA#;  --  ../include/SDL/SDL_opengl.h:2710
   GL_MAX_OPTIMIZED_VERTEX_SHADER_VARIANTS_EXT : constant := 16#87CB#;  --  ../include/SDL/SDL_opengl.h:2711
   GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCAL_CONSTANTS_EXT : constant := 16#87CC#;  --  ../include/SDL/SDL_opengl.h:2712
   GL_MAX_OPTIMIZED_VERTEX_SHADER_INVARIANTS_EXT : constant := 16#87CD#;  --  ../include/SDL/SDL_opengl.h:2713
   GL_MAX_OPTIMIZED_VERTEX_SHADER_LOCALS_EXT : constant := 16#87CE#;  --  ../include/SDL/SDL_opengl.h:2714
   GL_VERTEX_SHADER_INSTRUCTIONS_EXT : constant := 16#87CF#;  --  ../include/SDL/SDL_opengl.h:2715
   GL_VERTEX_SHADER_VARIANTS_EXT : constant := 16#87D0#;  --  ../include/SDL/SDL_opengl.h:2716
   GL_VERTEX_SHADER_INVARIANTS_EXT : constant := 16#87D1#;  --  ../include/SDL/SDL_opengl.h:2717
   GL_VERTEX_SHADER_LOCAL_CONSTANTS_EXT : constant := 16#87D2#;  --  ../include/SDL/SDL_opengl.h:2718
   GL_VERTEX_SHADER_LOCALS_EXT : constant := 16#87D3#;  --  ../include/SDL/SDL_opengl.h:2719
   GL_VERTEX_SHADER_OPTIMIZED_EXT : constant := 16#87D4#;  --  ../include/SDL/SDL_opengl.h:2720
   GL_X_EXT : constant := 16#87D5#;  --  ../include/SDL/SDL_opengl.h:2721
   GL_Y_EXT : constant := 16#87D6#;  --  ../include/SDL/SDL_opengl.h:2722
   GL_Z_EXT : constant := 16#87D7#;  --  ../include/SDL/SDL_opengl.h:2723
   GL_W_EXT : constant := 16#87D8#;  --  ../include/SDL/SDL_opengl.h:2724
   GL_NEGATIVE_X_EXT : constant := 16#87D9#;  --  ../include/SDL/SDL_opengl.h:2725
   GL_NEGATIVE_Y_EXT : constant := 16#87DA#;  --  ../include/SDL/SDL_opengl.h:2726
   GL_NEGATIVE_Z_EXT : constant := 16#87DB#;  --  ../include/SDL/SDL_opengl.h:2727
   GL_NEGATIVE_W_EXT : constant := 16#87DC#;  --  ../include/SDL/SDL_opengl.h:2728
   GL_ZERO_EXT : constant := 16#87DD#;  --  ../include/SDL/SDL_opengl.h:2729
   GL_ONE_EXT : constant := 16#87DE#;  --  ../include/SDL/SDL_opengl.h:2730
   GL_NEGATIVE_ONE_EXT : constant := 16#87DF#;  --  ../include/SDL/SDL_opengl.h:2731
   GL_NORMALIZED_RANGE_EXT : constant := 16#87E0#;  --  ../include/SDL/SDL_opengl.h:2732
   GL_FULL_RANGE_EXT : constant := 16#87E1#;  --  ../include/SDL/SDL_opengl.h:2733
   GL_CURRENT_VERTEX_EXT : constant := 16#87E2#;  --  ../include/SDL/SDL_opengl.h:2734
   GL_MVP_MATRIX_EXT : constant := 16#87E3#;  --  ../include/SDL/SDL_opengl.h:2735
   GL_VARIANT_VALUE_EXT : constant := 16#87E4#;  --  ../include/SDL/SDL_opengl.h:2736
   GL_VARIANT_DATATYPE_EXT : constant := 16#87E5#;  --  ../include/SDL/SDL_opengl.h:2737
   GL_VARIANT_ARRAY_STRIDE_EXT : constant := 16#87E6#;  --  ../include/SDL/SDL_opengl.h:2738
   GL_VARIANT_ARRAY_TYPE_EXT : constant := 16#87E7#;  --  ../include/SDL/SDL_opengl.h:2739
   GL_VARIANT_ARRAY_EXT : constant := 16#87E8#;  --  ../include/SDL/SDL_opengl.h:2740
   GL_VARIANT_ARRAY_POINTER_EXT : constant := 16#87E9#;  --  ../include/SDL/SDL_opengl.h:2741
   GL_INVARIANT_VALUE_EXT : constant := 16#87EA#;  --  ../include/SDL/SDL_opengl.h:2742
   GL_INVARIANT_DATATYPE_EXT : constant := 16#87EB#;  --  ../include/SDL/SDL_opengl.h:2743
   GL_LOCAL_CONSTANT_VALUE_EXT : constant := 16#87EC#;  --  ../include/SDL/SDL_opengl.h:2744
   GL_LOCAL_CONSTANT_DATATYPE_EXT : constant := 16#87ED#;  --  ../include/SDL/SDL_opengl.h:2745

   GL_MAX_VERTEX_STREAMS_ATI : constant := 16#876B#;  --  ../include/SDL/SDL_opengl.h:2749
   GL_VERTEX_STREAM0_ATI : constant := 16#876C#;  --  ../include/SDL/SDL_opengl.h:2750
   GL_VERTEX_STREAM1_ATI : constant := 16#876D#;  --  ../include/SDL/SDL_opengl.h:2751
   GL_VERTEX_STREAM2_ATI : constant := 16#876E#;  --  ../include/SDL/SDL_opengl.h:2752
   GL_VERTEX_STREAM3_ATI : constant := 16#876F#;  --  ../include/SDL/SDL_opengl.h:2753
   GL_VERTEX_STREAM4_ATI : constant := 16#8770#;  --  ../include/SDL/SDL_opengl.h:2754
   GL_VERTEX_STREAM5_ATI : constant := 16#8771#;  --  ../include/SDL/SDL_opengl.h:2755
   GL_VERTEX_STREAM6_ATI : constant := 16#8772#;  --  ../include/SDL/SDL_opengl.h:2756
   GL_VERTEX_STREAM7_ATI : constant := 16#8773#;  --  ../include/SDL/SDL_opengl.h:2757
   GL_VERTEX_SOURCE_ATI : constant := 16#8774#;  --  ../include/SDL/SDL_opengl.h:2758

   GL_ELEMENT_ARRAY_ATI : constant := 16#8768#;  --  ../include/SDL/SDL_opengl.h:2762
   GL_ELEMENT_ARRAY_TYPE_ATI : constant := 16#8769#;  --  ../include/SDL/SDL_opengl.h:2763
   GL_ELEMENT_ARRAY_POINTER_ATI : constant := 16#876A#;  --  ../include/SDL/SDL_opengl.h:2764

   GL_QUAD_MESH_SUN : constant := 16#8614#;  --  ../include/SDL/SDL_opengl.h:2768
   GL_TRIANGLE_MESH_SUN : constant := 16#8615#;  --  ../include/SDL/SDL_opengl.h:2769

   GL_SLICE_ACCUM_SUN : constant := 16#85CC#;  --  ../include/SDL/SDL_opengl.h:2773

   GL_MULTISAMPLE_FILTER_HINT_NV : constant := 16#8534#;  --  ../include/SDL/SDL_opengl.h:2777

   GL_DEPTH_CLAMP_NV : constant := 16#864F#;  --  ../include/SDL/SDL_opengl.h:2781

   GL_PIXEL_COUNTER_BITS_NV : constant := 16#8864#;  --  ../include/SDL/SDL_opengl.h:2785
   GL_CURRENT_OCCLUSION_QUERY_ID_NV : constant := 16#8865#;  --  ../include/SDL/SDL_opengl.h:2786
   GL_PIXEL_COUNT_NV : constant := 16#8866#;  --  ../include/SDL/SDL_opengl.h:2787
   GL_PIXEL_COUNT_AVAILABLE_NV : constant := 16#8867#;  --  ../include/SDL/SDL_opengl.h:2788

   GL_POINT_SPRITE_NV : constant := 16#8861#;  --  ../include/SDL/SDL_opengl.h:2792
   GL_COORD_REPLACE_NV : constant := 16#8862#;  --  ../include/SDL/SDL_opengl.h:2793
   GL_POINT_SPRITE_R_MODE_NV : constant := 16#8863#;  --  ../include/SDL/SDL_opengl.h:2794

   GL_OFFSET_PROJECTIVE_TEXTURE_2D_NV : constant := 16#8850#;  --  ../include/SDL/SDL_opengl.h:2798
   GL_OFFSET_PROJECTIVE_TEXTURE_2D_SCALE_NV : constant := 16#8851#;  --  ../include/SDL/SDL_opengl.h:2799
   GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_NV : constant := 16#8852#;  --  ../include/SDL/SDL_opengl.h:2800
   GL_OFFSET_PROJECTIVE_TEXTURE_RECTANGLE_SCALE_NV : constant := 16#8853#;  --  ../include/SDL/SDL_opengl.h:2801
   GL_OFFSET_HILO_TEXTURE_2D_NV : constant := 16#8854#;  --  ../include/SDL/SDL_opengl.h:2802
   GL_OFFSET_HILO_TEXTURE_RECTANGLE_NV : constant := 16#8855#;  --  ../include/SDL/SDL_opengl.h:2803
   GL_OFFSET_HILO_PROJECTIVE_TEXTURE_2D_NV : constant := 16#8856#;  --  ../include/SDL/SDL_opengl.h:2804
   GL_OFFSET_HILO_PROJECTIVE_TEXTURE_RECTANGLE_NV : constant := 16#8857#;  --  ../include/SDL/SDL_opengl.h:2805
   GL_DEPENDENT_HILO_TEXTURE_2D_NV : constant := 16#8858#;  --  ../include/SDL/SDL_opengl.h:2806
   GL_DEPENDENT_RGB_TEXTURE_3D_NV : constant := 16#8859#;  --  ../include/SDL/SDL_opengl.h:2807
   GL_DEPENDENT_RGB_TEXTURE_CUBE_MAP_NV : constant := 16#885A#;  --  ../include/SDL/SDL_opengl.h:2808
   GL_DOT_PRODUCT_PASS_THROUGH_NV : constant := 16#885B#;  --  ../include/SDL/SDL_opengl.h:2809
   GL_DOT_PRODUCT_TEXTURE_1D_NV : constant := 16#885C#;  --  ../include/SDL/SDL_opengl.h:2810
   GL_DOT_PRODUCT_AFFINE_DEPTH_REPLACE_NV : constant := 16#885D#;  --  ../include/SDL/SDL_opengl.h:2811
   GL_HILO8_NV : constant := 16#885E#;  --  ../include/SDL/SDL_opengl.h:2812
   GL_SIGNED_HILO8_NV : constant := 16#885F#;  --  ../include/SDL/SDL_opengl.h:2813
   GL_FORCE_BLUE_TO_ONE_NV : constant := 16#8860#;  --  ../include/SDL/SDL_opengl.h:2814

   GL_STENCIL_TEST_TWO_SIDE_EXT : constant := 16#8910#;  --  ../include/SDL/SDL_opengl.h:2824
   GL_ACTIVE_STENCIL_FACE_EXT : constant := 16#8911#;  --  ../include/SDL/SDL_opengl.h:2825

   GL_TEXT_FRAGMENT_SHADER_ATI : constant := 16#8200#;  --  ../include/SDL/SDL_opengl.h:2829

   GL_UNPACK_CLIENT_STORAGE_APPLE : constant := 16#85B2#;  --  ../include/SDL/SDL_opengl.h:2833

   GL_ELEMENT_ARRAY_APPLE : constant := 16#8768#;  --  ../include/SDL/SDL_opengl.h:2837
   GL_ELEMENT_ARRAY_TYPE_APPLE : constant := 16#8769#;  --  ../include/SDL/SDL_opengl.h:2838
   GL_ELEMENT_ARRAY_POINTER_APPLE : constant := 16#876A#;  --  ../include/SDL/SDL_opengl.h:2839

   GL_DRAW_PIXELS_APPLE : constant := 16#8A0A#;  --  ../include/SDL/SDL_opengl.h:2843
   GL_FENCE_APPLE : constant := 16#8A0B#;  --  ../include/SDL/SDL_opengl.h:2844

   GL_VERTEX_ARRAY_BINDING_APPLE : constant := 16#85B5#;  --  ../include/SDL/SDL_opengl.h:2848

   GL_VERTEX_ARRAY_RANGE_APPLE : constant := 16#851D#;  --  ../include/SDL/SDL_opengl.h:2852
   GL_VERTEX_ARRAY_RANGE_LENGTH_APPLE : constant := 16#851E#;  --  ../include/SDL/SDL_opengl.h:2853
   GL_VERTEX_ARRAY_STORAGE_HINT_APPLE : constant := 16#851F#;  --  ../include/SDL/SDL_opengl.h:2854
   GL_VERTEX_ARRAY_RANGE_POINTER_APPLE : constant := 16#8521#;  --  ../include/SDL/SDL_opengl.h:2855
   GL_STORAGE_CACHED_APPLE : constant := 16#85BE#;  --  ../include/SDL/SDL_opengl.h:2856
   GL_STORAGE_SHARED_APPLE : constant := 16#85BF#;  --  ../include/SDL/SDL_opengl.h:2857

   GL_YCBCR_422_APPLE : constant := 16#85B9#;  --  ../include/SDL/SDL_opengl.h:2861
   GL_UNSIGNED_SHORT_8_8_APPLE : constant := 16#85BA#;  --  ../include/SDL/SDL_opengl.h:2862
   GL_UNSIGNED_SHORT_8_8_REV_APPLE : constant := 16#85BB#;  --  ../include/SDL/SDL_opengl.h:2863

   GL_RGB_S3TC : constant := 16#83A0#;  --  ../include/SDL/SDL_opengl.h:2867
   GL_RGB4_S3TC : constant := 16#83A1#;  --  ../include/SDL/SDL_opengl.h:2868
   GL_RGBA_S3TC : constant := 16#83A2#;  --  ../include/SDL/SDL_opengl.h:2869
   GL_RGBA4_S3TC : constant := 16#83A3#;  --  ../include/SDL/SDL_opengl.h:2870

   GL_MAX_DRAW_BUFFERS_ATI : constant := 16#8824#;  --  ../include/SDL/SDL_opengl.h:2874
   GL_DRAW_BUFFER0_ATI : constant := 16#8825#;  --  ../include/SDL/SDL_opengl.h:2875
   GL_DRAW_BUFFER1_ATI : constant := 16#8826#;  --  ../include/SDL/SDL_opengl.h:2876
   GL_DRAW_BUFFER2_ATI : constant := 16#8827#;  --  ../include/SDL/SDL_opengl.h:2877
   GL_DRAW_BUFFER3_ATI : constant := 16#8828#;  --  ../include/SDL/SDL_opengl.h:2878
   GL_DRAW_BUFFER4_ATI : constant := 16#8829#;  --  ../include/SDL/SDL_opengl.h:2879
   GL_DRAW_BUFFER5_ATI : constant := 16#882A#;  --  ../include/SDL/SDL_opengl.h:2880
   GL_DRAW_BUFFER6_ATI : constant := 16#882B#;  --  ../include/SDL/SDL_opengl.h:2881
   GL_DRAW_BUFFER7_ATI : constant := 16#882C#;  --  ../include/SDL/SDL_opengl.h:2882
   GL_DRAW_BUFFER8_ATI : constant := 16#882D#;  --  ../include/SDL/SDL_opengl.h:2883
   GL_DRAW_BUFFER9_ATI : constant := 16#882E#;  --  ../include/SDL/SDL_opengl.h:2884
   GL_DRAW_BUFFER10_ATI : constant := 16#882F#;  --  ../include/SDL/SDL_opengl.h:2885
   GL_DRAW_BUFFER11_ATI : constant := 16#8830#;  --  ../include/SDL/SDL_opengl.h:2886
   GL_DRAW_BUFFER12_ATI : constant := 16#8831#;  --  ../include/SDL/SDL_opengl.h:2887
   GL_DRAW_BUFFER13_ATI : constant := 16#8832#;  --  ../include/SDL/SDL_opengl.h:2888
   GL_DRAW_BUFFER14_ATI : constant := 16#8833#;  --  ../include/SDL/SDL_opengl.h:2889
   GL_DRAW_BUFFER15_ATI : constant := 16#8834#;  --  ../include/SDL/SDL_opengl.h:2890

   GL_TYPE_RGBA_FLOAT_ATI : constant := 16#8820#;  --  ../include/SDL/SDL_opengl.h:2894
   GL_COLOR_CLEAR_UNCLAMPED_VALUE_ATI : constant := 16#8835#;  --  ../include/SDL/SDL_opengl.h:2895

   GL_MODULATE_ADD_ATI : constant := 16#8744#;  --  ../include/SDL/SDL_opengl.h:2899
   GL_MODULATE_SIGNED_ADD_ATI : constant := 16#8745#;  --  ../include/SDL/SDL_opengl.h:2900
   GL_MODULATE_SUBTRACT_ATI : constant := 16#8746#;  --  ../include/SDL/SDL_opengl.h:2901

   GL_RGBA_FLOAT32_ATI : constant := 16#8814#;  --  ../include/SDL/SDL_opengl.h:2905
   GL_RGB_FLOAT32_ATI : constant := 16#8815#;  --  ../include/SDL/SDL_opengl.h:2906
   GL_ALPHA_FLOAT32_ATI : constant := 16#8816#;  --  ../include/SDL/SDL_opengl.h:2907
   GL_INTENSITY_FLOAT32_ATI : constant := 16#8817#;  --  ../include/SDL/SDL_opengl.h:2908
   GL_LUMINANCE_FLOAT32_ATI : constant := 16#8818#;  --  ../include/SDL/SDL_opengl.h:2909
   GL_LUMINANCE_ALPHA_FLOAT32_ATI : constant := 16#8819#;  --  ../include/SDL/SDL_opengl.h:2910
   GL_RGBA_FLOAT16_ATI : constant := 16#881A#;  --  ../include/SDL/SDL_opengl.h:2911
   GL_RGB_FLOAT16_ATI : constant := 16#881B#;  --  ../include/SDL/SDL_opengl.h:2912
   GL_ALPHA_FLOAT16_ATI : constant := 16#881C#;  --  ../include/SDL/SDL_opengl.h:2913
   GL_INTENSITY_FLOAT16_ATI : constant := 16#881D#;  --  ../include/SDL/SDL_opengl.h:2914
   GL_LUMINANCE_FLOAT16_ATI : constant := 16#881E#;  --  ../include/SDL/SDL_opengl.h:2915
   GL_LUMINANCE_ALPHA_FLOAT16_ATI : constant := 16#881F#;  --  ../include/SDL/SDL_opengl.h:2916

   GL_FLOAT_R_NV : constant := 16#8880#;  --  ../include/SDL/SDL_opengl.h:2920
   GL_FLOAT_RG_NV : constant := 16#8881#;  --  ../include/SDL/SDL_opengl.h:2921
   GL_FLOAT_RGB_NV : constant := 16#8882#;  --  ../include/SDL/SDL_opengl.h:2922
   GL_FLOAT_RGBA_NV : constant := 16#8883#;  --  ../include/SDL/SDL_opengl.h:2923
   GL_FLOAT_R16_NV : constant := 16#8884#;  --  ../include/SDL/SDL_opengl.h:2924
   GL_FLOAT_R32_NV : constant := 16#8885#;  --  ../include/SDL/SDL_opengl.h:2925
   GL_FLOAT_RG16_NV : constant := 16#8886#;  --  ../include/SDL/SDL_opengl.h:2926
   GL_FLOAT_RG32_NV : constant := 16#8887#;  --  ../include/SDL/SDL_opengl.h:2927
   GL_FLOAT_RGB16_NV : constant := 16#8888#;  --  ../include/SDL/SDL_opengl.h:2928
   GL_FLOAT_RGB32_NV : constant := 16#8889#;  --  ../include/SDL/SDL_opengl.h:2929
   GL_FLOAT_RGBA16_NV : constant := 16#888A#;  --  ../include/SDL/SDL_opengl.h:2930
   GL_FLOAT_RGBA32_NV : constant := 16#888B#;  --  ../include/SDL/SDL_opengl.h:2931
   GL_TEXTURE_FLOAT_COMPONENTS_NV : constant := 16#888C#;  --  ../include/SDL/SDL_opengl.h:2932
   GL_FLOAT_CLEAR_COLOR_VALUE_NV : constant := 16#888D#;  --  ../include/SDL/SDL_opengl.h:2933
   GL_FLOAT_RGBA_MODE_NV : constant := 16#888E#;  --  ../include/SDL/SDL_opengl.h:2934

   GL_MAX_FRAGMENT_PROGRAM_LOCAL_PARAMETERS_NV : constant := 16#8868#;  --  ../include/SDL/SDL_opengl.h:2938
   GL_FRAGMENT_PROGRAM_NV : constant := 16#8870#;  --  ../include/SDL/SDL_opengl.h:2939
   GL_MAX_TEXTURE_COORDS_NV : constant := 16#8871#;  --  ../include/SDL/SDL_opengl.h:2940
   GL_MAX_TEXTURE_IMAGE_UNITS_NV : constant := 16#8872#;  --  ../include/SDL/SDL_opengl.h:2941
   GL_FRAGMENT_PROGRAM_BINDING_NV : constant := 16#8873#;  --  ../include/SDL/SDL_opengl.h:2942
   GL_PROGRAM_ERROR_STRING_NV : constant := 16#8874#;  --  ../include/SDL/SDL_opengl.h:2943

   GL_HALF_FLOAT_NV : constant := 16#140B#;  --  ../include/SDL/SDL_opengl.h:2947

   GL_WRITE_PIXEL_DATA_RANGE_NV : constant := 16#8878#;  --  ../include/SDL/SDL_opengl.h:2951
   GL_READ_PIXEL_DATA_RANGE_NV : constant := 16#8879#;  --  ../include/SDL/SDL_opengl.h:2952
   GL_WRITE_PIXEL_DATA_RANGE_LENGTH_NV : constant := 16#887A#;  --  ../include/SDL/SDL_opengl.h:2953
   GL_READ_PIXEL_DATA_RANGE_LENGTH_NV : constant := 16#887B#;  --  ../include/SDL/SDL_opengl.h:2954
   GL_WRITE_PIXEL_DATA_RANGE_POINTER_NV : constant := 16#887C#;  --  ../include/SDL/SDL_opengl.h:2955
   GL_READ_PIXEL_DATA_RANGE_POINTER_NV : constant := 16#887D#;  --  ../include/SDL/SDL_opengl.h:2956

   GL_PRIMITIVE_RESTART_NV : constant := 16#8558#;  --  ../include/SDL/SDL_opengl.h:2960
   GL_PRIMITIVE_RESTART_INDEX_NV : constant := 16#8559#;  --  ../include/SDL/SDL_opengl.h:2961

   GL_TEXTURE_UNSIGNED_REMAP_MODE_NV : constant := 16#888F#;  --  ../include/SDL/SDL_opengl.h:2965

   GL_STENCIL_BACK_FUNC_ATI : constant := 16#8800#;  --  ../include/SDL/SDL_opengl.h:2975
   GL_STENCIL_BACK_FAIL_ATI : constant := 16#8801#;  --  ../include/SDL/SDL_opengl.h:2976
   GL_STENCIL_BACK_PASS_DEPTH_FAIL_ATI : constant := 16#8802#;  --  ../include/SDL/SDL_opengl.h:2977
   GL_STENCIL_BACK_PASS_DEPTH_PASS_ATI : constant := 16#8803#;  --  ../include/SDL/SDL_opengl.h:2978

   GL_IMPLEMENTATION_COLOR_READ_TYPE_OES : constant := 16#8B9A#;  --  ../include/SDL/SDL_opengl.h:2985
   GL_IMPLEMENTATION_COLOR_READ_FORMAT_OES : constant := 16#8B9B#;  --  ../include/SDL/SDL_opengl.h:2986

   GL_DEPTH_BOUNDS_TEST_EXT : constant := 16#8890#;  --  ../include/SDL/SDL_opengl.h:2990
   GL_DEPTH_BOUNDS_EXT : constant := 16#8891#;  --  ../include/SDL/SDL_opengl.h:2991

   GL_MIRROR_CLAMP_EXT : constant := 16#8742#;  --  ../include/SDL/SDL_opengl.h:2995
   GL_MIRROR_CLAMP_TO_EDGE_EXT : constant := 16#8743#;  --  ../include/SDL/SDL_opengl.h:2996
   GL_MIRROR_CLAMP_TO_BORDER_EXT : constant := 16#8912#;  --  ../include/SDL/SDL_opengl.h:2997
   --  unsupported macro: GL_BLEND_EQUATION_RGB_EXT GL_BLEND_EQUATION

   GL_BLEND_EQUATION_ALPHA_EXT : constant := 16#883D#;  --  ../include/SDL/SDL_opengl.h:3002

   GL_PACK_INVERT_MESA : constant := 16#8758#;  --  ../include/SDL/SDL_opengl.h:3006

   GL_UNSIGNED_SHORT_8_8_MESA : constant := 16#85BA#;  --  ../include/SDL/SDL_opengl.h:3010
   GL_UNSIGNED_SHORT_8_8_REV_MESA : constant := 16#85BB#;  --  ../include/SDL/SDL_opengl.h:3011
   GL_YCBCR_MESA : constant := 16#8757#;  --  ../include/SDL/SDL_opengl.h:3012

   GL_PIXEL_PACK_BUFFER_EXT : constant := 16#88EB#;  --  ../include/SDL/SDL_opengl.h:3016
   GL_PIXEL_UNPACK_BUFFER_EXT : constant := 16#88EC#;  --  ../include/SDL/SDL_opengl.h:3017
   GL_PIXEL_PACK_BUFFER_BINDING_EXT : constant := 16#88ED#;  --  ../include/SDL/SDL_opengl.h:3018
   GL_PIXEL_UNPACK_BUFFER_BINDING_EXT : constant := 16#88EF#;  --  ../include/SDL/SDL_opengl.h:3019

   GL_MAX_PROGRAM_EXEC_INSTRUCTIONS_NV : constant := 16#88F4#;  --  ../include/SDL/SDL_opengl.h:3026
   GL_MAX_PROGRAM_CALL_DEPTH_NV : constant := 16#88F5#;  --  ../include/SDL/SDL_opengl.h:3027
   GL_MAX_PROGRAM_IF_DEPTH_NV : constant := 16#88F6#;  --  ../include/SDL/SDL_opengl.h:3028
   GL_MAX_PROGRAM_LOOP_DEPTH_NV : constant := 16#88F7#;  --  ../include/SDL/SDL_opengl.h:3029
   GL_MAX_PROGRAM_LOOP_COUNT_NV : constant := 16#88F8#;  --  ../include/SDL/SDL_opengl.h:3030

   GL_INVALID_FRAMEBUFFER_OPERATION_EXT : constant := 16#0506#;  --  ../include/SDL/SDL_opengl.h:3043
   GL_MAX_RENDERBUFFER_SIZE_EXT : constant := 16#84E8#;  --  ../include/SDL/SDL_opengl.h:3044
   GL_FRAMEBUFFER_BINDING_EXT : constant := 16#8CA6#;  --  ../include/SDL/SDL_opengl.h:3045
   GL_RENDERBUFFER_BINDING_EXT : constant := 16#8CA7#;  --  ../include/SDL/SDL_opengl.h:3046
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE_EXT : constant := 16#8CD0#;  --  ../include/SDL/SDL_opengl.h:3047
   GL_FRAMEBUFFER_ATTACHMENT_OBJECT_NAME_EXT : constant := 16#8CD1#;  --  ../include/SDL/SDL_opengl.h:3048
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL_EXT : constant := 16#8CD2#;  --  ../include/SDL/SDL_opengl.h:3049
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE_EXT : constant := 16#8CD3#;  --  ../include/SDL/SDL_opengl.h:3050
   GL_FRAMEBUFFER_ATTACHMENT_TEXTURE_3D_ZOFFSET_EXT : constant := 16#8CD4#;  --  ../include/SDL/SDL_opengl.h:3051
   GL_FRAMEBUFFER_COMPLETE_EXT : constant := 16#8CD5#;  --  ../include/SDL/SDL_opengl.h:3052
   GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT : constant := 16#8CD6#;  --  ../include/SDL/SDL_opengl.h:3053
   GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT : constant := 16#8CD7#;  --  ../include/SDL/SDL_opengl.h:3054
   GL_FRAMEBUFFER_INCOMPLETE_DUPLICATE_ATTACHMENT_EXT : constant := 16#8CD8#;  --  ../include/SDL/SDL_opengl.h:3055
   GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT : constant := 16#8CD9#;  --  ../include/SDL/SDL_opengl.h:3056
   GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT : constant := 16#8CDA#;  --  ../include/SDL/SDL_opengl.h:3057
   GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT : constant := 16#8CDB#;  --  ../include/SDL/SDL_opengl.h:3058
   GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT : constant := 16#8CDC#;  --  ../include/SDL/SDL_opengl.h:3059
   GL_FRAMEBUFFER_UNSUPPORTED_EXT : constant := 16#8CDD#;  --  ../include/SDL/SDL_opengl.h:3060
   GL_MAX_COLOR_ATTACHMENTS_EXT : constant := 16#8CDF#;  --  ../include/SDL/SDL_opengl.h:3061
   GL_COLOR_ATTACHMENT0_EXT : constant := 16#8CE0#;  --  ../include/SDL/SDL_opengl.h:3062
   GL_COLOR_ATTACHMENT1_EXT : constant := 16#8CE1#;  --  ../include/SDL/SDL_opengl.h:3063
   GL_COLOR_ATTACHMENT2_EXT : constant := 16#8CE2#;  --  ../include/SDL/SDL_opengl.h:3064
   GL_COLOR_ATTACHMENT3_EXT : constant := 16#8CE3#;  --  ../include/SDL/SDL_opengl.h:3065
   GL_COLOR_ATTACHMENT4_EXT : constant := 16#8CE4#;  --  ../include/SDL/SDL_opengl.h:3066
   GL_COLOR_ATTACHMENT5_EXT : constant := 16#8CE5#;  --  ../include/SDL/SDL_opengl.h:3067
   GL_COLOR_ATTACHMENT6_EXT : constant := 16#8CE6#;  --  ../include/SDL/SDL_opengl.h:3068
   GL_COLOR_ATTACHMENT7_EXT : constant := 16#8CE7#;  --  ../include/SDL/SDL_opengl.h:3069
   GL_COLOR_ATTACHMENT8_EXT : constant := 16#8CE8#;  --  ../include/SDL/SDL_opengl.h:3070
   GL_COLOR_ATTACHMENT9_EXT : constant := 16#8CE9#;  --  ../include/SDL/SDL_opengl.h:3071
   GL_COLOR_ATTACHMENT10_EXT : constant := 16#8CEA#;  --  ../include/SDL/SDL_opengl.h:3072
   GL_COLOR_ATTACHMENT11_EXT : constant := 16#8CEB#;  --  ../include/SDL/SDL_opengl.h:3073
   GL_COLOR_ATTACHMENT12_EXT : constant := 16#8CEC#;  --  ../include/SDL/SDL_opengl.h:3074
   GL_COLOR_ATTACHMENT13_EXT : constant := 16#8CED#;  --  ../include/SDL/SDL_opengl.h:3075
   GL_COLOR_ATTACHMENT14_EXT : constant := 16#8CEE#;  --  ../include/SDL/SDL_opengl.h:3076
   GL_COLOR_ATTACHMENT15_EXT : constant := 16#8CEF#;  --  ../include/SDL/SDL_opengl.h:3077
   GL_DEPTH_ATTACHMENT_EXT : constant := 16#8D00#;  --  ../include/SDL/SDL_opengl.h:3078
   GL_STENCIL_ATTACHMENT_EXT : constant := 16#8D20#;  --  ../include/SDL/SDL_opengl.h:3079
   GL_FRAMEBUFFER_EXT : constant := 16#8D40#;  --  ../include/SDL/SDL_opengl.h:3080
   GL_RENDERBUFFER_EXT : constant := 16#8D41#;  --  ../include/SDL/SDL_opengl.h:3081
   GL_RENDERBUFFER_WIDTH_EXT : constant := 16#8D42#;  --  ../include/SDL/SDL_opengl.h:3082
   GL_RENDERBUFFER_HEIGHT_EXT : constant := 16#8D43#;  --  ../include/SDL/SDL_opengl.h:3083
   GL_RENDERBUFFER_INTERNAL_FORMAT_EXT : constant := 16#8D44#;  --  ../include/SDL/SDL_opengl.h:3084
   GL_STENCIL_INDEX1_EXT : constant := 16#8D46#;  --  ../include/SDL/SDL_opengl.h:3085
   GL_STENCIL_INDEX4_EXT : constant := 16#8D47#;  --  ../include/SDL/SDL_opengl.h:3086
   GL_STENCIL_INDEX8_EXT : constant := 16#8D48#;  --  ../include/SDL/SDL_opengl.h:3087
   GL_STENCIL_INDEX16_EXT : constant := 16#8D49#;  --  ../include/SDL/SDL_opengl.h:3088
   GL_RENDERBUFFER_RED_SIZE_EXT : constant := 16#8D50#;  --  ../include/SDL/SDL_opengl.h:3089
   GL_RENDERBUFFER_GREEN_SIZE_EXT : constant := 16#8D51#;  --  ../include/SDL/SDL_opengl.h:3090
   GL_RENDERBUFFER_BLUE_SIZE_EXT : constant := 16#8D52#;  --  ../include/SDL/SDL_opengl.h:3091
   GL_RENDERBUFFER_ALPHA_SIZE_EXT : constant := 16#8D53#;  --  ../include/SDL/SDL_opengl.h:3092
   GL_RENDERBUFFER_DEPTH_SIZE_EXT : constant := 16#8D54#;  --  ../include/SDL/SDL_opengl.h:3093
   GL_RENDERBUFFER_STENCIL_SIZE_EXT : constant := 16#8D55#;  --  ../include/SDL/SDL_opengl.h:3094

   GL_VERSION_1_2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3151

   GL_VERSION_1_3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3233

   GL_VERSION_1_4 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3331

   GL_VERSION_1_5 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3427

   GL_VERSION_2_0 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3471

   GL_ARB_multitexture : constant := 1;  --  ../include/SDL/SDL_opengl.h:3663

   GL_ARB_transpose_matrix : constant := 1;  --  ../include/SDL/SDL_opengl.h:3737

   GL_ARB_multisample : constant := 1;  --  ../include/SDL/SDL_opengl.h:3751

   GL_ARB_texture_env_add : constant := 1;  --  ../include/SDL/SDL_opengl.h:3759

   GL_ARB_texture_cube_map : constant := 1;  --  ../include/SDL/SDL_opengl.h:3763

   GL_ARB_texture_compression : constant := 1;  --  ../include/SDL/SDL_opengl.h:3767

   GL_ARB_texture_border_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:3787

   GL_ARB_point_parameters : constant := 1;  --  ../include/SDL/SDL_opengl.h:3791

   GL_ARB_vertex_blend : constant := 1;  --  ../include/SDL/SDL_opengl.h:3801

   GL_ARB_matrix_palette : constant := 1;  --  ../include/SDL/SDL_opengl.h:3827

   GL_ARB_texture_env_combine : constant := 1;  --  ../include/SDL/SDL_opengl.h:3843

   GL_ARB_texture_env_crossbar : constant := 1;  --  ../include/SDL/SDL_opengl.h:3847

   GL_ARB_texture_env_dot3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:3851

   GL_ARB_texture_mirrored_repeat : constant := 1;  --  ../include/SDL/SDL_opengl.h:3855

   GL_ARB_depth_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:3859

   GL_ARB_shadow : constant := 1;  --  ../include/SDL/SDL_opengl.h:3863

   GL_ARB_shadow_ambient : constant := 1;  --  ../include/SDL/SDL_opengl.h:3867

   GL_ARB_window_pos : constant := 1;  --  ../include/SDL/SDL_opengl.h:3871

   GL_ARB_vertex_program : constant := 1;  --  ../include/SDL/SDL_opengl.h:3909

   GL_ARB_fragment_program : constant := 1;  --  ../include/SDL/SDL_opengl.h:4039

   GL_ARB_vertex_buffer_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:4044

   GL_ARB_occlusion_query : constant := 1;  --  ../include/SDL/SDL_opengl.h:4072

   GL_ARB_shader_objects : constant := 1;  --  ../include/SDL/SDL_opengl.h:4094

   GL_ARB_vertex_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:4178

   GL_ARB_fragment_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:4190

   GL_ARB_shading_language_100 : constant := 1;  --  ../include/SDL/SDL_opengl.h:4194

   GL_ARB_texture_non_power_of_two : constant := 1;  --  ../include/SDL/SDL_opengl.h:4198

   GL_ARB_point_sprite : constant := 1;  --  ../include/SDL/SDL_opengl.h:4202

   GL_ARB_fragment_program_shadow : constant := 1;  --  ../include/SDL/SDL_opengl.h:4206

   GL_ARB_draw_buffers : constant := 1;  --  ../include/SDL/SDL_opengl.h:4210

   GL_ARB_texture_rectangle : constant := 1;  --  ../include/SDL/SDL_opengl.h:4218

   GL_ARB_color_buffer_float : constant := 1;  --  ../include/SDL/SDL_opengl.h:4222

   GL_ARB_half_float_pixel : constant := 1;  --  ../include/SDL/SDL_opengl.h:4230

   GL_ARB_texture_float : constant := 1;  --  ../include/SDL/SDL_opengl.h:4234

   GL_ARB_pixel_buffer_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:4238

   GL_EXT_abgr : constant := 1;  --  ../include/SDL/SDL_opengl.h:4242

   GL_EXT_blend_color : constant := 1;  --  ../include/SDL/SDL_opengl.h:4246

   GL_EXT_polygon_offset : constant := 1;  --  ../include/SDL/SDL_opengl.h:4254

   GL_EXT_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4262

   GL_EXT_texture3D : constant := 1;  --  ../include/SDL/SDL_opengl.h:4266

   GL_SGIS_texture_filter4 : constant := 1;  --  ../include/SDL/SDL_opengl.h:4276

   GL_EXT_subtexture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4286

   GL_EXT_copy_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4296

   GL_EXT_histogram : constant := 1;  --  ../include/SDL/SDL_opengl.h:4312

   GL_EXT_convolution : constant := 1;  --  ../include/SDL/SDL_opengl.h:4338

   GL_EXT_color_matrix : constant := 1;  --  ../include/SDL/SDL_opengl.h:4370

   GL_SGI_color_table : constant := 1;  --  ../include/SDL/SDL_opengl.h:4374

   GL_SGIX_pixel_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4394

   GL_SGIS_pixel_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4402

   GL_SGIS_texture4D : constant := 1;  --  ../include/SDL/SDL_opengl.h:4420

   GL_SGI_texture_color_table : constant := 1;  --  ../include/SDL/SDL_opengl.h:4430

   GL_EXT_cmyka : constant := 1;  --  ../include/SDL/SDL_opengl.h:4434

   GL_EXT_texture_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:4438

   GL_SGIS_detail_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4456

   GL_SGIS_sharpen_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4466

   GL_EXT_packed_pixels : constant := 1;  --  ../include/SDL/SDL_opengl.h:4476

   GL_SGIS_texture_lod : constant := 1;  --  ../include/SDL/SDL_opengl.h:4480

   GL_SGIS_multisample : constant := 1;  --  ../include/SDL/SDL_opengl.h:4484

   GL_EXT_rescale_normal : constant := 1;  --  ../include/SDL/SDL_opengl.h:4494

   GL_EXT_misc_attribute : constant := 1;  --  ../include/SDL/SDL_opengl.h:4522

   GL_SGIS_generate_mipmap : constant := 1;  --  ../include/SDL/SDL_opengl.h:4526

   GL_SGIX_clipmap : constant := 1;  --  ../include/SDL/SDL_opengl.h:4530

   GL_SGIX_shadow : constant := 1;  --  ../include/SDL/SDL_opengl.h:4534

   GL_SGIS_texture_edge_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:4538

   GL_SGIS_texture_border_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:4542

   GL_EXT_blend_minmax : constant := 1;  --  ../include/SDL/SDL_opengl.h:4546

   GL_EXT_blend_subtract : constant := 1;  --  ../include/SDL/SDL_opengl.h:4554

   GL_EXT_blend_logic_op : constant := 1;  --  ../include/SDL/SDL_opengl.h:4558

   GL_SGIX_interlace : constant := 1;  --  ../include/SDL/SDL_opengl.h:4562

   GL_SGIX_pixel_tiles : constant := 1;  --  ../include/SDL/SDL_opengl.h:4566

   GL_SGIX_texture_select : constant := 1;  --  ../include/SDL/SDL_opengl.h:4570

   GL_SGIX_sprite : constant := 1;  --  ../include/SDL/SDL_opengl.h:4574

   GL_SGIX_texture_multi_buffer : constant := 1;  --  ../include/SDL/SDL_opengl.h:4588

   GL_EXT_point_parameters : constant := 1;  --  ../include/SDL/SDL_opengl.h:4592

   GL_SGIS_point_parameters : constant := 1;  --  ../include/SDL/SDL_opengl.h:4602

   GL_SGIX_instruments : constant := 1;  --  ../include/SDL/SDL_opengl.h:4612

   GL_SGIX_texture_scale_bias : constant := 1;  --  ../include/SDL/SDL_opengl.h:4630

   GL_SGIX_framezoom : constant := 1;  --  ../include/SDL/SDL_opengl.h:4634

   GL_SGIX_tag_sample_buffer : constant := 1;  --  ../include/SDL/SDL_opengl.h:4642

   GL_SGIX_polynomial_ffd : constant := 1;  --  ../include/SDL/SDL_opengl.h:4650

   GL_SGIX_reference_plane : constant := 1;  --  ../include/SDL/SDL_opengl.h:4664

   GL_SGIX_flush_raster : constant := 1;  --  ../include/SDL/SDL_opengl.h:4672

   GL_SGIX_depth_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4680

   GL_SGIS_fog_function : constant := 1;  --  ../include/SDL/SDL_opengl.h:4684

   GL_SGIX_fog_offset : constant := 1;  --  ../include/SDL/SDL_opengl.h:4694

   GL_HP_image_transform : constant := 1;  --  ../include/SDL/SDL_opengl.h:4698

   GL_HP_convolution_border_modes : constant := 1;  --  ../include/SDL/SDL_opengl.h:4716

   GL_SGIX_texture_add_env : constant := 1;  --  ../include/SDL/SDL_opengl.h:4720

   GL_EXT_color_subtable : constant := 1;  --  ../include/SDL/SDL_opengl.h:4724

   GL_PGI_vertex_hints : constant := 1;  --  ../include/SDL/SDL_opengl.h:4734

   GL_PGI_misc_hints : constant := 1;  --  ../include/SDL/SDL_opengl.h:4738

   GL_EXT_clip_volume_hint : constant := 1;  --  ../include/SDL/SDL_opengl.h:4760

   GL_SGIX_list_priority : constant := 1;  --  ../include/SDL/SDL_opengl.h:4764

   GL_SGIX_ir_instrument1 : constant := 1;  --  ../include/SDL/SDL_opengl.h:4782

   GL_SGIX_calligraphic_fragment : constant := 1;  --  ../include/SDL/SDL_opengl.h:4786

   GL_SGIX_texture_lod_bias : constant := 1;  --  ../include/SDL/SDL_opengl.h:4790

   GL_SGIX_shadow_ambient : constant := 1;  --  ../include/SDL/SDL_opengl.h:4794

   GL_EXT_index_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4798

   GL_EXT_index_material : constant := 1;  --  ../include/SDL/SDL_opengl.h:4802

   GL_EXT_index_func : constant := 1;  --  ../include/SDL/SDL_opengl.h:4810

   GL_EXT_index_array_formats : constant := 1;  --  ../include/SDL/SDL_opengl.h:4818

   GL_EXT_compiled_vertex_array : constant := 1;  --  ../include/SDL/SDL_opengl.h:4822

   GL_EXT_cull_vertex : constant := 1;  --  ../include/SDL/SDL_opengl.h:4832

   GL_SGIX_ycrcb : constant := 1;  --  ../include/SDL/SDL_opengl.h:4842

   GL_SGIX_fragment_lighting : constant := 1;  --  ../include/SDL/SDL_opengl.h:4846

   GL_IBM_rasterpos_clip : constant := 1;  --  ../include/SDL/SDL_opengl.h:4888

   GL_HP_texture_lighting : constant := 1;  --  ../include/SDL/SDL_opengl.h:4892

   GL_EXT_draw_range_elements : constant := 1;  --  ../include/SDL/SDL_opengl.h:4896

   GL_WIN_phong_shading : constant := 1;  --  ../include/SDL/SDL_opengl.h:4904

   GL_WIN_specular_fog : constant := 1;  --  ../include/SDL/SDL_opengl.h:4908

   GL_EXT_light_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:4912

   GL_SGIX_blend_alpha_minmax : constant := 1;  --  ../include/SDL/SDL_opengl.h:4924

   GL_SGIX_async : constant := 1;  --  ../include/SDL/SDL_opengl.h:4932

   GL_SGIX_async_pixel : constant := 1;  --  ../include/SDL/SDL_opengl.h:4950

   GL_SGIX_async_histogram : constant := 1;  --  ../include/SDL/SDL_opengl.h:4954

   GL_INTEL_parallel_arrays : constant := 1;  --  ../include/SDL/SDL_opengl.h:4958

   GL_HP_occlusion_test : constant := 1;  --  ../include/SDL/SDL_opengl.h:4972

   GL_EXT_pixel_transform : constant := 1;  --  ../include/SDL/SDL_opengl.h:4976

   GL_EXT_pixel_transform_color_table : constant := 1;  --  ../include/SDL/SDL_opengl.h:4990

   GL_EXT_shared_texture_palette : constant := 1;  --  ../include/SDL/SDL_opengl.h:4994

   GL_EXT_separate_specular_color : constant := 1;  --  ../include/SDL/SDL_opengl.h:4998

   GL_EXT_secondary_color : constant := 1;  --  ../include/SDL/SDL_opengl.h:5002

   GL_EXT_texture_perturb_normal : constant := 1;  --  ../include/SDL/SDL_opengl.h:5042

   GL_EXT_multi_draw_arrays : constant := 1;  --  ../include/SDL/SDL_opengl.h:5050

   GL_EXT_fog_coord : constant := 1;  --  ../include/SDL/SDL_opengl.h:5060

   GL_REND_screen_coordinates : constant := 1;  --  ../include/SDL/SDL_opengl.h:5076

   GL_EXT_coordinate_frame : constant := 1;  --  ../include/SDL/SDL_opengl.h:5080

   GL_EXT_texture_env_combine : constant := 1;  --  ../include/SDL/SDL_opengl.h:5130

   GL_APPLE_specular_vector : constant := 1;  --  ../include/SDL/SDL_opengl.h:5134

   GL_APPLE_transform_hint : constant := 1;  --  ../include/SDL/SDL_opengl.h:5138

   GL_SGIX_fog_scale : constant := 1;  --  ../include/SDL/SDL_opengl.h:5142

   GL_SUNX_constant_data : constant := 1;  --  ../include/SDL/SDL_opengl.h:5146

   GL_SUN_global_alpha : constant := 1;  --  ../include/SDL/SDL_opengl.h:5154

   GL_SUN_triangle_list : constant := 1;  --  ../include/SDL/SDL_opengl.h:5176

   GL_SUN_vertex : constant := 1;  --  ../include/SDL/SDL_opengl.h:5196

   GL_EXT_blend_func_separate : constant := 1;  --  ../include/SDL/SDL_opengl.h:5282

   GL_INGR_blend_func_separate : constant := 1;  --  ../include/SDL/SDL_opengl.h:5290

   GL_INGR_color_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:5298

   GL_INGR_interlace_read : constant := 1;  --  ../include/SDL/SDL_opengl.h:5302

   GL_EXT_stencil_wrap : constant := 1;  --  ../include/SDL/SDL_opengl.h:5306

   GL_EXT_422_pixels : constant := 1;  --  ../include/SDL/SDL_opengl.h:5310

   GL_NV_texgen_reflection : constant := 1;  --  ../include/SDL/SDL_opengl.h:5314

   GL_SUN_convolution_border_modes : constant := 1;  --  ../include/SDL/SDL_opengl.h:5318

   GL_EXT_texture_env_add : constant := 1;  --  ../include/SDL/SDL_opengl.h:5322

   GL_EXT_texture_lod_bias : constant := 1;  --  ../include/SDL/SDL_opengl.h:5326

   GL_EXT_texture_filter_anisotropic : constant := 1;  --  ../include/SDL/SDL_opengl.h:5330

   GL_EXT_vertex_weighting : constant := 1;  --  ../include/SDL/SDL_opengl.h:5334

   GL_NV_light_max_exponent : constant := 1;  --  ../include/SDL/SDL_opengl.h:5346

   GL_NV_vertex_array_range : constant := 1;  --  ../include/SDL/SDL_opengl.h:5350

   GL_NV_register_combiners : constant := 1;  --  ../include/SDL/SDL_opengl.h:5360

   GL_NV_fog_distance : constant := 1;  --  ../include/SDL/SDL_opengl.h:5392

   GL_NV_texgen_emboss : constant := 1;  --  ../include/SDL/SDL_opengl.h:5396

   GL_NV_blend_square : constant := 1;  --  ../include/SDL/SDL_opengl.h:5400

   GL_NV_texture_env_combine4 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5404

   GL_MESA_resize_buffers : constant := 1;  --  ../include/SDL/SDL_opengl.h:5408

   GL_MESA_window_pos : constant := 1;  --  ../include/SDL/SDL_opengl.h:5416

   GL_IBM_cull_vertex : constant := 1;  --  ../include/SDL/SDL_opengl.h:5470

   GL_IBM_multimode_draw_arrays : constant := 1;  --  ../include/SDL/SDL_opengl.h:5474

   GL_IBM_vertex_array_lists : constant := 1;  --  ../include/SDL/SDL_opengl.h:5484

   GL_SGIX_subsample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5506

   GL_SGIX_ycrcba : constant := 1;  --  ../include/SDL/SDL_opengl.h:5510

   GL_SGIX_ycrcb_subsample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5514

   GL_SGIX_depth_pass_instrument : constant := 1;  --  ../include/SDL/SDL_opengl.h:5518

   GL_3DFX_texture_compression_FXT1 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5522

   GL_3DFX_multisample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5526

   GL_3DFX_tbuffer : constant := 1;  --  ../include/SDL/SDL_opengl.h:5530

   GL_EXT_multisample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5538

   GL_SGIX_vertex_preclip : constant := 1;  --  ../include/SDL/SDL_opengl.h:5548

   GL_SGIX_convolution_accuracy : constant := 1;  --  ../include/SDL/SDL_opengl.h:5552

   GL_SGIX_resample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5556

   GL_SGIS_point_line_texgen : constant := 1;  --  ../include/SDL/SDL_opengl.h:5560

   GL_SGIS_texture_color_mask : constant := 1;  --  ../include/SDL/SDL_opengl.h:5564

   GL_SGIX_igloo_interface : constant := 1;  --  ../include/SDL/SDL_opengl.h:5572

   GL_EXT_texture_env_dot3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5580

   GL_ATI_texture_mirror_once : constant := 1;  --  ../include/SDL/SDL_opengl.h:5584

   GL_NV_fence : constant := 1;  --  ../include/SDL/SDL_opengl.h:5588

   GL_NV_evaluators : constant := 1;  --  ../include/SDL/SDL_opengl.h:5608

   GL_NV_packed_depth_stencil : constant := 1;  --  ../include/SDL/SDL_opengl.h:5632

   GL_NV_register_combiners2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5636

   GL_NV_texture_compression_vtc : constant := 1;  --  ../include/SDL/SDL_opengl.h:5646

   GL_NV_texture_rectangle : constant := 1;  --  ../include/SDL/SDL_opengl.h:5650

   GL_NV_texture_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:5654

   GL_NV_texture_shader2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5658

   GL_NV_vertex_array_range2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:5662

   GL_NV_vertex_program : constant := 1;  --  ../include/SDL/SDL_opengl.h:5666

   GL_SGIX_texture_coordinate_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:5800

   GL_SGIX_scalebias_hint : constant := 1;  --  ../include/SDL/SDL_opengl.h:5804

   GL_OML_interlace : constant := 1;  --  ../include/SDL/SDL_opengl.h:5808

   GL_OML_subsample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5812

   GL_OML_resample : constant := 1;  --  ../include/SDL/SDL_opengl.h:5816

   GL_NV_copy_depth_to_color : constant := 1;  --  ../include/SDL/SDL_opengl.h:5820

   GL_ATI_envmap_bumpmap : constant := 1;  --  ../include/SDL/SDL_opengl.h:5824

   GL_ATI_fragment_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:5838

   GL_ATI_pn_triangles : constant := 1;  --  ../include/SDL/SDL_opengl.h:5872

   GL_ATI_vertex_array_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:5882

   GL_EXT_vertex_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:5912

   GL_ATI_vertex_streams : constant := 1;  --  ../include/SDL/SDL_opengl.h:6002

   GL_ATI_element_array : constant := 1;  --  ../include/SDL/SDL_opengl.h:6098

   GL_SUN_mesh_array : constant := 1;  --  ../include/SDL/SDL_opengl.h:6110

   GL_SUN_slice_accum : constant := 1;  --  ../include/SDL/SDL_opengl.h:6118

   GL_NV_multisample_filter_hint : constant := 1;  --  ../include/SDL/SDL_opengl.h:6122

   GL_NV_depth_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:6126

   GL_NV_occlusion_query : constant := 1;  --  ../include/SDL/SDL_opengl.h:6130

   GL_NV_point_sprite : constant := 1;  --  ../include/SDL/SDL_opengl.h:6150

   GL_NV_texture_shader3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6160

   GL_NV_vertex_program1_1 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6164

   GL_EXT_shadow_funcs : constant := 1;  --  ../include/SDL/SDL_opengl.h:6168

   GL_EXT_stencil_two_side : constant := 1;  --  ../include/SDL/SDL_opengl.h:6172

   GL_ATI_text_fragment_shader : constant := 1;  --  ../include/SDL/SDL_opengl.h:6180

   GL_APPLE_client_storage : constant := 1;  --  ../include/SDL/SDL_opengl.h:6184

   GL_APPLE_element_array : constant := 1;  --  ../include/SDL/SDL_opengl.h:6188

   GL_APPLE_fence : constant := 1;  --  ../include/SDL/SDL_opengl.h:6204

   GL_APPLE_vertex_array_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:6226

   GL_APPLE_vertex_array_range : constant := 1;  --  ../include/SDL/SDL_opengl.h:6240

   GL_APPLE_ycbcr_422 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6252

   GL_S3_s3tc : constant := 1;  --  ../include/SDL/SDL_opengl.h:6256

   GL_ATI_draw_buffers : constant := 1;  --  ../include/SDL/SDL_opengl.h:6260

   GL_ATI_pixel_format_float : constant := 1;  --  ../include/SDL/SDL_opengl.h:6268

   GL_ATI_texture_env_combine3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6275

   GL_ATI_texture_float : constant := 1;  --  ../include/SDL/SDL_opengl.h:6279

   GL_NV_float_buffer : constant := 1;  --  ../include/SDL/SDL_opengl.h:6283

   GL_NV_fragment_program : constant := 1;  --  ../include/SDL/SDL_opengl.h:6287

   GL_NV_half_float : constant := 1;  --  ../include/SDL/SDL_opengl.h:6306

   GL_NV_pixel_data_range : constant := 1;  --  ../include/SDL/SDL_opengl.h:6404

   GL_NV_primitive_restart : constant := 1;  --  ../include/SDL/SDL_opengl.h:6414

   GL_NV_texture_expand_normal : constant := 1;  --  ../include/SDL/SDL_opengl.h:6424

   GL_NV_vertex_program2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6428

   GL_ATI_map_object_buffer : constant := 1;  --  ../include/SDL/SDL_opengl.h:6432

   GL_ATI_separate_stencil : constant := 1;  --  ../include/SDL/SDL_opengl.h:6442

   GL_ATI_vertex_attrib_array_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:6452

   GL_OES_read_format : constant := 1;  --  ../include/SDL/SDL_opengl.h:6464

   GL_EXT_depth_bounds_test : constant := 1;  --  ../include/SDL/SDL_opengl.h:6468

   GL_EXT_texture_mirror_clamp : constant := 1;  --  ../include/SDL/SDL_opengl.h:6476

   GL_EXT_blend_equation_separate : constant := 1;  --  ../include/SDL/SDL_opengl.h:6480

   GL_MESA_pack_invert : constant := 1;  --  ../include/SDL/SDL_opengl.h:6488

   GL_MESA_ycbcr_texture : constant := 1;  --  ../include/SDL/SDL_opengl.h:6492

   GL_EXT_pixel_buffer_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:6496

   GL_NV_fragment_program_option : constant := 1;  --  ../include/SDL/SDL_opengl.h:6500

   GL_NV_fragment_program2 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6504

   GL_NV_vertex_program2_option : constant := 1;  --  ../include/SDL/SDL_opengl.h:6508

   GL_NV_vertex_program3 : constant := 1;  --  ../include/SDL/SDL_opengl.h:6512

   GL_EXT_framebuffer_object : constant := 1;  --  ../include/SDL/SDL_opengl.h:6516

   GL_GREMEDY_string_marker : constant := 1;  --  ../include/SDL/SDL_opengl.h:6556

   subtype GLchar is char;  -- ../include/SDL/SDL_opengl.h:3106

   subtype GLintptr is umingw_h.ptrdiff_t;  -- ../include/SDL/SDL_opengl.h:3115

   subtype GLsizeiptr is umingw_h.ptrdiff_t;  -- ../include/SDL/SDL_opengl.h:3116

   subtype GLintptrARB is umingw_h.ptrdiff_t;  -- ../include/SDL/SDL_opengl.h:3126

   subtype GLsizeiptrARB is umingw_h.ptrdiff_t;  -- ../include/SDL/SDL_opengl.h:3127

   subtype GLcharARB is char;  -- ../include/SDL/SDL_opengl.h:3133

   subtype GLhandleARB is unsigned;  -- ../include/SDL/SDL_opengl.h:3137

   subtype GLhalfARB is unsigned_short;  -- ../include/SDL/SDL_opengl.h:3143

   subtype GLhalfNV is unsigned_short;  -- ../include/SDL/SDL_opengl.h:3147

   type PFNGLBLENDCOLORPROC is access procedure
        (arg1 : GL_gl_h.GLclampf;
         arg2 : GL_gl_h.GLclampf;
         arg3 : GL_gl_h.GLclampf;
         arg4 : GL_gl_h.GLclampf);
   pragma Convention (C, PFNGLBLENDCOLORPROC);  -- ../include/SDL/SDL_opengl.h:3192

   type PFNGLBLENDEQUATIONPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDEQUATIONPROC);  -- ../include/SDL/SDL_opengl.h:3193

   type PFNGLDRAWRANGEELEMENTSPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLDRAWRANGEELEMENTSPROC);  -- ../include/SDL/SDL_opengl.h:3194

   type PFNGLCOLORTABLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCOLORTABLEPROC);  -- ../include/SDL/SDL_opengl.h:3195

   type PFNGLCOLORTABLEPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLORTABLEPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3196

   type PFNGLCOLORTABLEPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOLORTABLEPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3197

   type PFNGLCOPYCOLORTABLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCOLORTABLEPROC);  -- ../include/SDL/SDL_opengl.h:3198

   type PFNGLGETCOLORTABLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETCOLORTABLEPROC);  -- ../include/SDL/SDL_opengl.h:3199

   type PFNGLGETCOLORTABLEPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3200

   type PFNGLGETCOLORTABLEPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3201

   type PFNGLCOLORSUBTABLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCOLORSUBTABLEPROC);  -- ../include/SDL/SDL_opengl.h:3202

   type PFNGLCOPYCOLORSUBTABLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCOLORSUBTABLEPROC);  -- ../include/SDL/SDL_opengl.h:3203

   type PFNGLCONVOLUTIONFILTER1DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCONVOLUTIONFILTER1DPROC);  -- ../include/SDL/SDL_opengl.h:3204

   type PFNGLCONVOLUTIONFILTER2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCONVOLUTIONFILTER2DPROC);  -- ../include/SDL/SDL_opengl.h:3205

   type PFNGLCONVOLUTIONPARAMETERFPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERFPROC);  -- ../include/SDL/SDL_opengl.h:3206

   type PFNGLCONVOLUTIONPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3207

   type PFNGLCONVOLUTIONPARAMETERIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERIPROC);  -- ../include/SDL/SDL_opengl.h:3208

   type PFNGLCONVOLUTIONPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3209

   type PFNGLCOPYCONVOLUTIONFILTER1DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCONVOLUTIONFILTER1DPROC);  -- ../include/SDL/SDL_opengl.h:3210

   type PFNGLCOPYCONVOLUTIONFILTER2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCONVOLUTIONFILTER2DPROC);  -- ../include/SDL/SDL_opengl.h:3211

   type PFNGLGETCONVOLUTIONFILTERPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETCONVOLUTIONFILTERPROC);  -- ../include/SDL/SDL_opengl.h:3212

   type PFNGLGETCONVOLUTIONPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCONVOLUTIONPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3213

   type PFNGLGETCONVOLUTIONPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCONVOLUTIONPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3214

   type PFNGLGETSEPARABLEFILTERPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address);
   pragma Convention (C, PFNGLGETSEPARABLEFILTERPROC);  -- ../include/SDL/SDL_opengl.h:3215

   type PFNGLSEPARABLEFILTER2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : System.Address;
         arg8 : System.Address);
   pragma Convention (C, PFNGLSEPARABLEFILTER2DPROC);  -- ../include/SDL/SDL_opengl.h:3216

   type PFNGLGETHISTOGRAMPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLboolean;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : System.Address);
   pragma Convention (C, PFNGLGETHISTOGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3217

   type PFNGLGETHISTOGRAMPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETHISTOGRAMPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3218

   type PFNGLGETHISTOGRAMPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETHISTOGRAMPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3219

   type PFNGLGETMINMAXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLboolean;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : System.Address);
   pragma Convention (C, PFNGLGETMINMAXPROC);  -- ../include/SDL/SDL_opengl.h:3220

   type PFNGLGETMINMAXPARAMETERFVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETMINMAXPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3221

   type PFNGLGETMINMAXPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETMINMAXPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3222

   type PFNGLHISTOGRAMPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLHISTOGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3223

   type PFNGLMINMAXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLMINMAXPROC);  -- ../include/SDL/SDL_opengl.h:3224

   type PFNGLRESETHISTOGRAMPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLRESETHISTOGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3225

   type PFNGLRESETMINMAXPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLRESETMINMAXPROC);  -- ../include/SDL/SDL_opengl.h:3226

   type PFNGLTEXIMAGE3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLenum;
         arg9 : GL_gl_h.GLenum;
         arg10 : System.Address);
   pragma Convention (C, PFNGLTEXIMAGE3DPROC);  -- ../include/SDL/SDL_opengl.h:3227

   type PFNGLTEXSUBIMAGE3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLenum;
         arg10 : GL_gl_h.GLenum;
         arg11 : System.Address);
   pragma Convention (C, PFNGLTEXSUBIMAGE3DPROC);  -- ../include/SDL/SDL_opengl.h:3228

   type PFNGLCOPYTEXSUBIMAGE3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYTEXSUBIMAGE3DPROC);  -- ../include/SDL/SDL_opengl.h:3229

   type PFNGLACTIVETEXTUREPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLACTIVETEXTUREPROC);  -- ../include/SDL/SDL_opengl.h:3282

   type PFNGLCLIENTACTIVETEXTUREPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLCLIENTACTIVETEXTUREPROC);  -- ../include/SDL/SDL_opengl.h:3283

   type PFNGLMULTITEXCOORD1DPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD1DPROC);  -- ../include/SDL/SDL_opengl.h:3284

   type PFNGLMULTITEXCOORD1DVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD1DVPROC);  -- ../include/SDL/SDL_opengl.h:3285

   type PFNGLMULTITEXCOORD1FPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD1FPROC);  -- ../include/SDL/SDL_opengl.h:3286

   type PFNGLMULTITEXCOORD1FVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD1FVPROC);  -- ../include/SDL/SDL_opengl.h:3287

   type PFNGLMULTITEXCOORD1IPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD1IPROC);  -- ../include/SDL/SDL_opengl.h:3288

   type PFNGLMULTITEXCOORD1IVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD1IVPROC);  -- ../include/SDL/SDL_opengl.h:3289

   type PFNGLMULTITEXCOORD1SPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD1SPROC);  -- ../include/SDL/SDL_opengl.h:3290

   type PFNGLMULTITEXCOORD1SVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD1SVPROC);  -- ../include/SDL/SDL_opengl.h:3291

   type PFNGLMULTITEXCOORD2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD2DPROC);  -- ../include/SDL/SDL_opengl.h:3292

   type PFNGLMULTITEXCOORD2DVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD2DVPROC);  -- ../include/SDL/SDL_opengl.h:3293

   type PFNGLMULTITEXCOORD2FPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD2FPROC);  -- ../include/SDL/SDL_opengl.h:3294

   type PFNGLMULTITEXCOORD2FVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD2FVPROC);  -- ../include/SDL/SDL_opengl.h:3295

   type PFNGLMULTITEXCOORD2IPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD2IPROC);  -- ../include/SDL/SDL_opengl.h:3296

   type PFNGLMULTITEXCOORD2IVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD2IVPROC);  -- ../include/SDL/SDL_opengl.h:3297

   type PFNGLMULTITEXCOORD2SPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD2SPROC);  -- ../include/SDL/SDL_opengl.h:3298

   type PFNGLMULTITEXCOORD2SVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD2SVPROC);  -- ../include/SDL/SDL_opengl.h:3299

   type PFNGLMULTITEXCOORD3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD3DPROC);  -- ../include/SDL/SDL_opengl.h:3300

   type PFNGLMULTITEXCOORD3DVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD3DVPROC);  -- ../include/SDL/SDL_opengl.h:3301

   type PFNGLMULTITEXCOORD3FPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD3FPROC);  -- ../include/SDL/SDL_opengl.h:3302

   type PFNGLMULTITEXCOORD3FVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD3FVPROC);  -- ../include/SDL/SDL_opengl.h:3303

   type PFNGLMULTITEXCOORD3IPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD3IPROC);  -- ../include/SDL/SDL_opengl.h:3304

   type PFNGLMULTITEXCOORD3IVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD3IVPROC);  -- ../include/SDL/SDL_opengl.h:3305

   type PFNGLMULTITEXCOORD3SPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD3SPROC);  -- ../include/SDL/SDL_opengl.h:3306

   type PFNGLMULTITEXCOORD3SVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD3SVPROC);  -- ../include/SDL/SDL_opengl.h:3307

   type PFNGLMULTITEXCOORD4DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD4DPROC);  -- ../include/SDL/SDL_opengl.h:3308

   type PFNGLMULTITEXCOORD4DVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD4DVPROC);  -- ../include/SDL/SDL_opengl.h:3309

   type PFNGLMULTITEXCOORD4FPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD4FPROC);  -- ../include/SDL/SDL_opengl.h:3310

   type PFNGLMULTITEXCOORD4FVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD4FVPROC);  -- ../include/SDL/SDL_opengl.h:3311

   type PFNGLMULTITEXCOORD4IPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD4IPROC);  -- ../include/SDL/SDL_opengl.h:3312

   type PFNGLMULTITEXCOORD4IVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD4IVPROC);  -- ../include/SDL/SDL_opengl.h:3313

   type PFNGLMULTITEXCOORD4SPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD4SPROC);  -- ../include/SDL/SDL_opengl.h:3314

   type PFNGLMULTITEXCOORD4SVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD4SVPROC);  -- ../include/SDL/SDL_opengl.h:3315

   type PFNGLLOADTRANSPOSEMATRIXFPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLLOADTRANSPOSEMATRIXFPROC);  -- ../include/SDL/SDL_opengl.h:3316

   type PFNGLLOADTRANSPOSEMATRIXDPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLLOADTRANSPOSEMATRIXDPROC);  -- ../include/SDL/SDL_opengl.h:3317

   type PFNGLMULTTRANSPOSEMATRIXFPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTTRANSPOSEMATRIXFPROC);  -- ../include/SDL/SDL_opengl.h:3318

   type PFNGLMULTTRANSPOSEMATRIXDPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTTRANSPOSEMATRIXDPROC);  -- ../include/SDL/SDL_opengl.h:3319

   type PFNGLSAMPLECOVERAGEPROC is access procedure (arg1 : GL_gl_h.GLclampf; arg2 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLSAMPLECOVERAGEPROC);  -- ../include/SDL/SDL_opengl.h:3320

   type PFNGLCOMPRESSEDTEXIMAGE3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLsizei;
         arg9 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE3DPROC);  -- ../include/SDL/SDL_opengl.h:3321

   type PFNGLCOMPRESSEDTEXIMAGE2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLsizei;
         arg8 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE2DPROC);  -- ../include/SDL/SDL_opengl.h:3322

   type PFNGLCOMPRESSEDTEXIMAGE1DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE1DPROC);  -- ../include/SDL/SDL_opengl.h:3323

   type PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLenum;
         arg10 : GL_gl_h.GLsizei;
         arg11 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE3DPROC);  -- ../include/SDL/SDL_opengl.h:3324

   type PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLenum;
         arg8 : GL_gl_h.GLsizei;
         arg9 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE2DPROC);  -- ../include/SDL/SDL_opengl.h:3325

   type PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLsizei;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE1DPROC);  -- ../include/SDL/SDL_opengl.h:3326

   type PFNGLGETCOMPRESSEDTEXIMAGEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETCOMPRESSEDTEXIMAGEPROC);  -- ../include/SDL/SDL_opengl.h:3327

   type PFNGLBLENDFUNCSEPARATEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDFUNCSEPARATEPROC);  -- ../include/SDL/SDL_opengl.h:3379

   type PFNGLFOGCOORDFPROC is access procedure (arg1 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFOGCOORDFPROC);  -- ../include/SDL/SDL_opengl.h:3380

   type PFNGLFOGCOORDFVPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFOGCOORDFVPROC);  -- ../include/SDL/SDL_opengl.h:3381

   type PFNGLFOGCOORDDPROC is access procedure (arg1 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLFOGCOORDDPROC);  -- ../include/SDL/SDL_opengl.h:3382

   type PFNGLFOGCOORDDVPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLFOGCOORDDVPROC);  -- ../include/SDL/SDL_opengl.h:3383

   type PFNGLFOGCOORDPOINTERPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLFOGCOORDPOINTERPROC);  -- ../include/SDL/SDL_opengl.h:3384

   type PFNGLMULTIDRAWARRAYSPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWARRAYSPROC);  -- ../include/SDL/SDL_opengl.h:3385

   type PFNGLMULTIDRAWELEMENTSPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWELEMENTSPROC);  -- ../include/SDL/SDL_opengl.h:3386

   type PFNGLPOINTPARAMETERFPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFPROC);  -- ../include/SDL/SDL_opengl.h:3387

   type PFNGLPOINTPARAMETERFVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFVPROC);  -- ../include/SDL/SDL_opengl.h:3388

   type PFNGLPOINTPARAMETERIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLPOINTPARAMETERIPROC);  -- ../include/SDL/SDL_opengl.h:3389

   type PFNGLPOINTPARAMETERIVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLPOINTPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3390

   type PFNGLSECONDARYCOLOR3BPROC is access procedure
        (arg1 : GL_gl_h.GLbyte;
         arg2 : GL_gl_h.GLbyte;
         arg3 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3BPROC);  -- ../include/SDL/SDL_opengl.h:3391

   type PFNGLSECONDARYCOLOR3BVPROC is access procedure (arg1 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3BVPROC);  -- ../include/SDL/SDL_opengl.h:3392

   type PFNGLSECONDARYCOLOR3DPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLSECONDARYCOLOR3DPROC);  -- ../include/SDL/SDL_opengl.h:3393

   type PFNGLSECONDARYCOLOR3DVPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLSECONDARYCOLOR3DVPROC);  -- ../include/SDL/SDL_opengl.h:3394

   type PFNGLSECONDARYCOLOR3FPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSECONDARYCOLOR3FPROC);  -- ../include/SDL/SDL_opengl.h:3395

   type PFNGLSECONDARYCOLOR3FVPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSECONDARYCOLOR3FVPROC);  -- ../include/SDL/SDL_opengl.h:3396

   type PFNGLSECONDARYCOLOR3IPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3IPROC);  -- ../include/SDL/SDL_opengl.h:3397

   type PFNGLSECONDARYCOLOR3IVPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3IVPROC);  -- ../include/SDL/SDL_opengl.h:3398

   type PFNGLSECONDARYCOLOR3SPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3SPROC);  -- ../include/SDL/SDL_opengl.h:3399

   type PFNGLSECONDARYCOLOR3SVPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3SVPROC);  -- ../include/SDL/SDL_opengl.h:3400

   type PFNGLSECONDARYCOLOR3UBPROC is access procedure
        (arg1 : GL_gl_h.GLubyte;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UBPROC);  -- ../include/SDL/SDL_opengl.h:3401

   type PFNGLSECONDARYCOLOR3UBVPROC is access procedure (arg1 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UBVPROC);  -- ../include/SDL/SDL_opengl.h:3402

   type PFNGLSECONDARYCOLOR3UIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UIPROC);  -- ../include/SDL/SDL_opengl.h:3403

   type PFNGLSECONDARYCOLOR3UIVPROC is access procedure (arg1 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UIVPROC);  -- ../include/SDL/SDL_opengl.h:3404

   type PFNGLSECONDARYCOLOR3USPROC is access procedure
        (arg1 : GL_gl_h.GLushort;
         arg2 : GL_gl_h.GLushort;
         arg3 : GL_gl_h.GLushort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3USPROC);  -- ../include/SDL/SDL_opengl.h:3405

   type PFNGLSECONDARYCOLOR3USVPROC is access procedure (arg1 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3USVPROC);  -- ../include/SDL/SDL_opengl.h:3406

   type PFNGLSECONDARYCOLORPOINTERPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLSECONDARYCOLORPOINTERPROC);  -- ../include/SDL/SDL_opengl.h:3407

   type PFNGLWINDOWPOS2DPROC is access procedure (arg1 : GL_gl_h.GLdouble; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DPROC);  -- ../include/SDL/SDL_opengl.h:3408

   type PFNGLWINDOWPOS2DVPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DVPROC);  -- ../include/SDL/SDL_opengl.h:3409

   type PFNGLWINDOWPOS2FPROC is access procedure (arg1 : GL_gl_h.GLfloat; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FPROC);  -- ../include/SDL/SDL_opengl.h:3410

   type PFNGLWINDOWPOS2FVPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FVPROC);  -- ../include/SDL/SDL_opengl.h:3411

   type PFNGLWINDOWPOS2IPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IPROC);  -- ../include/SDL/SDL_opengl.h:3412

   type PFNGLWINDOWPOS2IVPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IVPROC);  -- ../include/SDL/SDL_opengl.h:3413

   type PFNGLWINDOWPOS2SPROC is access procedure (arg1 : GL_gl_h.GLshort; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SPROC);  -- ../include/SDL/SDL_opengl.h:3414

   type PFNGLWINDOWPOS2SVPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SVPROC);  -- ../include/SDL/SDL_opengl.h:3415

   type PFNGLWINDOWPOS3DPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DPROC);  -- ../include/SDL/SDL_opengl.h:3416

   type PFNGLWINDOWPOS3DVPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DVPROC);  -- ../include/SDL/SDL_opengl.h:3417

   type PFNGLWINDOWPOS3FPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FPROC);  -- ../include/SDL/SDL_opengl.h:3418

   type PFNGLWINDOWPOS3FVPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FVPROC);  -- ../include/SDL/SDL_opengl.h:3419

   type PFNGLWINDOWPOS3IPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IPROC);  -- ../include/SDL/SDL_opengl.h:3420

   type PFNGLWINDOWPOS3IVPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IVPROC);  -- ../include/SDL/SDL_opengl.h:3421

   type PFNGLWINDOWPOS3SPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SPROC);  -- ../include/SDL/SDL_opengl.h:3422

   type PFNGLWINDOWPOS3SVPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SVPROC);  -- ../include/SDL/SDL_opengl.h:3423

   type PFNGLGENQUERIESPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENQUERIESPROC);  -- ../include/SDL/SDL_opengl.h:3449

   type PFNGLDELETEQUERIESPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEQUERIESPROC);  -- ../include/SDL/SDL_opengl.h:3450

   type PFNGLISQUERYPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISQUERYPROC);  -- ../include/SDL/SDL_opengl.h:3451

   type PFNGLBEGINQUERYPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBEGINQUERYPROC);  -- ../include/SDL/SDL_opengl.h:3452

   type PFNGLENDQUERYPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLENDQUERYPROC);  -- ../include/SDL/SDL_opengl.h:3453

   type PFNGLGETQUERYIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETQUERYIVPROC);  -- ../include/SDL/SDL_opengl.h:3454

   type PFNGLGETQUERYOBJECTIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETQUERYOBJECTIVPROC);  -- ../include/SDL/SDL_opengl.h:3455

   type PFNGLGETQUERYOBJECTUIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGETQUERYOBJECTUIVPROC);  -- ../include/SDL/SDL_opengl.h:3456

   type PFNGLBINDBUFFERPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDBUFFERPROC);  -- ../include/SDL/SDL_opengl.h:3457

   type PFNGLDELETEBUFFERSPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEBUFFERSPROC);  -- ../include/SDL/SDL_opengl.h:3458

   type PFNGLGENBUFFERSPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENBUFFERSPROC);  -- ../include/SDL/SDL_opengl.h:3459

   type PFNGLISBUFFERPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISBUFFERPROC);  -- ../include/SDL/SDL_opengl.h:3460

   type PFNGLBUFFERDATAPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLsizeiptr;
         arg3 : System.Address;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBUFFERDATAPROC);  -- ../include/SDL/SDL_opengl.h:3461

   type PFNGLBUFFERSUBDATAPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLintptr;
         arg3 : GLsizeiptr;
         arg4 : System.Address);
   pragma Convention (C, PFNGLBUFFERSUBDATAPROC);  -- ../include/SDL/SDL_opengl.h:3462

   type PFNGLGETBUFFERSUBDATAPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLintptr;
         arg3 : GLsizeiptr;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETBUFFERSUBDATAPROC);  -- ../include/SDL/SDL_opengl.h:3463

   type PFNGLMAPBUFFERPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum) return System.Address;
   pragma Convention (C, PFNGLMAPBUFFERPROC);  -- ../include/SDL/SDL_opengl.h:3464

   type PFNGLUNMAPBUFFERPROC is access function (arg1 : GL_gl_h.GLenum) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLUNMAPBUFFERPROC);  -- ../include/SDL/SDL_opengl.h:3465

   type PFNGLGETBUFFERPARAMETERIVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETBUFFERPARAMETERIVPROC);  -- ../include/SDL/SDL_opengl.h:3466

   type PFNGLGETBUFFERPOINTERVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETBUFFERPOINTERVPROC);  -- ../include/SDL/SDL_opengl.h:3467

   type PFNGLBLENDEQUATIONSEPARATEPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDEQUATIONSEPARATEPROC);  -- ../include/SDL/SDL_opengl.h:3567

   type PFNGLDRAWBUFFERSPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLenum);
   pragma Convention (C, PFNGLDRAWBUFFERSPROC);  -- ../include/SDL/SDL_opengl.h:3568

   type PFNGLSTENCILOPSEPARATEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSTENCILOPSEPARATEPROC);  -- ../include/SDL/SDL_opengl.h:3569

   type PFNGLSTENCILFUNCSEPARATEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSTENCILFUNCSEPARATEPROC);  -- ../include/SDL/SDL_opengl.h:3570

   type PFNGLSTENCILMASKSEPARATEPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSTENCILMASKSEPARATEPROC);  -- ../include/SDL/SDL_opengl.h:3571

   type PFNGLATTACHSHADERPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLATTACHSHADERPROC);  -- ../include/SDL/SDL_opengl.h:3572

   type PFNGLBINDATTRIBLOCATIONPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GLchar);
   pragma Convention (C, PFNGLBINDATTRIBLOCATIONPROC);  -- ../include/SDL/SDL_opengl.h:3573

   type PFNGLCOMPILESHADERPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLCOMPILESHADERPROC);  -- ../include/SDL/SDL_opengl.h:3574

   type PFNGLCREATEPROGRAMPROC is access function return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLCREATEPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3575

   type PFNGLCREATESHADERPROC is access function (arg1 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLCREATESHADERPROC);  -- ../include/SDL/SDL_opengl.h:3576

   type PFNGLDELETEPROGRAMPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3577

   type PFNGLDELETESHADERPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETESHADERPROC);  -- ../include/SDL/SDL_opengl.h:3578

   type PFNGLDETACHSHADERPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDETACHSHADERPROC);  -- ../include/SDL/SDL_opengl.h:3579

   type PFNGLDISABLEVERTEXATTRIBARRAYPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDISABLEVERTEXATTRIBARRAYPROC);  -- ../include/SDL/SDL_opengl.h:3580

   type PFNGLENABLEVERTEXATTRIBARRAYPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLENABLEVERTEXATTRIBARRAYPROC);  -- ../include/SDL/SDL_opengl.h:3581

   type PFNGLGETACTIVEATTRIBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLsizei;
         arg5 : access GL_gl_h.GLint;
         arg6 : access GL_gl_h.GLenum;
         arg7 : access GLchar);
   pragma Convention (C, PFNGLGETACTIVEATTRIBPROC);  -- ../include/SDL/SDL_opengl.h:3582

   type PFNGLGETACTIVEUNIFORMPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLsizei;
         arg5 : access GL_gl_h.GLint;
         arg6 : access GL_gl_h.GLenum;
         arg7 : access GLchar);
   pragma Convention (C, PFNGLGETACTIVEUNIFORMPROC);  -- ../include/SDL/SDL_opengl.h:3583

   type PFNGLGETATTACHEDSHADERSPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGETATTACHEDSHADERSPROC);  -- ../include/SDL/SDL_opengl.h:3584

   type PFNGLGETATTRIBLOCATIONPROC is access function (arg1 : GL_gl_h.GLuint; arg2 : access GLchar) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLGETATTRIBLOCATIONPROC);  -- ../include/SDL/SDL_opengl.h:3585

   type PFNGLGETPROGRAMIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETPROGRAMIVPROC);  -- ../include/SDL/SDL_opengl.h:3586

   type PFNGLGETPROGRAMINFOLOGPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLchar);
   pragma Convention (C, PFNGLGETPROGRAMINFOLOGPROC);  -- ../include/SDL/SDL_opengl.h:3587

   type PFNGLGETSHADERIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETSHADERIVPROC);  -- ../include/SDL/SDL_opengl.h:3588

   type PFNGLGETSHADERINFOLOGPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLchar);
   pragma Convention (C, PFNGLGETSHADERINFOLOGPROC);  -- ../include/SDL/SDL_opengl.h:3589

   type PFNGLGETSHADERSOURCEPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLchar);
   pragma Convention (C, PFNGLGETSHADERSOURCEPROC);  -- ../include/SDL/SDL_opengl.h:3590

   type PFNGLGETUNIFORMLOCATIONPROC is access function (arg1 : GL_gl_h.GLuint; arg2 : access GLchar) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLGETUNIFORMLOCATIONPROC);  -- ../include/SDL/SDL_opengl.h:3591

   type PFNGLGETUNIFORMFVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETUNIFORMFVPROC);  -- ../include/SDL/SDL_opengl.h:3592

   type PFNGLGETUNIFORMIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETUNIFORMIVPROC);  -- ../include/SDL/SDL_opengl.h:3593

   type PFNGLGETVERTEXATTRIBDVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETVERTEXATTRIBDVPROC);  -- ../include/SDL/SDL_opengl.h:3594

   type PFNGLGETVERTEXATTRIBFVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVERTEXATTRIBFVPROC);  -- ../include/SDL/SDL_opengl.h:3595

   type PFNGLGETVERTEXATTRIBIVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVERTEXATTRIBIVPROC);  -- ../include/SDL/SDL_opengl.h:3596

   type PFNGLGETVERTEXATTRIBPOINTERVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETVERTEXATTRIBPOINTERVPROC);  -- ../include/SDL/SDL_opengl.h:3597

   type PFNGLISPROGRAMPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3598

   type PFNGLISSHADERPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISSHADERPROC);  -- ../include/SDL/SDL_opengl.h:3599

   type PFNGLLINKPROGRAMPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLLINKPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3600

   type PFNGLSHADERSOURCEPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLSHADERSOURCEPROC);  -- ../include/SDL/SDL_opengl.h:3601

   type PFNGLUSEPROGRAMPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLUSEPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3602

   type PFNGLUNIFORM1FPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM1FPROC);  -- ../include/SDL/SDL_opengl.h:3603

   type PFNGLUNIFORM2FPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM2FPROC);  -- ../include/SDL/SDL_opengl.h:3604

   type PFNGLUNIFORM3FPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM3FPROC);  -- ../include/SDL/SDL_opengl.h:3605

   type PFNGLUNIFORM4FPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM4FPROC);  -- ../include/SDL/SDL_opengl.h:3606

   type PFNGLUNIFORM1IPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM1IPROC);  -- ../include/SDL/SDL_opengl.h:3607

   type PFNGLUNIFORM2IPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM2IPROC);  -- ../include/SDL/SDL_opengl.h:3608

   type PFNGLUNIFORM3IPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM3IPROC);  -- ../include/SDL/SDL_opengl.h:3609

   type PFNGLUNIFORM4IPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM4IPROC);  -- ../include/SDL/SDL_opengl.h:3610

   type PFNGLUNIFORM1FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM1FVPROC);  -- ../include/SDL/SDL_opengl.h:3611

   type PFNGLUNIFORM2FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM2FVPROC);  -- ../include/SDL/SDL_opengl.h:3612

   type PFNGLUNIFORM3FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM3FVPROC);  -- ../include/SDL/SDL_opengl.h:3613

   type PFNGLUNIFORM4FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM4FVPROC);  -- ../include/SDL/SDL_opengl.h:3614

   type PFNGLUNIFORM1IVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM1IVPROC);  -- ../include/SDL/SDL_opengl.h:3615

   type PFNGLUNIFORM2IVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM2IVPROC);  -- ../include/SDL/SDL_opengl.h:3616

   type PFNGLUNIFORM3IVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM3IVPROC);  -- ../include/SDL/SDL_opengl.h:3617

   type PFNGLUNIFORM4IVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM4IVPROC);  -- ../include/SDL/SDL_opengl.h:3618

   type PFNGLUNIFORMMATRIX2FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX2FVPROC);  -- ../include/SDL/SDL_opengl.h:3619

   type PFNGLUNIFORMMATRIX3FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX3FVPROC);  -- ../include/SDL/SDL_opengl.h:3620

   type PFNGLUNIFORMMATRIX4FVPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX4FVPROC);  -- ../include/SDL/SDL_opengl.h:3621

   type PFNGLVALIDATEPROGRAMPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVALIDATEPROGRAMPROC);  -- ../include/SDL/SDL_opengl.h:3622

   type PFNGLVERTEXATTRIB1DPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DPROC);  -- ../include/SDL/SDL_opengl.h:3623

   type PFNGLVERTEXATTRIB1DVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DVPROC);  -- ../include/SDL/SDL_opengl.h:3624

   type PFNGLVERTEXATTRIB1FPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FPROC);  -- ../include/SDL/SDL_opengl.h:3625

   type PFNGLVERTEXATTRIB1FVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FVPROC);  -- ../include/SDL/SDL_opengl.h:3626

   type PFNGLVERTEXATTRIB1SPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SPROC);  -- ../include/SDL/SDL_opengl.h:3627

   type PFNGLVERTEXATTRIB1SVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SVPROC);  -- ../include/SDL/SDL_opengl.h:3628

   type PFNGLVERTEXATTRIB2DPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DPROC);  -- ../include/SDL/SDL_opengl.h:3629

   type PFNGLVERTEXATTRIB2DVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DVPROC);  -- ../include/SDL/SDL_opengl.h:3630

   type PFNGLVERTEXATTRIB2FPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FPROC);  -- ../include/SDL/SDL_opengl.h:3631

   type PFNGLVERTEXATTRIB2FVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FVPROC);  -- ../include/SDL/SDL_opengl.h:3632

   type PFNGLVERTEXATTRIB2SPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SPROC);  -- ../include/SDL/SDL_opengl.h:3633

   type PFNGLVERTEXATTRIB2SVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SVPROC);  -- ../include/SDL/SDL_opengl.h:3634

   type PFNGLVERTEXATTRIB3DPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DPROC);  -- ../include/SDL/SDL_opengl.h:3635

   type PFNGLVERTEXATTRIB3DVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DVPROC);  -- ../include/SDL/SDL_opengl.h:3636

   type PFNGLVERTEXATTRIB3FPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FPROC);  -- ../include/SDL/SDL_opengl.h:3637

   type PFNGLVERTEXATTRIB3FVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FVPROC);  -- ../include/SDL/SDL_opengl.h:3638

   type PFNGLVERTEXATTRIB3SPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SPROC);  -- ../include/SDL/SDL_opengl.h:3639

   type PFNGLVERTEXATTRIB3SVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SVPROC);  -- ../include/SDL/SDL_opengl.h:3640

   type PFNGLVERTEXATTRIB4NBVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NBVPROC);  -- ../include/SDL/SDL_opengl.h:3641

   type PFNGLVERTEXATTRIB4NIVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXATTRIB4NIVPROC);  -- ../include/SDL/SDL_opengl.h:3642

   type PFNGLVERTEXATTRIB4NSVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4NSVPROC);  -- ../include/SDL/SDL_opengl.h:3643

   type PFNGLVERTEXATTRIB4NUBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUBPROC);  -- ../include/SDL/SDL_opengl.h:3644

   type PFNGLVERTEXATTRIB4NUBVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUBVPROC);  -- ../include/SDL/SDL_opengl.h:3645

   type PFNGLVERTEXATTRIB4NUIVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUIVPROC);  -- ../include/SDL/SDL_opengl.h:3646

   type PFNGLVERTEXATTRIB4NUSVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUSVPROC);  -- ../include/SDL/SDL_opengl.h:3647

   type PFNGLVERTEXATTRIB4BVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4BVPROC);  -- ../include/SDL/SDL_opengl.h:3648

   type PFNGLVERTEXATTRIB4DPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DPROC);  -- ../include/SDL/SDL_opengl.h:3649

   type PFNGLVERTEXATTRIB4DVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DVPROC);  -- ../include/SDL/SDL_opengl.h:3650

   type PFNGLVERTEXATTRIB4FPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FPROC);  -- ../include/SDL/SDL_opengl.h:3651

   type PFNGLVERTEXATTRIB4FVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FVPROC);  -- ../include/SDL/SDL_opengl.h:3652

   type PFNGLVERTEXATTRIB4IVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXATTRIB4IVPROC);  -- ../include/SDL/SDL_opengl.h:3653

   type PFNGLVERTEXATTRIB4SPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SPROC);  -- ../include/SDL/SDL_opengl.h:3654

   type PFNGLVERTEXATTRIB4SVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SVPROC);  -- ../include/SDL/SDL_opengl.h:3655

   type PFNGLVERTEXATTRIB4UBVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4UBVPROC);  -- ../include/SDL/SDL_opengl.h:3656

   type PFNGLVERTEXATTRIB4UIVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVERTEXATTRIB4UIVPROC);  -- ../include/SDL/SDL_opengl.h:3657

   type PFNGLVERTEXATTRIB4USVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLVERTEXATTRIB4USVPROC);  -- ../include/SDL/SDL_opengl.h:3658

   type PFNGLVERTEXATTRIBPOINTERPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLboolean;
         arg5 : GL_gl_h.GLsizei;
         arg6 : System.Address);
   pragma Convention (C, PFNGLVERTEXATTRIBPOINTERPROC);  -- ../include/SDL/SDL_opengl.h:3659

   type PFNGLACTIVETEXTUREARBPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLACTIVETEXTUREARBPROC);  -- ../include/SDL/SDL_opengl.h:3700

   type PFNGLCLIENTACTIVETEXTUREARBPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLCLIENTACTIVETEXTUREARBPROC);  -- ../include/SDL/SDL_opengl.h:3701

   type PFNGLMULTITEXCOORD1DARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD1DARBPROC);  -- ../include/SDL/SDL_opengl.h:3702

   type PFNGLMULTITEXCOORD1DVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD1DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3703

   type PFNGLMULTITEXCOORD1FARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD1FARBPROC);  -- ../include/SDL/SDL_opengl.h:3704

   type PFNGLMULTITEXCOORD1FVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD1FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3705

   type PFNGLMULTITEXCOORD1IARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD1IARBPROC);  -- ../include/SDL/SDL_opengl.h:3706

   type PFNGLMULTITEXCOORD1IVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD1IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3707

   type PFNGLMULTITEXCOORD1SARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD1SARBPROC);  -- ../include/SDL/SDL_opengl.h:3708

   type PFNGLMULTITEXCOORD1SVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD1SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3709

   type PFNGLMULTITEXCOORD2DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD2DARBPROC);  -- ../include/SDL/SDL_opengl.h:3710

   type PFNGLMULTITEXCOORD2DVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD2DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3711

   type PFNGLMULTITEXCOORD2FARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD2FARBPROC);  -- ../include/SDL/SDL_opengl.h:3712

   type PFNGLMULTITEXCOORD2FVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD2FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3713

   type PFNGLMULTITEXCOORD2IARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD2IARBPROC);  -- ../include/SDL/SDL_opengl.h:3714

   type PFNGLMULTITEXCOORD2IVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD2IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3715

   type PFNGLMULTITEXCOORD2SARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD2SARBPROC);  -- ../include/SDL/SDL_opengl.h:3716

   type PFNGLMULTITEXCOORD2SVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD2SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3717

   type PFNGLMULTITEXCOORD3DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD3DARBPROC);  -- ../include/SDL/SDL_opengl.h:3718

   type PFNGLMULTITEXCOORD3DVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD3DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3719

   type PFNGLMULTITEXCOORD3FARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD3FARBPROC);  -- ../include/SDL/SDL_opengl.h:3720

   type PFNGLMULTITEXCOORD3FVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD3FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3721

   type PFNGLMULTITEXCOORD3IARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD3IARBPROC);  -- ../include/SDL/SDL_opengl.h:3722

   type PFNGLMULTITEXCOORD3IVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD3IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3723

   type PFNGLMULTITEXCOORD3SARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD3SARBPROC);  -- ../include/SDL/SDL_opengl.h:3724

   type PFNGLMULTITEXCOORD3SVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD3SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3725

   type PFNGLMULTITEXCOORD4DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD4DARBPROC);  -- ../include/SDL/SDL_opengl.h:3726

   type PFNGLMULTITEXCOORD4DVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTITEXCOORD4DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3727

   type PFNGLMULTITEXCOORD4FARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD4FARBPROC);  -- ../include/SDL/SDL_opengl.h:3728

   type PFNGLMULTITEXCOORD4FVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTITEXCOORD4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3729

   type PFNGLMULTITEXCOORD4IARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD4IARBPROC);  -- ../include/SDL/SDL_opengl.h:3730

   type PFNGLMULTITEXCOORD4IVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTITEXCOORD4IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3731

   type PFNGLMULTITEXCOORD4SARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD4SARBPROC);  -- ../include/SDL/SDL_opengl.h:3732

   type PFNGLMULTITEXCOORD4SVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLMULTITEXCOORD4SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3733

   type PFNGLLOADTRANSPOSEMATRIXFARBPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLLOADTRANSPOSEMATRIXFARBPROC);  -- ../include/SDL/SDL_opengl.h:3744

   type PFNGLLOADTRANSPOSEMATRIXDARBPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLLOADTRANSPOSEMATRIXDARBPROC);  -- ../include/SDL/SDL_opengl.h:3745

   type PFNGLMULTTRANSPOSEMATRIXFARBPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMULTTRANSPOSEMATRIXFARBPROC);  -- ../include/SDL/SDL_opengl.h:3746

   type PFNGLMULTTRANSPOSEMATRIXDARBPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLMULTTRANSPOSEMATRIXDARBPROC);  -- ../include/SDL/SDL_opengl.h:3747

   type PFNGLSAMPLECOVERAGEARBPROC is access procedure (arg1 : GL_gl_h.GLclampf; arg2 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLSAMPLECOVERAGEARBPROC);  -- ../include/SDL/SDL_opengl.h:3755

   type PFNGLCOMPRESSEDTEXIMAGE3DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLsizei;
         arg9 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE3DARBPROC);  -- ../include/SDL/SDL_opengl.h:3777

   type PFNGLCOMPRESSEDTEXIMAGE2DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLsizei;
         arg8 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE2DARBPROC);  -- ../include/SDL/SDL_opengl.h:3778

   type PFNGLCOMPRESSEDTEXIMAGE1DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXIMAGE1DARBPROC);  -- ../include/SDL/SDL_opengl.h:3779

   type PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLenum;
         arg10 : GL_gl_h.GLsizei;
         arg11 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE3DARBPROC);  -- ../include/SDL/SDL_opengl.h:3780

   type PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLenum;
         arg8 : GL_gl_h.GLsizei;
         arg9 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE2DARBPROC);  -- ../include/SDL/SDL_opengl.h:3781

   type PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLsizei;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCOMPRESSEDTEXSUBIMAGE1DARBPROC);  -- ../include/SDL/SDL_opengl.h:3782

   type PFNGLGETCOMPRESSEDTEXIMAGEARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETCOMPRESSEDTEXIMAGEARBPROC);  -- ../include/SDL/SDL_opengl.h:3783

   type PFNGLPOINTPARAMETERFARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFARBPROC);  -- ../include/SDL/SDL_opengl.h:3796

   type PFNGLPOINTPARAMETERFVARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFVARBPROC);  -- ../include/SDL/SDL_opengl.h:3797

   type PFNGLWEIGHTBVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLWEIGHTBVARBPROC);  -- ../include/SDL/SDL_opengl.h:3814

   type PFNGLWEIGHTSVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWEIGHTSVARBPROC);  -- ../include/SDL/SDL_opengl.h:3815

   type PFNGLWEIGHTIVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWEIGHTIVARBPROC);  -- ../include/SDL/SDL_opengl.h:3816

   type PFNGLWEIGHTFVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWEIGHTFVARBPROC);  -- ../include/SDL/SDL_opengl.h:3817

   type PFNGLWEIGHTDVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWEIGHTDVARBPROC);  -- ../include/SDL/SDL_opengl.h:3818

   type PFNGLWEIGHTUBVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLWEIGHTUBVARBPROC);  -- ../include/SDL/SDL_opengl.h:3819

   type PFNGLWEIGHTUSVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLWEIGHTUSVARBPROC);  -- ../include/SDL/SDL_opengl.h:3820

   type PFNGLWEIGHTUIVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLWEIGHTUIVARBPROC);  -- ../include/SDL/SDL_opengl.h:3821

   type PFNGLWEIGHTPOINTERARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLWEIGHTPOINTERARBPROC);  -- ../include/SDL/SDL_opengl.h:3822

   type PFNGLVERTEXBLENDARBPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXBLENDARBPROC);  -- ../include/SDL/SDL_opengl.h:3823

   type PFNGLCURRENTPALETTEMATRIXARBPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCURRENTPALETTEMATRIXARBPROC);  -- ../include/SDL/SDL_opengl.h:3835

   type PFNGLMATRIXINDEXUBVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLMATRIXINDEXUBVARBPROC);  -- ../include/SDL/SDL_opengl.h:3836

   type PFNGLMATRIXINDEXUSVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLMATRIXINDEXUSVARBPROC);  -- ../include/SDL/SDL_opengl.h:3837

   type PFNGLMATRIXINDEXUIVARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLMATRIXINDEXUIVARBPROC);  -- ../include/SDL/SDL_opengl.h:3838

   type PFNGLMATRIXINDEXPOINTERARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLMATRIXINDEXPOINTERARBPROC);  -- ../include/SDL/SDL_opengl.h:3839

   type PFNGLWINDOWPOS2DARBPROC is access procedure (arg1 : GL_gl_h.GLdouble; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DARBPROC);  -- ../include/SDL/SDL_opengl.h:3890

   type PFNGLWINDOWPOS2DVARBPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3891

   type PFNGLWINDOWPOS2FARBPROC is access procedure (arg1 : GL_gl_h.GLfloat; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FARBPROC);  -- ../include/SDL/SDL_opengl.h:3892

   type PFNGLWINDOWPOS2FVARBPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3893

   type PFNGLWINDOWPOS2IARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IARBPROC);  -- ../include/SDL/SDL_opengl.h:3894

   type PFNGLWINDOWPOS2IVARBPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3895

   type PFNGLWINDOWPOS2SARBPROC is access procedure (arg1 : GL_gl_h.GLshort; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SARBPROC);  -- ../include/SDL/SDL_opengl.h:3896

   type PFNGLWINDOWPOS2SVARBPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3897

   type PFNGLWINDOWPOS3DARBPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DARBPROC);  -- ../include/SDL/SDL_opengl.h:3898

   type PFNGLWINDOWPOS3DVARBPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3899

   type PFNGLWINDOWPOS3FARBPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FARBPROC);  -- ../include/SDL/SDL_opengl.h:3900

   type PFNGLWINDOWPOS3FVARBPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3901

   type PFNGLWINDOWPOS3IARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IARBPROC);  -- ../include/SDL/SDL_opengl.h:3902

   type PFNGLWINDOWPOS3IVARBPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IVARBPROC);  -- ../include/SDL/SDL_opengl.h:3903

   type PFNGLWINDOWPOS3SARBPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SARBPROC);  -- ../include/SDL/SDL_opengl.h:3904

   type PFNGLWINDOWPOS3SVARBPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3905

   type PFNGLVERTEXATTRIB1DARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DARBPROC);  -- ../include/SDL/SDL_opengl.h:3974

   type PFNGLVERTEXATTRIB1DVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3975

   type PFNGLVERTEXATTRIB1FARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FARBPROC);  -- ../include/SDL/SDL_opengl.h:3976

   type PFNGLVERTEXATTRIB1FVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3977

   type PFNGLVERTEXATTRIB1SARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SARBPROC);  -- ../include/SDL/SDL_opengl.h:3978

   type PFNGLVERTEXATTRIB1SVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3979

   type PFNGLVERTEXATTRIB2DARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DARBPROC);  -- ../include/SDL/SDL_opengl.h:3980

   type PFNGLVERTEXATTRIB2DVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3981

   type PFNGLVERTEXATTRIB2FARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FARBPROC);  -- ../include/SDL/SDL_opengl.h:3982

   type PFNGLVERTEXATTRIB2FVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3983

   type PFNGLVERTEXATTRIB2SARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SARBPROC);  -- ../include/SDL/SDL_opengl.h:3984

   type PFNGLVERTEXATTRIB2SVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3985

   type PFNGLVERTEXATTRIB3DARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DARBPROC);  -- ../include/SDL/SDL_opengl.h:3986

   type PFNGLVERTEXATTRIB3DVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DVARBPROC);  -- ../include/SDL/SDL_opengl.h:3987

   type PFNGLVERTEXATTRIB3FARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FARBPROC);  -- ../include/SDL/SDL_opengl.h:3988

   type PFNGLVERTEXATTRIB3FVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FVARBPROC);  -- ../include/SDL/SDL_opengl.h:3989

   type PFNGLVERTEXATTRIB3SARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SARBPROC);  -- ../include/SDL/SDL_opengl.h:3990

   type PFNGLVERTEXATTRIB3SVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SVARBPROC);  -- ../include/SDL/SDL_opengl.h:3991

   type PFNGLVERTEXATTRIB4NBVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NBVARBPROC);  -- ../include/SDL/SDL_opengl.h:3992

   type PFNGLVERTEXATTRIB4NIVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXATTRIB4NIVARBPROC);  -- ../include/SDL/SDL_opengl.h:3993

   type PFNGLVERTEXATTRIB4NSVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4NSVARBPROC);  -- ../include/SDL/SDL_opengl.h:3994

   type PFNGLVERTEXATTRIB4NUBARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUBARBPROC);  -- ../include/SDL/SDL_opengl.h:3995

   type PFNGLVERTEXATTRIB4NUBVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUBVARBPROC);  -- ../include/SDL/SDL_opengl.h:3996

   type PFNGLVERTEXATTRIB4NUIVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUIVARBPROC);  -- ../include/SDL/SDL_opengl.h:3997

   type PFNGLVERTEXATTRIB4NUSVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLVERTEXATTRIB4NUSVARBPROC);  -- ../include/SDL/SDL_opengl.h:3998

   type PFNGLVERTEXATTRIB4BVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4BVARBPROC);  -- ../include/SDL/SDL_opengl.h:3999

   type PFNGLVERTEXATTRIB4DARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DARBPROC);  -- ../include/SDL/SDL_opengl.h:4000

   type PFNGLVERTEXATTRIB4DVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DVARBPROC);  -- ../include/SDL/SDL_opengl.h:4001

   type PFNGLVERTEXATTRIB4FARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FARBPROC);  -- ../include/SDL/SDL_opengl.h:4002

   type PFNGLVERTEXATTRIB4FVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4003

   type PFNGLVERTEXATTRIB4IVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXATTRIB4IVARBPROC);  -- ../include/SDL/SDL_opengl.h:4004

   type PFNGLVERTEXATTRIB4SARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SARBPROC);  -- ../include/SDL/SDL_opengl.h:4005

   type PFNGLVERTEXATTRIB4SVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SVARBPROC);  -- ../include/SDL/SDL_opengl.h:4006

   type PFNGLVERTEXATTRIB4UBVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4UBVARBPROC);  -- ../include/SDL/SDL_opengl.h:4007

   type PFNGLVERTEXATTRIB4UIVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVERTEXATTRIB4UIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4008

   type PFNGLVERTEXATTRIB4USVARBPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLVERTEXATTRIB4USVARBPROC);  -- ../include/SDL/SDL_opengl.h:4009

   type PFNGLVERTEXATTRIBPOINTERARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLboolean;
         arg5 : GL_gl_h.GLsizei;
         arg6 : System.Address);
   pragma Convention (C, PFNGLVERTEXATTRIBPOINTERARBPROC);  -- ../include/SDL/SDL_opengl.h:4010

   type PFNGLENABLEVERTEXATTRIBARRAYARBPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLENABLEVERTEXATTRIBARRAYARBPROC);  -- ../include/SDL/SDL_opengl.h:4011

   type PFNGLDISABLEVERTEXATTRIBARRAYARBPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDISABLEVERTEXATTRIBARRAYARBPROC);  -- ../include/SDL/SDL_opengl.h:4012

   type PFNGLPROGRAMSTRINGARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLPROGRAMSTRINGARBPROC);  -- ../include/SDL/SDL_opengl.h:4013

   type PFNGLBINDPROGRAMARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDPROGRAMARBPROC);  -- ../include/SDL/SDL_opengl.h:4014

   type PFNGLDELETEPROGRAMSARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEPROGRAMSARBPROC);  -- ../include/SDL/SDL_opengl.h:4015

   type PFNGLGENPROGRAMSARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENPROGRAMSARBPROC);  -- ../include/SDL/SDL_opengl.h:4016

   type PFNGLPROGRAMENVPARAMETER4DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble;
         arg6 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMENVPARAMETER4DARBPROC);  -- ../include/SDL/SDL_opengl.h:4017

   type PFNGLPROGRAMENVPARAMETER4DVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMENVPARAMETER4DVARBPROC);  -- ../include/SDL/SDL_opengl.h:4018

   type PFNGLPROGRAMENVPARAMETER4FARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMENVPARAMETER4FARBPROC);  -- ../include/SDL/SDL_opengl.h:4019

   type PFNGLPROGRAMENVPARAMETER4FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMENVPARAMETER4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4020

   type PFNGLPROGRAMLOCALPARAMETER4DARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble;
         arg6 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMLOCALPARAMETER4DARBPROC);  -- ../include/SDL/SDL_opengl.h:4021

   type PFNGLPROGRAMLOCALPARAMETER4DVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMLOCALPARAMETER4DVARBPROC);  -- ../include/SDL/SDL_opengl.h:4022

   type PFNGLPROGRAMLOCALPARAMETER4FARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMLOCALPARAMETER4FARBPROC);  -- ../include/SDL/SDL_opengl.h:4023

   type PFNGLPROGRAMLOCALPARAMETER4FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMLOCALPARAMETER4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4024

   type PFNGLGETPROGRAMENVPARAMETERDVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETPROGRAMENVPARAMETERDVARBPROC);  -- ../include/SDL/SDL_opengl.h:4025

   type PFNGLGETPROGRAMENVPARAMETERFVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETPROGRAMENVPARAMETERFVARBPROC);  -- ../include/SDL/SDL_opengl.h:4026

   type PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETPROGRAMLOCALPARAMETERDVARBPROC);  -- ../include/SDL/SDL_opengl.h:4027

   type PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETPROGRAMLOCALPARAMETERFVARBPROC);  -- ../include/SDL/SDL_opengl.h:4028

   type PFNGLGETPROGRAMIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETPROGRAMIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4029

   type PFNGLGETPROGRAMSTRINGARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETPROGRAMSTRINGARBPROC);  -- ../include/SDL/SDL_opengl.h:4030

   type PFNGLGETVERTEXATTRIBDVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETVERTEXATTRIBDVARBPROC);  -- ../include/SDL/SDL_opengl.h:4031

   type PFNGLGETVERTEXATTRIBFVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVERTEXATTRIBFVARBPROC);  -- ../include/SDL/SDL_opengl.h:4032

   type PFNGLGETVERTEXATTRIBIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVERTEXATTRIBIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4033

   type PFNGLGETVERTEXATTRIBPOINTERVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETVERTEXATTRIBPOINTERVARBPROC);  -- ../include/SDL/SDL_opengl.h:4034

   type PFNGLISPROGRAMARBPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISPROGRAMARBPROC);  -- ../include/SDL/SDL_opengl.h:4035

   type PFNGLBINDBUFFERARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDBUFFERARBPROC);  -- ../include/SDL/SDL_opengl.h:4058

   type PFNGLDELETEBUFFERSARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEBUFFERSARBPROC);  -- ../include/SDL/SDL_opengl.h:4059

   type PFNGLGENBUFFERSARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENBUFFERSARBPROC);  -- ../include/SDL/SDL_opengl.h:4060

   type PFNGLISBUFFERARBPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISBUFFERARBPROC);  -- ../include/SDL/SDL_opengl.h:4061

   type PFNGLBUFFERDATAARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLsizeiptrARB;
         arg3 : System.Address;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBUFFERDATAARBPROC);  -- ../include/SDL/SDL_opengl.h:4062

   type PFNGLBUFFERSUBDATAARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLintptrARB;
         arg3 : GLsizeiptrARB;
         arg4 : System.Address);
   pragma Convention (C, PFNGLBUFFERSUBDATAARBPROC);  -- ../include/SDL/SDL_opengl.h:4063

   type PFNGLGETBUFFERSUBDATAARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLintptrARB;
         arg3 : GLsizeiptrARB;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETBUFFERSUBDATAARBPROC);  -- ../include/SDL/SDL_opengl.h:4064

   type PFNGLMAPBUFFERARBPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum) return System.Address;
   pragma Convention (C, PFNGLMAPBUFFERARBPROC);  -- ../include/SDL/SDL_opengl.h:4065

   type PFNGLUNMAPBUFFERARBPROC is access function (arg1 : GL_gl_h.GLenum) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLUNMAPBUFFERARBPROC);  -- ../include/SDL/SDL_opengl.h:4066

   type PFNGLGETBUFFERPARAMETERIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETBUFFERPARAMETERIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4067

   type PFNGLGETBUFFERPOINTERVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETBUFFERPOINTERVARBPROC);  -- ../include/SDL/SDL_opengl.h:4068

   type PFNGLGENQUERIESARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENQUERIESARBPROC);  -- ../include/SDL/SDL_opengl.h:4083

   type PFNGLDELETEQUERIESARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEQUERIESARBPROC);  -- ../include/SDL/SDL_opengl.h:4084

   type PFNGLISQUERYARBPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISQUERYARBPROC);  -- ../include/SDL/SDL_opengl.h:4085

   type PFNGLBEGINQUERYARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBEGINQUERYARBPROC);  -- ../include/SDL/SDL_opengl.h:4086

   type PFNGLENDQUERYARBPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLENDQUERYARBPROC);  -- ../include/SDL/SDL_opengl.h:4087

   type PFNGLGETQUERYIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETQUERYIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4088

   type PFNGLGETQUERYOBJECTIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETQUERYOBJECTIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4089

   type PFNGLGETQUERYOBJECTUIVARBPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGETQUERYOBJECTUIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4090

   type PFNGLDELETEOBJECTARBPROC is access procedure (arg1 : GLhandleARB);
   pragma Convention (C, PFNGLDELETEOBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4136

   type PFNGLGETHANDLEARBPROC is access function (arg1 : GL_gl_h.GLenum) return GLhandleARB;
   pragma Convention (C, PFNGLGETHANDLEARBPROC);  -- ../include/SDL/SDL_opengl.h:4137

   type PFNGLDETACHOBJECTARBPROC is access procedure (arg1 : GLhandleARB; arg2 : GLhandleARB);
   pragma Convention (C, PFNGLDETACHOBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4138

   type PFNGLCREATESHADEROBJECTARBPROC is access function (arg1 : GL_gl_h.GLenum) return GLhandleARB;
   pragma Convention (C, PFNGLCREATESHADEROBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4139

   type PFNGLSHADERSOURCEARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLSHADERSOURCEARBPROC);  -- ../include/SDL/SDL_opengl.h:4140

   type PFNGLCOMPILESHADERARBPROC is access procedure (arg1 : GLhandleARB);
   pragma Convention (C, PFNGLCOMPILESHADERARBPROC);  -- ../include/SDL/SDL_opengl.h:4141

   type PFNGLCREATEPROGRAMOBJECTARBPROC is access function return GLhandleARB;
   pragma Convention (C, PFNGLCREATEPROGRAMOBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4142

   type PFNGLATTACHOBJECTARBPROC is access procedure (arg1 : GLhandleARB; arg2 : GLhandleARB);
   pragma Convention (C, PFNGLATTACHOBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4143

   type PFNGLLINKPROGRAMARBPROC is access procedure (arg1 : GLhandleARB);
   pragma Convention (C, PFNGLLINKPROGRAMARBPROC);  -- ../include/SDL/SDL_opengl.h:4144

   type PFNGLUSEPROGRAMOBJECTARBPROC is access procedure (arg1 : GLhandleARB);
   pragma Convention (C, PFNGLUSEPROGRAMOBJECTARBPROC);  -- ../include/SDL/SDL_opengl.h:4145

   type PFNGLVALIDATEPROGRAMARBPROC is access procedure (arg1 : GLhandleARB);
   pragma Convention (C, PFNGLVALIDATEPROGRAMARBPROC);  -- ../include/SDL/SDL_opengl.h:4146

   type PFNGLUNIFORM1FARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM1FARBPROC);  -- ../include/SDL/SDL_opengl.h:4147

   type PFNGLUNIFORM2FARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM2FARBPROC);  -- ../include/SDL/SDL_opengl.h:4148

   type PFNGLUNIFORM3FARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM3FARBPROC);  -- ../include/SDL/SDL_opengl.h:4149

   type PFNGLUNIFORM4FARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM4FARBPROC);  -- ../include/SDL/SDL_opengl.h:4150

   type PFNGLUNIFORM1IARBPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM1IARBPROC);  -- ../include/SDL/SDL_opengl.h:4151

   type PFNGLUNIFORM2IARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM2IARBPROC);  -- ../include/SDL/SDL_opengl.h:4152

   type PFNGLUNIFORM3IARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM3IARBPROC);  -- ../include/SDL/SDL_opengl.h:4153

   type PFNGLUNIFORM4IARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM4IARBPROC);  -- ../include/SDL/SDL_opengl.h:4154

   type PFNGLUNIFORM1FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM1FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4155

   type PFNGLUNIFORM2FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM2FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4156

   type PFNGLUNIFORM3FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM3FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4157

   type PFNGLUNIFORM4FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORM4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4158

   type PFNGLUNIFORM1IVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM1IVARBPROC);  -- ../include/SDL/SDL_opengl.h:4159

   type PFNGLUNIFORM2IVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM2IVARBPROC);  -- ../include/SDL/SDL_opengl.h:4160

   type PFNGLUNIFORM3IVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM3IVARBPROC);  -- ../include/SDL/SDL_opengl.h:4161

   type PFNGLUNIFORM4IVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLUNIFORM4IVARBPROC);  -- ../include/SDL/SDL_opengl.h:4162

   type PFNGLUNIFORMMATRIX2FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX2FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4163

   type PFNGLUNIFORMMATRIX3FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX3FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4164

   type PFNGLUNIFORMMATRIX4FVARBPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLboolean;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLUNIFORMMATRIX4FVARBPROC);  -- ../include/SDL/SDL_opengl.h:4165

   type PFNGLGETOBJECTPARAMETERFVARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETOBJECTPARAMETERFVARBPROC);  -- ../include/SDL/SDL_opengl.h:4166

   type PFNGLGETOBJECTPARAMETERIVARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETOBJECTPARAMETERIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4167

   type PFNGLGETINFOLOGARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLcharARB);
   pragma Convention (C, PFNGLGETINFOLOGARBPROC);  -- ../include/SDL/SDL_opengl.h:4168

   type PFNGLGETATTACHEDOBJECTSARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLhandleARB);
   pragma Convention (C, PFNGLGETATTACHEDOBJECTSARBPROC);  -- ../include/SDL/SDL_opengl.h:4169

   type PFNGLGETUNIFORMLOCATIONARBPROC is access function (arg1 : GLhandleARB; arg2 : access GLcharARB) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLGETUNIFORMLOCATIONARBPROC);  -- ../include/SDL/SDL_opengl.h:4170

   type PFNGLGETACTIVEUNIFORMARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLsizei;
         arg5 : access GL_gl_h.GLint;
         arg6 : access GL_gl_h.GLenum;
         arg7 : access GLcharARB);
   pragma Convention (C, PFNGLGETACTIVEUNIFORMARBPROC);  -- ../include/SDL/SDL_opengl.h:4171

   type PFNGLGETUNIFORMFVARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETUNIFORMFVARBPROC);  -- ../include/SDL/SDL_opengl.h:4172

   type PFNGLGETUNIFORMIVARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETUNIFORMIVARBPROC);  -- ../include/SDL/SDL_opengl.h:4173

   type PFNGLGETSHADERSOURCEARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : access GLcharARB);
   pragma Convention (C, PFNGLGETSHADERSOURCEARBPROC);  -- ../include/SDL/SDL_opengl.h:4174

   type PFNGLBINDATTRIBLOCATIONARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GLcharARB);
   pragma Convention (C, PFNGLBINDATTRIBLOCATIONARBPROC);  -- ../include/SDL/SDL_opengl.h:4184

   type PFNGLGETACTIVEATTRIBARBPROC is access procedure
        (arg1 : GLhandleARB;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLsizei;
         arg5 : access GL_gl_h.GLint;
         arg6 : access GL_gl_h.GLenum;
         arg7 : access GLcharARB);
   pragma Convention (C, PFNGLGETACTIVEATTRIBARBPROC);  -- ../include/SDL/SDL_opengl.h:4185

   type PFNGLGETATTRIBLOCATIONARBPROC is access function (arg1 : GLhandleARB; arg2 : access GLcharARB) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLGETATTRIBLOCATIONARBPROC);  -- ../include/SDL/SDL_opengl.h:4186

   type PFNGLDRAWBUFFERSARBPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLenum);
   pragma Convention (C, PFNGLDRAWBUFFERSARBPROC);  -- ../include/SDL/SDL_opengl.h:4214

   type PFNGLCLAMPCOLORARBPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLCLAMPCOLORARBPROC);  -- ../include/SDL/SDL_opengl.h:4226

   type PFNGLBLENDCOLOREXTPROC is access procedure
        (arg1 : GL_gl_h.GLclampf;
         arg2 : GL_gl_h.GLclampf;
         arg3 : GL_gl_h.GLclampf;
         arg4 : GL_gl_h.GLclampf);
   pragma Convention (C, PFNGLBLENDCOLOREXTPROC);  -- ../include/SDL/SDL_opengl.h:4250

   type PFNGLPOLYGONOFFSETEXTPROC is access procedure (arg1 : GL_gl_h.GLfloat; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOLYGONOFFSETEXTPROC);  -- ../include/SDL/SDL_opengl.h:4258

   type PFNGLTEXIMAGE3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLenum;
         arg9 : GL_gl_h.GLenum;
         arg10 : System.Address);
   pragma Convention (C, PFNGLTEXIMAGE3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4271

   type PFNGLTEXSUBIMAGE3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLenum;
         arg10 : GL_gl_h.GLenum;
         arg11 : System.Address);
   pragma Convention (C, PFNGLTEXSUBIMAGE3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4272

   type PFNGLGETTEXFILTERFUNCSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETTEXFILTERFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4281

   type PFNGLTEXFILTERFUNCSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXFILTERFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4282

   type PFNGLTEXSUBIMAGE1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : System.Address);
   pragma Convention (C, PFNGLTEXSUBIMAGE1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4291

   type PFNGLTEXSUBIMAGE2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLenum;
         arg8 : GL_gl_h.GLenum;
         arg9 : System.Address);
   pragma Convention (C, PFNGLTEXSUBIMAGE2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4292

   type PFNGLCOPYTEXIMAGE1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOPYTEXIMAGE1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4304

   type PFNGLCOPYTEXIMAGE2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOPYTEXIMAGE2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4305

   type PFNGLCOPYTEXSUBIMAGE1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYTEXSUBIMAGE1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4306

   type PFNGLCOPYTEXSUBIMAGE2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYTEXSUBIMAGE2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4307

   type PFNGLCOPYTEXSUBIMAGE3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYTEXSUBIMAGE3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4308

   type PFNGLGETHISTOGRAMEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLboolean;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : System.Address);
   pragma Convention (C, PFNGLGETHISTOGRAMEXTPROC);  -- ../include/SDL/SDL_opengl.h:4325

   type PFNGLGETHISTOGRAMPARAMETERFVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETHISTOGRAMPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4326

   type PFNGLGETHISTOGRAMPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETHISTOGRAMPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4327

   type PFNGLGETMINMAXEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLboolean;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : System.Address);
   pragma Convention (C, PFNGLGETMINMAXEXTPROC);  -- ../include/SDL/SDL_opengl.h:4328

   type PFNGLGETMINMAXPARAMETERFVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETMINMAXPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4329

   type PFNGLGETMINMAXPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETMINMAXPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4330

   type PFNGLHISTOGRAMEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLHISTOGRAMEXTPROC);  -- ../include/SDL/SDL_opengl.h:4331

   type PFNGLMINMAXEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLMINMAXEXTPROC);  -- ../include/SDL/SDL_opengl.h:4332

   type PFNGLRESETHISTOGRAMEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLRESETHISTOGRAMEXTPROC);  -- ../include/SDL/SDL_opengl.h:4333

   type PFNGLRESETMINMAXEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLRESETMINMAXEXTPROC);  -- ../include/SDL/SDL_opengl.h:4334

   type PFNGLCONVOLUTIONFILTER1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCONVOLUTIONFILTER1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4354

   type PFNGLCONVOLUTIONFILTER2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : System.Address);
   pragma Convention (C, PFNGLCONVOLUTIONFILTER2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4355

   type PFNGLCONVOLUTIONPARAMETERFEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERFEXTPROC);  -- ../include/SDL/SDL_opengl.h:4356

   type PFNGLCONVOLUTIONPARAMETERFVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4357

   type PFNGLCONVOLUTIONPARAMETERIEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERIEXTPROC);  -- ../include/SDL/SDL_opengl.h:4358

   type PFNGLCONVOLUTIONPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLCONVOLUTIONPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4359

   type PFNGLCOPYCONVOLUTIONFILTER1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCONVOLUTIONFILTER1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4360

   type PFNGLCOPYCONVOLUTIONFILTER2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCONVOLUTIONFILTER2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4361

   type PFNGLGETCONVOLUTIONFILTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETCONVOLUTIONFILTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:4362

   type PFNGLGETCONVOLUTIONPARAMETERFVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCONVOLUTIONPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4363

   type PFNGLGETCONVOLUTIONPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCONVOLUTIONPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4364

   type PFNGLGETSEPARABLEFILTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address;
         arg5 : System.Address;
         arg6 : System.Address);
   pragma Convention (C, PFNGLGETSEPARABLEFILTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:4365

   type PFNGLSEPARABLEFILTER2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : System.Address;
         arg8 : System.Address);
   pragma Convention (C, PFNGLSEPARABLEFILTER2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:4366

   type PFNGLCOLORTABLESGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCOLORTABLESGIPROC);  -- ../include/SDL/SDL_opengl.h:4384

   type PFNGLCOLORTABLEPARAMETERFVSGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLORTABLEPARAMETERFVSGIPROC);  -- ../include/SDL/SDL_opengl.h:4385

   type PFNGLCOLORTABLEPARAMETERIVSGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOLORTABLEPARAMETERIVSGIPROC);  -- ../include/SDL/SDL_opengl.h:4386

   type PFNGLCOPYCOLORTABLESGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCOLORTABLESGIPROC);  -- ../include/SDL/SDL_opengl.h:4387

   type PFNGLGETCOLORTABLESGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address);
   pragma Convention (C, PFNGLGETCOLORTABLESGIPROC);  -- ../include/SDL/SDL_opengl.h:4388

   type PFNGLGETCOLORTABLEPARAMETERFVSGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERFVSGIPROC);  -- ../include/SDL/SDL_opengl.h:4389

   type PFNGLGETCOLORTABLEPARAMETERIVSGIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCOLORTABLEPARAMETERIVSGIPROC);  -- ../include/SDL/SDL_opengl.h:4390

   type PFNGLPIXELTEXGENSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLPIXELTEXGENSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4398

   type PFNGLPIXELTEXGENPARAMETERISGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLPIXELTEXGENPARAMETERISGISPROC);  -- ../include/SDL/SDL_opengl.h:4411

   type PFNGLPIXELTEXGENPARAMETERIVSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLPIXELTEXGENPARAMETERIVSGISPROC);  -- ../include/SDL/SDL_opengl.h:4412

   type PFNGLPIXELTEXGENPARAMETERFSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPIXELTEXGENPARAMETERFSGISPROC);  -- ../include/SDL/SDL_opengl.h:4413

   type PFNGLPIXELTEXGENPARAMETERFVSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPIXELTEXGENPARAMETERFVSGISPROC);  -- ../include/SDL/SDL_opengl.h:4414

   type PFNGLGETPIXELTEXGENPARAMETERIVSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETPIXELTEXGENPARAMETERIVSGISPROC);  -- ../include/SDL/SDL_opengl.h:4415

   type PFNGLGETPIXELTEXGENPARAMETERFVSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETPIXELTEXGENPARAMETERFVSGISPROC);  -- ../include/SDL/SDL_opengl.h:4416

   type PFNGLTEXIMAGE4DSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLint;
         arg9 : GL_gl_h.GLenum;
         arg10 : GL_gl_h.GLenum;
         arg11 : System.Address);
   pragma Convention (C, PFNGLTEXIMAGE4DSGISPROC);  -- ../include/SDL/SDL_opengl.h:4425

   type PFNGLTEXSUBIMAGE4DSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLsizei;
         arg8 : GL_gl_h.GLsizei;
         arg9 : GL_gl_h.GLsizei;
         arg10 : GL_gl_h.GLsizei;
         arg11 : GL_gl_h.GLenum;
         arg12 : GL_gl_h.GLenum;
         arg13 : System.Address);
   pragma Convention (C, PFNGLTEXSUBIMAGE4DSGISPROC);  -- ../include/SDL/SDL_opengl.h:4426

   type PFNGLARETEXTURESRESIDENTEXTPROC is access function
        (arg1 : GL_gl_h.GLsizei;
         arg2 : access GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLboolean) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLARETEXTURESRESIDENTEXTPROC);  -- ../include/SDL/SDL_opengl.h:4447

   type PFNGLBINDTEXTUREEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDTEXTUREEXTPROC);  -- ../include/SDL/SDL_opengl.h:4448

   type PFNGLDELETETEXTURESEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETETEXTURESEXTPROC);  -- ../include/SDL/SDL_opengl.h:4449

   type PFNGLGENTEXTURESEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENTEXTURESEXTPROC);  -- ../include/SDL/SDL_opengl.h:4450

   type PFNGLISTEXTUREEXTPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISTEXTUREEXTPROC);  -- ../include/SDL/SDL_opengl.h:4451

   type PFNGLPRIORITIZETEXTURESEXTPROC is access procedure
        (arg1 : GL_gl_h.GLsizei;
         arg2 : access GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLclampf);
   pragma Convention (C, PFNGLPRIORITIZETEXTURESEXTPROC);  -- ../include/SDL/SDL_opengl.h:4452

   type PFNGLDETAILTEXFUNCSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLDETAILTEXFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4461

   type PFNGLGETDETAILTEXFUNCSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETDETAILTEXFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4462

   type PFNGLSHARPENTEXFUNCSGISPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSHARPENTEXFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4471

   type PFNGLGETSHARPENTEXFUNCSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETSHARPENTEXFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4472

   type PFNGLSAMPLEMASKSGISPROC is access procedure (arg1 : GL_gl_h.GLclampf; arg2 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLSAMPLEMASKSGISPROC);  -- ../include/SDL/SDL_opengl.h:4489

   type PFNGLSAMPLEPATTERNSGISPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSAMPLEPATTERNSGISPROC);  -- ../include/SDL/SDL_opengl.h:4490

   type PFNGLBLENDEQUATIONEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDEQUATIONEXTPROC);  -- ../include/SDL/SDL_opengl.h:4550

   type PFNGLSPRITEPARAMETERFSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSPRITEPARAMETERFSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4581

   type PFNGLSPRITEPARAMETERFVSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSPRITEPARAMETERFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4582

   type PFNGLSPRITEPARAMETERISGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLSPRITEPARAMETERISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4583

   type PFNGLSPRITEPARAMETERIVSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLSPRITEPARAMETERIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4584

   type PFNGLPOINTPARAMETERFEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFEXTPROC);  -- ../include/SDL/SDL_opengl.h:4597

   type PFNGLPOINTPARAMETERFVEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4598

   type PFNGLPOINTPARAMETERFSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFSGISPROC);  -- ../include/SDL/SDL_opengl.h:4607

   type PFNGLPOINTPARAMETERFVSGISPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPOINTPARAMETERFVSGISPROC);  -- ../include/SDL/SDL_opengl.h:4608

   type PFNGLGETINSTRUMENTSSGIXPROC is access function return GL_gl_h.GLint;
   pragma Convention (C, PFNGLGETINSTRUMENTSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4621

   type PFNGLINSTRUMENTSBUFFERSGIXPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLINSTRUMENTSBUFFERSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4622

   type PFNGLPOLLINSTRUMENTSSGIXPROC is access function (arg1 : access GL_gl_h.GLint) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLPOLLINSTRUMENTSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4623

   type PFNGLREADINSTRUMENTSSGIXPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLREADINSTRUMENTSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4624

   type PFNGLSTARTINSTRUMENTSSGIXPROC is access procedure;
   pragma Convention (C, PFNGLSTARTINSTRUMENTSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4625

   type PFNGLSTOPINSTRUMENTSSGIXPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLSTOPINSTRUMENTSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4626

   type PFNGLFRAMEZOOMSGIXPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAMEZOOMSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4638

   type PFNGLTAGSAMPLEBUFFERSGIXPROC is access procedure;
   pragma Convention (C, PFNGLTAGSAMPLEBUFFERSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4646

   type PFNGLDEFORMATIONMAP3DSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLdouble;
         arg7 : GL_gl_h.GLdouble;
         arg8 : GL_gl_h.GLint;
         arg9 : GL_gl_h.GLint;
         arg10 : GL_gl_h.GLdouble;
         arg11 : GL_gl_h.GLdouble;
         arg12 : GL_gl_h.GLint;
         arg13 : GL_gl_h.GLint;
         arg14 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLDEFORMATIONMAP3DSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4657

   type PFNGLDEFORMATIONMAP3FSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLint;
         arg9 : GL_gl_h.GLint;
         arg10 : GL_gl_h.GLfloat;
         arg11 : GL_gl_h.GLfloat;
         arg12 : GL_gl_h.GLint;
         arg13 : GL_gl_h.GLint;
         arg14 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLDEFORMATIONMAP3FSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4658

   type PFNGLDEFORMSGIXPROC is access procedure (arg1 : GL_gl_h.GLbitfield);
   pragma Convention (C, PFNGLDEFORMSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4659

   type PFNGLLOADIDENTITYDEFORMATIONMAPSGIXPROC is access procedure (arg1 : GL_gl_h.GLbitfield);
   pragma Convention (C, PFNGLLOADIDENTITYDEFORMATIONMAPSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4660

   type PFNGLREFERENCEPLANESGIXPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLREFERENCEPLANESGIXPROC);  -- ../include/SDL/SDL_opengl.h:4668

   type PFNGLFLUSHRASTERSGIXPROC is access procedure;
   pragma Convention (C, PFNGLFLUSHRASTERSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4676

   type PFNGLFOGFUNCSGISPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFOGFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4689

   type PFNGLGETFOGFUNCSGISPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETFOGFUNCSGISPROC);  -- ../include/SDL/SDL_opengl.h:4690

   type PFNGLIMAGETRANSFORMPARAMETERIHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLIMAGETRANSFORMPARAMETERIHPPROC);  -- ../include/SDL/SDL_opengl.h:4707

   type PFNGLIMAGETRANSFORMPARAMETERFHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLIMAGETRANSFORMPARAMETERFHPPROC);  -- ../include/SDL/SDL_opengl.h:4708

   type PFNGLIMAGETRANSFORMPARAMETERIVHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLIMAGETRANSFORMPARAMETERIVHPPROC);  -- ../include/SDL/SDL_opengl.h:4709

   type PFNGLIMAGETRANSFORMPARAMETERFVHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLIMAGETRANSFORMPARAMETERFVHPPROC);  -- ../include/SDL/SDL_opengl.h:4710

   type PFNGLGETIMAGETRANSFORMPARAMETERIVHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETIMAGETRANSFORMPARAMETERIVHPPROC);  -- ../include/SDL/SDL_opengl.h:4711

   type PFNGLGETIMAGETRANSFORMPARAMETERFVHPPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETIMAGETRANSFORMPARAMETERFVHPPROC);  -- ../include/SDL/SDL_opengl.h:4712

   type PFNGLCOLORSUBTABLEEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLCOLORSUBTABLEEXTPROC);  -- ../include/SDL/SDL_opengl.h:4729

   type PFNGLCOPYCOLORSUBTABLEEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLCOPYCOLORSUBTABLEEXTPROC);  -- ../include/SDL/SDL_opengl.h:4730

   type PFNGLHINTPGIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLHINTPGIPROC);  -- ../include/SDL/SDL_opengl.h:4742

   type PFNGLGETLISTPARAMETERFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETLISTPARAMETERFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4773

   type PFNGLGETLISTPARAMETERIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETLISTPARAMETERIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4774

   type PFNGLLISTPARAMETERFSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLLISTPARAMETERFSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4775

   type PFNGLLISTPARAMETERFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLLISTPARAMETERFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4776

   type PFNGLLISTPARAMETERISGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLLISTPARAMETERISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4777

   type PFNGLLISTPARAMETERIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLLISTPARAMETERIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4778

   type PFNGLINDEXMATERIALEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLINDEXMATERIALEXTPROC);  -- ../include/SDL/SDL_opengl.h:4806

   type PFNGLINDEXFUNCEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLclampf);
   pragma Convention (C, PFNGLINDEXFUNCEXTPROC);  -- ../include/SDL/SDL_opengl.h:4814

   type PFNGLLOCKARRAYSEXTPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLLOCKARRAYSEXTPROC);  -- ../include/SDL/SDL_opengl.h:4827

   type PFNGLUNLOCKARRAYSEXTPROC is access procedure;
   pragma Convention (C, PFNGLUNLOCKARRAYSEXTPROC);  -- ../include/SDL/SDL_opengl.h:4828

   type PFNGLCULLPARAMETERDVEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLCULLPARAMETERDVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4837

   type PFNGLCULLPARAMETERFVEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCULLPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4838

   type PFNGLFRAGMENTCOLORMATERIALSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLFRAGMENTCOLORMATERIALSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4867

   type PFNGLFRAGMENTLIGHTFSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTLIGHTFSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4868

   type PFNGLFRAGMENTLIGHTFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTLIGHTFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4869

   type PFNGLFRAGMENTLIGHTISGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTLIGHTISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4870

   type PFNGLFRAGMENTLIGHTIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTLIGHTIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4871

   type PFNGLFRAGMENTLIGHTMODELFSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTLIGHTMODELFSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4872

   type PFNGLFRAGMENTLIGHTMODELFVSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTLIGHTMODELFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4873

   type PFNGLFRAGMENTLIGHTMODELISGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTLIGHTMODELISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4874

   type PFNGLFRAGMENTLIGHTMODELIVSGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTLIGHTMODELIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4875

   type PFNGLFRAGMENTMATERIALFSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTMATERIALFSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4876

   type PFNGLFRAGMENTMATERIALFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFRAGMENTMATERIALFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4877

   type PFNGLFRAGMENTMATERIALISGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTMATERIALISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4878

   type PFNGLFRAGMENTMATERIALIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAGMENTMATERIALIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4879

   type PFNGLGETFRAGMENTLIGHTFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETFRAGMENTLIGHTFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4880

   type PFNGLGETFRAGMENTLIGHTIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETFRAGMENTLIGHTIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4881

   type PFNGLGETFRAGMENTMATERIALFVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETFRAGMENTMATERIALFVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4882

   type PFNGLGETFRAGMENTMATERIALIVSGIXPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETFRAGMENTMATERIALIVSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4883

   type PFNGLLIGHTENVISGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLLIGHTENVISGIXPROC);  -- ../include/SDL/SDL_opengl.h:4884

   type PFNGLDRAWRANGEELEMENTSEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLenum;
         arg6 : System.Address);
   pragma Convention (C, PFNGLDRAWRANGEELEMENTSEXTPROC);  -- ../include/SDL/SDL_opengl.h:4900

   type PFNGLAPPLYTEXTUREEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLAPPLYTEXTUREEXTPROC);  -- ../include/SDL/SDL_opengl.h:4918

   type PFNGLTEXTURELIGHTEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLTEXTURELIGHTEXTPROC);  -- ../include/SDL/SDL_opengl.h:4919

   type PFNGLTEXTUREMATERIALEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLTEXTUREMATERIALEXTPROC);  -- ../include/SDL/SDL_opengl.h:4920

   type PFNGLASYNCMARKERSGIXPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLASYNCMARKERSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4941

   type PFNGLFINISHASYNCSGIXPROC is access function (arg1 : access GL_gl_h.GLuint) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLFINISHASYNCSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4942

   type PFNGLPOLLASYNCSGIXPROC is access function (arg1 : access GL_gl_h.GLuint) return GL_gl_h.GLint;
   pragma Convention (C, PFNGLPOLLASYNCSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4943

   type PFNGLGENASYNCMARKERSSGIXPROC is access function (arg1 : GL_gl_h.GLsizei) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLGENASYNCMARKERSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4944

   type PFNGLDELETEASYNCMARKERSSGIXPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDELETEASYNCMARKERSSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4945

   type PFNGLISASYNCMARKERSGIXPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISASYNCMARKERSGIXPROC);  -- ../include/SDL/SDL_opengl.h:4946

   type PFNGLVERTEXPOINTERVINTELPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLVERTEXPOINTERVINTELPROC);  -- ../include/SDL/SDL_opengl.h:4965

   type PFNGLNORMALPOINTERVINTELPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, PFNGLNORMALPOINTERVINTELPROC);  -- ../include/SDL/SDL_opengl.h:4966

   type PFNGLCOLORPOINTERVINTELPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLCOLORPOINTERVINTELPROC);  -- ../include/SDL/SDL_opengl.h:4967

   type PFNGLTEXCOORDPOINTERVINTELPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLTEXCOORDPOINTERVINTELPROC);  -- ../include/SDL/SDL_opengl.h:4968

   type PFNGLPIXELTRANSFORMPARAMETERIEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLPIXELTRANSFORMPARAMETERIEXTPROC);  -- ../include/SDL/SDL_opengl.h:4983

   type PFNGLPIXELTRANSFORMPARAMETERFEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPIXELTRANSFORMPARAMETERFEXTPROC);  -- ../include/SDL/SDL_opengl.h:4984

   type PFNGLPIXELTRANSFORMPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLPIXELTRANSFORMPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4985

   type PFNGLPIXELTRANSFORMPARAMETERFVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPIXELTRANSFORMPARAMETERFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:4986

   type PFNGLSECONDARYCOLOR3BEXTPROC is access procedure
        (arg1 : GL_gl_h.GLbyte;
         arg2 : GL_gl_h.GLbyte;
         arg3 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3BEXTPROC);  -- ../include/SDL/SDL_opengl.h:5022

   type PFNGLSECONDARYCOLOR3BVEXTPROC is access procedure (arg1 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3BVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5023

   type PFNGLSECONDARYCOLOR3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLSECONDARYCOLOR3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:5024

   type PFNGLSECONDARYCOLOR3DVEXTPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLSECONDARYCOLOR3DVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5025

   type PFNGLSECONDARYCOLOR3FEXTPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSECONDARYCOLOR3FEXTPROC);  -- ../include/SDL/SDL_opengl.h:5026

   type PFNGLSECONDARYCOLOR3FVEXTPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSECONDARYCOLOR3FVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5027

   type PFNGLSECONDARYCOLOR3IEXTPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3IEXTPROC);  -- ../include/SDL/SDL_opengl.h:5028

   type PFNGLSECONDARYCOLOR3IVEXTPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3IVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5029

   type PFNGLSECONDARYCOLOR3SEXTPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3SEXTPROC);  -- ../include/SDL/SDL_opengl.h:5030

   type PFNGLSECONDARYCOLOR3SVEXTPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3SVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5031

   type PFNGLSECONDARYCOLOR3UBEXTPROC is access procedure
        (arg1 : GL_gl_h.GLubyte;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UBEXTPROC);  -- ../include/SDL/SDL_opengl.h:5032

   type PFNGLSECONDARYCOLOR3UBVEXTPROC is access procedure (arg1 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UBVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5033

   type PFNGLSECONDARYCOLOR3UIEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UIEXTPROC);  -- ../include/SDL/SDL_opengl.h:5034

   type PFNGLSECONDARYCOLOR3UIVEXTPROC is access procedure (arg1 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSECONDARYCOLOR3UIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5035

   type PFNGLSECONDARYCOLOR3USEXTPROC is access procedure
        (arg1 : GL_gl_h.GLushort;
         arg2 : GL_gl_h.GLushort;
         arg3 : GL_gl_h.GLushort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3USEXTPROC);  -- ../include/SDL/SDL_opengl.h:5036

   type PFNGLSECONDARYCOLOR3USVEXTPROC is access procedure (arg1 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLSECONDARYCOLOR3USVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5037

   type PFNGLSECONDARYCOLORPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLSECONDARYCOLORPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5038

   type PFNGLTEXTURENORMALEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLTEXTURENORMALEXTPROC);  -- ../include/SDL/SDL_opengl.h:5046

   type PFNGLMULTIDRAWARRAYSEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWARRAYSEXTPROC);  -- ../include/SDL/SDL_opengl.h:5055

   type PFNGLMULTIDRAWELEMENTSEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWELEMENTSEXTPROC);  -- ../include/SDL/SDL_opengl.h:5056

   type PFNGLFOGCOORDFEXTPROC is access procedure (arg1 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFOGCOORDFEXTPROC);  -- ../include/SDL/SDL_opengl.h:5068

   type PFNGLFOGCOORDFVEXTPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLFOGCOORDFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5069

   type PFNGLFOGCOORDDEXTPROC is access procedure (arg1 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLFOGCOORDDEXTPROC);  -- ../include/SDL/SDL_opengl.h:5070

   type PFNGLFOGCOORDDVEXTPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLFOGCOORDDVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5071

   type PFNGLFOGCOORDPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLFOGCOORDPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5072

   type PFNGLTANGENT3BEXTPROC is access procedure
        (arg1 : GL_gl_h.GLbyte;
         arg2 : GL_gl_h.GLbyte;
         arg3 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLTANGENT3BEXTPROC);  -- ../include/SDL/SDL_opengl.h:5105

   type PFNGLTANGENT3BVEXTPROC is access procedure (arg1 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLTANGENT3BVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5106

   type PFNGLTANGENT3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLTANGENT3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:5107

   type PFNGLTANGENT3DVEXTPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLTANGENT3DVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5108

   type PFNGLTANGENT3FEXTPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTANGENT3FEXTPROC);  -- ../include/SDL/SDL_opengl.h:5109

   type PFNGLTANGENT3FVEXTPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTANGENT3FVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5110

   type PFNGLTANGENT3IEXTPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLTANGENT3IEXTPROC);  -- ../include/SDL/SDL_opengl.h:5111

   type PFNGLTANGENT3IVEXTPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLTANGENT3IVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5112

   type PFNGLTANGENT3SEXTPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLTANGENT3SEXTPROC);  -- ../include/SDL/SDL_opengl.h:5113

   type PFNGLTANGENT3SVEXTPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLTANGENT3SVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5114

   type PFNGLBINORMAL3BEXTPROC is access procedure
        (arg1 : GL_gl_h.GLbyte;
         arg2 : GL_gl_h.GLbyte;
         arg3 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLBINORMAL3BEXTPROC);  -- ../include/SDL/SDL_opengl.h:5115

   type PFNGLBINORMAL3BVEXTPROC is access procedure (arg1 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLBINORMAL3BVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5116

   type PFNGLBINORMAL3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLBINORMAL3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:5117

   type PFNGLBINORMAL3DVEXTPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLBINORMAL3DVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5118

   type PFNGLBINORMAL3FEXTPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLBINORMAL3FEXTPROC);  -- ../include/SDL/SDL_opengl.h:5119

   type PFNGLBINORMAL3FVEXTPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLBINORMAL3FVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5120

   type PFNGLBINORMAL3IEXTPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLBINORMAL3IEXTPROC);  -- ../include/SDL/SDL_opengl.h:5121

   type PFNGLBINORMAL3IVEXTPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLBINORMAL3IVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5122

   type PFNGLBINORMAL3SEXTPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLBINORMAL3SEXTPROC);  -- ../include/SDL/SDL_opengl.h:5123

   type PFNGLBINORMAL3SVEXTPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLBINORMAL3SVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5124

   type PFNGLTANGENTPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLTANGENTPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5125

   type PFNGLBINORMALPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLBINORMALPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5126

   type PFNGLFINISHTEXTURESUNXPROC is access procedure;
   pragma Convention (C, PFNGLFINISHTEXTURESUNXPROC);  -- ../include/SDL/SDL_opengl.h:5150

   type PFNGLGLOBALALPHAFACTORBSUNPROC is access procedure (arg1 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORBSUNPROC);  -- ../include/SDL/SDL_opengl.h:5165

   type PFNGLGLOBALALPHAFACTORSSUNPROC is access procedure (arg1 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORSSUNPROC);  -- ../include/SDL/SDL_opengl.h:5166

   type PFNGLGLOBALALPHAFACTORISUNPROC is access procedure (arg1 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORISUNPROC);  -- ../include/SDL/SDL_opengl.h:5167

   type PFNGLGLOBALALPHAFACTORFSUNPROC is access procedure (arg1 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORFSUNPROC);  -- ../include/SDL/SDL_opengl.h:5168

   type PFNGLGLOBALALPHAFACTORDSUNPROC is access procedure (arg1 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORDSUNPROC);  -- ../include/SDL/SDL_opengl.h:5169

   type PFNGLGLOBALALPHAFACTORUBSUNPROC is access procedure (arg1 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORUBSUNPROC);  -- ../include/SDL/SDL_opengl.h:5170

   type PFNGLGLOBALALPHAFACTORUSSUNPROC is access procedure (arg1 : GL_gl_h.GLushort);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORUSSUNPROC);  -- ../include/SDL/SDL_opengl.h:5171

   type PFNGLGLOBALALPHAFACTORUISUNPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGLOBALALPHAFACTORUISUNPROC);  -- ../include/SDL/SDL_opengl.h:5172

   type PFNGLREPLACEMENTCODEUISUNPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLREPLACEMENTCODEUISUNPROC);  -- ../include/SDL/SDL_opengl.h:5186

   type PFNGLREPLACEMENTCODEUSSUNPROC is access procedure (arg1 : GL_gl_h.GLushort);
   pragma Convention (C, PFNGLREPLACEMENTCODEUSSUNPROC);  -- ../include/SDL/SDL_opengl.h:5187

   type PFNGLREPLACEMENTCODEUBSUNPROC is access procedure (arg1 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLREPLACEMENTCODEUBSUNPROC);  -- ../include/SDL/SDL_opengl.h:5188

   type PFNGLREPLACEMENTCODEUIVSUNPROC is access procedure (arg1 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLREPLACEMENTCODEUIVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5189

   type PFNGLREPLACEMENTCODEUSVSUNPROC is access procedure (arg1 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLREPLACEMENTCODEUSVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5190

   type PFNGLREPLACEMENTCODEUBVSUNPROC is access procedure (arg1 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLREPLACEMENTCODEUBVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5191

   type PFNGLREPLACEMENTCODEPOINTERSUNPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLREPLACEMENTCODEPOINTERSUNPROC);  -- ../include/SDL/SDL_opengl.h:5192

   type PFNGLCOLOR4UBVERTEX2FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLubyte;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4UBVERTEX2FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5239

   type PFNGLCOLOR4UBVERTEX2FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLubyte; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4UBVERTEX2FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5240

   type PFNGLCOLOR4UBVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLubyte;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4UBVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5241

   type PFNGLCOLOR4UBVERTEX3FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLubyte; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4UBVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5242

   type PFNGLCOLOR3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5243

   type PFNGLCOLOR3FVERTEX3FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLfloat; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5244

   type PFNGLNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5245

   type PFNGLNORMAL3FVERTEX3FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLfloat; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5246

   type PFNGLCOLOR4FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat;
         arg10 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5247

   type PFNGLCOLOR4FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOLOR4FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5248

   type PFNGLTEXCOORD2FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5249

   type PFNGLTEXCOORD2FVERTEX3FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLfloat; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5250

   type PFNGLTEXCOORD4FVERTEX4FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD4FVERTEX4FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5251

   type PFNGLTEXCOORD4FVERTEX4FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLfloat; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD4FVERTEX4FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5252

   type PFNGLTEXCOORD2FCOLOR4UBVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLubyte;
         arg6 : GL_gl_h.GLubyte;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR4UBVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5253

   type PFNGLTEXCOORD2FCOLOR4UBVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLubyte;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR4UBVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5254

   type PFNGLTEXCOORD2FCOLOR3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5255

   type PFNGLTEXCOORD2FCOLOR3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5256

   type PFNGLTEXCOORD2FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5257

   type PFNGLTEXCOORD2FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5258

   type PFNGLTEXCOORD2FCOLOR4FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat;
         arg10 : GL_gl_h.GLfloat;
         arg11 : GL_gl_h.GLfloat;
         arg12 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR4FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5259

   type PFNGLTEXCOORD2FCOLOR4FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD2FCOLOR4FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5260

   type PFNGLTEXCOORD4FCOLOR4FNORMAL3FVERTEX4FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat;
         arg10 : GL_gl_h.GLfloat;
         arg11 : GL_gl_h.GLfloat;
         arg12 : GL_gl_h.GLfloat;
         arg13 : GL_gl_h.GLfloat;
         arg14 : GL_gl_h.GLfloat;
         arg15 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD4FCOLOR4FNORMAL3FVERTEX4FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5261

   type PFNGLTEXCOORD4FCOLOR4FNORMAL3FVERTEX4FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLfloat;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXCOORD4FCOLOR4FNORMAL3FVERTEX4FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5262

   type PFNGLREPLACEMENTCODEUIVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUIVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5263

   type PFNGLREPLACEMENTCODEUIVERTEX3FVSUNPROC is access procedure (arg1 : access GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUIVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5264

   type PFNGLREPLACEMENTCODEUICOLOR4UBVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLubyte;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR4UBVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5265

   type PFNGLREPLACEMENTCODEUICOLOR4UBVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLubyte;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR4UBVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5266

   type PFNGLREPLACEMENTCODEUICOLOR3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5267

   type PFNGLREPLACEMENTCODEUICOLOR3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5268

   type PFNGLREPLACEMENTCODEUINORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUINORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5269

   type PFNGLREPLACEMENTCODEUINORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUINORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5270

   type PFNGLREPLACEMENTCODEUICOLOR4FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat;
         arg10 : GL_gl_h.GLfloat;
         arg11 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR4FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5271

   type PFNGLREPLACEMENTCODEUICOLOR4FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUICOLOR4FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5272

   type PFNGLREPLACEMENTCODEUITEXCOORD2FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5273

   type PFNGLREPLACEMENTCODEUITEXCOORD2FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5274

   type PFNGLREPLACEMENTCODEUITEXCOORD2FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5275

   type PFNGLREPLACEMENTCODEUITEXCOORD2FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5276

   type PFNGLREPLACEMENTCODEUITEXCOORD2FCOLOR4FNORMAL3FVERTEX3FSUNPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat;
         arg8 : GL_gl_h.GLfloat;
         arg9 : GL_gl_h.GLfloat;
         arg10 : GL_gl_h.GLfloat;
         arg11 : GL_gl_h.GLfloat;
         arg12 : GL_gl_h.GLfloat;
         arg13 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FCOLOR4FNORMAL3FVERTEX3FSUNPROC);  -- ../include/SDL/SDL_opengl.h:5277

   type PFNGLREPLACEMENTCODEUITEXCOORD2FCOLOR4FNORMAL3FVERTEX3FVSUNPROC is access procedure
        (arg1 : access GL_gl_h.GLuint;
         arg2 : access GL_gl_h.GLfloat;
         arg3 : access GL_gl_h.GLfloat;
         arg4 : access GL_gl_h.GLfloat;
         arg5 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLREPLACEMENTCODEUITEXCOORD2FCOLOR4FNORMAL3FVERTEX3FVSUNPROC);  -- ../include/SDL/SDL_opengl.h:5278

   type PFNGLBLENDFUNCSEPARATEEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDFUNCSEPARATEEXTPROC);  -- ../include/SDL/SDL_opengl.h:5286

   type PFNGLBLENDFUNCSEPARATEINGRPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDFUNCSEPARATEINGRPROC);  -- ../include/SDL/SDL_opengl.h:5294

   type PFNGLVERTEXWEIGHTFEXTPROC is access procedure (arg1 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXWEIGHTFEXTPROC);  -- ../include/SDL/SDL_opengl.h:5340

   type PFNGLVERTEXWEIGHTFVEXTPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXWEIGHTFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5341

   type PFNGLVERTEXWEIGHTPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLsizei;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address);
   pragma Convention (C, PFNGLVERTEXWEIGHTPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5342

   type PFNGLFLUSHVERTEXARRAYRANGENVPROC is access procedure;
   pragma Convention (C, PFNGLFLUSHVERTEXARRAYRANGENVPROC);  -- ../include/SDL/SDL_opengl.h:5355

   type PFNGLVERTEXARRAYRANGENVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : System.Address);
   pragma Convention (C, PFNGLVERTEXARRAYRANGENVPROC);  -- ../include/SDL/SDL_opengl.h:5356

   type PFNGLCOMBINERPARAMETERFVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOMBINERPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5376

   type PFNGLCOMBINERPARAMETERFNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOMBINERPARAMETERFNVPROC);  -- ../include/SDL/SDL_opengl.h:5377

   type PFNGLCOMBINERPARAMETERIVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOMBINERPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5378

   type PFNGLCOMBINERPARAMETERINVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOMBINERPARAMETERINVPROC);  -- ../include/SDL/SDL_opengl.h:5379

   type PFNGLCOMBINERINPUTNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLCOMBINERINPUTNVPROC);  -- ../include/SDL/SDL_opengl.h:5380

   type PFNGLCOMBINEROUTPUTNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum;
         arg7 : GL_gl_h.GLenum;
         arg8 : GL_gl_h.GLboolean;
         arg9 : GL_gl_h.GLboolean;
         arg10 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLCOMBINEROUTPUTNVPROC);  -- ../include/SDL/SDL_opengl.h:5381

   type PFNGLFINALCOMBINERINPUTNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLFINALCOMBINERINPUTNVPROC);  -- ../include/SDL/SDL_opengl.h:5382

   type PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCOMBINERINPUTPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5383

   type PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCOMBINERINPUTPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5384

   type PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCOMBINEROUTPUTPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5385

   type PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETCOMBINEROUTPUTPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5386

   type PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETFINALCOMBINERINPUTPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5387

   type PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETFINALCOMBINERINPUTPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5388

   type PFNGLRESIZEBUFFERSMESAPROC is access procedure;
   pragma Convention (C, PFNGLRESIZEBUFFERSMESAPROC);  -- ../include/SDL/SDL_opengl.h:5412

   type PFNGLWINDOWPOS2DMESAPROC is access procedure (arg1 : GL_gl_h.GLdouble; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DMESAPROC);  -- ../include/SDL/SDL_opengl.h:5443

   type PFNGLWINDOWPOS2DVMESAPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS2DVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5444

   type PFNGLWINDOWPOS2FMESAPROC is access procedure (arg1 : GL_gl_h.GLfloat; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FMESAPROC);  -- ../include/SDL/SDL_opengl.h:5445

   type PFNGLWINDOWPOS2FVMESAPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS2FVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5446

   type PFNGLWINDOWPOS2IMESAPROC is access procedure (arg1 : GL_gl_h.GLint; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IMESAPROC);  -- ../include/SDL/SDL_opengl.h:5447

   type PFNGLWINDOWPOS2IVMESAPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS2IVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5448

   type PFNGLWINDOWPOS2SMESAPROC is access procedure (arg1 : GL_gl_h.GLshort; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SMESAPROC);  -- ../include/SDL/SDL_opengl.h:5449

   type PFNGLWINDOWPOS2SVMESAPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS2SVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5450

   type PFNGLWINDOWPOS3DMESAPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DMESAPROC);  -- ../include/SDL/SDL_opengl.h:5451

   type PFNGLWINDOWPOS3DVMESAPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS3DVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5452

   type PFNGLWINDOWPOS3FMESAPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FMESAPROC);  -- ../include/SDL/SDL_opengl.h:5453

   type PFNGLWINDOWPOS3FVMESAPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS3FVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5454

   type PFNGLWINDOWPOS3IMESAPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IMESAPROC);  -- ../include/SDL/SDL_opengl.h:5455

   type PFNGLWINDOWPOS3IVMESAPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS3IVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5456

   type PFNGLWINDOWPOS3SMESAPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SMESAPROC);  -- ../include/SDL/SDL_opengl.h:5457

   type PFNGLWINDOWPOS3SVMESAPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS3SVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5458

   type PFNGLWINDOWPOS4DMESAPROC is access procedure
        (arg1 : GL_gl_h.GLdouble;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS4DMESAPROC);  -- ../include/SDL/SDL_opengl.h:5459

   type PFNGLWINDOWPOS4DVMESAPROC is access procedure (arg1 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLWINDOWPOS4DVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5460

   type PFNGLWINDOWPOS4FMESAPROC is access procedure
        (arg1 : GL_gl_h.GLfloat;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS4FMESAPROC);  -- ../include/SDL/SDL_opengl.h:5461

   type PFNGLWINDOWPOS4FVMESAPROC is access procedure (arg1 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLWINDOWPOS4FVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5462

   type PFNGLWINDOWPOS4IMESAPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS4IMESAPROC);  -- ../include/SDL/SDL_opengl.h:5463

   type PFNGLWINDOWPOS4IVMESAPROC is access procedure (arg1 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLWINDOWPOS4IVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5464

   type PFNGLWINDOWPOS4SMESAPROC is access procedure
        (arg1 : GL_gl_h.GLshort;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS4SMESAPROC);  -- ../include/SDL/SDL_opengl.h:5465

   type PFNGLWINDOWPOS4SVMESAPROC is access procedure (arg1 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLWINDOWPOS4SVMESAPROC);  -- ../include/SDL/SDL_opengl.h:5466

   type PFNGLMULTIMODEDRAWARRAYSIBMPROC is access procedure
        (arg1 : access GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTIMODEDRAWARRAYSIBMPROC);  -- ../include/SDL/SDL_opengl.h:5479

   type PFNGLMULTIMODEDRAWELEMENTSIBMPROC is access procedure
        (arg1 : access GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLsizei;
         arg3 : GL_gl_h.GLenum;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLMULTIMODEDRAWELEMENTSIBMPROC);  -- ../include/SDL/SDL_opengl.h:5480

   type PFNGLCOLORPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLCOLORPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5495

   type PFNGLSECONDARYCOLORPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLSECONDARYCOLORPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5496

   type PFNGLEDGEFLAGPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : System.Address;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLEDGEFLAGPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5497

   type PFNGLFOGCOORDPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : System.Address;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFOGCOORDPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5498

   type PFNGLINDEXPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : System.Address;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLINDEXPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5499

   type PFNGLNORMALPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : System.Address;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLNORMALPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5500

   type PFNGLTEXCOORDPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLTEXCOORDPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5501

   type PFNGLVERTEXPOINTERLISTIBMPROC is access procedure
        (arg1 : GL_gl_h.GLint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXPOINTERLISTIBMPROC);  -- ../include/SDL/SDL_opengl.h:5502

   type PFNGLTBUFFERMASK3DFXPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLTBUFFERMASK3DFXPROC);  -- ../include/SDL/SDL_opengl.h:5534

   type PFNGLSAMPLEMASKEXTPROC is access procedure (arg1 : GL_gl_h.GLclampf; arg2 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLSAMPLEMASKEXTPROC);  -- ../include/SDL/SDL_opengl.h:5543

   type PFNGLSAMPLEPATTERNEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSAMPLEPATTERNEXTPROC);  -- ../include/SDL/SDL_opengl.h:5544

   type PFNGLTEXTURECOLORMASKSGISPROC is access procedure
        (arg1 : GL_gl_h.GLboolean;
         arg2 : GL_gl_h.GLboolean;
         arg3 : GL_gl_h.GLboolean;
         arg4 : GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLTEXTURECOLORMASKSGISPROC);  -- ../include/SDL/SDL_opengl.h:5568

   type PFNGLIGLOOINTERFACESGIXPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, PFNGLIGLOOINTERFACESGIXPROC);  -- ../include/SDL/SDL_opengl.h:5576

   type PFNGLDELETEFENCESNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEFENCESNVPROC);  -- ../include/SDL/SDL_opengl.h:5598

   type PFNGLGENFENCESNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENFENCESNVPROC);  -- ../include/SDL/SDL_opengl.h:5599

   type PFNGLISFENCENVPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISFENCENVPROC);  -- ../include/SDL/SDL_opengl.h:5600

   type PFNGLTESTFENCENVPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLTESTFENCENVPROC);  -- ../include/SDL/SDL_opengl.h:5601

   type PFNGLGETFENCEIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETFENCEIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5602

   type PFNGLFINISHFENCENVPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLFINISHFENCENVPROC);  -- ../include/SDL/SDL_opengl.h:5603

   type PFNGLSETFENCENVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSETFENCENVPROC);  -- ../include/SDL/SDL_opengl.h:5604

   type PFNGLMAPCONTROLPOINTSNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLint;
         arg7 : GL_gl_h.GLint;
         arg8 : GL_gl_h.GLboolean;
         arg9 : System.Address);
   pragma Convention (C, PFNGLMAPCONTROLPOINTSNVPROC);  -- ../include/SDL/SDL_opengl.h:5620

   type PFNGLMAPPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLMAPPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5621

   type PFNGLMAPPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLMAPPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5622

   type PFNGLGETMAPCONTROLPOINTSNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLboolean;
         arg7 : System.Address);
   pragma Convention (C, PFNGLGETMAPCONTROLPOINTSNVPROC);  -- ../include/SDL/SDL_opengl.h:5623

   type PFNGLGETMAPPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETMAPPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5624

   type PFNGLGETMAPPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETMAPPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5625

   type PFNGLGETMAPATTRIBPARAMETERIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETMAPATTRIBPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5626

   type PFNGLGETMAPATTRIBPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETMAPATTRIBPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5627

   type PFNGLEVALMAPSNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLEVALMAPSNVPROC);  -- ../include/SDL/SDL_opengl.h:5628

   type PFNGLCOMBINERSTAGEPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLCOMBINERSTAGEPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5641

   type PFNGLGETCOMBINERSTAGEPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETCOMBINERSTAGEPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5642

   type PFNGLAREPROGRAMSRESIDENTNVPROC is access function
        (arg1 : GL_gl_h.GLsizei;
         arg2 : access GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLboolean) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLAREPROGRAMSRESIDENTNVPROC);  -- ../include/SDL/SDL_opengl.h:5733

   type PFNGLBINDPROGRAMNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDPROGRAMNVPROC);  -- ../include/SDL/SDL_opengl.h:5734

   type PFNGLDELETEPROGRAMSNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEPROGRAMSNVPROC);  -- ../include/SDL/SDL_opengl.h:5735

   type PFNGLEXECUTEPROGRAMNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLEXECUTEPROGRAMNVPROC);  -- ../include/SDL/SDL_opengl.h:5736

   type PFNGLGENPROGRAMSNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENPROGRAMSNVPROC);  -- ../include/SDL/SDL_opengl.h:5737

   type PFNGLGETPROGRAMPARAMETERDVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETPROGRAMPARAMETERDVNVPROC);  -- ../include/SDL/SDL_opengl.h:5738

   type PFNGLGETPROGRAMPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETPROGRAMPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5739

   type PFNGLGETPROGRAMIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETPROGRAMIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5740

   type PFNGLGETPROGRAMSTRINGNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLGETPROGRAMSTRINGNVPROC);  -- ../include/SDL/SDL_opengl.h:5741

   type PFNGLGETTRACKMATRIXIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETTRACKMATRIXIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5742

   type PFNGLGETVERTEXATTRIBDVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETVERTEXATTRIBDVNVPROC);  -- ../include/SDL/SDL_opengl.h:5743

   type PFNGLGETVERTEXATTRIBFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVERTEXATTRIBFVNVPROC);  -- ../include/SDL/SDL_opengl.h:5744

   type PFNGLGETVERTEXATTRIBIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVERTEXATTRIBIVNVPROC);  -- ../include/SDL/SDL_opengl.h:5745

   type PFNGLGETVERTEXATTRIBPOINTERVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETVERTEXATTRIBPOINTERVNVPROC);  -- ../include/SDL/SDL_opengl.h:5746

   type PFNGLISPROGRAMNVPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISPROGRAMNVPROC);  -- ../include/SDL/SDL_opengl.h:5747

   type PFNGLLOADPROGRAMNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLLOADPROGRAMNVPROC);  -- ../include/SDL/SDL_opengl.h:5748

   type PFNGLPROGRAMPARAMETER4DNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble;
         arg6 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMPARAMETER4DNVPROC);  -- ../include/SDL/SDL_opengl.h:5749

   type PFNGLPROGRAMPARAMETER4DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMPARAMETER4DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5750

   type PFNGLPROGRAMPARAMETER4FNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMPARAMETER4FNVPROC);  -- ../include/SDL/SDL_opengl.h:5751

   type PFNGLPROGRAMPARAMETER4FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMPARAMETER4FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5752

   type PFNGLPROGRAMPARAMETERS4DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMPARAMETERS4DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5753

   type PFNGLPROGRAMPARAMETERS4FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMPARAMETERS4FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5754

   type PFNGLREQUESTRESIDENTPROGRAMSNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLREQUESTRESIDENTPROGRAMSNVPROC);  -- ../include/SDL/SDL_opengl.h:5755

   type PFNGLTRACKMATRIXNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLTRACKMATRIXNVPROC);  -- ../include/SDL/SDL_opengl.h:5756

   type PFNGLVERTEXATTRIBPOINTERNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : System.Address);
   pragma Convention (C, PFNGLVERTEXATTRIBPOINTERNVPROC);  -- ../include/SDL/SDL_opengl.h:5757

   type PFNGLVERTEXATTRIB1DNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DNVPROC);  -- ../include/SDL/SDL_opengl.h:5758

   type PFNGLVERTEXATTRIB1DVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB1DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5759

   type PFNGLVERTEXATTRIB1FNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FNVPROC);  -- ../include/SDL/SDL_opengl.h:5760

   type PFNGLVERTEXATTRIB1FVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB1FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5761

   type PFNGLVERTEXATTRIB1SNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SNVPROC);  -- ../include/SDL/SDL_opengl.h:5762

   type PFNGLVERTEXATTRIB1SVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB1SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5763

   type PFNGLVERTEXATTRIB2DNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DNVPROC);  -- ../include/SDL/SDL_opengl.h:5764

   type PFNGLVERTEXATTRIB2DVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB2DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5765

   type PFNGLVERTEXATTRIB2FNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FNVPROC);  -- ../include/SDL/SDL_opengl.h:5766

   type PFNGLVERTEXATTRIB2FVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB2FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5767

   type PFNGLVERTEXATTRIB2SNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SNVPROC);  -- ../include/SDL/SDL_opengl.h:5768

   type PFNGLVERTEXATTRIB2SVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB2SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5769

   type PFNGLVERTEXATTRIB3DNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DNVPROC);  -- ../include/SDL/SDL_opengl.h:5770

   type PFNGLVERTEXATTRIB3DVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB3DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5771

   type PFNGLVERTEXATTRIB3FNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FNVPROC);  -- ../include/SDL/SDL_opengl.h:5772

   type PFNGLVERTEXATTRIB3FVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB3FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5773

   type PFNGLVERTEXATTRIB3SNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SNVPROC);  -- ../include/SDL/SDL_opengl.h:5774

   type PFNGLVERTEXATTRIB3SVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB3SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5775

   type PFNGLVERTEXATTRIB4DNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DNVPROC);  -- ../include/SDL/SDL_opengl.h:5776

   type PFNGLVERTEXATTRIB4DVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIB4DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5777

   type PFNGLVERTEXATTRIB4FNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FNVPROC);  -- ../include/SDL/SDL_opengl.h:5778

   type PFNGLVERTEXATTRIB4FVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIB4FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5779

   type PFNGLVERTEXATTRIB4SNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SNVPROC);  -- ../include/SDL/SDL_opengl.h:5780

   type PFNGLVERTEXATTRIB4SVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIB4SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5781

   type PFNGLVERTEXATTRIB4UBNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLubyte;
         arg3 : GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLubyte;
         arg5 : GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4UBNVPROC);  -- ../include/SDL/SDL_opengl.h:5782

   type PFNGLVERTEXATTRIB4UBVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIB4UBVNVPROC);  -- ../include/SDL/SDL_opengl.h:5783

   type PFNGLVERTEXATTRIBS1DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIBS1DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5784

   type PFNGLVERTEXATTRIBS1FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIBS1FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5785

   type PFNGLVERTEXATTRIBS1SVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIBS1SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5786

   type PFNGLVERTEXATTRIBS2DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIBS2DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5787

   type PFNGLVERTEXATTRIBS2FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIBS2FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5788

   type PFNGLVERTEXATTRIBS2SVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIBS2SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5789

   type PFNGLVERTEXATTRIBS3DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIBS3DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5790

   type PFNGLVERTEXATTRIBS3FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIBS3FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5791

   type PFNGLVERTEXATTRIBS3SVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIBS3SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5792

   type PFNGLVERTEXATTRIBS4DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXATTRIBS4DVNVPROC);  -- ../include/SDL/SDL_opengl.h:5793

   type PFNGLVERTEXATTRIBS4FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXATTRIBS4FVNVPROC);  -- ../include/SDL/SDL_opengl.h:5794

   type PFNGLVERTEXATTRIBS4SVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXATTRIBS4SVNVPROC);  -- ../include/SDL/SDL_opengl.h:5795

   type PFNGLVERTEXATTRIBS4UBVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVERTEXATTRIBS4UBVNVPROC);  -- ../include/SDL/SDL_opengl.h:5796

   type PFNGLTEXBUMPPARAMETERIVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLTEXBUMPPARAMETERIVATIPROC);  -- ../include/SDL/SDL_opengl.h:5831

   type PFNGLTEXBUMPPARAMETERFVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLTEXBUMPPARAMETERFVATIPROC);  -- ../include/SDL/SDL_opengl.h:5832

   type PFNGLGETTEXBUMPPARAMETERIVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETTEXBUMPPARAMETERIVATIPROC);  -- ../include/SDL/SDL_opengl.h:5833

   type PFNGLGETTEXBUMPPARAMETERFVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETTEXBUMPPARAMETERFVATIPROC);  -- ../include/SDL/SDL_opengl.h:5834

   type PFNGLGENFRAGMENTSHADERSATIPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLGENFRAGMENTSHADERSATIPROC);  -- ../include/SDL/SDL_opengl.h:5855

   type PFNGLBINDFRAGMENTSHADERATIPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDFRAGMENTSHADERATIPROC);  -- ../include/SDL/SDL_opengl.h:5856

   type PFNGLDELETEFRAGMENTSHADERATIPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEFRAGMENTSHADERATIPROC);  -- ../include/SDL/SDL_opengl.h:5857

   type PFNGLBEGINFRAGMENTSHADERATIPROC is access procedure;
   pragma Convention (C, PFNGLBEGINFRAGMENTSHADERATIPROC);  -- ../include/SDL/SDL_opengl.h:5858

   type PFNGLENDFRAGMENTSHADERATIPROC is access procedure;
   pragma Convention (C, PFNGLENDFRAGMENTSHADERATIPROC);  -- ../include/SDL/SDL_opengl.h:5859

   type PFNGLPASSTEXCOORDATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLPASSTEXCOORDATIPROC);  -- ../include/SDL/SDL_opengl.h:5860

   type PFNGLSAMPLEMAPATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSAMPLEMAPATIPROC);  -- ../include/SDL/SDL_opengl.h:5861

   type PFNGLCOLORFRAGMENTOP1ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLCOLORFRAGMENTOP1ATIPROC);  -- ../include/SDL/SDL_opengl.h:5862

   type PFNGLCOLORFRAGMENTOP2ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint;
         arg8 : GL_gl_h.GLuint;
         arg9 : GL_gl_h.GLuint;
         arg10 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLCOLORFRAGMENTOP2ATIPROC);  -- ../include/SDL/SDL_opengl.h:5863

   type PFNGLCOLORFRAGMENTOP3ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint;
         arg8 : GL_gl_h.GLuint;
         arg9 : GL_gl_h.GLuint;
         arg10 : GL_gl_h.GLuint;
         arg11 : GL_gl_h.GLuint;
         arg12 : GL_gl_h.GLuint;
         arg13 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLCOLORFRAGMENTOP3ATIPROC);  -- ../include/SDL/SDL_opengl.h:5864

   type PFNGLALPHAFRAGMENTOP1ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLALPHAFRAGMENTOP1ATIPROC);  -- ../include/SDL/SDL_opengl.h:5865

   type PFNGLALPHAFRAGMENTOP2ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint;
         arg8 : GL_gl_h.GLuint;
         arg9 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLALPHAFRAGMENTOP2ATIPROC);  -- ../include/SDL/SDL_opengl.h:5866

   type PFNGLALPHAFRAGMENTOP3ATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint;
         arg8 : GL_gl_h.GLuint;
         arg9 : GL_gl_h.GLuint;
         arg10 : GL_gl_h.GLuint;
         arg11 : GL_gl_h.GLuint;
         arg12 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLALPHAFRAGMENTOP3ATIPROC);  -- ../include/SDL/SDL_opengl.h:5867

   type PFNGLSETFRAGMENTSHADERCONSTANTATIPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLSETFRAGMENTSHADERCONSTANTATIPROC);  -- ../include/SDL/SDL_opengl.h:5868

   type PFNGLPNTRIANGLESIATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLPNTRIANGLESIATIPROC);  -- ../include/SDL/SDL_opengl.h:5877

   type PFNGLPNTRIANGLESFATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPNTRIANGLESFATIPROC);  -- ../include/SDL/SDL_opengl.h:5878

   type PFNGLNEWOBJECTBUFFERATIPROC is access function
        (arg1 : GL_gl_h.GLsizei;
         arg2 : System.Address;
         arg3 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLNEWOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:5897

   type PFNGLISOBJECTBUFFERATIPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:5898

   type PFNGLUPDATEOBJECTBUFFERATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : System.Address;
         arg5 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLUPDATEOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:5899

   type PFNGLGETOBJECTBUFFERFVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETOBJECTBUFFERFVATIPROC);  -- ../include/SDL/SDL_opengl.h:5900

   type PFNGLGETOBJECTBUFFERIVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETOBJECTBUFFERIVATIPROC);  -- ../include/SDL/SDL_opengl.h:5901

   type PFNGLFREEOBJECTBUFFERATIPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLFREEOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:5902

   type PFNGLARRAYOBJECTATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLsizei;
         arg5 : GL_gl_h.GLuint;
         arg6 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLARRAYOBJECTATIPROC);  -- ../include/SDL/SDL_opengl.h:5903

   type PFNGLGETARRAYOBJECTFVATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETARRAYOBJECTFVATIPROC);  -- ../include/SDL/SDL_opengl.h:5904

   type PFNGLGETARRAYOBJECTIVATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETARRAYOBJECTIVATIPROC);  -- ../include/SDL/SDL_opengl.h:5905

   type PFNGLVARIANTARRAYOBJECTATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVARIANTARRAYOBJECTATIPROC);  -- ../include/SDL/SDL_opengl.h:5906

   type PFNGLGETVARIANTARRAYOBJECTFVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVARIANTARRAYOBJECTFVATIPROC);  -- ../include/SDL/SDL_opengl.h:5907

   type PFNGLGETVARIANTARRAYOBJECTIVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVARIANTARRAYOBJECTIVATIPROC);  -- ../include/SDL/SDL_opengl.h:5908

   type PFNGLBEGINVERTEXSHADEREXTPROC is access procedure;
   pragma Convention (C, PFNGLBEGINVERTEXSHADEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5957

   type PFNGLENDVERTEXSHADEREXTPROC is access procedure;
   pragma Convention (C, PFNGLENDVERTEXSHADEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5958

   type PFNGLBINDVERTEXSHADEREXTPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDVERTEXSHADEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5959

   type PFNGLGENVERTEXSHADERSEXTPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLGENVERTEXSHADERSEXTPROC);  -- ../include/SDL/SDL_opengl.h:5960

   type PFNGLDELETEVERTEXSHADEREXTPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEVERTEXSHADEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5961

   type PFNGLSHADEROP1EXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSHADEROP1EXTPROC);  -- ../include/SDL/SDL_opengl.h:5962

   type PFNGLSHADEROP2EXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSHADEROP2EXTPROC);  -- ../include/SDL/SDL_opengl.h:5963

   type PFNGLSHADEROP3EXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSHADEROP3EXTPROC);  -- ../include/SDL/SDL_opengl.h:5964

   type PFNGLSWIZZLEEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSWIZZLEEXTPROC);  -- ../include/SDL/SDL_opengl.h:5965

   type PFNGLWRITEMASKEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum;
         arg5 : GL_gl_h.GLenum;
         arg6 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLWRITEMASKEXTPROC);  -- ../include/SDL/SDL_opengl.h:5966

   type PFNGLINSERTCOMPONENTEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLINSERTCOMPONENTEXTPROC);  -- ../include/SDL/SDL_opengl.h:5967

   type PFNGLEXTRACTCOMPONENTEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLEXTRACTCOMPONENTEXTPROC);  -- ../include/SDL/SDL_opengl.h:5968

   type PFNGLGENSYMBOLSEXTPROC is access function
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLuint) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLGENSYMBOLSEXTPROC);  -- ../include/SDL/SDL_opengl.h:5969

   type PFNGLSETINVARIANTEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLSETINVARIANTEXTPROC);  -- ../include/SDL/SDL_opengl.h:5970

   type PFNGLSETLOCALCONSTANTEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLSETLOCALCONSTANTEXTPROC);  -- ../include/SDL/SDL_opengl.h:5971

   type PFNGLVARIANTBVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLVARIANTBVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5972

   type PFNGLVARIANTSVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVARIANTSVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5973

   type PFNGLVARIANTIVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVARIANTIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5974

   type PFNGLVARIANTFVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVARIANTFVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5975

   type PFNGLVARIANTDVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVARIANTDVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5976

   type PFNGLVARIANTUBVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLubyte);
   pragma Convention (C, PFNGLVARIANTUBVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5977

   type PFNGLVARIANTUSVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLushort);
   pragma Convention (C, PFNGLVARIANTUSVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5978

   type PFNGLVARIANTUIVEXTPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVARIANTUIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5979

   type PFNGLVARIANTPOINTEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLuint;
         arg4 : System.Address);
   pragma Convention (C, PFNGLVARIANTPOINTEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5980

   type PFNGLENABLEVARIANTCLIENTSTATEEXTPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLENABLEVARIANTCLIENTSTATEEXTPROC);  -- ../include/SDL/SDL_opengl.h:5981

   type PFNGLDISABLEVARIANTCLIENTSTATEEXTPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDISABLEVARIANTCLIENTSTATEEXTPROC);  -- ../include/SDL/SDL_opengl.h:5982

   type PFNGLBINDLIGHTPARAMETEREXTPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLBINDLIGHTPARAMETEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5983

   type PFNGLBINDMATERIALPARAMETEREXTPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLBINDMATERIALPARAMETEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5984

   type PFNGLBINDTEXGENPARAMETEREXTPROC is access function
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLBINDTEXGENPARAMETEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5985

   type PFNGLBINDTEXTUREUNITPARAMETEREXTPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLBINDTEXTUREUNITPARAMETEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5986

   type PFNGLBINDPARAMETEREXTPROC is access function (arg1 : GL_gl_h.GLenum) return GL_gl_h.GLuint;
   pragma Convention (C, PFNGLBINDPARAMETEREXTPROC);  -- ../include/SDL/SDL_opengl.h:5987

   type PFNGLISVARIANTENABLEDEXTPROC is access function (arg1 : GL_gl_h.GLuint; arg2 : GL_gl_h.GLenum) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISVARIANTENABLEDEXTPROC);  -- ../include/SDL/SDL_opengl.h:5988

   type PFNGLGETVARIANTBOOLEANVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLGETVARIANTBOOLEANVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5989

   type PFNGLGETVARIANTINTEGERVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVARIANTINTEGERVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5990

   type PFNGLGETVARIANTFLOATVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVARIANTFLOATVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5991

   type PFNGLGETVARIANTPOINTERVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : System.Address);
   pragma Convention (C, PFNGLGETVARIANTPOINTERVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5992

   type PFNGLGETINVARIANTBOOLEANVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLGETINVARIANTBOOLEANVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5993

   type PFNGLGETINVARIANTINTEGERVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETINVARIANTINTEGERVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5994

   type PFNGLGETINVARIANTFLOATVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETINVARIANTFLOATVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5995

   type PFNGLGETLOCALCONSTANTBOOLEANVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLboolean);
   pragma Convention (C, PFNGLGETLOCALCONSTANTBOOLEANVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5996

   type PFNGLGETLOCALCONSTANTINTEGERVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETLOCALCONSTANTINTEGERVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5997

   type PFNGLGETLOCALCONSTANTFLOATVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETLOCALCONSTANTFLOATVEXTPROC);  -- ../include/SDL/SDL_opengl.h:5998

   type PFNGLVERTEXSTREAM1SATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM1SATIPROC);  -- ../include/SDL/SDL_opengl.h:6050

   type PFNGLVERTEXSTREAM1SVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM1SVATIPROC);  -- ../include/SDL/SDL_opengl.h:6051

   type PFNGLVERTEXSTREAM1IATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM1IATIPROC);  -- ../include/SDL/SDL_opengl.h:6052

   type PFNGLVERTEXSTREAM1IVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM1IVATIPROC);  -- ../include/SDL/SDL_opengl.h:6053

   type PFNGLVERTEXSTREAM1FATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM1FATIPROC);  -- ../include/SDL/SDL_opengl.h:6054

   type PFNGLVERTEXSTREAM1FVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM1FVATIPROC);  -- ../include/SDL/SDL_opengl.h:6055

   type PFNGLVERTEXSTREAM1DATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM1DATIPROC);  -- ../include/SDL/SDL_opengl.h:6056

   type PFNGLVERTEXSTREAM1DVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM1DVATIPROC);  -- ../include/SDL/SDL_opengl.h:6057

   type PFNGLVERTEXSTREAM2SATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM2SATIPROC);  -- ../include/SDL/SDL_opengl.h:6058

   type PFNGLVERTEXSTREAM2SVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM2SVATIPROC);  -- ../include/SDL/SDL_opengl.h:6059

   type PFNGLVERTEXSTREAM2IATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM2IATIPROC);  -- ../include/SDL/SDL_opengl.h:6060

   type PFNGLVERTEXSTREAM2IVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM2IVATIPROC);  -- ../include/SDL/SDL_opengl.h:6061

   type PFNGLVERTEXSTREAM2FATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM2FATIPROC);  -- ../include/SDL/SDL_opengl.h:6062

   type PFNGLVERTEXSTREAM2FVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM2FVATIPROC);  -- ../include/SDL/SDL_opengl.h:6063

   type PFNGLVERTEXSTREAM2DATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM2DATIPROC);  -- ../include/SDL/SDL_opengl.h:6064

   type PFNGLVERTEXSTREAM2DVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM2DVATIPROC);  -- ../include/SDL/SDL_opengl.h:6065

   type PFNGLVERTEXSTREAM3SATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM3SATIPROC);  -- ../include/SDL/SDL_opengl.h:6066

   type PFNGLVERTEXSTREAM3SVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM3SVATIPROC);  -- ../include/SDL/SDL_opengl.h:6067

   type PFNGLVERTEXSTREAM3IATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM3IATIPROC);  -- ../include/SDL/SDL_opengl.h:6068

   type PFNGLVERTEXSTREAM3IVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM3IVATIPROC);  -- ../include/SDL/SDL_opengl.h:6069

   type PFNGLVERTEXSTREAM3FATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM3FATIPROC);  -- ../include/SDL/SDL_opengl.h:6070

   type PFNGLVERTEXSTREAM3FVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM3FVATIPROC);  -- ../include/SDL/SDL_opengl.h:6071

   type PFNGLVERTEXSTREAM3DATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM3DATIPROC);  -- ../include/SDL/SDL_opengl.h:6072

   type PFNGLVERTEXSTREAM3DVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM3DVATIPROC);  -- ../include/SDL/SDL_opengl.h:6073

   type PFNGLVERTEXSTREAM4SATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort;
         arg5 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM4SATIPROC);  -- ../include/SDL/SDL_opengl.h:6074

   type PFNGLVERTEXSTREAM4SVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLVERTEXSTREAM4SVATIPROC);  -- ../include/SDL/SDL_opengl.h:6075

   type PFNGLVERTEXSTREAM4IATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM4IATIPROC);  -- ../include/SDL/SDL_opengl.h:6076

   type PFNGLVERTEXSTREAM4IVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXSTREAM4IVATIPROC);  -- ../include/SDL/SDL_opengl.h:6077

   type PFNGLVERTEXSTREAM4FATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM4FATIPROC);  -- ../include/SDL/SDL_opengl.h:6078

   type PFNGLVERTEXSTREAM4FVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXSTREAM4FVATIPROC);  -- ../include/SDL/SDL_opengl.h:6079

   type PFNGLVERTEXSTREAM4DATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM4DATIPROC);  -- ../include/SDL/SDL_opengl.h:6080

   type PFNGLVERTEXSTREAM4DVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLVERTEXSTREAM4DVATIPROC);  -- ../include/SDL/SDL_opengl.h:6081

   type PFNGLNORMALSTREAM3BATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLbyte;
         arg3 : GL_gl_h.GLbyte;
         arg4 : GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLNORMALSTREAM3BATIPROC);  -- ../include/SDL/SDL_opengl.h:6082

   type PFNGLNORMALSTREAM3BVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLbyte);
   pragma Convention (C, PFNGLNORMALSTREAM3BVATIPROC);  -- ../include/SDL/SDL_opengl.h:6083

   type PFNGLNORMALSTREAM3SATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLshort;
         arg3 : GL_gl_h.GLshort;
         arg4 : GL_gl_h.GLshort);
   pragma Convention (C, PFNGLNORMALSTREAM3SATIPROC);  -- ../include/SDL/SDL_opengl.h:6084

   type PFNGLNORMALSTREAM3SVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLshort);
   pragma Convention (C, PFNGLNORMALSTREAM3SVATIPROC);  -- ../include/SDL/SDL_opengl.h:6085

   type PFNGLNORMALSTREAM3IATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLNORMALSTREAM3IATIPROC);  -- ../include/SDL/SDL_opengl.h:6086

   type PFNGLNORMALSTREAM3IVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLNORMALSTREAM3IVATIPROC);  -- ../include/SDL/SDL_opengl.h:6087

   type PFNGLNORMALSTREAM3FATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLfloat;
         arg3 : GL_gl_h.GLfloat;
         arg4 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLNORMALSTREAM3FATIPROC);  -- ../include/SDL/SDL_opengl.h:6088

   type PFNGLNORMALSTREAM3FVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLNORMALSTREAM3FVATIPROC);  -- ../include/SDL/SDL_opengl.h:6089

   type PFNGLNORMALSTREAM3DATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLdouble;
         arg3 : GL_gl_h.GLdouble;
         arg4 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLNORMALSTREAM3DATIPROC);  -- ../include/SDL/SDL_opengl.h:6090

   type PFNGLNORMALSTREAM3DVATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLNORMALSTREAM3DVATIPROC);  -- ../include/SDL/SDL_opengl.h:6091

   type PFNGLCLIENTACTIVEVERTEXSTREAMATIPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLCLIENTACTIVEVERTEXSTREAMATIPROC);  -- ../include/SDL/SDL_opengl.h:6092

   type PFNGLVERTEXBLENDENVIATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXBLENDENVIATIPROC);  -- ../include/SDL/SDL_opengl.h:6093

   type PFNGLVERTEXBLENDENVFATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLVERTEXBLENDENVFATIPROC);  -- ../include/SDL/SDL_opengl.h:6094

   type PFNGLELEMENTPOINTERATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, PFNGLELEMENTPOINTERATIPROC);  -- ../include/SDL/SDL_opengl.h:6104

   type PFNGLDRAWELEMENTARRAYATIPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDRAWELEMENTARRAYATIPROC);  -- ../include/SDL/SDL_opengl.h:6105

   type PFNGLDRAWRANGEELEMENTARRAYATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDRAWRANGEELEMENTARRAYATIPROC);  -- ../include/SDL/SDL_opengl.h:6106

   type PFNGLDRAWMESHARRAYSSUNPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDRAWMESHARRAYSSUNPROC);  -- ../include/SDL/SDL_opengl.h:6114

   type PFNGLGENOCCLUSIONQUERIESNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENOCCLUSIONQUERIESNVPROC);  -- ../include/SDL/SDL_opengl.h:6140

   type PFNGLDELETEOCCLUSIONQUERIESNVPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEOCCLUSIONQUERIESNVPROC);  -- ../include/SDL/SDL_opengl.h:6141

   type PFNGLISOCCLUSIONQUERYNVPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISOCCLUSIONQUERYNVPROC);  -- ../include/SDL/SDL_opengl.h:6142

   type PFNGLBEGINOCCLUSIONQUERYNVPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBEGINOCCLUSIONQUERYNVPROC);  -- ../include/SDL/SDL_opengl.h:6143

   type PFNGLENDOCCLUSIONQUERYNVPROC is access procedure;
   pragma Convention (C, PFNGLENDOCCLUSIONQUERYNVPROC);  -- ../include/SDL/SDL_opengl.h:6144

   type PFNGLGETOCCLUSIONQUERYIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETOCCLUSIONQUERYIVNVPROC);  -- ../include/SDL/SDL_opengl.h:6145

   type PFNGLGETOCCLUSIONQUERYUIVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGETOCCLUSIONQUERYUIVNVPROC);  -- ../include/SDL/SDL_opengl.h:6146

   type PFNGLPOINTPARAMETERINVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLPOINTPARAMETERINVPROC);  -- ../include/SDL/SDL_opengl.h:6155

   type PFNGLPOINTPARAMETERIVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLPOINTPARAMETERIVNVPROC);  -- ../include/SDL/SDL_opengl.h:6156

   type PFNGLACTIVESTENCILFACEEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLACTIVESTENCILFACEEXTPROC);  -- ../include/SDL/SDL_opengl.h:6176

   type PFNGLELEMENTPOINTERAPPLEPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : System.Address);
   pragma Convention (C, PFNGLELEMENTPOINTERAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6196

   type PFNGLDRAWELEMENTARRAYAPPLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDRAWELEMENTARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6197

   type PFNGLDRAWRANGEELEMENTARRAYAPPLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : GL_gl_h.GLint;
         arg5 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLDRAWRANGEELEMENTARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6198

   type PFNGLMULTIDRAWELEMENTARRAYAPPLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : access GL_gl_h.GLint;
         arg3 : access GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWELEMENTARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6199

   type PFNGLMULTIDRAWRANGEELEMENTARRAYAPPLEPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLuint;
         arg3 : GL_gl_h.GLuint;
         arg4 : access GL_gl_h.GLint;
         arg5 : access GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLMULTIDRAWRANGEELEMENTARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6200

   type PFNGLGENFENCESAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENFENCESAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6215

   type PFNGLDELETEFENCESAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEFENCESAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6216

   type PFNGLSETFENCEAPPLEPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSETFENCEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6217

   type PFNGLISFENCEAPPLEPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISFENCEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6218

   type PFNGLTESTFENCEAPPLEPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLTESTFENCEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6219

   type PFNGLFINISHFENCEAPPLEPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLFINISHFENCEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6220

   type PFNGLTESTOBJECTAPPLEPROC is access function (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLTESTOBJECTAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6221

   type PFNGLFINISHOBJECTAPPLEPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFINISHOBJECTAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6222

   type PFNGLBINDVERTEXARRAYAPPLEPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDVERTEXARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6233

   type PFNGLDELETEVERTEXARRAYSAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEVERTEXARRAYSAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6234

   type PFNGLGENVERTEXARRAYSAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENVERTEXARRAYSAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6235

   type PFNGLISVERTEXARRAYAPPLEPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISVERTEXARRAYAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6236

   type PFNGLVERTEXARRAYRANGEAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : System.Address);
   pragma Convention (C, PFNGLVERTEXARRAYRANGEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6246

   type PFNGLFLUSHVERTEXARRAYRANGEAPPLEPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : System.Address);
   pragma Convention (C, PFNGLFLUSHVERTEXARRAYRANGEAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6247

   type PFNGLVERTEXARRAYPARAMETERIAPPLEPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLVERTEXARRAYPARAMETERIAPPLEPROC);  -- ../include/SDL/SDL_opengl.h:6248

   type PFNGLDRAWBUFFERSATIPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLenum);
   pragma Convention (C, PFNGLDRAWBUFFERSATIPROC);  -- ../include/SDL/SDL_opengl.h:6264

   type PFNGLPROGRAMNAMEDPARAMETER4FNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLfloat;
         arg5 : GL_gl_h.GLfloat;
         arg6 : GL_gl_h.GLfloat;
         arg7 : GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMNAMEDPARAMETER4FNVPROC);  -- ../include/SDL/SDL_opengl.h:6297

   type PFNGLPROGRAMNAMEDPARAMETER4DNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : GL_gl_h.GLdouble;
         arg5 : GL_gl_h.GLdouble;
         arg6 : GL_gl_h.GLdouble;
         arg7 : GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMNAMEDPARAMETER4DNVPROC);  -- ../include/SDL/SDL_opengl.h:6298

   type PFNGLPROGRAMNAMEDPARAMETER4FVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLPROGRAMNAMEDPARAMETER4FVNVPROC);  -- ../include/SDL/SDL_opengl.h:6299

   type PFNGLPROGRAMNAMEDPARAMETER4DVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLPROGRAMNAMEDPARAMETER4DVNVPROC);  -- ../include/SDL/SDL_opengl.h:6300

   type PFNGLGETPROGRAMNAMEDPARAMETERFVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETPROGRAMNAMEDPARAMETERFVNVPROC);  -- ../include/SDL/SDL_opengl.h:6301

   type PFNGLGETPROGRAMNAMEDPARAMETERDVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GL_gl_h.GLubyte;
         arg4 : access GL_gl_h.GLdouble);
   pragma Convention (C, PFNGLGETPROGRAMNAMEDPARAMETERDVNVPROC);  -- ../include/SDL/SDL_opengl.h:6302

   type PFNGLVERTEX2HNVPROC is access procedure (arg1 : GLhalfNV; arg2 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEX2HNVPROC);  -- ../include/SDL/SDL_opengl.h:6355

   type PFNGLVERTEX2HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEX2HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6356

   type PFNGLVERTEX3HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEX3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6357

   type PFNGLVERTEX3HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEX3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6358

   type PFNGLVERTEX4HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEX4HNVPROC);  -- ../include/SDL/SDL_opengl.h:6359

   type PFNGLVERTEX4HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEX4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6360

   type PFNGLNORMAL3HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLNORMAL3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6361

   type PFNGLNORMAL3HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLNORMAL3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6362

   type PFNGLCOLOR3HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLCOLOR3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6363

   type PFNGLCOLOR3HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLCOLOR3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6364

   type PFNGLCOLOR4HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV);
   pragma Convention (C, PFNGLCOLOR4HNVPROC);  -- ../include/SDL/SDL_opengl.h:6365

   type PFNGLCOLOR4HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLCOLOR4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6366

   type PFNGLTEXCOORD1HNVPROC is access procedure (arg1 : GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD1HNVPROC);  -- ../include/SDL/SDL_opengl.h:6367

   type PFNGLTEXCOORD1HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD1HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6368

   type PFNGLTEXCOORD2HNVPROC is access procedure (arg1 : GLhalfNV; arg2 : GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD2HNVPROC);  -- ../include/SDL/SDL_opengl.h:6369

   type PFNGLTEXCOORD2HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD2HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6370

   type PFNGLTEXCOORD3HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6371

   type PFNGLTEXCOORD3HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6372

   type PFNGLTEXCOORD4HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD4HNVPROC);  -- ../include/SDL/SDL_opengl.h:6373

   type PFNGLTEXCOORD4HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLTEXCOORD4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6374

   type PFNGLMULTITEXCOORD1HNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD1HNVPROC);  -- ../include/SDL/SDL_opengl.h:6375

   type PFNGLMULTITEXCOORD1HVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD1HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6376

   type PFNGLMULTITEXCOORD2HNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD2HNVPROC);  -- ../include/SDL/SDL_opengl.h:6377

   type PFNGLMULTITEXCOORD2HVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD2HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6378

   type PFNGLMULTITEXCOORD3HNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6379

   type PFNGLMULTITEXCOORD3HVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6380

   type PFNGLMULTITEXCOORD4HNVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV;
         arg5 : GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD4HNVPROC);  -- ../include/SDL/SDL_opengl.h:6381

   type PFNGLMULTITEXCOORD4HVNVPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLMULTITEXCOORD4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6382

   type PFNGLFOGCOORDHNVPROC is access procedure (arg1 : GLhalfNV);
   pragma Convention (C, PFNGLFOGCOORDHNVPROC);  -- ../include/SDL/SDL_opengl.h:6383

   type PFNGLFOGCOORDHVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLFOGCOORDHVNVPROC);  -- ../include/SDL/SDL_opengl.h:6384

   type PFNGLSECONDARYCOLOR3HNVPROC is access procedure
        (arg1 : GLhalfNV;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLSECONDARYCOLOR3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6385

   type PFNGLSECONDARYCOLOR3HVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLSECONDARYCOLOR3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6386

   type PFNGLVERTEXWEIGHTHNVPROC is access procedure (arg1 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEXWEIGHTHNVPROC);  -- ../include/SDL/SDL_opengl.h:6387

   type PFNGLVERTEXWEIGHTHVNVPROC is access procedure (arg1 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXWEIGHTHVNVPROC);  -- ../include/SDL/SDL_opengl.h:6388

   type PFNGLVERTEXATTRIB1HNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB1HNVPROC);  -- ../include/SDL/SDL_opengl.h:6389

   type PFNGLVERTEXATTRIB1HVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB1HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6390

   type PFNGLVERTEXATTRIB2HNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB2HNVPROC);  -- ../include/SDL/SDL_opengl.h:6391

   type PFNGLVERTEXATTRIB2HVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB2HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6392

   type PFNGLVERTEXATTRIB3HNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB3HNVPROC);  -- ../include/SDL/SDL_opengl.h:6393

   type PFNGLVERTEXATTRIB3HVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6394

   type PFNGLVERTEXATTRIB4HNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GLhalfNV;
         arg3 : GLhalfNV;
         arg4 : GLhalfNV;
         arg5 : GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB4HNVPROC);  -- ../include/SDL/SDL_opengl.h:6395

   type PFNGLVERTEXATTRIB4HVNVPROC is access procedure (arg1 : GL_gl_h.GLuint; arg2 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIB4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6396

   type PFNGLVERTEXATTRIBS1HVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIBS1HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6397

   type PFNGLVERTEXATTRIBS2HVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIBS2HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6398

   type PFNGLVERTEXATTRIBS3HVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIBS3HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6399

   type PFNGLVERTEXATTRIBS4HVNVPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLsizei;
         arg3 : access GLhalfNV);
   pragma Convention (C, PFNGLVERTEXATTRIBS4HVNVPROC);  -- ../include/SDL/SDL_opengl.h:6400

   type PFNGLPIXELDATARANGENVPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLsizei;
         arg3 : System.Address);
   pragma Convention (C, PFNGLPIXELDATARANGENVPROC);  -- ../include/SDL/SDL_opengl.h:6409

   type PFNGLFLUSHPIXELDATARANGENVPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLFLUSHPIXELDATARANGENVPROC);  -- ../include/SDL/SDL_opengl.h:6410

   type PFNGLPRIMITIVERESTARTNVPROC is access procedure;
   pragma Convention (C, PFNGLPRIMITIVERESTARTNVPROC);  -- ../include/SDL/SDL_opengl.h:6419

   type PFNGLPRIMITIVERESTARTINDEXNVPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLPRIMITIVERESTARTINDEXNVPROC);  -- ../include/SDL/SDL_opengl.h:6420

   type PFNGLMAPOBJECTBUFFERATIPROC is access function (arg1 : GL_gl_h.GLuint) return System.Address;
   pragma Convention (C, PFNGLMAPOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:6437

   type PFNGLUNMAPOBJECTBUFFERATIPROC is access procedure (arg1 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLUNMAPOBJECTBUFFERATIPROC);  -- ../include/SDL/SDL_opengl.h:6438

   type PFNGLSTENCILOPSEPARATEATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLSTENCILOPSEPARATEATIPROC);  -- ../include/SDL/SDL_opengl.h:6447

   type PFNGLSTENCILFUNCSEPARATEATIPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLint;
         arg4 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLSTENCILFUNCSEPARATEATIPROC);  -- ../include/SDL/SDL_opengl.h:6448

   type PFNGLVERTEXATTRIBARRAYOBJECTATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLint;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLboolean;
         arg5 : GL_gl_h.GLsizei;
         arg6 : GL_gl_h.GLuint;
         arg7 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLVERTEXATTRIBARRAYOBJECTATIPROC);  -- ../include/SDL/SDL_opengl.h:6458

   type PFNGLGETVERTEXATTRIBARRAYOBJECTFVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLfloat);
   pragma Convention (C, PFNGLGETVERTEXATTRIBARRAYOBJECTFVATIPROC);  -- ../include/SDL/SDL_opengl.h:6459

   type PFNGLGETVERTEXATTRIBARRAYOBJECTIVATIPROC is access procedure
        (arg1 : GL_gl_h.GLuint;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETVERTEXATTRIBARRAYOBJECTIVATIPROC);  -- ../include/SDL/SDL_opengl.h:6460

   type PFNGLDEPTHBOUNDSEXTPROC is access procedure (arg1 : GL_gl_h.GLclampd; arg2 : GL_gl_h.GLclampd);
   pragma Convention (C, PFNGLDEPTHBOUNDSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6472

   type PFNGLBLENDEQUATIONSEPARATEEXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLBLENDEQUATIONSEPARATEEXTPROC);  -- ../include/SDL/SDL_opengl.h:6484

   type PFNGLISRENDERBUFFEREXTPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISRENDERBUFFEREXTPROC);  -- ../include/SDL/SDL_opengl.h:6536

   type PFNGLBINDRENDERBUFFEREXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDRENDERBUFFEREXTPROC);  -- ../include/SDL/SDL_opengl.h:6537

   type PFNGLDELETERENDERBUFFERSEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETERENDERBUFFERSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6538

   type PFNGLGENRENDERBUFFERSEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENRENDERBUFFERSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6539

   type PFNGLRENDERBUFFERSTORAGEEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLsizei;
         arg4 : GL_gl_h.GLsizei);
   pragma Convention (C, PFNGLRENDERBUFFERSTORAGEEXTPROC);  -- ../include/SDL/SDL_opengl.h:6540

   type PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETRENDERBUFFERPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:6541

   type PFNGLISFRAMEBUFFEREXTPROC is access function (arg1 : GL_gl_h.GLuint) return GL_gl_h.GLboolean;
   pragma Convention (C, PFNGLISFRAMEBUFFEREXTPROC);  -- ../include/SDL/SDL_opengl.h:6542

   type PFNGLBINDFRAMEBUFFEREXTPROC is access procedure (arg1 : GL_gl_h.GLenum; arg2 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLBINDFRAMEBUFFEREXTPROC);  -- ../include/SDL/SDL_opengl.h:6543

   type PFNGLDELETEFRAMEBUFFERSEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLDELETEFRAMEBUFFERSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6544

   type PFNGLGENFRAMEBUFFERSEXTPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : access GL_gl_h.GLuint);
   pragma Convention (C, PFNGLGENFRAMEBUFFERSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6545

   type PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC is access function (arg1 : GL_gl_h.GLenum) return GL_gl_h.GLenum;
   pragma Convention (C, PFNGLCHECKFRAMEBUFFERSTATUSEXTPROC);  -- ../include/SDL/SDL_opengl.h:6546

   type PFNGLFRAMEBUFFERTEXTURE1DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAMEBUFFERTEXTURE1DEXTPROC);  -- ../include/SDL/SDL_opengl.h:6547

   type PFNGLFRAMEBUFFERTEXTURE2DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAMEBUFFERTEXTURE2DEXTPROC);  -- ../include/SDL/SDL_opengl.h:6548

   type PFNGLFRAMEBUFFERTEXTURE3DEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLuint;
         arg5 : GL_gl_h.GLint;
         arg6 : GL_gl_h.GLint);
   pragma Convention (C, PFNGLFRAMEBUFFERTEXTURE3DEXTPROC);  -- ../include/SDL/SDL_opengl.h:6549

   type PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : GL_gl_h.GLuint);
   pragma Convention (C, PFNGLFRAMEBUFFERRENDERBUFFEREXTPROC);  -- ../include/SDL/SDL_opengl.h:6550

   type PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC is access procedure
        (arg1 : GL_gl_h.GLenum;
         arg2 : GL_gl_h.GLenum;
         arg3 : GL_gl_h.GLenum;
         arg4 : access GL_gl_h.GLint);
   pragma Convention (C, PFNGLGETFRAMEBUFFERATTACHMENTPARAMETERIVEXTPROC);  -- ../include/SDL/SDL_opengl.h:6551

   type PFNGLGENERATEMIPMAPEXTPROC is access procedure (arg1 : GL_gl_h.GLenum);
   pragma Convention (C, PFNGLGENERATEMIPMAPEXTPROC);  -- ../include/SDL/SDL_opengl.h:6552

   type PFNGLSTRINGMARKERGREMEDYPROC is access procedure (arg1 : GL_gl_h.GLsizei; arg2 : System.Address);
   pragma Convention (C, PFNGLSTRINGMARKERGREMEDYPROC);  -- ../include/SDL/SDL_opengl.h:6560

end SDL_SDL_opengl_h;
