pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package SDL_scancode_h is

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
  -- *  \file SDL_scancode.h
  -- *
  -- *  Defines keyboard scancodes.
  --  

  --*
  -- *  \brief The SDL keyboard scancode representation.
  -- *
  -- *  Values of this type are used to represent keyboard keys, among other places
  -- *  in the \link SDL_Keysym::scancode key.keysym.scancode \endlink field of the
  -- *  SDL_Event structure.
  -- *
  -- *  The values in this enumeration are based on the USB usage page standard:
  -- *  http://www.usb.org/developers/hidpage/Hut1_12v2.pdf
  --  

  --*
  --     *  \name Usage page 0x07
  --     *
  --     *  These values are from usage page 0x07 (USB keyboard page).
  --      

  -- @{  
  --*< Located at the lower left of the return
  --                                  *   key on ISO keyboards and at the right end
  --                                  *   of the QWERTY row on ANSI keyboards.
  --                                  *   Produces REVERSE SOLIDUS (backslash) and
  --                                  *   VERTICAL LINE in a US layout, REVERSE
  --                                  *   SOLIDUS and VERTICAL LINE in a UK Mac
  --                                  *   layout, NUMBER SIGN and TILDE in a UK
  --                                  *   Windows layout, DOLLAR SIGN and POUND SIGN
  --                                  *   in a Swiss German layout, NUMBER SIGN and
  --                                  *   APOSTROPHE in a German layout, GRAVE
  --                                  *   ACCENT and POUND SIGN in a French Mac
  --                                  *   layout, and ASTERISK and MICRO SIGN in a
  --                                  *   French Windows layout.
  --                                   

  --*< ISO USB keyboards actually use this code
  --                                  *   instead of 49 for the same key, but all
  --                                  *   OSes I've seen treat the two codes
  --                                  *   identically. So, as an implementor, unless
  --                                  *   your keyboard generates both of those
  --                                  *   codes and your OS treats them differently,
  --                                  *   you should generate SDL_SCANCODE_BACKSLASH
  --                                  *   instead of this code. As a user, you
  --                                  *   should not rely on this code because SDL
  --                                  *   will never generate it with most (all?)
  --                                  *   keyboards.
  --                                   

  --*< Located in the top left corner (on both ANSI
  --                              *   and ISO keyboards). Produces GRAVE ACCENT and
  --                              *   TILDE in a US Windows layout and in US and UK
  --                              *   Mac layouts on ANSI keyboards, GRAVE ACCENT
  --                              *   and NOT SIGN in a UK Windows layout, SECTION
  --                              *   SIGN and PLUS-MINUS SIGN in US and UK Mac
  --                              *   layouts on ISO keyboards, SECTION SIGN and
  --                              *   DEGREE SIGN in a Swiss German layout (Mac:
  --                              *   only on ISO keyboards), CIRCUMFLEX ACCENT and
  --                              *   DEGREE SIGN in a German layout (Mac: only on
  --                              *   ISO keyboards), SUPERSCRIPT TWO and TILDE in a
  --                              *   French Windows layout, COMMERCIAL AT and
  --                              *   NUMBER SIGN in a French Mac layout on ISO
  --                              *   keyboards, and LESS-THAN SIGN and GREATER-THAN
  --                              *   SIGN in a Swiss German, German, or French Mac
  --                              *   layout on ANSI keyboards.
  --                               

  --*< insert on PC, help on some Mac keyboards (but
  --                                   does send code 73, not 117)  

  --*< num lock on PC, clear on Mac keyboards
  --                                      

  --*< This is the additional key that ISO
  --                                        *   keyboards have over ANSI ones,
  --                                        *   located between left shift and Y.
  --                                        *   Produces GRAVE ACCENT and TILDE in a
  --                                        *   US or UK Mac layout, REVERSE SOLIDUS
  --                                        *   (backslash) and VERTICAL LINE in a
  --                                        *   US or UK Windows layout, and
  --                                        *   LESS-THAN SIGN and GREATER-THAN SIGN
  --                                        *   in a Swiss German, German, or French
  --                                        *   layout.  

  --*< windows contextual menu, compose  
  --*< The USB document says this is a status flag,
  --                               *   not a physical key - but some Mac keyboards
  --                               *   do have a power key.  

  --*< redo  
  -- not sure whether there's a reason to enable these  
  --     SDL_SCANCODE_LOCKINGCAPSLOCK = 130,   
  --     SDL_SCANCODE_LOCKINGNUMLOCK = 131,  
  --     SDL_SCANCODE_LOCKINGSCROLLLOCK = 132,  
  --*< used on Asian keyboards, see
  --                                            footnotes in USB doc  

  --*< Yen  
  --*< Hangul/English toggle  
  --*< Hanja conversion  
  --*< Katakana  
  --*< Hiragana  
  --*< Zenkaku/Hankaku  
  --*< reserved  
  --*< reserved  
  --*< reserved  
  --*< reserved  
  --*< Erase-Eaze  
  --*< alt, option  
  --*< windows, command (apple), meta  
  --*< alt gr, option  
  --*< windows, command (apple), meta  
  --*< I'm not sure if this is really not covered
  --                                 *   by any of the above, but since there's a
  --                                 *   special KMOD_MODE for it I'm adding it here
  --                                  

  -- @}  
  -- Usage page 0x07  
  --*
  --     *  \name Usage page 0x0C
  --     *
  --     *  These values are mapped from usage page 0x0C (USB consumer page).
  --      

  -- @{  
  -- @}  
  -- Usage page 0x0C  
  --*
  --     *  \name Walther keys
  --     *
  --     *  These are values that Christian Walther added (for mac keyboard?).
  --      

  -- @{  
  --*< display mirroring/dual display
  --                                           switch, video mode switch  

  -- @}  
  -- Walther keys  
  --*
  --     *  \name Usage page 0x0C (additional media keys)
  --     *
  --     *  These values are mapped from usage page 0x0C (USB consumer page).
  --      

  -- @{  
  -- @}  
  -- Usage page 0x0C (additional media keys)  
  -- Add any other keys here.  
  --*< not a key, just marks the number of scancodes
  --                                 for array bounds  

   subtype SDL_Scancode is unsigned;
   SDL_SCANCODE_UNKNOWN : constant unsigned := 0;
   SDL_SCANCODE_A : constant unsigned := 4;
   SDL_SCANCODE_B : constant unsigned := 5;
   SDL_SCANCODE_C : constant unsigned := 6;
   SDL_SCANCODE_D : constant unsigned := 7;
   SDL_SCANCODE_E : constant unsigned := 8;
   SDL_SCANCODE_F : constant unsigned := 9;
   SDL_SCANCODE_G : constant unsigned := 10;
   SDL_SCANCODE_H : constant unsigned := 11;
   SDL_SCANCODE_I : constant unsigned := 12;
   SDL_SCANCODE_J : constant unsigned := 13;
   SDL_SCANCODE_K : constant unsigned := 14;
   SDL_SCANCODE_L : constant unsigned := 15;
   SDL_SCANCODE_M : constant unsigned := 16;
   SDL_SCANCODE_N : constant unsigned := 17;
   SDL_SCANCODE_O : constant unsigned := 18;
   SDL_SCANCODE_P : constant unsigned := 19;
   SDL_SCANCODE_Q : constant unsigned := 20;
   SDL_SCANCODE_R : constant unsigned := 21;
   SDL_SCANCODE_S : constant unsigned := 22;
   SDL_SCANCODE_T : constant unsigned := 23;
   SDL_SCANCODE_U : constant unsigned := 24;
   SDL_SCANCODE_V : constant unsigned := 25;
   SDL_SCANCODE_W : constant unsigned := 26;
   SDL_SCANCODE_X : constant unsigned := 27;
   SDL_SCANCODE_Y : constant unsigned := 28;
   SDL_SCANCODE_Z : constant unsigned := 29;
   SDL_SCANCODE_1 : constant unsigned := 30;
   SDL_SCANCODE_2 : constant unsigned := 31;
   SDL_SCANCODE_3 : constant unsigned := 32;
   SDL_SCANCODE_4 : constant unsigned := 33;
   SDL_SCANCODE_5 : constant unsigned := 34;
   SDL_SCANCODE_6 : constant unsigned := 35;
   SDL_SCANCODE_7 : constant unsigned := 36;
   SDL_SCANCODE_8 : constant unsigned := 37;
   SDL_SCANCODE_9 : constant unsigned := 38;
   SDL_SCANCODE_0 : constant unsigned := 39;
   SDL_SCANCODE_RETURN : constant unsigned := 40;
   SDL_SCANCODE_ESCAPE : constant unsigned := 41;
   SDL_SCANCODE_BACKSPACE : constant unsigned := 42;
   SDL_SCANCODE_TAB : constant unsigned := 43;
   SDL_SCANCODE_SPACE : constant unsigned := 44;
   SDL_SCANCODE_MINUS : constant unsigned := 45;
   SDL_SCANCODE_EQUALS : constant unsigned := 46;
   SDL_SCANCODE_LEFTBRACKET : constant unsigned := 47;
   SDL_SCANCODE_RIGHTBRACKET : constant unsigned := 48;
   SDL_SCANCODE_BACKSLASH : constant unsigned := 49;
   SDL_SCANCODE_NONUSHASH : constant unsigned := 50;
   SDL_SCANCODE_SEMICOLON : constant unsigned := 51;
   SDL_SCANCODE_APOSTROPHE : constant unsigned := 52;
   SDL_SCANCODE_GRAVE : constant unsigned := 53;
   SDL_SCANCODE_COMMA : constant unsigned := 54;
   SDL_SCANCODE_PERIOD : constant unsigned := 55;
   SDL_SCANCODE_SLASH : constant unsigned := 56;
   SDL_SCANCODE_CAPSLOCK : constant unsigned := 57;
   SDL_SCANCODE_F1 : constant unsigned := 58;
   SDL_SCANCODE_F2 : constant unsigned := 59;
   SDL_SCANCODE_F3 : constant unsigned := 60;
   SDL_SCANCODE_F4 : constant unsigned := 61;
   SDL_SCANCODE_F5 : constant unsigned := 62;
   SDL_SCANCODE_F6 : constant unsigned := 63;
   SDL_SCANCODE_F7 : constant unsigned := 64;
   SDL_SCANCODE_F8 : constant unsigned := 65;
   SDL_SCANCODE_F9 : constant unsigned := 66;
   SDL_SCANCODE_F10 : constant unsigned := 67;
   SDL_SCANCODE_F11 : constant unsigned := 68;
   SDL_SCANCODE_F12 : constant unsigned := 69;
   SDL_SCANCODE_PRINTSCREEN : constant unsigned := 70;
   SDL_SCANCODE_SCROLLLOCK : constant unsigned := 71;
   SDL_SCANCODE_PAUSE : constant unsigned := 72;
   SDL_SCANCODE_INSERT : constant unsigned := 73;
   SDL_SCANCODE_HOME : constant unsigned := 74;
   SDL_SCANCODE_PAGEUP : constant unsigned := 75;
   SDL_SCANCODE_DELETE : constant unsigned := 76;
   SDL_SCANCODE_END : constant unsigned := 77;
   SDL_SCANCODE_PAGEDOWN : constant unsigned := 78;
   SDL_SCANCODE_RIGHT : constant unsigned := 79;
   SDL_SCANCODE_LEFT : constant unsigned := 80;
   SDL_SCANCODE_DOWN : constant unsigned := 81;
   SDL_SCANCODE_UP : constant unsigned := 82;
   SDL_SCANCODE_NUMLOCKCLEAR : constant unsigned := 83;
   SDL_SCANCODE_KP_DIVIDE : constant unsigned := 84;
   SDL_SCANCODE_KP_MULTIPLY : constant unsigned := 85;
   SDL_SCANCODE_KP_MINUS : constant unsigned := 86;
   SDL_SCANCODE_KP_PLUS : constant unsigned := 87;
   SDL_SCANCODE_KP_ENTER : constant unsigned := 88;
   SDL_SCANCODE_KP_1 : constant unsigned := 89;
   SDL_SCANCODE_KP_2 : constant unsigned := 90;
   SDL_SCANCODE_KP_3 : constant unsigned := 91;
   SDL_SCANCODE_KP_4 : constant unsigned := 92;
   SDL_SCANCODE_KP_5 : constant unsigned := 93;
   SDL_SCANCODE_KP_6 : constant unsigned := 94;
   SDL_SCANCODE_KP_7 : constant unsigned := 95;
   SDL_SCANCODE_KP_8 : constant unsigned := 96;
   SDL_SCANCODE_KP_9 : constant unsigned := 97;
   SDL_SCANCODE_KP_0 : constant unsigned := 98;
   SDL_SCANCODE_KP_PERIOD : constant unsigned := 99;
   SDL_SCANCODE_NONUSBACKSLASH : constant unsigned := 100;
   SDL_SCANCODE_APPLICATION : constant unsigned := 101;
   SDL_SCANCODE_POWER : constant unsigned := 102;
   SDL_SCANCODE_KP_EQUALS : constant unsigned := 103;
   SDL_SCANCODE_F13 : constant unsigned := 104;
   SDL_SCANCODE_F14 : constant unsigned := 105;
   SDL_SCANCODE_F15 : constant unsigned := 106;
   SDL_SCANCODE_F16 : constant unsigned := 107;
   SDL_SCANCODE_F17 : constant unsigned := 108;
   SDL_SCANCODE_F18 : constant unsigned := 109;
   SDL_SCANCODE_F19 : constant unsigned := 110;
   SDL_SCANCODE_F20 : constant unsigned := 111;
   SDL_SCANCODE_F21 : constant unsigned := 112;
   SDL_SCANCODE_F22 : constant unsigned := 113;
   SDL_SCANCODE_F23 : constant unsigned := 114;
   SDL_SCANCODE_F24 : constant unsigned := 115;
   SDL_SCANCODE_EXECUTE : constant unsigned := 116;
   SDL_SCANCODE_HELP : constant unsigned := 117;
   SDL_SCANCODE_MENU : constant unsigned := 118;
   SDL_SCANCODE_SELECT : constant unsigned := 119;
   SDL_SCANCODE_STOP : constant unsigned := 120;
   SDL_SCANCODE_AGAIN : constant unsigned := 121;
   SDL_SCANCODE_UNDO : constant unsigned := 122;
   SDL_SCANCODE_CUT : constant unsigned := 123;
   SDL_SCANCODE_COPY : constant unsigned := 124;
   SDL_SCANCODE_PASTE : constant unsigned := 125;
   SDL_SCANCODE_FIND : constant unsigned := 126;
   SDL_SCANCODE_MUTE : constant unsigned := 127;
   SDL_SCANCODE_VOLUMEUP : constant unsigned := 128;
   SDL_SCANCODE_VOLUMEDOWN : constant unsigned := 129;
   SDL_SCANCODE_KP_COMMA : constant unsigned := 133;
   SDL_SCANCODE_KP_EQUALSAS400 : constant unsigned := 134;
   SDL_SCANCODE_INTERNATIONAL1 : constant unsigned := 135;
   SDL_SCANCODE_INTERNATIONAL2 : constant unsigned := 136;
   SDL_SCANCODE_INTERNATIONAL3 : constant unsigned := 137;
   SDL_SCANCODE_INTERNATIONAL4 : constant unsigned := 138;
   SDL_SCANCODE_INTERNATIONAL5 : constant unsigned := 139;
   SDL_SCANCODE_INTERNATIONAL6 : constant unsigned := 140;
   SDL_SCANCODE_INTERNATIONAL7 : constant unsigned := 141;
   SDL_SCANCODE_INTERNATIONAL8 : constant unsigned := 142;
   SDL_SCANCODE_INTERNATIONAL9 : constant unsigned := 143;
   SDL_SCANCODE_LANG1 : constant unsigned := 144;
   SDL_SCANCODE_LANG2 : constant unsigned := 145;
   SDL_SCANCODE_LANG3 : constant unsigned := 146;
   SDL_SCANCODE_LANG4 : constant unsigned := 147;
   SDL_SCANCODE_LANG5 : constant unsigned := 148;
   SDL_SCANCODE_LANG6 : constant unsigned := 149;
   SDL_SCANCODE_LANG7 : constant unsigned := 150;
   SDL_SCANCODE_LANG8 : constant unsigned := 151;
   SDL_SCANCODE_LANG9 : constant unsigned := 152;
   SDL_SCANCODE_ALTERASE : constant unsigned := 153;
   SDL_SCANCODE_SYSREQ : constant unsigned := 154;
   SDL_SCANCODE_CANCEL : constant unsigned := 155;
   SDL_SCANCODE_CLEAR : constant unsigned := 156;
   SDL_SCANCODE_PRIOR : constant unsigned := 157;
   SDL_SCANCODE_RETURN2 : constant unsigned := 158;
   SDL_SCANCODE_SEPARATOR : constant unsigned := 159;
   SDL_SCANCODE_OUT : constant unsigned := 160;
   SDL_SCANCODE_OPER : constant unsigned := 161;
   SDL_SCANCODE_CLEARAGAIN : constant unsigned := 162;
   SDL_SCANCODE_CRSEL : constant unsigned := 163;
   SDL_SCANCODE_EXSEL : constant unsigned := 164;
   SDL_SCANCODE_KP_00 : constant unsigned := 176;
   SDL_SCANCODE_KP_000 : constant unsigned := 177;
   SDL_SCANCODE_THOUSANDSSEPARATOR : constant unsigned := 178;
   SDL_SCANCODE_DECIMALSEPARATOR : constant unsigned := 179;
   SDL_SCANCODE_CURRENCYUNIT : constant unsigned := 180;
   SDL_SCANCODE_CURRENCYSUBUNIT : constant unsigned := 181;
   SDL_SCANCODE_KP_LEFTPAREN : constant unsigned := 182;
   SDL_SCANCODE_KP_RIGHTPAREN : constant unsigned := 183;
   SDL_SCANCODE_KP_LEFTBRACE : constant unsigned := 184;
   SDL_SCANCODE_KP_RIGHTBRACE : constant unsigned := 185;
   SDL_SCANCODE_KP_TAB : constant unsigned := 186;
   SDL_SCANCODE_KP_BACKSPACE : constant unsigned := 187;
   SDL_SCANCODE_KP_A : constant unsigned := 188;
   SDL_SCANCODE_KP_B : constant unsigned := 189;
   SDL_SCANCODE_KP_C : constant unsigned := 190;
   SDL_SCANCODE_KP_D : constant unsigned := 191;
   SDL_SCANCODE_KP_E : constant unsigned := 192;
   SDL_SCANCODE_KP_F : constant unsigned := 193;
   SDL_SCANCODE_KP_XOR : constant unsigned := 194;
   SDL_SCANCODE_KP_POWER : constant unsigned := 195;
   SDL_SCANCODE_KP_PERCENT : constant unsigned := 196;
   SDL_SCANCODE_KP_LESS : constant unsigned := 197;
   SDL_SCANCODE_KP_GREATER : constant unsigned := 198;
   SDL_SCANCODE_KP_AMPERSAND : constant unsigned := 199;
   SDL_SCANCODE_KP_DBLAMPERSAND : constant unsigned := 200;
   SDL_SCANCODE_KP_VERTICALBAR : constant unsigned := 201;
   SDL_SCANCODE_KP_DBLVERTICALBAR : constant unsigned := 202;
   SDL_SCANCODE_KP_COLON : constant unsigned := 203;
   SDL_SCANCODE_KP_HASH : constant unsigned := 204;
   SDL_SCANCODE_KP_SPACE : constant unsigned := 205;
   SDL_SCANCODE_KP_AT : constant unsigned := 206;
   SDL_SCANCODE_KP_EXCLAM : constant unsigned := 207;
   SDL_SCANCODE_KP_MEMSTORE : constant unsigned := 208;
   SDL_SCANCODE_KP_MEMRECALL : constant unsigned := 209;
   SDL_SCANCODE_KP_MEMCLEAR : constant unsigned := 210;
   SDL_SCANCODE_KP_MEMADD : constant unsigned := 211;
   SDL_SCANCODE_KP_MEMSUBTRACT : constant unsigned := 212;
   SDL_SCANCODE_KP_MEMMULTIPLY : constant unsigned := 213;
   SDL_SCANCODE_KP_MEMDIVIDE : constant unsigned := 214;
   SDL_SCANCODE_KP_PLUSMINUS : constant unsigned := 215;
   SDL_SCANCODE_KP_CLEAR : constant unsigned := 216;
   SDL_SCANCODE_KP_CLEARENTRY : constant unsigned := 217;
   SDL_SCANCODE_KP_BINARY : constant unsigned := 218;
   SDL_SCANCODE_KP_OCTAL : constant unsigned := 219;
   SDL_SCANCODE_KP_DECIMAL : constant unsigned := 220;
   SDL_SCANCODE_KP_HEXADECIMAL : constant unsigned := 221;
   SDL_SCANCODE_LCTRL : constant unsigned := 224;
   SDL_SCANCODE_LSHIFT : constant unsigned := 225;
   SDL_SCANCODE_LALT : constant unsigned := 226;
   SDL_SCANCODE_LGUI : constant unsigned := 227;
   SDL_SCANCODE_RCTRL : constant unsigned := 228;
   SDL_SCANCODE_RSHIFT : constant unsigned := 229;
   SDL_SCANCODE_RALT : constant unsigned := 230;
   SDL_SCANCODE_RGUI : constant unsigned := 231;
   SDL_SCANCODE_MODE : constant unsigned := 257;
   SDL_SCANCODE_AUDIONEXT : constant unsigned := 258;
   SDL_SCANCODE_AUDIOPREV : constant unsigned := 259;
   SDL_SCANCODE_AUDIOSTOP : constant unsigned := 260;
   SDL_SCANCODE_AUDIOPLAY : constant unsigned := 261;
   SDL_SCANCODE_AUDIOMUTE : constant unsigned := 262;
   SDL_SCANCODE_MEDIASELECT : constant unsigned := 263;
   SDL_SCANCODE_WWW : constant unsigned := 264;
   SDL_SCANCODE_MAIL : constant unsigned := 265;
   SDL_SCANCODE_CALCULATOR : constant unsigned := 266;
   SDL_SCANCODE_COMPUTER : constant unsigned := 267;
   SDL_SCANCODE_AC_SEARCH : constant unsigned := 268;
   SDL_SCANCODE_AC_HOME : constant unsigned := 269;
   SDL_SCANCODE_AC_BACK : constant unsigned := 270;
   SDL_SCANCODE_AC_FORWARD : constant unsigned := 271;
   SDL_SCANCODE_AC_STOP : constant unsigned := 272;
   SDL_SCANCODE_AC_REFRESH : constant unsigned := 273;
   SDL_SCANCODE_AC_BOOKMARKS : constant unsigned := 274;
   SDL_SCANCODE_BRIGHTNESSDOWN : constant unsigned := 275;
   SDL_SCANCODE_BRIGHTNESSUP : constant unsigned := 276;
   SDL_SCANCODE_DISPLAYSWITCH : constant unsigned := 277;
   SDL_SCANCODE_KBDILLUMTOGGLE : constant unsigned := 278;
   SDL_SCANCODE_KBDILLUMDOWN : constant unsigned := 279;
   SDL_SCANCODE_KBDILLUMUP : constant unsigned := 280;
   SDL_SCANCODE_EJECT : constant unsigned := 281;
   SDL_SCANCODE_SLEEP : constant unsigned := 282;
   SDL_SCANCODE_APP1 : constant unsigned := 283;
   SDL_SCANCODE_APP2 : constant unsigned := 284;
   SDL_SCANCODE_AUDIOREWIND : constant unsigned := 285;
   SDL_SCANCODE_AUDIOFASTFORWARD : constant unsigned := 286;
   SDL_NUM_SCANCODES : constant unsigned := 512;  -- ..\SDL2_tmp\SDL_scancode.h:409

  -- vi: set ts=4 sw=4 expandtab:  
end SDL_scancode_h;
