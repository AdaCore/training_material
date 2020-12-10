pragma Ada_2005;
pragma Style_Checks (Off);

with Interfaces.C; use Interfaces.C;

package SDL_blendmode_h is

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
  -- *  \file SDL_blendmode.h
  -- *
  -- *  Header file declaring the SDL_BlendMode enumeration
  --  

  -- Set up for C function definitions, even when using C++  
  --*
  -- *  \brief The blend mode used in SDL_RenderCopy() and drawing operations.
  --  

  --*< no blending
  --                                              dstRGBA = srcRGBA  

  --*< alpha blending
  --                                              dstRGB = (srcRGB * srcA) + (dstRGB * (1-srcA))
  --                                              dstA = srcA + (dstA * (1-srcA))  

  --*< additive blending
  --                                              dstRGB = (srcRGB * srcA) + dstRGB
  --                                              dstA = dstA  

  --*< color modulate
  --                                              dstRGB = srcRGB * dstRGB
  --                                              dstA = dstA  

  -- Additional custom blend modes can be returned by SDL_ComposeCustomBlendMode()  
   subtype SDL_BlendMode is unsigned;
   SDL_BLENDMODE_NONE : constant unsigned := 0;
   SDL_BLENDMODE_BLEND : constant unsigned := 1;
   SDL_BLENDMODE_ADD : constant unsigned := 2;
   SDL_BLENDMODE_MOD : constant unsigned := 4;
   SDL_BLENDMODE_INVALID : constant unsigned := 2147483647;  -- ..\SDL2_tmp\SDL_blendmode.h:57

  --*
  -- *  \brief The blend operation used when combining source and destination pixel components
  --  

  --*< dst + src: supported by all renderers  
  --*< dst - src : supported by D3D9, D3D11, OpenGL, OpenGLES  
  --*< src - dst : supported by D3D9, D3D11, OpenGL, OpenGLES  
  --*< min(dst, src) : supported by D3D11  
  --*< max(dst, src) : supported by D3D11  
   subtype SDL_BlendOperation is unsigned;
   SDL_BLENDOPERATION_ADD : constant unsigned := 1;
   SDL_BLENDOPERATION_SUBTRACT : constant unsigned := 2;
   SDL_BLENDOPERATION_REV_SUBTRACT : constant unsigned := 3;
   SDL_BLENDOPERATION_MINIMUM : constant unsigned := 4;
   SDL_BLENDOPERATION_MAXIMUM : constant unsigned := 5;  -- ..\SDL2_tmp\SDL_blendmode.h:70

  --*
  -- *  \brief The normalized factor used to multiply pixel components
  --  

  --*< 0, 0, 0, 0  
  --*< 1, 1, 1, 1  
  --*< srcR, srcG, srcB, srcA  
  --*< 1-srcR, 1-srcG, 1-srcB, 1-srcA  
  --*< srcA, srcA, srcA, srcA  
  --*< 1-srcA, 1-srcA, 1-srcA, 1-srcA  
  --*< dstR, dstG, dstB, dstA  
  --*< 1-dstR, 1-dstG, 1-dstB, 1-dstA  
  --*< dstA, dstA, dstA, dstA  
  --*< 1-dstA, 1-dstA, 1-dstA, 1-dstA  
   subtype SDL_BlendFactor is unsigned;
   SDL_BLENDFACTOR_ZERO : constant unsigned := 1;
   SDL_BLENDFACTOR_ONE : constant unsigned := 2;
   SDL_BLENDFACTOR_SRC_COLOR : constant unsigned := 3;
   SDL_BLENDFACTOR_ONE_MINUS_SRC_COLOR : constant unsigned := 4;
   SDL_BLENDFACTOR_SRC_ALPHA : constant unsigned := 5;
   SDL_BLENDFACTOR_ONE_MINUS_SRC_ALPHA : constant unsigned := 6;
   SDL_BLENDFACTOR_DST_COLOR : constant unsigned := 7;
   SDL_BLENDFACTOR_ONE_MINUS_DST_COLOR : constant unsigned := 8;
   SDL_BLENDFACTOR_DST_ALPHA : constant unsigned := 9;
   SDL_BLENDFACTOR_ONE_MINUS_DST_ALPHA : constant unsigned := 10;  -- ..\SDL2_tmp\SDL_blendmode.h:88

  --*
  -- *  \brief Create a custom blend mode, which may or may not be supported by a given renderer
  -- *
  -- *  \param srcColorFactor
  -- *  \param dstColorFactor
  -- *  \param colorOperation
  -- *  \param srcAlphaFactor
  -- *  \param dstAlphaFactor
  -- *  \param alphaOperation
  -- *
  -- *  The result of the blend mode operation will be:
  -- *      dstRGB = dstRGB * dstColorFactor colorOperation srcRGB * srcColorFactor
  -- *  and
  -- *      dstA = dstA * dstAlphaFactor alphaOperation srcA * srcAlphaFactor
  --  

   function SDL_ComposeCustomBlendMode
     (srcColorFactor : SDL_BlendFactor;
      dstColorFactor : SDL_BlendFactor;
      colorOperation : SDL_BlendOperation;
      srcAlphaFactor : SDL_BlendFactor;
      dstAlphaFactor : SDL_BlendFactor;
      alphaOperation : SDL_BlendOperation) return SDL_BlendMode;  -- ..\SDL2_tmp\SDL_blendmode.h:105
   pragma Import (C, SDL_ComposeCustomBlendMode, "SDL_ComposeCustomBlendMode");

  -- Ends C function definitions when using C++  
  -- vi: set ts=4 sw=4 expandtab:  
end SDL_blendmode_h;
