with Ada.Real_Time; use Ada.Real_Time;

with STM32F4.SDRAM;               use STM32F4.SDRAM;

package body STM32F4.LCD is

   function As_Word is new Ada.Unchecked_Conversion
     (Source => System.Address, Target => Word);

   -------------------
   -- Init_LCD_GPIO --
   -------------------

   procedure Init_LCD_GPIO is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_A);
      Enable_Clock (GPIO_B);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_D);
      Enable_Clock (GPIO_F);
      Enable_Clock (GPIO_G);

      Configure_Alternate_Function (GPIO_A,
                                    (Pin_3, Pin_4, Pin_6, Pin_11, Pin_12),
                                    GPIO_AF_LTDC);

      Conf.Speed       := Speed_50MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;
      Conf.Locked      := True;
      Configure_IO (GPIO_A, (Pin_3, Pin_4, Pin_6, Pin_11, Pin_12), Conf);

      Configure_Alternate_Function (GPIO_B, (Pin_0, Pin_1), GPIO_AF_LTDC_2);
      Configure_Alternate_Function (GPIO_B, (Pin_8, Pin_9, Pin_10, Pin_11),
                                    GPIO_AF_LTDC);
      Configure_IO (GPIO_B, (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10, Pin_11),
                    Conf);

      Configure_Alternate_Function (GPIO_C, (Pin_6, Pin_7, Pin_10),
                                    GPIO_AF_LTDC);
      Configure_IO (GPIO_C, (Pin_6, Pin_7, Pin_10), Conf);

      Configure_Alternate_Function (GPIO_D, (Pin_3, Pin_6), GPIO_AF_LTDC);
      Configure_IO (GPIO_D, (Pin_3, Pin_6), Conf);

      Configure_Alternate_Function (GPIO_F, Pin_10, GPIO_AF_LTDC);
      Configure_IO (GPIO_F, Pin_10, Conf);

      Configure_Alternate_Function (GPIO_G, (Pin_6, Pin_7, Pin_11),
                                    GPIO_AF_LTDC);
      Configure_Alternate_Function (GPIO_G, (Pin_10, Pin_12), GPIO_AF_LTDC_2);
      Configure_IO (GPIO_G, (Pin_6, Pin_7, Pin_10, Pin_11, Pin_12), Conf);
   end Init_LCD_GPIO;

   ------------------
   -- Init_LCD_SPI --
   ------------------

   procedure Init_LCD_SPI is
      Conf     : GPIO_Port_Configuration;
      SPI_Conf : SPI_Configuration;
   begin
      Enable_Clock (SCK_GPIO);
      Enable_Clock (MISO_GPIO);
      Enable_Clock (MOSI_GPIO);
      Enable_Clock (LCD_SPI);

      Configure_Alternate_Function (SCK_GPIO, SCK_Pin, SCK_AF);
      Configure_Alternate_Function (MISO_GPIO, MISO_Pin, MISO_AF);
      Configure_Alternate_Function (MOSI_GPIO, MOSI_Pin, MOSI_AF);

      Conf.Speed       := Speed_25MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Pull_Down;
      Conf.Locked      := True;
      Configure_IO (SCK_GPIO, SCK_Pin, Conf);
      Configure_IO (MISO_GPIO, MISO_Pin, Conf);
      Configure_IO (MOSI_GPIO, MOSI_Pin, Conf);

      Reset (LCD_SPI);

      if not Enabled (LCD_SPI) then
         SPI_Conf.Direction           := D2Lines_FullDuplex;
         SPI_Conf.Mode                := Master;
         SPI_Conf.Data_Size           := Data_8;
         SPI_Conf.Clock_Polarity      := Low;
         SPI_Conf.Clock_Phase         := P1Edge;
         SPI_Conf.Slave_Management    := Soft;
         SPI_Conf.Baud_Rate_Prescaler := BRP_16;
         SPI_Conf.First_Bit           := MSB;
         SPI_Conf.CRC_Poly            := 7;
         Configure (LCD_SPI, SPI_Conf);
         Enable (LCD_SPI);
      end if;
   end Init_LCD_SPI;

   -----------------
   -- Chip_Select --
   -----------------

   procedure Chip_Select (Enabled : Boolean) is
   begin
      if Enabled then
         Clear (NCS_GPIO, NCS_Pin);
      else
         Set (NCS_GPIO, NCS_Pin);
      end if;
   end Chip_Select;

   -------------------------
   -- LCD_CtrlLinesConfig --
   -------------------------

   procedure LCD_CtrlLinesConfig is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (NCS_GPIO);
      Enable_Clock (WRX_GPIO);

      Conf.Speed       := Speed_50MHz;
      Conf.Mode        := Mode_Out;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;
      Conf.Locked      := True;
      Configure_IO (NCS_GPIO, NCS_Pin, Conf);

      Configure_IO (WRX_GPIO, WRX_Pin, Conf);
      Chip_Select (false);
   end LCD_CtrlLinesConfig;

   ----------------------
   -- LCD_WriteCommand --
   ----------------------

   procedure LCD_WriteCommand (Cmd : Half_Word) is
   begin
      Clear (WRX_GPIO, WRX_Pin);

      Chip_Select (true);

      Send (LCD_SPI, Cmd);
      while not Tx_Is_Empty (LCD_SPI) loop
         null;
      end loop;

      while Busy (LCD_SPI) loop
         null;
      end loop;

      Chip_Select (false);
   end LCD_WriteCommand;

   -------------------
   -- LCD_WriteData --
   -------------------

   procedure LCD_WriteData (Cmd : Half_Word) is
   begin
      Set (WRX_GPIO, WRX_Pin);

      Chip_Select (true);

      Send (LCD_SPI, Cmd);

      while not Tx_Is_Empty (LCD_SPI) loop
         null;
      end loop;

      while Busy (LCD_SPI) loop
         --  if SPI_Get_Flags (LCD_SPI).Frame_Fmt_Error = 1 then
         --     raise Program_Error;
         --  end if;
         null;
      end loop;

      Chip_Select (false);
   end LCD_WriteData;

   --------------------
   -- Relative_Delay --
   --------------------

   procedure Relative_Delay (Ms : Integer) is
      Next_Start : Time := Clock;
      Period     : constant Time_Span := Milliseconds (Ms);
   begin
      Next_Start := Next_Start + Period;
      delay until Next_Start;
   end Relative_Delay;

   -----------------
   -- LCD_PowerOn --
   -----------------

   procedure LCD_PowerOn is
   begin
      LCD_WriteCommand (16#CA#);
      LCD_WriteData (16#C3#);
      LCD_WriteData (16#08#);
      LCD_WriteData (16#50#);
      LCD_WriteCommand (LCD_POWERB);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#C1#);
      LCD_WriteData (16#30#);
      LCD_WriteCommand (LCD_POWER_SEQ);
      LCD_WriteData (16#64#);
      LCD_WriteData (16#03#);
      LCD_WriteData (16#12#);
      LCD_WriteData (16#81#);
      LCD_WriteCommand (LCD_DTCA);
      LCD_WriteData (16#85#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#78#);
      LCD_WriteCommand (LCD_POWERA);
      LCD_WriteData (16#39#);
      LCD_WriteData (16#2C#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#34#);
      LCD_WriteData (16#02#);
      LCD_WriteCommand (LCD_PRC);
      LCD_WriteData (16#20#);
      LCD_WriteCommand (LCD_DTCB);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_FRC);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#1B#);
      LCD_WriteCommand (LCD_DFC);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#A2#);
      LCD_WriteCommand (LCD_POWER1);
      LCD_WriteData (16#10#);
      LCD_WriteCommand (LCD_POWER2);
      LCD_WriteData (16#10#);
      LCD_WriteCommand (LCD_VCOM1);
      LCD_WriteData (16#45#);
      LCD_WriteData (16#15#);
      LCD_WriteCommand (LCD_VCOM2);
      LCD_WriteData (16#90#);
      LCD_WriteCommand (LCD_MAC);
      LCD_WriteData (16#C8#);
      LCD_WriteCommand (LCD_3GAMMA_EN);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_RGB_INTERFACE);
      LCD_WriteData (16#C2#);
      LCD_WriteCommand (LCD_DFC);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#A7#);
      LCD_WriteData (16#27#);
      LCD_WriteData (16#04#);

      --  colomn address set
      LCD_WriteCommand (LCD_COLUMN_ADDR);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#EF#);
      --  Page Address Set
      LCD_WriteCommand (LCD_PAGE_ADDR);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#01#);
      LCD_WriteData (16#3F#);
      LCD_WriteCommand (LCD_INTERFACE);
      LCD_WriteData (16#01#);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#06#);

      LCD_WriteCommand (LCD_GRAM);
      Relative_Delay (200);

      LCD_WriteCommand (LCD_GAMMA);
      LCD_WriteData (16#01#);

      LCD_WriteCommand (LCD_PGAMMA);
      LCD_WriteData (16#0F#);
      LCD_WriteData (16#29#);
      LCD_WriteData (16#24#);
      LCD_WriteData (16#0C#);
      LCD_WriteData (16#0E#);
      LCD_WriteData (16#09#);
      LCD_WriteData (16#4E#);
      LCD_WriteData (16#78#);
      LCD_WriteData (16#3C#);
      LCD_WriteData (16#09#);
      LCD_WriteData (16#13#);
      LCD_WriteData (16#05#);
      LCD_WriteData (16#17#);
      LCD_WriteData (16#11#);
      LCD_WriteData (16#00#);
      LCD_WriteCommand (LCD_NGAMMA);
      LCD_WriteData (16#00#);
      LCD_WriteData (16#16#);
      LCD_WriteData (16#1B#);
      LCD_WriteData (16#04#);
      LCD_WriteData (16#11#);
      LCD_WriteData (16#07#);
      LCD_WriteData (16#31#);
      LCD_WriteData (16#33#);
      LCD_WriteData (16#42#);
      LCD_WriteData (16#05#);
      LCD_WriteData (16#0C#);
      LCD_WriteData (16#0A#);
      LCD_WriteData (16#28#);
      LCD_WriteData (16#2F#);
      LCD_WriteData (16#0F#);

      LCD_WriteCommand (LCD_SLEEP_OUT);
      Relative_Delay (200);
      LCD_WriteCommand (LCD_DISPLAY_ON);
      --  GRAM start writing
      LCD_WriteCommand (LCD_GRAM);
   end LCD_PowerOn;

   -------------------
   -- Reload_Config --
   -------------------

   procedure Reload_Config is
   begin
      LTDC.SRC.IMR := 1;
      loop
         exit when LTDC.SRC.IMR = 0;
      end loop;
   end Reload_Config;

   -------------------
   -- Reload_Config --
   -------------------

   procedure Reload_Config_Vbr is
   begin
      LTDC.SRC.VBR := 1;
      loop
         exit when LTDC.SRC.VBR = 0;
      end loop;
   end Reload_Config_Vbr;

   ---------------
   -- Get_Layer --
   ---------------

   function Get_Layer (Layer : LCD_Layer) return Layer_Access is
   begin
      if Layer = Layer1 then
         return Layer1_Reg'Access;
      else
         return Layer2_Reg'Access;
      end if;
   end Get_Layer;

   ----------------------
   -- Initialize_Layer --
   ----------------------

   procedure Initialize_Layer
     (Layer             : LCD_Layer;
      Pixel_Fmt         : Word;
      Blending_Factor_1 : Bits_3;
      Blending_Factor_2 : Bits_3)
   is
      L : constant Layer_Access := Get_Layer (Layer);
      FB : constant Frame_Buffer_Access := Current_Frame_Buffer (Layer);
   begin
      --  Clear Layer frame buffer
      FB.all := (others => Black);

      --  Windowing configuration.

      declare
         WHPC       : LWHPC_Registers  := L.all.WHPC;
         WVPC       : LWVPC_Registers  := L.all.WVPC;
         PFC        : LPFC_Register    := L.all.PFC;
         CAC        : LCAC_Registers   := L.all.CAC;
         DCC        : LDCC_Registers   := L.all.DCC;
         BFC        : LBFC_Registers   := L.all.BFC;
         CFBL       : LCFBL_Registers  := L.all.CFBL;
         CFBLN      : LCFBLN_Registers := L.all.CFBLN;
      begin

         WHPC.Horizontal_Start := 3;
         WHPC.Horizontal_Stop := (LCD_PIXEL_WIDTH + 3 - 1);

         WVPC.Vertical_Start := 4;
         WVPC.Vertical_Stop := (LCD_PIXEL_HEIGHT + 4 - 1);

         L.all.WHPC := WHPC;
         L.all.WVPC := WVPC;

         PFC := Pixel_Fmt;
         L.all.PFC := PFC;

         CAC.CONSTA := 255;
         L.all.CAC := CAC;

         DCC.DCBlue  := 0;
         DCC.DCGreen := 0;
         DCC.DCRed   := 0;
         DCC.DCALPHA := 0;
         L.all.DCC := DCC;

         BFC.BF1 := Blending_Factor_1;
         BFC.BF2 := Blending_Factor_2;
         L.all.BFC := BFC;

         CFBL.CFBLL := (LCD_PIXEL_WIDTH * 2) + 3;

         CFBL.CFBP := LCD_PIXEL_WIDTH * 2;

         L.all.CFBL := CFBL;

         CFBLN.CFBLNBR := LCD_PIXEL_HEIGHT;
         L.all.CFBLN := CFBLN;

      end;

--        Flip_Buffers;
      Reload_Config;
   end Initialize_Layer;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      LCD_CtrlLinesConfig;

      Chip_Select (true);
      Chip_Select (false);

      Init_LCD_SPI;

      LCD_PowerOn;

      LTDC_Clock_Enable;

      Init_LCD_GPIO;

      STM32F4.SDRAM.Initialize;

      LTDC.GC.VSPOL := Polarity_active_Low;
      LTDC.GC.HSPOL := Polarity_active_Low;
      LTDC.GC.DEPOL := Polarity_active_Low;
      LTDC.GC.PCPOL := Polarity_active_Low;

      LTDC.BCC.BCRed := 16#FF#;

      Set_PLLSAI_Factors (LCD => 4,  SAI1 => 7, VCO => 192, DivR => 2);

      Enable_PLLSAI;

      LTDC.SSC.HSW := 9;
      LTDC.SSC.VSH := 1;
      declare
         SCC : Word with Volatile, Address => System'To_Address (SSCR_Base);
      begin
         SCC := (9 * (2**16)) or (1);
      end;

      LTDC.BPC.AHBP := 29;
      LTDC.BPC.AVBP := 3;
      LTDC.AWC.AAW := 269;
      LTDC.AWC.AAH := 323;
      LTDC.TWC.TOTALW := 279;
      LTDC.TWC.TOTALH := 327;

      Set_Background (16#00#, 16#00#, 16#00#);

      Reload_Config;

      declare
         GC : GC_Registers;
      begin
         --  Enable LTDC
         GC := LTDC.GC;
         GC.LTDCEN := 1;
         LTDC.GC := GC;
      end;

      STM32F4.LCD.Initialize_Layer (Layer1, Default_Pixel_Fmt,
                                    BF1_Pixel_Alpha,
                                    BF2_Pixel_Alpha);

--        STM32F4.LCD.Initialize_Layer (Layer2, Default_Pixel_Fmt,
--                                      BF1_Pixel_Alpha,
--                                      BF2_Pixel_Alpha);

--        STM32F4.LCD.Set_Layer_State (Layer2, Disabled);

      Reload_Config;
      Set_Layer_State (Layer1, Enabled);

      declare
         GC : GC_Registers;
      begin
         GC := LTDC.GC;
         --  enable Dither
         GC.DEN := 1;
         LTDC.GC := GC;
      end;
   end Initialize;

   --------------------------
   -- Current_Frame_Buffer --
   --------------------------

   function Current_Frame_Buffer
     (Layer : LCD_Layer)
      return Frame_Buffer_Access
   is
   begin
      return Frame_Buffer_Array (Layer, Current_Frame_Buffer_Index)'Access;
   end Current_Frame_Buffer;

   --------------------
   -- Set_Layer_CFBA --
   --------------------

   procedure Set_Layer_FB_Address
     (Layer : LCD_Layer; FBA : Frame_Buffer_Access)
   is
      L : constant Layer_Access := Get_Layer (Layer);
   begin
         L.all.CFBA := As_Word (FBA.all'Address);
   end Set_Layer_FB_Address;

   ---------------------
   -- Set_Layer_State --
   ---------------------

   procedure Set_Layer_State (Layer : LCD_Layer; State : Layer_State) is
      L : constant Layer_Access := Get_Layer (Layer);
      Ctrl : LC_Registers;
   begin
      Ctrl := L.all.Ctrl;
      Ctrl.LEN := (if State = Enabled then 1 else 0);
      L.all.Ctrl := Ctrl;
      Reload_Config_Vbr;
   end Set_Layer_State;

   --------------------
   -- Set_Background --
   --------------------

   procedure Set_Background (R, G, B : Byte) is
   begin
      LTDC.BCC.BCRed := R;
      LTDC.BCC.BCBlue := G;
      LTDC.BCC.BCGreen := B;
   end Set_Background;

   ---------------
   -- Set_Pixel --
   ---------------

   procedure Set_Pixel
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height;
      Value : Pixel)
   is
      FB : constant Frame_Buffer_Access := Current_Frame_Buffer (Layer);
      Pix_Index : constant Frame_Buffer_Range :=
        Frame_Buffer_Range (X + Y * LCD_PIXEL_WIDTH);
   begin
      FB (Pix_Index) := Value;
   end Set_Pixel;

   -----------------
   -- Pixel_Value --
   -----------------

   function Pixel_Value
     (Layer : LCD_Layer;
      X     : Width;
      Y     : Height)
      return Pixel
   is
      FB : constant Frame_Buffer_Access := Current_Frame_Buffer (Layer);
      Pix_Index : constant Frame_Buffer_Range :=
        Frame_Buffer_Range (X + Y * LCD_PIXEL_WIDTH);
   begin
      return FB (Pix_Index);
   end Pixel_Value;

   ------------------
   -- Flip_Buffers --
   ------------------

   procedure Flip_Buffers is
   begin
      Set_Layer_FB_Address
        (Layer1, Frame_Buffer_Array
           (Layer1, Current_Frame_Buffer_Index)'Access);

      Current_Frame_Buffer_Index := Current_Frame_Buffer_Index + 1;

      Reload_Config_Vbr;

--        Current_Frame_Buffer_Index := Current_Frame_Buffer_Index + 1;
--        Current_Frame_Buffer (Layer1).all := (others => Black);
--        Current_Frame_Buffer_Index := Current_Frame_Buffer_Index + 1;
   end Flip_Buffers;

   procedure Flip_Copy_Buffers is
   begin
      Flip_Buffers;
      Frame_Buffer_Array(Layer1, Current_Frame_Buffer_Index) := Frame_Buffer_Array(Layer1, Current_Frame_Buffer_Index + 1);
   end Flip_Copy_Buffers;



end STM32F4.LCD;

