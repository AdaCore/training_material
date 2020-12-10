with STM32F4.SPI;  use STM32F4.SPI;
with STM32F4.GPIO; use STM32F4.GPIO;
with Ada.Unchecked_Conversion;
with Fonts; use Fonts;

package body STM32F4.GYRO is

   function As_Half_Word is new Ada.Unchecked_Conversion
     (Source => Byte, Target => Half_Word);

   L3GD20_SPI : SPI_Port renames SPI_5;

   SCK_GPIO : GPIO_Port renames GPIO_F;
   SCK_Pin  : GPIO_Pins renames Pin_7;
   SCK_AF   : GPIO_AF   renames GPIO_AF_SPI5;

   MISO_GPIO : GPIO_Port renames GPIO_F;
   MISO_Pin  : GPIO_Pins renames Pin_8;
   MISO_AF   : GPIO_AF   renames GPIO_AF_SPI5;

   MOSI_GPIO : GPIO_Port renames GPIO_F;
   MOSI_Pin  : GPIO_Pins renames Pin_9;
   MOSI_AF   : GPIO_AF   renames GPIO_AF_SPI5;

   CS_GPIO : GPIO_Port renames GPIO_C;
   CS_Pin  : GPIO_Pins renames Pin_1;

   Int1_GPIO : GPIO_Port renames GPIO_A;
   Int1_Pin  : GPIO_Pins renames Pin_1;
   Int2_GPIO : GPIO_Port renames GPIO_A;
   Int2_Pin  : GPIO_Pins renames Pin_2;

   Sensitivity_250dps  : constant := 114.285;
   Sensitivity_500dps  : constant := 57.1429;
   Sensitivity_2000dps : constant := 14.285;

   --  L3GD20 Registers

   WHO_AM_I_REG      : constant := 16#0F#; --  device identification register
   CTRL_REG1_REG     : constant := 16#20#; --  Control register 1
   CTRL_REG2_REG     : constant := 16#21#; --  Control register 2
   CTRL_REG3_REG     : constant := 16#22#; --  Control register 3
   CTRL_REG4_REG     : constant := 16#23#; --  Control register 4
   CTRL_REG5_REG     : constant := 16#24#; --  Control register 5
   REFERENCE_REG_REG : constant := 16#25#; --  Reference register
   OUT_TEMP_REG      : constant := 16#26#; --  Out temp register
   STATUS_REG        : constant := 16#27#; --  Status register
   OUT_X_L_REG       : constant := 16#28#; --  Output Register X
   OUT_X_H_REG       : constant := 16#29#; --  Output Register X
   OUT_Y_L_REG       : constant := 16#2A#; --  Output Register Y
   OUT_Y_H_REG       : constant := 16#2B#; --  Output Register Y
   OUT_Z_L_REG       : constant := 16#2C#; --  Output Register Z
   OUT_Z_H_REG       : constant := 16#2D#; --  Output Register Z
   FIFO_CTRL_REG_REG : constant := 16#2E#; --  Fifo control Register
   FIFO_SRC_REG_REG  : constant := 16#2F#; --  Fifo src Register
   INT1_CFG_REG      : constant := 16#30#; --  Interrupt 1 configuration Register
   INT1_SRC_REG      : constant := 16#31#; --  Interrupt 1 source Register
   INT1_TSH_XH_REG   : constant := 16#32#; --  Interrupt 1 Threshold X register
   INT1_TSH_XL_REG   : constant := 16#33#; --  Interrupt 1 Threshold X register
   INT1_TSH_YH_REG   : constant := 16#34#; --  Interrupt 1 Threshold Y register
   INT1_TSH_YL_REG   : constant := 16#35#; --  Interrupt 1 Threshold Y register
   INT1_TSH_ZH_REG   : constant := 16#36#; --  Interrupt 1 Threshold Z register
   INT1_TSH_ZL_REG   : constant := 16#37#; --  Interrupt 1 Threshold Z register
   INT1_DURATION_REG : constant := 16#38#; --  Interrupt 1 DURATION register

   --  Read/Write command
   READWRITE_CMD    : constant := 16#80#;
   --  Multiple byte read/write command
   MULTIPLEBYTE_CMD : constant := 16#40#;
   NO_DATA          : constant := 16#00#;

   procedure Chip_Select (Enabled : Boolean) is
   begin
      if Enabled then
         --  GPIO Reset = select
         GPIO_Reset (CS_GPIO, CS_Pin);
      else
         --  GPIO Set = deselect
         GPIO_Set (CS_GPIO, CS_Pin);
      end if;
   end Chip_Select;

   type Buffer is array (Natural range <>) of Byte;

   function Send_Byte (Data : Byte) return Byte is
   begin
      loop
         exit when SPI_Tx_Is_Empty (L3GD20_SPI);
      end loop;

      SPI_Send_Data (L3GD20_SPI, Half_Word (Data));

      loop
         exit when not SPI_Rx_Is_Empty (L3GD20_SPI);
      end loop;

      return SPI_Read_Data (L3GD20_SPI);
   end Send_Byte;

   procedure Send_Byte (Data : Byte) is
      Ret : Byte with Unreferenced;
   begin
      Ret := Send_Byte (Data);
   end Send_Byte;

   procedure Write (Addr : Byte; Buff : Buffer) is
      Full_Addr : Byte := Addr;
   begin
      if Buff'Length > 1 then
         Full_Addr := Addr or MULTIPLEBYTE_CMD;
      end if;

      Chip_Select (true);

      --  Send Register address
      Send_Byte (Full_Addr);

      for Data of Buff loop
         Send_Byte (Data);
      end loop;

      Chip_Select (false);
   end Write;

   procedure Read (Addr : Byte; Buff : out Buffer) is
      Full_Addr : Byte := Addr or READWRITE_CMD;
   begin
      if Buff'Length > 1 then
         Full_Addr := Addr or MULTIPLEBYTE_CMD;
      end if;

      Chip_Select (true);

      --  Send Register address
      Send_Byte (Full_Addr);

      for Data of Buff loop
         Data := Send_Byte (NO_DATA);
      end loop;

      Chip_Select (false);
   end Read;

   procedure L3GD20_Ctrl_Lines is
      SPI_Conf  : SPI_Config;
      GPIO_Conf : GPIO_Config;
   begin
      GPIO_Enable (SCK_GPIO);
      GPIO_Enable (MISO_GPIO);
      GPIO_Enable (MOSI_GPIO);
      GPIO_Enable (CS_GPIO);
      GPIO_Enable (Int1_GPIO);
      GPIO_Enable (Int2_GPIO);

      SPI_CLK_Enable (L3GD20_SPI);

      Configure_Alternate_Function (SCK_GPIO,  SCK_Pin,  SCK_AF);
      Configure_Alternate_Function (MISO_GPIO, MISO_Pin, MISO_AF);
      Configure_Alternate_Function (MOSI_GPIO, MOSI_Pin, MOSI_AF);

      GPIO_Conf.Speed := Speed_25MHz;
      GPIO_Conf.Mode   := Mode_AF;
      GPIO_Conf.Output_Type  := Type_PP;
      GPIO_Conf.Resistors   := Pull_Down;
      GPIO_Conf.Lock := True;
      Configure (SCK_GPIO,  SCK_Pin,  GPIO_Conf);
      Configure (MISO_GPIO, MISO_Pin, GPIO_Conf);
      Configure (MOSI_GPIO, MOSI_Pin, GPIO_Conf);

      SPI_Set_Enable (L3GD20_SPI, False);
      SPI_Conf.Direction           := D2Lines_FullDuplex;
      SPI_Conf.Mode                := Master;
      SPI_Conf.Data_Size           := Data_8;
      SPI_Conf.Clock_Polarity      := Low;
      SPI_Conf.Clock_Phase         := P1Edge;
      SPI_Conf.Slave_Management    := Soft;
      SPI_Conf.Baud_Rate_Prescaler := BRP_16;
      SPI_Conf.First_Bit           := MSB;
      SPI_Conf.CRC_Poly            := 7;
      SPI_Set_Config (L3GD20_SPI, SPI_Conf);

      SPI_Set_Enable (L3GD20_SPI, True);

      GPIO_Conf.Speed := Speed_25MHz;
      GPIO_Conf.Mode   := Mode_OUT;
      GPIO_Conf.Output_Type  := Type_PP;
      GPIO_Conf.Resistors   := Floating;
      GPIO_Conf.Lock := True;
      Configure (CS_GPIO, CS_Pin, GPIO_Conf);

      Chip_Select (false);

      GPIO_Conf.Speed := Speed_25MHz;
      GPIO_Conf.Mode   := Mode_IN;
      GPIO_Conf.Output_Type  := Type_PP;
      GPIO_Conf.Resistors   := Floating;
      GPIO_Conf.Lock := True;
      Configure (Int1_GPIO, Int1_Pin, GPIO_Conf);
      Configure (Int2_GPIO, Int2_Pin, GPIO_Conf);
   end L3GD20_Ctrl_Lines;

   procedure Initialize (Conf : Gyro_Config) is
      Ctrl1 : Buffer (1 .. 1);
      Ctrl4 : Buffer (1 .. 1);
   begin
      L3GD20_Ctrl_Lines;

      Read (WHO_AM_I_REG, Ctrl1);

      Draw_String (0, 110, "ID :" & Ctrl1 (1)'Img & "    ");

      Ctrl1 (1) := Conf.Power_Mode or Conf.Output_DataRate
        or Conf.Axes_Enable or Conf.Band_Width;

      Ctrl4 (1) := Conf.BlockData_Update or Conf.Endianness or Conf.Full_Scale;

      Write (CTRL_REG1_REG, Ctrl1);
      Write (CTRL_REG4_REG, Ctrl4);
   end Initialize;

   procedure Set_Filter_Config (Conf : Filter_Config) is
      Ctrl2 : Buffer (1 .. 1);
   begin
      Read (CTRL_REG2_REG, Ctrl2);

      Ctrl2 (1) := Ctrl2 (1) and 16#C0#;

      Ctrl2 (1) := Ctrl2 (1) or Conf.Mode_Selection or Conf.CutOff_Frequency;

      Write (CTRL_REG2_REG, Ctrl2);
   end Set_Filter_Config;

   procedure Set_Filter_State (State : Filter_State) is
      Ctrl5 : Buffer (1 .. 1);
   begin
      Read (CTRL_REG5_REG, Ctrl5);

      if State = Enabled then
         --  Set HPen
         Ctrl5 (1) := Ctrl5 (1) or 16#01#;
      else
         --  Clear HPen
         Ctrl5 (1) := Ctrl5 (1) and 16#EF#;
      end if;

      Write (CTRL_REG5_REG, Ctrl5);
   end Set_Filter_State;

   function Get_Data_Status return Byte is
      Reg : Buffer (1 .. 1);
   begin
      Read (STATUS_REG, Reg);
      return Reg (1);
   end Get_Data_Status;

   procedure Reboot is
      Ctrl5 : Buffer (1 .. 1);
   begin
      Read (CTRL_REG5_REG, Ctrl5);

      --  Enable the reboot memory
      Ctrl5 (1) := Ctrl5 (1) or 16#80#;

      Write (CTRL_REG5_REG, Ctrl5);
   end Reboot;

   function Read_Angle_Rate return Angles_Rates is
      Ctrl4       : Buffer (1 .. 1);
      Status      : Byte;
      Reg_Data    : Buffer (0 .. 5);
      Raw_Data    : array (0 .. 2) of Half_Word;
      Sensitivity : Float := 0.0;
      Ret         : Angles_Rates;
   begin

      loop
         Status := Get_Data_Status;

         --  Wait for new Data
         exit when (Status and 16#8#) /= 0;

         --  Data overrun
         if (Status and 16#80#) /= 0 then
            raise Program_Error;
         end if;
      end loop;

      Read (CTRL_REG4_REG, Ctrl4);
      Read (OUT_X_L_REG, Reg_Data);

      --  Check data alignment (Big Endian or Little Endian)
      for Index in Raw_Data'Range loop
         if (Ctrl4 (1) and 16#40#) /= 0 then
            Raw_Data (Index) :=
              As_Half_Word (Reg_Data (2 * Index + 1) * (2**8))
              + As_Half_Word (Reg_Data (2 * Index));
         else
            Raw_Data (Index) := As_Half_Word (Reg_Data (2 * Index) * (2**8))
              + As_Half_Word (Reg_Data (2 * Index + 1));
         end if;
      end loop;

      if (Ctrl4 (1) and 16#30#) = 16#00# then
         Sensitivity := Sensitivity_250dps;
      elsif (Ctrl4 (1) and 16#30#) = 16#10# then
         Sensitivity := Sensitivity_500dps;
      elsif (Ctrl4 (1) and 16#30#) = 16#20# then
         Sensitivity := Sensitivity_2000dps;
      end if;

      Draw_String (0, 80, "Raw_Data (0):" & Raw_Data (0)'Img & "   ");
      Draw_String (0, 88, "Raw_Data (1):" & Raw_Data (1)'Img & "   ");
      Draw_String (0, 96, "Raw_Data (2):" & Raw_Data (2)'Img & "   ");

      if Raw_Data (1) = 16#FF#
        or else Raw_Data (1) = 16#FF#
        or else  Raw_Data (1) = 16#FF#
      then
         Ret := (others => 0.0);
      else
         Ret (X) := Float (Raw_Data (0)) / Sensitivity;
         Ret (Y) := Float (Raw_Data (1)) / Sensitivity;
         Ret (Z) := Float (Raw_Data (2)) / Sensitivity;
      end if;

      return Ret;
   end Read_Angle_Rate;

end STM32F4.GYRO;
