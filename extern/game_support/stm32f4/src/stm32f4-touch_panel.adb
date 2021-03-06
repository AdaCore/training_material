with Ada.Real_Time; use Ada.Real_Time;
with STM32F4.RCC;

package body STM32F4.Touch_Panel is

   function Read_Data (Data_Addr : Byte) return Half_Word is
      Data : Half_Word;
   begin
      Generate_Start (TP_I2C, Enabled);
      Wait_For_State (TP_I2C, Start_Bit, Enabled);

      Send_7Bit_Address (TP_I2C, IOE_ADDR, Transmitter);
      Wait_For_State (TP_I2C, Address_Sent, Enabled);
      Clear_Address_Sent_Status (TP_I2C);

      Wait_For_State (TP_I2C, Tx_Data_Register_Empty, Enabled);

      Send_Data (TP_I2C, Data_Addr);

      Generate_Start (TP_I2C, Enabled);
      Wait_For_State (TP_I2C, Start_Bit, Enabled);

      Send_7Bit_Address (TP_I2C, IOE_ADDR, Receiver);
      Wait_For_State (TP_I2C, Address_Sent, Enabled);

      Set_Ack_Config (TP_I2C, Disabled);
      Set_Nack_Config (TP_I2C, Next);

      Clear_Address_Sent_Status (TP_I2C);

      Wait_For_State (TP_I2C, Byte_Transfer_Finished, Enabled);

      Generate_Stop (TP_I2C, Enabled);

      Data := Half_Word (Read_Data (TP_I2C)) * (2**8);
      Data := Data or Half_Word (Read_Data (TP_I2C));

      Set_Ack_Config (TP_I2C, Enabled);
      Set_Nack_Config (TP_I2C, Current);

      return Data;
   end Read_Data;

   function Read_Register (Reg_Addr : Byte) return Byte is
      Data : Byte;
   begin
      Generate_Start (TP_I2C, Enabled);
      Wait_For_State (TP_I2C, Start_Bit, Enabled);

      Set_Ack_Config (TP_I2C, Disabled);

      Send_7Bit_Address (TP_I2C, IOE_ADDR, Transmitter);
      Wait_For_State (TP_I2C, Address_Sent, Enabled);
      Clear_Address_Sent_Status (TP_I2C);

      Wait_For_State (TP_I2C, Tx_Data_Register_Empty, Enabled);

      Send_Data (TP_I2C, Reg_Addr);

      while
         not Status (TP_I2C, Tx_Data_Register_Empty) or else
         not Status (TP_I2C, Byte_Transfer_Finished)
      loop
         null;
      end loop;

      Generate_Start (TP_I2C, Enabled);
      Wait_For_State (TP_I2C, Start_Bit, Enabled);

      Send_7Bit_Address (TP_I2C, IOE_ADDR, Receiver);
      Wait_For_State (TP_I2C, Address_Sent, Enabled);
      Clear_Address_Sent_Status (TP_I2C);

      Wait_For_State (TP_I2C, Rx_Data_Register_Not_Empty, Enabled);

      Generate_Stop (TP_I2C, Enabled);

      Data := Read_Data (TP_I2C);

      Set_Ack_Config (TP_I2C, Enabled);

      return Data;
   end Read_Register;

   procedure Write_Register (Reg_Addr : Byte; Data : Byte) is
   begin
      Generate_Start (TP_I2C, Enabled);
      Wait_For_State (TP_I2C, Start_Bit, Enabled);

      Set_Ack_Config (TP_I2C, Disabled);

      Send_7Bit_Address (TP_I2C, IOE_ADDR, Transmitter);
      Wait_For_State (TP_I2C, Address_Sent, Enabled);
      Clear_Address_Sent_Status (TP_I2C);

      Wait_For_State (TP_I2C, Tx_Data_Register_Empty, Enabled);

      Send_Data (TP_I2C, Reg_Addr);

      Wait_For_State (TP_I2C, Tx_Data_Register_Empty, Enabled);

      Send_Data (TP_I2C, Data);

      while
        not Status (TP_I2C, Tx_Data_Register_Empty) or else
        not Status (TP_I2C, Byte_Transfer_Finished)
      loop
         null;
      end loop;

      Generate_Stop (TP_I2C, Enabled);
   end Write_Register;

   procedure TP_Ctrl_Lines is
      GPIO_Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (SDA_GPIO);
      Enable_Clock (SCL_GPIO);

      Enable_Clock (TP_I2C);

      Reset (TP_I2C);

      Enable_Clock (TP_I2C);

      Configure_Alternate_Function (SCL_GPIO, SCL_Pin, SCL_AF);
      Configure_Alternate_Function (SDA_GPIO, SDA_Pin, SDA_AF);

      GPIO_Conf.Speed       := Speed_25MHz;
      GPIO_Conf.Mode        := Mode_AF;
      GPIO_Conf.Output_Type := Open_Drain;
      GPIO_Conf.Resistors   := Floating;
      GPIO_Conf.Locked      := True;
      Configure_IO (SCL_GPIO, SCL_Pin, GPIO_Conf);
      Configure_IO (SDA_GPIO, SDA_Pin, GPIO_Conf);
   end TP_Ctrl_Lines;

   procedure TP_I2C_Config is
   begin
      STM32F4.RCC.I2C3_Force_Reset;
      STM32F4.RCC.I2C3_Release_Reset;

      Configure
        (TP_I2C,
         Mode        => I2C_Mode,
         Duty_Cycle  => DutyCycle_2,
         Own_Address => 16#00#,
         Ack         => Ack_Enable,
         Ack_Address => AcknowledgedAddress_7bit,
         Clock_Speed => 1_000);

      Set_State (TP_I2C, Enabled);
   end TP_I2C_Config;

   procedure IOE_Reset is
   begin
      Write_Register (IOE_REG_SYS_CTRL1, 16#02#);

      --  Give some time for the reset
      delay until Clock + Milliseconds (2);

      Write_Register (IOE_REG_SYS_CTRL1, 16#00#);
   end IOE_Reset;

   procedure IOE_Function_Command (Func : Byte; Enabled : Boolean) is
      Reg : Byte := Read_Register (IOE_REG_SYS_CTRL2);
   begin
      --  CTRL2 functions are disabled when corresponding bit is set

      if Enabled then
         Reg := Reg and (not Func);
      else
         Reg := Reg or Func;
      end if;

      Write_Register (IOE_REG_SYS_CTRL2, Reg);
   end IOE_Function_Command;

   procedure IOE_AF_Config (Pin : Byte; Enabled : Boolean) is
      Reg : Byte := Read_Register (IOE_REG_GPIO_AF);
   begin
      if Enabled then
         Reg := Reg or Pin;
      else
         Reg := Reg and (not Pin);
      end if;

      Write_Register (IOE_REG_GPIO_AF, Reg);
   end IOE_AF_Config;

   function Get_IOE_ID return Half_Word is
   begin
      return (Half_Word (Read_Register (0)) * (2**8))
        or Half_Word (Read_Register (1));
   end Get_IOE_ID;

   procedure Initialize is
   begin
      TP_Ctrl_Lines;
      TP_I2C_Config;

      delay until Clock + Milliseconds (100);

      if Get_IOE_ID /= 16#0811# then
         raise Program_Error;
      end if;

      IOE_Reset;

      IOE_Function_Command (IOE_ADC_FCT, true);
      IOE_Function_Command (IOE_TP_FCT, true);

      Write_Register (IOE_REG_ADC_CTRL1, 16#49#);

      delay until Clock + Milliseconds (2);

      Write_Register (IOE_REG_ADC_CTRL2, 16#01#);

      IOE_AF_Config (TOUCH_IO_ALL, false);

      Write_Register (IOE_REG_TP_CFG, 16#9A#);

      Write_Register (IOE_REG_FIFO_TH, 16#01#);

      Write_Register (IOE_REG_FIFO_STA, 16#01#);

      Write_Register (IOE_REG_FIFO_TH, 16#00#);

      Write_Register (IOE_REG_TP_FRACT_XYZ, 16#00#);

      Write_Register (IOE_REG_TP_I_DRIVE, 16#01#);

      Write_Register (IOE_REG_TP_CTRL, 16#01#);

      Write_Register (IOE_REG_INT_STA, 16#FF#);
   end Initialize;

   function Read_X return LCD.Width is
      X, XR : Integer;
      Raw_X : Half_Word;
   begin
      Raw_X := Read_Data (IOE_REG_TP_DATA_X);
      X := Integer (Raw_X);

      if X <= 3000 then
         X := 3900 - X;
      else
         X := 3800 - X;
      end if;

      XR := X / 15;

      if XR < LCD.Width'First then
         XR := LCD.Width'First;
      elsif XR > LCD.Width'Last then
         XR := LCD.Width'Last;
      end if;
      return LCD.Width (XR);
   end Read_X;

   function Read_Y return LCD.Height is
      Y, YR : Integer;
      Raw_Y : Half_Word;
   begin
      Raw_Y := Read_Data (IOE_REG_TP_DATA_Y);
      Y := Integer (Raw_Y);

      Y := Y - 360;

      YR := Y / 11;

      if YR < LCD.Height'First then
         YR := LCD.Height'First;
      elsif YR > LCD.Height'Last then
         YR := LCD.Height'Last;
      end if;
      return LCD.Height (YR);
   end Read_Y;

   function Read_Z return Half_Word is
   begin
      return Read_Data (IOE_REG_TP_DATA_Z);
   end Read_Z;

   function Current_State return TP_State is
      State : TP_State;
      Ctrl : Byte;
      X : LCD.Width  := 0;
      Y : LCD.Height := 0;
      Z : Half_Word  := 0;
   begin
      --  Check Touch detected bit in CTRL register
      Ctrl := Read_Register (IOE_REG_TP_CTRL);
      State.Touch_Detected := (Ctrl and 16#80#) /= 0;

      if State.Touch_Detected then
         X := Read_X;
         Y := Read_Y;
         Z := Read_Z;
      end if;

      State.X := X;
      State.Y := Y;
      State.Z := Z;

      Write_Register (IOE_REG_FIFO_STA, 16#01#);
      Write_Register (IOE_REG_FIFO_STA, 16#00#);
      return State;
   end Current_State;

end STM32F4.Touch_Panel;
