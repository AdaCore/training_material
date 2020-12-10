with Ada.Real_Time; use Ada.Real_Time;

with STM32F4;      use STM32F4;
with STM32F4.GPIO; use STM32F4.GPIO;
with STM32F4.RCC;  use STM32F4.RCC;

with STM32F429_Discovery;  use STM32F429_Discovery;

package body STM32F4.SDRAM is

   --------------------
   -- Configure_GPIO --
   --------------------

   procedure Configure_GPIO is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_B);
      Enable_Clock (GPIO_C);
      Enable_Clock (GPIO_D);
      Enable_Clock (GPIO_E);
      Enable_Clock (GPIO_F);
      Enable_Clock (GPIO_G);

      Conf.Speed       := Speed_50MHz;
      Conf.Mode        := Mode_AF;
      Conf.Output_Type := Push_Pull;
      Conf.Resistors   := Floating;
      Conf.Locked      := True;

      Configure_Alternate_Function (GPIO_B, (Pin_5, Pin_6), GPIO_AF_FMC);

      Configure_IO (GPIO_B, (Pin_5, Pin_6), Conf);

      Configure_Alternate_Function (GPIO_C, Pin_0,  GPIO_AF_FMC);

      Configure_IO (GPIO_C, Pin_0, Conf);

      Configure_Alternate_Function (GPIO_D,
                                    (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10,
                                     Pin_14, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_D, (Pin_0, Pin_1, Pin_8, Pin_9, Pin_10, Pin_14,
                             Pin_15), Conf);

      Configure_Alternate_Function (GPIO_E, (Pin_0, Pin_1, Pin_7, Pin_8, Pin_9,
                                             Pin_10, Pin_11, Pin_12, Pin_13,
                                             Pin_14, Pin_15), GPIO_AF_FMC);

      Configure_IO (GPIO_E, (Pin_0, Pin_1, Pin_7, Pin_8, Pin_9, Pin_10, Pin_11,
                             Pin_12, Pin_13, Pin_14, Pin_15), Conf);

      Configure_Alternate_Function (GPIO_F,
                                    (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5,
                                     Pin_11, Pin_12, Pin_13, Pin_14, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_F, (Pin_0, Pin_1, Pin_2, Pin_3, Pin_4, Pin_5,
                             Pin_11, Pin_12, Pin_13, Pin_14, Pin_15), Conf);

      Configure_Alternate_Function (GPIO_G,
                                    (Pin_0, Pin_1, Pin_4, Pin_5,
                                     Pin_8, Pin_15),
                                    GPIO_AF_FMC);

      Configure_IO (GPIO_G, (Pin_0, Pin_1, Pin_4, Pin_5, Pin_8, Pin_15), Conf);
   end Configure_GPIO;

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

   ------------------------
   -- SDRAM_InitSequence --
   ------------------------

   procedure SDRAM_InitSequence is
      Cmd : FMC_SDRAM_Cmd_Conf;
   begin
      Cmd.CommandMode            := FMC_Command_Mode_CLK_Enabled;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Relative_Delay (100);

      Cmd.CommandMode            := FMC_Command_Mode_PALL;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Cmd.CommandMode            := FMC_Command_Mode_AutoRefresh;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 4;
      Cmd.ModeRegisterDefinition := 0;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      Cmd.CommandMode            := FMC_Command_Mode_LoadMode;
      Cmd.CommandTarget          := FMC_Command_Target_bank2;
      Cmd.AutoRefreshNumber      := 1;
      Cmd.ModeRegisterDefinition := SDRAM_MODEREG_BURST_LENGTH_2 or
        SDRAM_MODEREG_BURST_TYPE_SEQUENTIAL or
        SDRAM_MODEREG_CAS_LATENCY_3 or
        SDRAM_MODEREG_OPERATING_MODE_STANDARD or
        SDRAM_MODEREG_WRITEBURST_MODE_SINGLE;

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;

      FMC_SDRAM_Cmd (Cmd);

      FMC_Set_Refresh_Count (1386);

      loop
         exit when not FMC_Get_Flag (FMC_Bank2_SDRAM, FMC_FLAG_Busy);
      end loop;
   end SDRAM_InitSequence;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Timing_Conf : FMC_SDRAM_TimingInit_Config;
      SDRAM_Conf  : FMC_SDRAM_Init_Config;
   begin
      Configure_GPIO;

      FSMC_Clock_Enable;

      Timing_Conf.LoadToActiveDelay    := 2;
      Timing_Conf.ExitSelfRefreshDelay := 7;
      Timing_Conf.SelfRefreshTime      := 4;
      Timing_Conf.RowCycleDelay        := 7;
      Timing_Conf.WriteRecoveryTime    := 2;
      Timing_Conf.RPDelay              := 2;
      Timing_Conf.RCDDelay             := 2;

      SDRAM_Conf.Bank               := FMC_Bank2_SDRAM;
      SDRAM_Conf.ColumnBitsNumber   := FMC_ColumnBits_Number_8b;
      SDRAM_Conf.RowBitsNumber      := FMC_RowBits_Number_12b;
      SDRAM_Conf.SDMemoryDataWidth  := SDRAM_MEMORY_WIDTH;
      SDRAM_Conf.InternalBankNumber := FMC_InternalBank_Number_4;
      SDRAM_Conf.CASLatency         := SDRAM_CAS_LATENCY;
      SDRAM_Conf.WriteProtection    := FMC_Write_Protection_Disable;
      SDRAM_Conf.SDClockPeriod      := SDCLOCK_PERIOD;
      SDRAM_Conf.ReadBurst          := SDRAM_READBURST;
      SDRAM_Conf.ReadPipeDelay      := FMC_ReadPipe_Delay_1;
      SDRAM_Conf.Timing_Conf        := Timing_Conf;

      FMC_SDRAM_Init (SDRAM_Conf);
      SDRAM_InitSequence;
   end Initialize;

end STM32F4.SDRAM;
