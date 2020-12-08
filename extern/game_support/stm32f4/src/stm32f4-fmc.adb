package body STM32F4.FMC is

   procedure FMC_SDRAM_Init (SDRAM_Conf : FMC_SDRAM_Init_Config) is
      Tmpr1, Tmpr2, Tmpr3, Tmpr4 : Word := 0;
   begin
      --  SDRAM bank control register configuration
      Tmpr1 := SDRAM_Conf.ColumnBitsNumber or
        SDRAM_Conf.RowBitsNumber or
        SDRAM_Conf.SDMemoryDataWidth or
        SDRAM_Conf.InternalBankNumber or
        SDRAM_Conf.CASLatency or
        SDRAM_Conf.WriteProtection or
        SDRAM_Conf.SDClockPeriod or
        SDRAM_Conf.ReadBurst or
        SDRAM_Conf.ReadPipeDelay;

      if SDRAM_Conf.Bank = FMC_Bank1_SDRAM then
         Bank5_6.SDCR (SDRAM_Conf.Bank) := tmpr1;
      else   --  SDCR2 "don't care" bits configuration
         Tmpr3 := SDRAM_Conf.SDClockPeriod or
           SDRAM_Conf.ReadBurst or
           SDRAM_Conf.ReadPipeDelay;

         Bank5_6.SDCR (FMC_Bank1_SDRAM) := tmpr3;
         Bank5_6.SDCR (SDRAM_Conf.Bank) := tmpr1;
      end if;

      --  SDRAM bank timing register configuration
      if SDRAM_Conf.Bank = FMC_Bank1_SDRAM then
         Tmpr2 := ((SDRAM_Conf.Timing_Conf.LoadToActiveDelay) - 1) or
           (((SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay) - 1) * (2**4)) or
           (((SDRAM_Conf.Timing_Conf.SelfRefreshTime) - 1) * (2**8)) or
           (((SDRAM_Conf.Timing_Conf.RowCycleDelay) - 1) * (2**12)) or
           (((SDRAM_Conf.Timing_Conf.WriteRecoveryTime) - 1) * (2**16)) or
           (((SDRAM_Conf.Timing_Conf.RPDelay) - 1) * (2**20)) or
           (((SDRAM_Conf.Timing_Conf.RCDDelay) - 1) * (2**24));

         Bank5_6.SDTR (SDRAM_Conf.Bank) := tmpr2;
      else   --  SDTR "don't care bits configuration
         Tmpr2 := ((SDRAM_Conf.Timing_Conf.LoadToActiveDelay) - 1) or
                ((SDRAM_Conf.Timing_Conf.ExitSelfRefreshDelay - 1) * (2**4)) or
                (((SDRAM_Conf.Timing_Conf.SelfRefreshTime) - 1)  * (2**8)) or
                (((SDRAM_Conf.Timing_Conf.WriteRecoveryTime) - 1) * (2**16));

         Tmpr4 := (((SDRAM_Conf.Timing_Conf.RowCycleDelay)-1) * (2**12)) or
           (((SDRAM_Conf.Timing_Conf.RPDelay)-1) * (2**20));

         Bank5_6.SDTR (FMC_Bank1_SDRAM) := Tmpr4;
         Bank5_6.SDTR (SDRAM_Conf.Bank) := Tmpr2;
      end if;
   end FMC_SDRAM_Init;

   procedure FMC_SDRAM_Cmd (Cmd : FMC_SDRAM_Cmd_Conf) is
   begin
      Bank5_6.SDCMR := Cmd.CommandMode
        or Cmd.CommandTarget
        or ((Cmd.AutoRefreshNumber - 1) * (2**5))
        or (Cmd.ModeRegisterDefinition * (2**9));
   end FMC_SDRAM_Cmd;

   function FMC_Get_Flag (Bank : Word; Flag : Word) return Boolean is
      Reg : Word;
   begin
      case Bank is
         when FMC_Bank2_NAND =>
            Reg := Bank2.SR;
         when FMC_Bank3_NAND =>
            Reg := Bank3.SR;
         when FMC_Bank4_PCCARD =>
            Reg := Bank4.SR;
         when others =>
            Reg := Bank5_6.SDSR;
      end case;

      return (Reg and Flag) /= 0;
   end FMC_Get_Flag;

   procedure FMC_Set_Refresh_Count (Cnt : Word) is
   begin
      Bank5_6.SDRTR := Bank5_6.SDRTR or Cnt * 2;
   end FMC_Set_Refresh_Count;

end STM32F4.FMC;
