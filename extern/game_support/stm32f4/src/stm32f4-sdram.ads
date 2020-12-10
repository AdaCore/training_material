with STM32F4.FMC; use STM32F4.FMC;

package STM32F4.SDRAM is

   Bank_Address : constant := 16#D000_0000#;

   procedure Initialize;

private

   SDRAM_MEMORY_WIDTH : constant := FMC_SDMemory_Width_16b;
   SDRAM_CAS_LATENCY  : constant := FMC_CAS_Latency_3;
   SDCLOCK_PERIOD     : constant := FMC_SDClock_Period_2;
   SDRAM_READBURST    : constant := FMC_Read_Burst_Disable;

   SDRAM_MODEREG_BURST_LENGTH_1             : constant := 16#0000#;
   SDRAM_MODEREG_BURST_LENGTH_2             : constant := 16#0001#;
   SDRAM_MODEREG_BURST_LENGTH_4             : constant := 16#0002#;
   SDRAM_MODEREG_BURST_LENGTH_8             : constant := 16#0004#;
   SDRAM_MODEREG_BURST_TYPE_SEQUENTIAL      : constant := 16#0000#;
   SDRAM_MODEREG_BURST_TYPE_INTERLEAVED     : constant := 16#0008#;
   SDRAM_MODEREG_CAS_LATENCY_2              : constant := 16#0020#;
   SDRAM_MODEREG_CAS_LATENCY_3              : constant := 16#0030#;
   SDRAM_MODEREG_OPERATING_MODE_STANDARD    : constant := 16#0000#;
   SDRAM_MODEREG_WRITEBURST_MODE_PROGRAMMED : constant := 16#0000#;
   SDRAM_MODEREG_WRITEBURST_MODE_SINGLE     : constant := 16#0200#;

end STM32F4.SDRAM;
