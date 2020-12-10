package STM32F4.GYRO is

   I_AM_L3GD20 : constant := 16#D4#;

   -- Power_Mode_selection
   L3GD20_MODE_POWERDOWN : constant := 16#00#;
   L3GD20_MODE_ACTIVE    : constant := 16#08#;

   -- OutPut_DataRate_Selection
   L3GD20_OUTPUT_DATARATE_1 : constant := 16#00#;
   L3GD20_OUTPUT_DATARATE_2 : constant := 16#40#;
   L3GD20_OUTPUT_DATARATE_3 : constant := 16#80#;
   L3GD20_OUTPUT_DATARATE_4 : constant := 16#C0#;

   -- Axes_Selection
   L3GD20_X_ENABLE     : constant := 16#02#;
   L3GD20_Y_ENABLE     : constant := 16#01#;
   L3GD20_Z_ENABLE     : constant := 16#04#;
   L3GD20_AXES_ENABLE  : constant := 16#07#;
   L3GD20_AXES_DISABLE : constant := 16#00#;

   -- BandWidth_Selection
   L3GD20_BANDWIDTH_1 : constant := 16#00#;
   L3GD20_BANDWIDTH_2 : constant := 16#10#;
   L3GD20_BANDWIDTH_3 : constant := 16#20#;
   L3GD20_BANDWIDTH_4 : constant := 16#30#;

   -- Full_Scale_Selection
   L3GD20_FULLSCALE_250  : constant := 16#00#;
   L3GD20_FULLSCALE_500  : constant := 16#10#;
   L3GD20_FULLSCALE_2000 : constant := 16#20#;

   -- Block_Data_Update
   L3GD20_BlockDataUpdate_Continous : constant := 16#00#;
   L3GD20_BlockDataUpdate_Single    : constant := 16#80#;

   -- Endian_Data_selection
   L3GD20_BLE_LSB : constant := 16#00#;
   L3GD20_BLE_MSB : constant := 16#40#;

   -- High_Pass_Filter_status
   L3GD20_HIGHPASSFILTER_DISABLE : constant := 16#00#;
   L3GD20_HIGHPASSFILTER_ENABLE  : constant := 16#10#;

   -- INT1_Interrupt_status
   L3GD20_INT1INTERRUPT_DISABLE : constant := 16#00#;
   L3GD20_INT1INTERRUPT_ENABLE  : constant := 16#80#;

   -- INT2_Interrupt_status
   L3GD20_INT2INTERRUPT_DISABLE : constant := 16#00#;
   L3GD20_INT2INTERRUPT_ENABLE  : constant := 16#08#;

   -- INT1_Interrupt_ActiveEdge
   L3GD20_INT1INTERRUPT_LOW_EDGE  : constant := 16#20#;
   L3GD20_INT1INTERRUPT_HIGH_EDGE : constant := 16#00#;

   -- Boot_Mode_selection
   L3GD20_BOOT_NORMALMODE   : constant := 16#00#;
   L3GD20_BOOT_REBOOTMEMORY : constant := 16#80#;

   -- High_Pass_Filter_Mode
   L3GD20_HPM_NORMAL_MODE_RES : constant := 16#00#;
   L3GD20_HPM_REF_SIGNAL      : constant := 16#10#;
   L3GD20_HPM_NORMAL_MODE     : constant := 16#20#;
   L3GD20_HPM_AUTORESET_INT   : constant := 16#30#;

   -- High_Pass_CUT OFF_Frequency
   L3GD20_HPFCF_0 : constant := 16#00#;
   L3GD20_HPFCF_1 : constant := 16#01#;
   L3GD20_HPFCF_2 : constant := 16#02#;
   L3GD20_HPFCF_3 : constant := 16#03#;
   L3GD20_HPFCF_4 : constant := 16#04#;
   L3GD20_HPFCF_5 : constant := 16#05#;
   L3GD20_HPFCF_6 : constant := 16#06#;
   L3GD20_HPFCF_7 : constant := 16#07#;
   L3GD20_HPFCF_8 : constant := 16#08#;
   L3GD20_HPFCF_9 : constant := 16#09#;

   type Gyro_Config is record
      Power_Mode       : Byte; --  Power-down/Sleep/Normal Mode
      Output_DataRate  : Byte; --  OUT data rate
      Axes_Enable      : Byte; --  Axes enable
      Band_Width       : Byte; --  Bandwidth selection
      BlockData_Update : Byte; --  Block Data Update
      Endianness       : Byte; --  Endian Data selection
      Full_Scale       : Byte; --  Full Scale selection
   end record;

   type Filter_Config is record
      Mode_Selection   : Byte;
      CutOff_Frequency : Byte;
   end record;

   type Filter_State is (Enabled, Disabled);

   type Angles is (X, Y, Z);
   type Angles_Rates is array (Angles) of Float;

   procedure Initialize (Conf : Gyro_Config);
   procedure Set_Filter_Config (Conf : Filter_Config);
   procedure Set_Filter_State (State : Filter_State);
   function Get_Data_Status return Byte;
   procedure Reboot;
   function Read_Angle_Rate return Angles_Rates;

end STM32F4.GYRO;
