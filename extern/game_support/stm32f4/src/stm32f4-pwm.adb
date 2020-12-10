with System;
with STM32F4.RCC; use STM32F4.RCC;

package body STM32F4.PWM is

   type Single_CCMR_Output is record
      CCxS  : Bits_2;
      OCxFE : Bits_1;
      OCxPE : Bits_1;
      OCxM  : Bits_3;
      OCxCE : Bits_1;
   end record with Pack, Size => 8;

   type CCM1_Output_Register is record
      Ch1      : Single_CCMR_Output;
      Ch2      : Single_CCMR_Output;
      Reserved : Bits_16;
   end record with Pack, Size => 32;

   type CCM2_Output_Register is record
      Ch3      : Single_CCMR_Output;
      Ch4      : Single_CCMR_Output;
      Reserved : Bits_16;
   end record with Pack, Size => 32;

   type Single_CCE is record
      CCxE     : Bits_1;
      CCxP     : Bits_1;
      Reserved : Bits_1;
      CCxNP    : Bits_1;
   end record with Pack, Size => 4;

   type CCE_Register is array (Channel) of Single_CCE with Pack, Size => 16;

   type TIMx is record
      CR1           : Word;
      CR2           : Word;
      SMCR          : Word;
      DIER          : Word;
      SR            : Word;
      EGR           : Word;
      CCM1          : CCM1_Output_Register;
      CCM2          : CCM2_Output_Register;
      CCER          : CCE_Register;
      Reserved_CCER : Half_Word;
      CNT           : Word;
      PSC           : Word;
      ARR           : Word;
      Reserved_1    : Word;
      CCR1          : Word;
      CCR2          : Word;
      CCR3          : Word;
      CCR4          : Word;
      Reserved_2    : Word;
      DCR           : Word;
      DMAR          : Word;
      TIM2_OR       : Word;
      TIM5_OR       : Word;
   end record with Volatile, Pack;

   type TIMx_Access is access all TIMx;

   TIM2_Base : constant := APB1_Peripheral_Base + 16#0000#;
   TIM3_Base : constant := APB1_Peripheral_Base + 16#0400#;
   TIM4_Base : constant := APB1_Peripheral_Base + 16#0800#;
   TIM5_Base : constant := APB1_Peripheral_Base + 16#0C00#;

   TIM2_Reg : aliased TIMx with
     Volatile, Address => System'To_Address (TIM2_Base);
   TIM3_Reg : aliased TIMx with
     Volatile, Address => System'To_Address (TIM3_Base);
   TIM4_Reg : aliased TIMx with
     Volatile, Address => System'To_Address (TIM4_Base);
   TIM5_Reg : aliased TIMx with
     Volatile, Address => System'To_Address (TIM5_Base);

   function Get_Register_Access (Tim : Timer) return TIMx_Access;
   function Get_Register_Access (Tim : Timer) return TIMx_Access is
   begin
      case Tim is
         when TIM2 =>
            return TIM2_Reg'Unrestricted_Access;
         when TIM3 =>
            return TIM3_Reg'Unrestricted_Access;
         when TIM4 =>
            return TIM4_Reg'Unrestricted_Access;
         when TIM5 =>
            return TIM5_Reg'Unrestricted_Access;
      end case;
   end Get_Register_Access;

   procedure Enable_CLK (Tim : Timer) is
   begin
      case Tim is
         when TIM2 =>
            TIM2_Clock_Enable;
         when TIM3 =>
            TIM3_Clock_Enable;
         when TIM4 =>
            TIM4_Clock_Enable;
         when TIM5 =>
            TIM5_Clock_Enable;
      end case;
   end Enable_CLK;

   procedure Config_Timer (Tim : Timer; Conf : Timer_Config) is
      Auto_Reload : Float;
      Counter_Clk : Float;
      Reg     : constant TIMx_Access := Get_Register_Access (Tim);
      TIMCLK1 : constant Natural := Natural (System_Clock_Frequencies.TIMCLK1);

   begin
      Enable_Clk (Tim);

      --  counter_clk = TIMCLK1 / (prescaler + 1)

      Counter_Clk := Float (TIMCLK1) / Float (Conf.Prescaler + 1);

      --  Max_Period := 1.0 / Counter_Clk;
      --  --  Auto_Reload is a 16bits register so maximum value is 2**16 - 1
      --  Min_Period := Counter_Clk / Float ((2**16 - 1) + 1);

      --  if Conf.Frequency > Max_Freq or else Conf.Frequency < Min_Freq then
      --     raise Program_Error;
      --  end if;

      --  Write Prescaler to PSC
      Reg.PSC := Word (Conf.Prescaler);

      --  Write Auto_Reload to ARR
      --  Auto_Reload := (Counter_Clk / Frequency) - 1;
      --  Auto_Reload := (Counter_Clk / (1 / Period)) - 1;
      --  Auto_Reload := (Counter_Clk * Period) - 1;
      Auto_Reload := (Conf.Period * Counter_Clk) - 1.0;

      --  ??? There is a factor 10 error not identified
      Auto_Reload := Auto_Reload * 10.0;

      if Auto_Reload < 0.0 then
         raise Program_Error;
      end if;

      Reg.ARR := Word (Auto_Reload);

      --  Enable Timer
      Reg.CR1 := Reg.CR1 or 1;
   end Config_Timer;

   procedure Set_Duty_Percentage (Tim     : Timer;
                                  Ch      : Channel;
                                  Percent : Duty_Percentage)
   is
      Reg : constant TIMx_Access := Get_Register_Access (Tim);
      Cmp : Word;
      Auto_Reload : constant Natural := Natural (Reg.ARR);
   begin
      --  PWM_Duty_Cycle = (cmp_reg * 100) / (auto_reload  + 1)
      --  cmp_reg = (PWM_Duty_Cycle * (auto_reload  + 1)) / 100
      Cmp := Word ((Percent * (Auto_Reload + 1)) / 100);
      case Ch is
         when CH1 =>
            Reg.CCR1 := Cmp;
         when CH2 =>
            Reg.CCR2 := Cmp;
         when CH3 =>
            Reg.CCR3 := Cmp;
         when CH4 =>
            Reg.CCR4 := Cmp;
      end case;
   end Set_Duty_Percentage;

   procedure Set_Duty_Time (Tim : Timer; Ch : Channel; Time : Duty_Time) is
      Reg : constant TIMx_Access := Get_Register_Access (Tim);
      Counter_Clk : Float;
      Cmp : Word;
      TIMCLK1 : constant Natural := Natural (System_Clock_Frequencies.TIMCLK1);
   begin

      --  counter_clk = TIMCLK1 / (prescaler + 1)
      --  Time_per_Tick = 1 / counter_clk
      --  CMP = Duty_time / Time_Per_Tick

      Counter_Clk := Float (TIMCLK1 / (Natural (Reg.PSC) + 1));
      Cmp := Word (Time / (1.0 / Counter_Clk));

      --  ??? There is a factor 10 error not identified
      Cmp := Cmp * 10;

      case Ch is
         when CH1 =>
            Reg.CCR1 := Cmp;
         when CH2 =>
            Reg.CCR2 := Cmp;
         when CH3 =>
            Reg.CCR3 := Cmp;
         when CH4 =>
            Reg.CCR4 := Cmp;
      end case;
   end Set_Duty_Time;

   procedure Config_Channel (Tim  : Timer;
                             Ch   : Channel;
                             Conf : Channel_Config)
   is
      Reg  : constant TIMx_Access  := Get_Register_Access (Tim);
      Single_CCMR : Single_CCMR_Output;
   begin
      --  Only output is implemented
      if Conf.Mode /= Output then
         raise Program_Error;
      end if;

      Single_CCMR.CCxS  := 0;      --  Output Mode
      Single_CCMR.OCxM  := 2#110#; --  PWM1_Mode
      Single_CCMR.OCxFE := 0;
      Single_CCMR.OCxPE := 0;
      Single_CCMR.OCxCE := 0;

      case Ch is
         when CH1 .. CH2 =>
            declare
               CCM1 : CCM1_Output_Register := Reg.CCM1;
            begin
               if Ch = CH1 then
                  CCM1.Ch1 := Single_CCMR;
               else
                  CCM1.Ch2 := Single_CCMR;
               end if;
               Reg.CCM1 := CCM1;
            end;
         when CH3 .. CH4 =>
            declare
               CCM2 : CCM2_Output_Register := Reg.CCM2;
            begin
               if Ch = CH3 then
                  CCM2.Ch3 := Single_CCMR;
               else
                  CCM2.Ch4 := Single_CCMR;
               end if;

               Reg.CCM2 := CCM2;
            end;
      end case;

      --  As the channel is configured as output
      --   - CCxNP must be  0
      --   - CCxP  0 = active high 1 = active low
      --   - CCxE = 0 to not enable the output (we do it in a separate routine)

      declare
         CCE : CCE_Register := Reg.CCER;
      begin
         CCE (Ch).CCxNP := 0;
         CCE (Ch).CCxP := 0;
         CCE (Ch).CCxE := 0;

         Reg.CCER := CCE;
      end;
   end Config_Channel;

   procedure Set_Channel_State (Tim : Timer;
                                Ch : Channel;
                                State : Channel_State)
   is
      Reg  : constant TIMx_Access  := Get_Register_Access (Tim);
      CCER : CCE_Register := Reg.CCER;
      EGR  : Word := Reg.EGR;
   begin
      CCER (Ch).CCxE := (if State = Enabled then 1 else 0);

      --  Trigger an event to initialize preload register
      EGR := EGR or (2**(Channel'Pos (Ch) + 1));

      Reg.EGR  := EGR;
      Reg.CCER := CCER;
   end Set_Channel_State;

end STM32F4.PWM;
