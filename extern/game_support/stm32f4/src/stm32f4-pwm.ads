package STM32F4.PWM is
   type Timer is (TIM2, TIM3, TIM4, TIM5);

   type Timer_Config is record
      Prescaler : Half_Word;
      Period    : Float;
   end record;

   type Channel is (CH1, CH2, CH3, CH4);
   type Channel_State is (Enabled, Disabled);

   --  Intput not supported yet
   type PWM_Mode is (Output);

   type Channel_Config is record
      Mode : PWM_Mode;
   end record;

   subtype Duty_Percentage is Natural range 1 .. 100;
   subtype Duty_Time is Float;

   procedure Config_Timer (Tim : Timer; Conf : Timer_Config);
   procedure Config_Channel (Tim : Timer; Ch : Channel; Conf : Channel_Config);
   procedure Set_Channel_State (Tim : Timer;
                                Ch : Channel;
                                State : Channel_State);
   procedure Set_Duty_Percentage (Tim     : Timer;
                                  Ch      : Channel;
                                  Percent : Duty_Percentage);
   procedure Set_Duty_Time (Tim : Timer; Ch : Channel; Time : Duty_Time);

end STM32F4.PWM;
