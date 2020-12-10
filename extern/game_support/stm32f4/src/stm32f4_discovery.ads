------------------------------------------------------------------------------
--                                                                          --
--                Hardware Abstraction Layer for STM32 Targets              --
--                                                                          --
--             Copyright (C) 2014, Free Software Foundation, Inc.           --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This file provides declarations for STM32F407xx boards manufactured by
--  from ST Microelectronics.  For example: the STM32F4 Discovery kit.

with System;          use System;
with STM32F4.GPIO;    use STM32F4.GPIO;
with STM32F4.DMA;     use STM32F4.DMA;
with STM32F4.USARTs;  use STM32F4.USARTs;
with STM32F4.I2C;     use STM32F4.I2C;
with STM32F4.SPI;     use STM32F4.SPI;
with STM32F4.Timers;  use STM32F4.Timers;

use STM32F4;  -- for base addresses

package STM32F4_Discovery is
   pragma Elaborate_Body;

   subtype User_LED is GPIO_Pin;

   Green  : User_LED renames Pin_12;
   Orange : User_LED renames Pin_13;
   Red    : User_LED renames Pin_14;
   Blue   : User_LED renames Pin_15;

   LED3 : User_LED renames Orange;
   LED4 : User_LED renames Green;
   LED5 : User_LED renames Red;
   LED6 : User_LED renames Blue;

   procedure Initialize_LEDs;
   --  MUST be called prior to any use of the LEDs

   procedure On     (This : User_LED) with Inline;
   procedure Off    (This : User_LED) with Inline;
   procedure Toggle (This : User_LED) with Inline;

   procedure All_LEDs_Off with Inline;
   procedure All_LEDs_On  with Inline;


   GPIO_A : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOA_Base);
   GPIO_B : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOB_Base);
   GPIO_C : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOC_Base);
   GPIO_D : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOD_Base);
   GPIO_E : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOE_Base);
   GPIO_F : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOF_Base);
   GPIO_G : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOG_Base);
   GPIO_H : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOH_Base);
   GPIO_I : aliased GPIO_Port with Volatile, Address => System'To_Address (GPIOI_Base);

   procedure Enable_Clock (This : aliased in out GPIO_Port);

   USART_1 : aliased USART with Volatile, Address => System'To_Address (USART1_Base);
   USART_2 : aliased USART with Volatile, Address => System'To_Address (USART2_Base);
   USART_3 : aliased USART with Volatile, Address => System'To_Address (USART3_Base);
   USART_6 : aliased USART with Volatile, Address => System'To_Address (USART6_Base);

   procedure Enable_Clock (This : aliased in out USART);

   DMA_1 : aliased DMA_Controller with Volatile, Address => System'To_Address (DMA1_BASE);
   DMA_2 : aliased DMA_Controller with Volatile, Address => System'To_Address (DMA2_BASE);

   procedure Enable_Clock (This : aliased in out DMA_Controller);

   I2C_1 : aliased I2C_Port with Volatile, Address => System'To_Address (I2C1_Base);
   I2C_2 : aliased I2C_Port with Volatile, Address => System'To_Address (I2C2_Base);
   I2C_3 : aliased I2C_Port with Volatile, Address => System'To_Address (I2C3_Base);

   procedure Enable_Clock (This : aliased in out I2C_Port);

   procedure Reset (This : in out I2C_Port);

   SPI_1 : aliased SPI_Port with Volatile, Address => System'To_Address (SPI1_Base);
   SPI_2 : aliased SPI_Port with Volatile, Address => System'To_Address (SPI2_Base);
   SPI_3 : aliased SPI_Port with Volatile, Address => System'To_Address (SPI3_Base);

   procedure Enable_Clock (This : aliased in out SPI_Port);

   procedure Reset (This : in out SPI_Port);

   Timer_1 : Timer with Volatile, Address => System'To_Address (TIM1_Base);
   pragma Import (Ada, Timer_1);
   Timer_2 : Timer with Volatile, Address => System'To_Address (TIM2_Base);
   pragma Import (Ada, Timer_2);
   Timer_3 : Timer with Volatile, Address => System'To_Address (TIM3_Base);
   pragma Import (Ada, Timer_3);
   Timer_4 : Timer with Volatile, Address => System'To_Address (TIM4_Base);
   pragma Import (Ada, Timer_4);
   Timer_5 : Timer with Volatile, Address => System'To_Address (TIM5_Base);
   pragma Import (Ada, Timer_5);
   Timer_6 : Timer with Volatile, Address => System'To_Address (TIM6_Base);
   pragma Import (Ada, Timer_6);
   Timer_7 : Timer with Volatile, Address => System'To_Address (TIM7_Base);
   pragma Import (Ada, Timer_7);
   Timer_8 : Timer with Volatile, Address => System'To_Address (TIM8_Base);
   pragma Import (Ada, Timer_8);
   Timer_9 : Timer with Volatile, Address => System'To_Address (TIM9_Base);
   pragma Import (Ada, Timer_9);
   Timer_10 : Timer with Volatile, Address => System'To_Address (TIM10_Base);
   pragma Import (Ada, Timer_10);
   Timer_11 : Timer with Volatile, Address => System'To_Address (TIM11_Base);
   pragma Import (Ada, Timer_11);
   Timer_12 : Timer with Volatile, Address => System'To_Address (TIM12_Base);
   pragma Import (Ada, Timer_12);
   Timer_13 : Timer with Volatile, Address => System'To_Address (TIM13_Base);
   pragma Import (Ada, Timer_13);
   Timer_14 : Timer with Volatile, Address => System'To_Address (TIM14_Base);
   pragma Import (Ada, Timer_14);

   procedure Enable_Clock (This : in out Timer);

   procedure Reset (This : in out Timer);

end STM32F4_Discovery;
