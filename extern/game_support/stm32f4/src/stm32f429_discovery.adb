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

with STM32F4.RCC; use STM32F4.RCC;

package body STM32F429_Discovery is

   --------
   -- On --
   --------

   procedure On (This : User_LED) is
   begin
      Set (GPIO_G, This);
   end On;

   ---------
   -- Off --
   ---------

   procedure Off (This : User_LED) is
   begin
      Clear (GPIO_G, This);
   end Off;

   ------------
   -- Toggle --
   ------------

   procedure Toggle (This : User_LED) is
   begin
      Toggle (GPIO_G, This);
   end Toggle;


   All_LEDs  : constant GPIO_Pins := Green & Red;

   ------------------
   -- All_LEDs_Off --
   ------------------

   procedure All_LEDs_Off is
   begin
      Clear (GPIO_G, ALL_LEDs);
   end All_LEDs_Off;

   -----------------
   -- All_LEDs_On --
   -----------------

   procedure All_LEDs_On is
   begin
      Set (GPIO_G, ALL_LEDs);
   end All_LEDs_On;

   ---------------------
   -- Initialize_LEDs --
   ---------------------

   procedure Initialize_LEDs is
      Conf : GPIO_Port_Configuration;
   begin
      Enable_Clock (GPIO_G);

      Conf.Mode        := Mode_Out;
      Conf.Output_Type := Push_Pull;
      Conf.Speed       := Speed_100MHz;
      Conf.Resistors   := Floating;

      Configure_IO (GPIO_G, All_LEDs, Conf);
   end Initialize_LEDs;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out GPIO_Port) is
   begin
      if This'Address = System'To_Address (GPIOA_Base) then
         GPIOA_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOB_Base) then
         GPIOB_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOC_Base) then
         GPIOC_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOD_Base) then
         GPIOD_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOE_Base) then
         GPIOE_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOF_Base) then
         GPIOF_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOG_Base) then
         GPIOG_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOH_Base) then
         GPIOH_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOI_Base) then
         GPIOI_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOJ_Base) then
         GPIOJ_Clock_Enable;
      elsif This'Address = System'To_Address (GPIOK_Base) then
         GPIOK_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out USART) is
   begin
      if This'Address = System'To_Address (USART1_Base) then
         USART1_Clock_Enable;
      elsif This'Address = System'To_Address (USART2_Base) then
         USART2_Clock_Enable;
      elsif This'Address = System'To_Address (USART3_Base) then
         USART3_Clock_Enable;
      elsif This'Address = System'To_Address (USART6_Base) then
         USART6_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out DMA_Controller) is
   begin
      if This'Address = System'To_Address (STM32F4.DMA1_BASE) then
         DMA1_Clock_Enable;
      elsif This'Address = System'To_Address (STM32F4.DMA2_BASE) then
         DMA2_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out I2C_Port) is
   begin
      if This'Address = System'To_Address (I2C1_Base) then
         I2C1_Clock_Enable;
      elsif This'Address = System'To_Address (I2C2_Base) then
         I2C2_Clock_Enable;
      elsif This'Address = System'To_Address (I2C3_Base) then
         I2C3_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out I2C_Port) is
   begin
      if This'Address = System'To_Address (I2C1_Base) then
         I2C1_Force_Reset;
         I2C1_Release_Reset;
      elsif This'Address = System'To_Address (I2C2_Base) then
         I2C2_Force_Reset;
         I2C2_Release_Reset;
      elsif This'Address = System'To_Address (I2C3_Base) then
         I2C3_Force_Reset;
         I2C3_Release_Reset;
      else
         raise Program_Error;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : aliased in out SPI_Port) is
   begin
      if This'Address = System'To_Address (SPI1_Base) then
         SPI1_Force_Reset;
         SPI1_Release_Reset;
      elsif This'Address = System'To_Address (SPI2_Base) then
         SPI2_Force_Reset;
         SPI2_Release_Reset;
      elsif This'Address = System'To_Address (SPI3_Base) then
         SPI3_Force_Reset;
         SPI3_Release_Reset;
      elsif This'Address = System'To_Address (SPI4_Base) then
         SPI4_Force_Reset;
         SPI4_Release_Reset;
      elsif This'Address = System'To_Address (SPI5_Base) then
         SPI5_Force_Reset;
         SPI5_Release_Reset;
      elsif This'Address = System'To_Address (SPI6_Base) then
         SPI6_Force_Reset;
         SPI6_Release_Reset;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out SPI_Port) is
   begin
      if This'Address = System'To_Address (SPI1_Base) then
         SPI1_Clock_Enable;
      elsif This'Address = System'To_Address (SPI2_Base) then
         SPI2_Clock_Enable;
      elsif This'Address = System'To_Address (SPI3_Base) then
         SPI3_Clock_Enable;
      elsif This'Address = System'To_Address (SPI4_Base) then
         SPI4_Clock_Enable;
      elsif This'Address = System'To_Address (SPI5_Base) then
         SPI5_Clock_Enable;
      elsif This'Address = System'To_Address (SPI6_Base) then
         SPI6_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Reset;

   ------------------
   -- Enable_Clock --
   ------------------

   procedure Enable_Clock (This : in out Timer) is
   begin
      if This'Address = System'To_Address (TIM1_Base) then
         TIM1_Clock_Enable;
      elsif This'Address = System'To_Address (TIM2_Base) then
         TIM2_Clock_Enable;
      elsif This'Address = System'To_Address (TIM3_Base) then
         TIM3_Clock_Enable;
      elsif This'Address = System'To_Address (TIM4_Base) then
         TIM4_Clock_Enable;
      elsif This'Address = System'To_Address (TIM5_Base) then
         TIM5_Clock_Enable;
      elsif This'Address = System'To_Address (TIM6_Base) then
         TIM6_Clock_Enable;
      elsif This'Address = System'To_Address (TIM7_Base) then
         TIM7_Clock_Enable;
      elsif This'Address = System'To_Address (TIM8_Base) then
         TIM8_Clock_Enable;
      elsif This'Address = System'To_Address (TIM9_Base) then
         TIM9_Clock_Enable;
      elsif This'Address = System'To_Address (TIM10_Base) then
         TIM10_Clock_Enable;
      elsif This'Address = System'To_Address (TIM11_Base) then
         TIM11_Clock_Enable;
      elsif This'Address = System'To_Address (TIM12_Base) then
         TIM12_Clock_Enable;
      elsif This'Address = System'To_Address (TIM13_Base) then
         TIM13_Clock_Enable;
      elsif This'Address = System'To_Address (TIM14_Base) then
         TIM14_Clock_Enable;
      else
         raise Program_Error;
      end if;
   end Enable_Clock;

   -----------
   -- Reset --
   -----------

   procedure Reset (This : in out Timer) is
   begin
      if This'Address = System'To_Address (TIM1_Base) then
         TIM1_Force_Reset;
         TIM1_Release_Reset;
      elsif This'Address = System'To_Address (TIM2_Base) then
         TIM2_Force_Reset;
         TIM2_Release_Reset;
      elsif This'Address = System'To_Address (TIM3_Base) then
         TIM3_Force_Reset;
         TIM3_Release_Reset;
      elsif This'Address = System'To_Address (TIM4_Base) then
         TIM4_Force_Reset;
         TIM4_Release_Reset;
      elsif This'Address = System'To_Address (TIM5_Base) then
         TIM5_Force_Reset;
         TIM5_Release_Reset;
      elsif This'Address = System'To_Address (TIM6_Base) then
         TIM6_Force_Reset;
         TIM6_Release_Reset;
      elsif This'Address = System'To_Address (TIM7_Base) then
         TIM7_Force_Reset;
         TIM7_Release_Reset;
      elsif This'Address = System'To_Address (TIM8_Base) then
         TIM8_Force_Reset;
         TIM8_Release_Reset;
      elsif This'Address = System'To_Address (TIM9_Base) then
         TIM9_Force_Reset;
         TIM9_Release_Reset;
      elsif This'Address = System'To_Address (TIM10_Base) then
         TIM10_Force_Reset;
         TIM10_Release_Reset;
      elsif This'Address = System'To_Address (TIM11_Base) then
         TIM11_Force_Reset;
         TIM11_Release_Reset;
      elsif This'Address = System'To_Address (TIM12_Base) then
         TIM12_Force_Reset;
         TIM12_Release_Reset;
      elsif This'Address = System'To_Address (TIM13_Base) then
         TIM13_Force_Reset;
         TIM13_Release_Reset;
      elsif This'Address = System'To_Address (TIM14_Base) then
         TIM14_Force_Reset;
         TIM14_Release_Reset;
      else
         raise Program_Error;
      end if;
   end Reset;

end STM32F429_Discovery;
