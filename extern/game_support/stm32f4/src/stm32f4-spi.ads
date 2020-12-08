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

--  This file provides definitions for the STM32F4 (ARM Cortex M4F
--  from ST Microelectronics) Serial Peripheral Interface (SPI) facility.

package STM32F4.SPI is

   type SPI_Port is limited private;

   type SPI_Data_Direction is
     (D2Lines_FullDuplex, D2Lines_RxOnly, D1Line_Rx, D1Line_Tx);

   type SPI_Data_Size is (Data_16, Data_8);

   type SPI_Mode is (Master, Slave);

   type SPI_CLock_Polarity is (High, Low);

   type SPI_CLock_Phase is (P1Edge, P2Edge);

   type SPI_Slave_Management is (Soft, Hard);

   type SPI_Baud_Rate_Prescaler is
     (BRP_2, BRP_4, BRP_8, BRP_16, BRP_32, BRP_64, BRP_128, BRP_256);

   type SPI_First_Bit is (MSB, LSB);

   type SPI_Configuration is record
      Direction           : SPI_Data_Direction;
      Mode                : SPI_Mode;
      Data_Size           : SPI_Data_Size;
      Clock_Polarity      : SPI_Clock_Polarity;
      Clock_Phase         : SPI_Clock_Phase;
      Slave_Management    : SPI_Slave_Management;
      Baud_Rate_Prescaler : SPI_Baud_Rate_Prescaler;
      First_Bit           : SPI_First_Bit;
      CRC_Poly            : Half_Word;
   end record;

   procedure Configure (Port : in out SPI_Port; Conf : SPI_Configuration);

   procedure Enable (Port : in out SPI_Port);

   procedure Disable (Port : in out SPI_Port);

   function Enabled (Port : SPI_Port) return Boolean;

   procedure Send (Port : in out SPI_Port; Data : Half_Word);

   function Data (Port : SPI_Port) return Half_Word
     with Inline;

   procedure Send (Port : in out SPI_Port; Data : Byte);

   function Data (Port : SPI_Port) return Byte
     with Inline;

   function Rx_Is_Empty (Port : SPI_Port) return Boolean
     with Inline;

   function Tx_Is_Empty (Port : SPI_Port) return Boolean
     with Inline;

   function Busy (Port : SPI_Port) return Boolean
     with Inline;

   function Channel_Side_Indicated (Port : SPI_Port) return Boolean
     with Inline;

   function Underrun_Indicated (Port : SPI_Port) return Boolean
     with Inline;

   function CRC_Error_Indicated (Port : SPI_Port) return Boolean
     with Inline;

   function Mode_Fault_Indicated (Port : SPI_Port) return Boolean
     with Inline;

   function Overrun_Indicated (Port : SPI_Port) return Boolean
     with Inline;

   function Frame_Fmt_Error_Indicated (Port : SPI_Port) return Boolean
     with Inline;

private

   type SPI_Control_Register is record
      Clock_Phase    : Bits_1;
      Clock_Polarity : Bits_1;
      Master_Select  : Bits_1;
      Baud_Rate_Ctrl : Bits_3;
      SPI_Enable     : Bits_1;
      LSB_First      : Bits_1; --  Frame Format
      Slave_Select   : Bits_1;
      Soft_Slave_Mgt : Bits_1; --  Software Slave Management
      RXOnly         : Bits_1;
      Data_Frame_Fmt : Bits_1; --  1=16-bit 0=8-bit
      CRC_Next       : Bits_1; --  1=CRC Phase 0=No CRC Phase
      CRC_Enable     : Bits_1;
      Output_BiDir   : Bits_1; --  Output enable in bidirectional mode
      BiDir_Mode     : Bits_1; --  Bidirectional data mode enable
   end record with Pack, Volatile, Size => 16;

   type SPI_Control_Register2 is record
      RX_DMA_Enable           : Bits_1;
      TX_DMA_Enable           : Bits_1;
      SS_Out_Enable           : Bits_1;
      Reserved_1              : Bits_1;
      Frame_Fmt               : Bits_1; --  0=Motorola Mode 1=TI Mode
      Err_Int_Enable          : Bits_1;
      RX_Not_Empty_Int_Enable : Bits_1;
      TX_Empty_Int_Enable     : Bits_1;
      Reserved_2              : Bits_8;
   end record with Pack, Volatile, Size => 16;

   type SPI_I2S_Config_Register is record
      Channel_Length : Bits_1;
      Data_Length    : Bits_2;
      Clock_Polarity : Bits_1;
      I2S_Standard   : Bits_2; --  00==Philips 01=MSB (L) 10=LSB (R) 11=PCM
      Reserved_1     : Bits_1;
      PCM_Frame_Sync : Bits_1; --  0=Short 1=Long
      Config_Mode    : Bits_2; --  00=SlaveTX 01=SlaveRX 10=MasterTX11=MasterRX
      Enable         : Bits_1;
      Mode_Select    : Bits_1; --  0=SPI Mode 1=I2S Mode
      Reserved_2     : Bits_4;
   end record with Pack, Volatile, Size => 16;

   type SPI_I2S_Prescale_Register is record
      Linear_Prescler       : Bits_8;
      Odd_Factor            : Bits_1;
      Master_CLK_Out_Enable : Bits_1;
      Reserved              : Bits_6;
   end record with Pack, Volatile, Size => 16;

   type SPI_Status_Register is record
      RX_Buffer_Not_Empty : Boolean;
      TX_Buffer_Empty     : Boolean;
      Channel_Side        : Boolean;
      Underrun_Flag       : Boolean;
      CRC_Error_Flag      : Boolean;
      Mode_Fault          : Boolean;
      Overrun_Flag        : Boolean;
      Busy_Flag           : Boolean;
      Frame_Fmt_Error     : Boolean;
      Reserved            : Bits_7;
   end record with Pack, Volatile, Size => 16;

   type SPI_Port is record
      CTRL1       : SPI_Control_Register;
      Reserved_1  : Half_Word;
      CTRL2       : SPI_Control_Register2;
      Reserved_2  : Half_Word;
      Status      : SPI_Status_Register;
      Reserved_3  : Half_Word;
      Data        : Half_Word;
      Reserved_4  : Half_Word;
      CRC_Poly    : Half_Word; --  Default = 16#0007#
      Reserved_5  : Half_Word;
      RX_CRC      : Half_Word;
      Reserved_6  : Half_Word;
      TX_CRC      : Half_Word;
      Reserved_7  : Half_Word;
      I2S_Conf    : SPI_I2S_Config_Register;
      Reserved_8  : Half_Word;
      I2S_PreScal : SPI_I2S_Prescale_Register;
      Reserved_9  : Half_Word;
   end record with Pack, Volatile, Size => 9 * 32;

end STM32F4.SPI;
