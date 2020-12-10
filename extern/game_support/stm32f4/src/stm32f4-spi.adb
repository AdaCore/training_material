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

package body STM32F4.SPI is

   Baud_Rate_Value : constant array (SPI_Baud_Rate_Prescaler) of Bits_3 :=
     (BRP_2   => 2#000#,
      BRP_4   => 2#001#,
      BRP_8   => 2#010#,
      BRP_16  => 2#011#,
      BRP_32  => 2#100#,
      BRP_64  => 2#101#,
      BRP_128 => 2#110#,
      BRP_256 => 2#111#);

   ---------------
   -- Configure --
   ---------------

   procedure Configure (Port : in out SPI_Port; Conf : SPI_Configuration) is
      CTRL1    : SPI_Control_Register := Port.CTRL1;
      I2S_Conf : SPI_I2S_Config_Register := Port.I2S_Conf;
   begin
      case Conf.Mode is
         when Master =>
            CTRL1.Master_Select := 1;
            CTRL1.Slave_Select  := 1;
         when Slave =>
            CTRL1.Master_Select := 0;
            CTRL1.Slave_Select  := 0;
      end case;

      case Conf.Direction is
         when D2Lines_FullDuplex =>
            CTRL1.BiDir_Mode   := 0;
            CTRL1.Output_BiDir := 0;
            CTRL1.RXOnly       := 0;
         when D2Lines_RxOnly =>
            CTRL1.BiDir_Mode   := 0;
            CTRL1.Output_BiDir := 0;
            CTRL1.RXOnly       := 1;
         when D1Line_Rx =>
            CTRL1.BiDir_Mode   := 1;
            CTRL1.Output_BiDir := 0;
            CTRL1.RXOnly       := 0;
         when D1Line_Tx =>
            CTRL1.BiDir_Mode   := 1;
            CTRL1.Output_BiDir := 1;
            CTRL1.RXOnly       := 0;
      end case;

      CTRL1.Data_Frame_Fmt := (if Conf.Data_Size = Data_16 then 1 else 0);
      CTRL1.Clock_Polarity := (if Conf.Clock_Polarity = High then 1 else 0);
      CTRL1.Clock_Phase    := (if Conf.Clock_Phase = P2Edge then 1 else 0);
      CTRL1.Soft_Slave_Mgt := (if Conf.Slave_Management = Soft then 1 else 0);
      CTRL1.Baud_Rate_Ctrl := Baud_Rate_Value (Conf.Baud_Rate_Prescaler);
      CTRL1.LSB_First      := (if Conf.First_Bit = LSB then 1 else 0);

      Port.CTRL1 := CTRL1;
      --  Activate the SPI mode (Reset I2SMOD bit in I2SCFGR register)
      I2S_Conf := Port.I2S_Conf;
      I2S_Conf.Mode_Select := 0;
      Port.I2S_Conf := I2S_Conf;

      Port.CRC_Poly := Conf.CRC_Poly;
   end Configure;

   ------------
   -- Enable --
   ------------

   procedure Enable (Port : in out SPI_Port) is
      CTRL1 : SPI_Control_Register := Port.CTRL1;
   begin
      CTRL1.SPI_Enable := 1;
      Port.CTRL1 := CTRL1;
   end Enable;

   -------------
   -- Disable --
   -------------

   procedure Disable (Port : in out SPI_Port) is
      CTRL1 : SPI_Control_Register := Port.CTRL1;
   begin
      CTRL1.SPI_Enable := 0;
      Port.CTRL1 := CTRL1;
   end Disable;

   -------------
   -- Enabled --
   -------------

   function Enabled (Port : SPI_Port) return Boolean is
   begin
      return Port.CTRL1.SPI_Enable = 1;
   end Enabled;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Half_Word) is
   begin
      Port.Data := Data;
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Half_Word is
   begin
      return Port.Data;
   end Data;

   ----------
   -- Send --
   ----------

   procedure Send (Port : in out SPI_Port; Data : Byte) is
   begin
      Send (Port, Half_Word (Data));
   end Send;

   ----------
   -- Data --
   ----------

   function Data (Port : SPI_Port) return Byte is
   begin
      return Byte (Half_Word'(Data (Port)));
   end Data;

   -----------------
   -- Tx_Is_Empty --
   -----------------

   function Tx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.TX_Buffer_Empty;
   end Tx_Is_Empty;

   -----------------
   -- Rx_Is_Empty --
   -----------------

   function Rx_Is_Empty (Port : SPI_Port) return Boolean is
   begin
      return not Port.Status.RX_Buffer_Not_Empty;
   end Rx_Is_Empty;

   ----------
   -- Busy --
   ----------

   function Busy (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Busy_Flag;
   end Busy;

   ----------------------------
   -- Channel_Side_Indicated --
   ----------------------------

   function Channel_Side_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Channel_Side;
   end Channel_Side_Indicated;

   ------------------------
   -- Underrun_Indicated --
   ------------------------

   function Underrun_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Underrun_Flag;
   end Underrun_Indicated;

   -------------------------
   -- CRC_Error_Indicated --
   -------------------------

   function CRC_Error_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.CRC_Error_Flag;
   end CRC_Error_Indicated;

   --------------------------
   -- Mode_Fault_Indicated --
   --------------------------

   function Mode_Fault_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Mode_Fault;
   end Mode_Fault_Indicated;

   -----------------------
   -- Overrun_Indicated --
   -----------------------

   function Overrun_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Overrun_Flag;
   end Overrun_Indicated;

   -------------------------------
   -- Frame_Fmt_Error_Indicated --
   -------------------------------

   function Frame_Fmt_Error_Indicated (Port : SPI_Port) return Boolean is
   begin
      return Port.Status.Frame_Fmt_Error;
   end Frame_Fmt_Error_Indicated;

end STM32F4.SPI;
