with system.Storage_Elements;
package Counter
  with SPARK_Mode => On

-- Small external state tutorial exercise
--
-- This package uses an external counter which is incremented
-- by one every time:
-- A. its clock ticks,
-- B. it is read.
--
-- It has one query procedure Bump_And_Monitor to check if the
-- counter has reached its limit.

is
   Port : Integer;

   Limit : constant Integer := 3_000_000;

   procedure Bump_And_Monitor (Alarm : out Boolean);

end Counter;
