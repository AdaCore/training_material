
# Mach Calculator 
#### (Copyright (C) 2022, AdaCore)

## Disclaimer

This is free software;  you can redistribute it  and/or modify it  under terms of the  GNU General Public License as published  by the Free Software Foundation;  either version 3,  or (at your option) any later version.  This software is distributed in the hope  that it will be useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for  more details.  You should have  received  a copy of the GNU General  Public  License  distributed  with  this  software;   see  file COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy of the license.

## Description

This Ada code contains an algorithm to calculate an appromiation of the ratio of the speed of a body to the speed of sound in the surrounding medium (in this case air). From the user, an air temperature is given. Before calculating the speed of sound (Knots), the air temperature value must be converted to Kelvin (K). The speed of sound is then converted to the corresponding unit of the user provided measured air speed and the Mach value calculated and returned.

**__THIS CODE CONTAINS MULTIPLE (AND INTENTIONAL) BUGS. ITS PRIMARY PURPOSE IS TO DEMONSTRATE THE CAPABILITIES OF ADACORE TOOLING (GNATFUZZ, GNATTEST, CODEPEER, SPARK PRO ETC) TO IDENTIFY THE BUGS. IT IS NOT INTENDED FOR COMERCIAL USE!__**

## Original Repository

This code was copied on August 20, 2024 from the GitHub repository: https://github.com/ButcherAdaCore/mach_calculator
