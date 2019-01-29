!> @file user_init.f90
!--------------------------------------------------------------------------------!
! This file is part of PALM.
!
! PALM is free software: you can redistribute it and/or modify it under the terms
! of the GNU General Public License as published by the Free Software Foundation,
! either version 3 of the License, or (at your option) any later version.
!
! PALM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! PALM. If not, see <http://www.gnu.org/licenses/>.
!
! Copyright 1997-2016 Leibniz Universitaet Hannover
!--------------------------------------------------------------------------------!
!
! Current revisions:
! -----------------
! 
! 
! Former revisions:
! -----------------
! $Id: user_init.f90 1818 2016-04-06 15:53:27Z maronga $
!
! 1799 2016-04-05 08:35:55Z gronemeier
! Bugfix: added dots_num to variable list
!
! 1783 2016-03-06 18:36:17Z raasch
! netcdf module name changed + related changes
!
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
! 
! 1353 2014-04-08 15:21:23Z heinze
! REAL constants provided with KIND-attribute 
!
! 1320 2014-03-20 08:40:49Z raasch
! revision history before 2012 removed,
! comment fields (!:) to be used for variable explanations added to
! all variable declaration statements 
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! 211 2008-11-11 04:46:24Z raasch
! Former file user_interface.f90 split into one file per subroutine
!
! Description:
! ------------
!> Execution of user-defined initializing actions
!------------------------------------------------------------------------------!
 SUBROUTINE user_init
 

    USE control_parameters
    
    USE indices
    
    USE kinds
    
    USE netcdf_interface,                                                      &
        ONLY: dots_label, dots_unit, dots_num
    
    USE pegrid
    
    USE user

    USE arrays_3d
    USE basic_constants_and_equations_mod
    USE profil_parameter
    USE statistics

    !<KM_FLAG
    USE plankton_model

    IMPLICIT NONE
  
    !<KM_FLAG
    CALL LPM_setup
    
 END SUBROUTINE user_init

