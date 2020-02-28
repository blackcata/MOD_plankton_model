!> @file user_lpm_advec.f90
!------------------------------------------------------------------------------!
! This file is part of the PALM model system.
!
! PALM is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! PALM is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE.  See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! PALM. If not, see <http://www.gnu.org/licenses/>.
!
! Copyright 1997-2019 Leibniz Universitaet Hannover
!------------------------------------------------------------------------------!
!
! Current revisions:
! -----------------
! 
! 
! Former revisions:
! -----------------
! $Id: user_lpm_advec.f90 4182 2019-08-22 15:20:23Z scharf $
! Corrected "Former revisions" section
! 
! 3768 2019-02-27 14:35:58Z raasch
! variables commented + statement added to avoid compiler warnings about unused variables
! 
! 3655 2019-01-07 16:51:22Z knoop
! Corrected "Former revisions" section
!
! 211 2008-11-11 04:46:24Z raasch
! Former file user_interface.f90 split into one file per subroutine
!
! Description:
! ------------
!> Modification of initial particles by the user.
!------------------------------------------------------------------------------!
 SUBROUTINE user_lpm_advec( ip, jp, kp )
 

    USE kinds
    
    USE particle_attributes
    
    USE user

    !<KM_FLAG
    USE plankton_model

    IMPLICIT NONE

    INTEGER(iwp) ::  ip   !< index of particle grid box, x-direction
    INTEGER(iwp) ::  jp   !< index of particle grid box, y-direction
    INTEGER(iwp) ::  kp   !< index of particle grid box, z-direction

    !<KM_FLAG
    CALL LPM_phy_tend(ip, jp, kp)

 END SUBROUTINE user_lpm_advec

