!> @file user_actions.f90
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
! ------------------
! 
! 
! Former revisions:
! -----------------
! $Id: user_actions.f90 1961 2016-07-12 16:37:58Z suehring $
!
! 1960 2016-07-12 16:34:24Z suehring
! New CASE statement for scalar tendency
! 
! 1873 2016-04-18 14:50:06Z maronga
! Module renamed (removed _mod)
! 
! 
! 1850 2016-04-08 13:29:27Z maronga
! Module renamed
!
! 
! 1822 2016-04-07 07:49:42Z hoffmann
! qr/nr-tendency removed.
!
! 1682 2015-10-07 23:56:08Z knoop
! Code annotations made doxygen readable 
! 
! 1353 2014-04-08 15:21:23Z heinze
! REAL constants provided with KIND-attribute
! 
! 1320 2014-03-20 08:40:49Z raasch
! kind-parameters added to all INTEGER and REAL declaration statements,
! kinds are defined in new module kinds,
! revision history before 2012 removed,
! comment fields (!:) to be used for variable explanations added to
! all variable declaration statements 
!
! 1318 2014-03-17 13:35:16Z raasch
! module interfaces removed
!
! 1053 2012-11-13 17:11:03Z hoffmann
! +qr-tendency, nr-tendency
!
! 1036 2012-10-22 13:43:42Z raasch
! code put under GPL (PALM 3.9)
!
! 211 2008-11-11 04:46:24Z raasch
! Former file user_interface.f90 split into one file per subroutine
!
! Description:
! ------------
!> Execution of user-defined actions before or after single timesteps
!------------------------------------------------------------------------------!
 MODULE user_actions_mod
 

    PRIVATE
    PUBLIC user_actions

    INTERFACE user_actions
       MODULE PROCEDURE user_actions
       MODULE PROCEDURE user_actions_ij
    END INTERFACE user_actions

 CONTAINS


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Call for all grid points
!------------------------------------------------------------------------------!
    SUBROUTINE user_actions( location )

       USE control_parameters

       USE cpulog

       USE indices

       USE kinds

       USE pegrid

       USE user

       USE arrays_3d

       USE surface_mod                                          !<added by Iman

       IMPLICIT NONE

       CHARACTER (LEN=*) ::  location !< 

       INTEGER(iwp) ::  i !< 
       INTEGER(iwp) ::  j !< 
       INTEGER(iwp) ::  k !< 
       CALL cpu_log( log_point(24), 'user_actions', 'start' )

       SELECT CASE ( location )

          CASE ( 'before_timestep' )

          CASE ( 'after_integration' )

          CASE ( 'after_timestep' )

          CASE ( 'u-tendency' )

          CASE ( 'v-tendency' )


          CASE ( 'w-tendency' )


          CASE ( 'pt-tendency' )


          CASE ( 'sa-tendency' )


          CASE ( 'e-tendency' )


          CASE ( 'q-tendency' )
          
          
          CASE ( 's-tendency' )          


          CASE DEFAULT
             message_string = 'unknown location "' // location // '"'
             CALL message( 'user_actions', 'UI0001', 1, 2, 0, 6, 0 )

       END SELECT

       CALL cpu_log( log_point(24), 'user_actions', 'stop' )

    END SUBROUTINE user_actions


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Call for grid point i,j
!------------------------------------------------------------------------------!
    SUBROUTINE user_actions_ij( i, j, location )

       USE control_parameters
       USE kinds
       USE pegrid
       USE user
       USE arrays_3d
       USE indices
       USE grid_variables
       USE surface_mod                                          !<added by Iman
       USE statistics
       


       USE particle_attributes,                                                &
        ONLY:  grid_particles, number_of_particles, particles,                 &
               particle_advection_start, prt_count

       !<KM_FLAG 
       USE plankton_model

       IMPLICIT NONE

       CHARACTER (LEN=*) ::  location

       INTEGER(iwp) ::  i
       INTEGER(iwp) ::  j
       INTEGER(iwp) ::  k

!--    Here the user-defined actions follow
       SELECT CASE ( location )
          CASE ( 'u-tendency' )

          CASE ( 'v-tendency' )

          CASE ( 'w-tendency' )

          CASE ( 'pt-tendency' )

          !<KM_FLAG
              CALL LPM_irradiance(i,j)

              DO k = nzb, nzt
                  CALL LPM_pt_tend(k)
                  tend(k,j,i) = tend(k,j,i) + pt_tend
              END DO 

          CASE ( 'sa-tendency' )

          CASE ( 'e-tendency' )

          CASE ( 'q-tendency' )
          
          CASE ( 's-tendency' )

          !<KM_FLAG
               DO  k = nzt, nzb+1, -1
                  CALL LPM_s_tend(i,j,k)
                  tend(k,j,i) = tend(k,j,i) + s_tend
               ENDDO


          CASE ( 'before_timestep', 'after_integration', 'after_timestep' )
             message_string = 'location "' // location // '" is not ' // &
                             'allowed to be called with parameters "i" and "j"'
             CALL message( 'user_actions', 'UI0002', 1, 2, 0, 6, 0 )


          CASE DEFAULT
             message_string = 'unknown location "' // location // '"'
             CALL message( 'user_actions', 'UI0001', 1, 2, 0, 6, 0 )
             

       END SELECT

    END SUBROUTINE user_actions_ij

 END MODULE user_actions_mod
