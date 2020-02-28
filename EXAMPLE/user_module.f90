!> @file user_module.f90
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
! $Id: user_module.f90 4182 2019-08-22 15:20:23Z scharf $
! Corrected "Former revisions" section
! 
! 3986 2019-05-20 14:08:14Z Giersch
! Redundant integration of control parameters in user_rrd_global removed
! 
! 3911 2019-04-17 12:26:19Z knoop
! Bugfix: added before_prognostic_equations case in user_actions
! 
! 3768 2019-02-27 14:35:58Z raasch
! variables commented + statements added to avoid compiler warnings about unused variables
!
! 3767 2019-02-27 08:18:02Z raasch
! unused variable for file index removed from rrd-subroutines parameter list
! 
! 3747 2019-02-16 15:15:23Z gronemeier
! Add routine user_init_arrays
! 
! 3703 2019-01-29 16:43:53Z knoop
! An example for a user defined global variable has been added (Giersch)
!
! Revision 1.1  1998/03/24 15:29:04  raasch
! Initial revision
!
!
! Description:
! ------------
!> Declaration of user-defined variables. This module may only be used
!> in the user-defined routines (contained in user_interface.f90).
!------------------------------------------------------------------------------!
 MODULE user


    USE arrays_3d

    USE control_parameters

    USE cpulog

    USE indices

    USE kinds

    USE pegrid

    USE statistics

    USE surface_mod

    !<KM_FLAG
    USE inifor_defs,                                                           &
      ONLY: PI, G

    USE grid_variables,                                                        &
      ONLY: dx, dy

    USE plankton_model

    IMPLICIT NONE

    INTEGER(iwp) ::  dots_num_palm   !< 
    INTEGER(iwp) ::  dots_num_user = 0  !< 
    INTEGER(iwp) ::  user_idummy     !< 
    
    LOGICAL ::  user_module_enabled = .FALSE.   !< 
    
    REAL(wp) ::  user_rdummy   !< 
    REAL(wp) ::  radius 

!
!-- Sample for user-defined output
!    REAL(wp) :: global_parameter !< user defined global parameter 
!
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  u2       !< user defined array
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  u2_av    !< user defined array
!    REAL(wp), DIMENSION(:,:,:), ALLOCATABLE ::  ustvst   !< user defined array

    SAVE

    PRIVATE

!
!- Public functions
    PUBLIC &
       user_parin, &
       user_check_parameters, &
       user_check_data_output_ts, &
       user_check_data_output_pr, &
       user_check_data_output, &
       user_define_netcdf_grid, &
       user_init, &
       user_init_arrays, &
       user_header, &
       user_actions, &
       user_3d_data_averaging, &
       user_data_output_2d, &
       user_data_output_3d, &
       user_statistics, &
       user_rrd_global, &
       user_rrd_local, &
       user_wrd_global, &
       user_wrd_local, &
       user_last_actions

!
!- Public parameters, constants and initial values
   PUBLIC &
      user_module_enabled

    INTERFACE user_parin
       MODULE PROCEDURE user_parin
    END INTERFACE user_parin

    INTERFACE user_check_parameters
       MODULE PROCEDURE user_check_parameters
    END INTERFACE user_check_parameters

    INTERFACE user_check_data_output_ts
       MODULE PROCEDURE user_check_data_output_ts
    END INTERFACE user_check_data_output_ts

    INTERFACE user_check_data_output_pr
       MODULE PROCEDURE user_check_data_output_pr
    END INTERFACE user_check_data_output_pr

    INTERFACE user_check_data_output
       MODULE PROCEDURE user_check_data_output
    END INTERFACE user_check_data_output

    INTERFACE user_define_netcdf_grid
       MODULE PROCEDURE user_define_netcdf_grid
    END INTERFACE user_define_netcdf_grid

    INTERFACE user_init
       MODULE PROCEDURE user_init
    END INTERFACE user_init

    INTERFACE user_init_arrays
       MODULE PROCEDURE user_init_arrays
    END INTERFACE user_init_arrays

    INTERFACE user_header
       MODULE PROCEDURE user_header
    END INTERFACE user_header

    INTERFACE user_actions
       MODULE PROCEDURE user_actions
       MODULE PROCEDURE user_actions_ij
    END INTERFACE user_actions

    INTERFACE user_3d_data_averaging
       MODULE PROCEDURE user_3d_data_averaging
    END INTERFACE user_3d_data_averaging

    INTERFACE user_data_output_2d
       MODULE PROCEDURE user_data_output_2d
    END INTERFACE user_data_output_2d

    INTERFACE user_data_output_3d
       MODULE PROCEDURE user_data_output_3d
    END INTERFACE user_data_output_3d

    INTERFACE user_statistics
       MODULE PROCEDURE user_statistics
    END INTERFACE user_statistics

    INTERFACE user_rrd_global
       MODULE PROCEDURE user_rrd_global
    END INTERFACE user_rrd_global

    INTERFACE user_rrd_local
       MODULE PROCEDURE user_rrd_local
    END INTERFACE user_rrd_local

    INTERFACE user_wrd_global
       MODULE PROCEDURE user_wrd_global
    END INTERFACE user_wrd_global

    INTERFACE user_wrd_local
       MODULE PROCEDURE user_wrd_local
    END INTERFACE user_wrd_local

    INTERFACE user_last_actions
       MODULE PROCEDURE user_last_actions
    END INTERFACE user_last_actions


 CONTAINS


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Parin for &user_parameters for user module
!------------------------------------------------------------------------------!
 SUBROUTINE user_parin


    CHARACTER (LEN=80) ::  line   !< 

    INTEGER(iwp) ::  i                 !< 
    INTEGER(iwp) ::  j                 !< 


    NAMELIST /user_parameters/  &
       user_module_enabled, &
       data_output_pr_user, &
       data_output_user, &
       region, &
       data_output_masks_user, & 
       radius

!
!-- Next statement is to avoid compiler warnings about unused variables. Please remove in case
!-- that you are using them.
    IF ( dots_num_palm == 0  .OR.  dots_num_user == 0  .OR.  user_idummy == 0  .OR.                &
         user_rdummy == 0.0_wp )  CONTINUE

!
!-- Set revision number of this default interface version. It will be checked within
!-- the main program (palm). Please change the revision number in case that the
!-- current revision does not match with previous revisions (e.g. if routines
!-- have been added/deleted or if parameter lists in subroutines have been changed).
    user_interface_current_revision = 'r3703'

!
!-- Position the namelist-file at the beginning (it was already opened in
!-- parin), search for user-defined namelist-group ("userpar", but any other
!-- name can be choosed) and position the file at this line.
    REWIND ( 11 )

    line = ' '
    DO WHILE ( INDEX( line, '&user_parameters' ) == 0 )
       READ ( 11, '(A)', END=12 )  line
    ENDDO
    BACKSPACE ( 11 )

!-- Set default module switch to true
    user_module_enabled = .TRUE.

!-- Read user-defined namelist
    READ ( 11, user_parameters, ERR = 10 )

    GOTO 12

10  BACKSPACE( 11 )
    READ( 11 , '(A)') line
    CALL parin_fail_message( 'user_parameters', line )

12  CONTINUE

!
!-- Determine the number of user-defined profiles and append them to the
!-- standard data output (data_output_pr)
    IF ( user_module_enabled )  THEN
       IF ( data_output_pr_user(1) /= ' ' )  THEN
          i = 1
          DO WHILE ( data_output_pr(i) /= ' '  .AND.  i <= 100 )
             i = i + 1
          ENDDO
          j = 1
          DO WHILE ( data_output_pr_user(j) /= ' '  .AND.  j <= 100 )
             data_output_pr(i) = data_output_pr_user(j)
             max_pr_user_tmp   = max_pr_user_tmp + 1
             i = i + 1
             j = j + 1
          ENDDO
       ENDIF
    ENDIF


 END SUBROUTINE user_parin


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Check &userpar control parameters and deduce further quantities.
!------------------------------------------------------------------------------!
 SUBROUTINE user_check_parameters


!-- Here the user may add code to check the validity of further &userpar 
!-- control parameters or deduce further quantities.


 END SUBROUTINE user_check_parameters


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Set module-specific timeseries units and labels
!------------------------------------------------------------------------------!
 SUBROUTINE user_check_data_output_ts( dots_max, dots_num, dots_label, dots_unit )


    INTEGER(iwp),      INTENT(IN)     ::  dots_max
    INTEGER(iwp),      INTENT(INOUT)  ::  dots_num
    CHARACTER (LEN=*), DIMENSION(dots_max), INTENT(INOUT)  :: dots_label
    CHARACTER (LEN=*), DIMENSION(dots_max), INTENT(INOUT)  :: dots_unit

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    IF ( dots_num == 0  .OR.  dots_label(1)(1:1) == ' '  .OR.  dots_unit(1)(1:1) == ' ' )  CONTINUE

!
!-- Sample for user-defined time series
!-- For each time series quantity you have to give a label and a unit,
!-- which will be used for the NetCDF file. They must not contain more than
!-- seven characters. The value of dots_num has to be increased by the
!-- number of new time series quantities. Its old value has to be store in
!-- dots_num_palm. See routine user_statistics on how to output calculate
!-- and output these quantities.

!    dots_num_palm = dots_num

!    dots_num = dots_num + 1
!    dots_num_user = dots_num_user + 1
!    dots_label(dots_num) = 'abs_umx'
!    dots_unit(dots_num)  = 'm/s'

!    dots_num = dots_num + 1
!    dots_num_user = dots_num_user + 1
!    dots_label(dots_num) = 'abs_vmx'
!    dots_unit(dots_num)  = 'm/s'


 END SUBROUTINE user_check_data_output_ts


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Set the unit of user defined profile output quantities. For those variables
!> not recognized by the user, the parameter unit is set to "illegal", which
!> tells the calling routine that the output variable is not defined and leads
!> to a program abort.
!------------------------------------------------------------------------------!
 SUBROUTINE user_check_data_output_pr( variable, var_count, unit, dopr_unit )


    USE profil_parameter
    
    CHARACTER (LEN=*) ::  unit     !< 
    CHARACTER (LEN=*) ::  variable !< 
    CHARACTER (LEN=*),INTENT(INOUT) ::  dopr_unit !< local value of dopr_unit

    INTEGER(iwp) ::  user_pr_index !<
    INTEGER(iwp) ::  var_count     !<

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    IF ( unit(1:1) == ' '  .OR.  dopr_unit(1:1) == ' '  .OR.  var_count == 0 )  CONTINUE

    SELECT CASE ( TRIM( variable ) )

!
!--    Uncomment and extend the following lines, if necessary.
!--    Add additional CASE statements depending on the number of quantities
!--    for which profiles are to be calculated. The respective calculations
!--    to be performed have to be added in routine user_statistics.
!--    The quantities are (internally) identified by a user-profile-number
!--    (see variable "user_pr_index" below). The first user-profile must be assigned
!--    the number "pr_palm+1", the second one "pr_palm+2", etc. The respective
!--    user-profile-numbers have also to be used in routine user_statistics!

          CASE ( 'phy' )                      ! quantity string as given in
                                              ! data_output_pr_user
          user_pr_index = pr_palm + 1
          dopr_index(var_count)      = user_pr_index    ! quantities' user-profile-number
          dopr_unit                  = 'kg/m3'  ! quantity unit
          hom(:,2,user_pr_index,:)   = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)


          CASE ( 'w*phy*' )                      ! quantity string as given in
                                                 ! data_output_pr_user
          user_pr_index = pr_palm + 2
          dopr_index(var_count)      = user_pr_index    ! quantities' user-profile-number
          dopr_unit                  = 'kg/m2s'  ! quantity unit
          hom(:,2,user_pr_index,:)   = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

          CASE ( 'phy*2' )                      ! quantity string as given in
                                                ! data_output_pr_user
          user_pr_index = pr_palm + 3
          dopr_index(var_count)      = user_pr_index    ! quantities' user-profile-number
          dopr_unit                  = 'kg2/m6'  ! quantity unit
          hom(:,2,user_pr_index,:)   = SPREAD( zu, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

          CASE ( 'parc' )                      ! quantity string as given in
                                               ! data_output_pr_user
          user_pr_index = pr_palm + 4
          dopr_index(var_count)      = user_pr_index    ! quantities' user-profile-number
          dopr_unit                  = 'number of particles'  ! quantity unit
          hom(:,2,user_pr_index,:)   = SPREAD( zu, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

          CASE ( 'phy*s*' )                      ! quantity string as given in
                                                 ! data_output_pr_user
          user_pr_index = pr_palm + 5
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'number of particles'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zu, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

          CASE ( 'phy*s2*' )                      ! quantity string as given in
                                                  ! data_output_pr_user
          user_pr_index = pr_palm + 7
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'number'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zu, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)



          CASE ( 'phy2*s*' )                      ! quantity string as given in
                                                  ! data_output_pr_user
          user_pr_index = pr_palm + 8
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'number'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zu, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

          CASE ( 'phy2*w*' )                      ! quantity string as given in
                                                  ! data_output_pr_user
          user_pr_index = pr_palm + 9
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'number'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)



          CASE ( 's2*w*' )                      ! quantity string as given in
                                                ! data_output_pr_user
          user_pr_index = pr_palm + 10
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'number'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)


          CASE ( 'epsilon_RES' )                      ! quantity string as given in
                                                      ! data_output_pr_user
          user_pr_index = pr_palm + 6
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'm2/s3'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)


         CASE ( 'epsilon_SGS' )                      ! quantity string as given in
                                                     ! data_output_pr_user
          user_pr_index = pr_palm + 11
          dopr_index(var_count)       = user_pr_index    ! quantities' user-profile-number
          dopr_unit                   = 'm2/s3'  ! quantity unit
          hom(:,2,user_pr_index,:)    = SPREAD( zw, 2, statistic_regions+1 )
                                            ! grid on which the quantity is
                                            ! defined (use zu or zw)

       CASE DEFAULT
          unit = 'illegal'

    END SELECT


 END SUBROUTINE user_check_data_output_pr


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Set the unit of user defined output quantities. For those variables
!> not recognized by the user, the parameter unit is set to "illegal", which
!> tells the calling routine that the output variable is not defined and leads
!> to a program abort.
!------------------------------------------------------------------------------!
 SUBROUTINE user_check_data_output( variable, unit )


    CHARACTER (LEN=*) ::  unit     !< 
    CHARACTER (LEN=*) ::  variable !<


    SELECT CASE ( TRIM( variable ) )

!
!--    Uncomment and extend the following lines, if necessary
!       CASE ( 'u2' )
!          unit = 'm2/s2'
!
!       CASE ( 'u*v*' )
!          unit = 'm2/s2'
!
       CASE DEFAULT
          unit = 'illegal'

    END SELECT


 END SUBROUTINE user_check_data_output


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Initialize user-defined arrays
!------------------------------------------------------------------------------!
 SUBROUTINE user_init_arrays


!    INTEGER(iwp) :: i       !< loop index
!    INTEGER(iwp) :: j       !< loop index
!    INTEGER(iwp) :: region  !< index for loop over statistic regions

!
!-- Allocate user-defined arrays and set flags for statistic regions.
!-- Sample for user-defined output
!    ALLOCATE( u2(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!    ALLOCATE( ustvst(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )

!
!-- Example for defining a statistic region:
!     IF ( statistic_regions >= 1 )  THEN
!        region = 1
! 
!        rmask(:,:,region) = 0.0_wp
!        DO  i = nxl, nxr
!           IF ( i >= INT( 0.25 * nx ) .AND. i <= INT( 0.75 * nx ) )  THEN
!              DO  j = nys, nyn
!                 IF ( i >= INT( 0.25 * ny ) .AND. i <= INT( 0.75 * ny ) )  THEN
!                    rmask(j,i,region) = 1.0_wp
!                 ENDIF
!              ENDDO
!           ENDIF
!        ENDDO
! 
!     ENDIF

 END SUBROUTINE user_init_arrays


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Execution of user-defined initializing actions
!------------------------------------------------------------------------------!
 SUBROUTINE user_init

    CALL LPM_setup

 END SUBROUTINE user_init


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Set the grids on which user-defined output quantities are defined.
!> Allowed values for grid_x are "x" and "xu", for grid_y "y" and "yv", and
!> for grid_z "zu" and "zw".
!------------------------------------------------------------------------------!
 SUBROUTINE user_define_netcdf_grid( variable, found, grid_x, grid_y, grid_z )


    CHARACTER (LEN=*) ::  grid_x     !<
    CHARACTER (LEN=*) ::  grid_y     !<
    CHARACTER (LEN=*) ::  grid_z     !<
    CHARACTER (LEN=*) ::  variable   !<

    LOGICAL ::  found   !<


    SELECT CASE ( TRIM( variable ) )

!
!--    Uncomment and extend the following lines, if necessary
!       CASE ( 'u2', 'u2_xy', 'u2_xz', 'u2_yz' )
!          found  = .TRUE.
!          grid_x = 'xu'
!          grid_y = 'y'
!          grid_z = 'zu'

!       CASE ( 'u*v*', 'u*v*_xy', 'u*v*_xz', 'u*v*_yz' )
!          found  = .TRUE.
!          grid_x = 'x'
!          grid_y = 'y'
!          grid_z = 'zu'

       CASE DEFAULT
          found  = .FALSE.
          grid_x = 'none'
          grid_y = 'none'
          grid_z = 'none'

    END SELECT


 END SUBROUTINE user_define_netcdf_grid




!------------------------------------------------------------------------------!
! Description:
! ------------
!> Print a header with user-defined information.
!------------------------------------------------------------------------------!
 SUBROUTINE user_header( io )


    INTEGER(iwp) ::  i    !< 
    INTEGER(iwp) ::  io   !< 

!
!-- If no user-defined variables are read from the namelist-file, no
!-- information will be printed.
    IF ( .NOT. user_module_enabled )  THEN
       WRITE ( io, 100 )
       RETURN
    ENDIF

!
!-- Printing the information.
    WRITE ( io, 110 )

    IF ( statistic_regions /= 0 )  THEN
       WRITE ( io, 200 )
       DO  i = 0, statistic_regions
          WRITE ( io, 201 )  i, region(i)
       ENDDO
    ENDIF

!
!-- Format-descriptors
100 FORMAT (//' *** no user-defined variables found'/)
110 FORMAT (//1X,78('#')                                                       &
            //' User-defined variables and actions:'/                          &
              ' -----------------------------------'//)
200 FORMAT (' Output of profiles and time series for following regions:' /)
201 FORMAT (4X,'Region ',I1,':   ',A)


 END SUBROUTINE user_header


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Call for all grid points
!------------------------------------------------------------------------------!
 SUBROUTINE user_actions( location )


    CHARACTER (LEN=*) ::  location !< 

    INTEGER(iwp) ::  i !<
    INTEGER(iwp) ::  j !<
    INTEGER(iwp) ::  k !<

    CALL cpu_log( log_point(24), 'user_actions', 'start' )

!
!-- Here the user-defined actions follow
!-- No calls for single grid points are allowed at locations before and
!-- after the timestep, since these calls are not within an i,j-loop
    SELECT CASE ( location )

       CASE ( 'before_timestep' )


       CASE ( 'before_prognostic_equations' )


       CASE ( 'after_integration' )


       CASE ( 'after_timestep' )


       CASE ( 'u-tendency' )


       CASE ( 'v-tendency' )


       CASE ( 'w-tendency' )


       CASE ( 'pt-tendency' )
           DO i = nxlg, nxrg
               DO j = nysg, nyng

               !<KM_FLAG
                   CALL LPM_irradiance(i,j)
                   DO k = nzb, nzt
                       CALL LPM_pt_tend(k)
                       tend(k,j,i) = tend(k,j,i) + pt_tend
                   END DO

               END DO
           END DO

       CASE ( 'sa-tendency' )


       CASE ( 'e-tendency' )


       CASE ( 'q-tendency' )


       CASE ( 's-tendency' )
           DO i = nxlg, nxrg
               DO j = nysg, nyng

               !<KM_FLAG
                    DO  k = nzt, nzb+1, -1
                       CALL LPM_s_tend(i,j,k)
                       tend(k,j,i) = tend(k,j,i) + s_tend
                    ENDDO

               END DO
           END DO

       CASE DEFAULT
        

    END SELECT

    CALL cpu_log( log_point(24), 'user_actions', 'stop' )

 END SUBROUTINE user_actions


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Call for grid point i,j
!------------------------------------------------------------------------------!
 SUBROUTINE user_actions_ij( i, j, location )


    CHARACTER (LEN=*) ::  location

    INTEGER(iwp) ::  i
    INTEGER(iwp) ::  j

!
!-- Here the user-defined actions follow
    SELECT CASE ( location )

       CASE ( 'u-tendency' )

!
!--       Next line is to avoid compiler warning about unused variables. Please remove.
          IF ( i == 0  .OR.  j == 0 )  CONTINUE

!
!--       Enter actions to be done in the u-tendency term here


       CASE ( 'v-tendency' )


       CASE ( 'w-tendency' )


       CASE ( 'pt-tendency' )


       CASE ( 'sa-tendency' )


       CASE ( 'e-tendency' )


       CASE ( 'q-tendency' )


       CASE ( 's-tendency' )


       CASE DEFAULT
          CONTINUE

    END SELECT

 END SUBROUTINE user_actions_ij


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Sum up and time-average user-defined output quantities as well as allocate
!> the array necessary for storing the average.
!------------------------------------------------------------------------------!
 SUBROUTINE user_3d_data_averaging( mode, variable )


    CHARACTER (LEN=*) ::  mode    !< 
    CHARACTER (LEN=*) :: variable !< 

!    INTEGER(iwp) ::  i !<
!    INTEGER(iwp) ::  j !<
!    INTEGER(iwp) ::  k !<

    IF ( mode == 'allocate' )  THEN

       SELECT CASE ( TRIM( variable ) )

!
!--       Uncomment and extend the following lines, if necessary.
!--       The arrays for storing the user defined quantities (here u2_av) have
!--       to be declared and defined by the user!
!--       Sample for user-defined output:
!          CASE ( 'u2' )
!             IF ( .NOT. ALLOCATED( u2_av ) )  THEN
!                ALLOCATE( u2_av(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!             ENDIF
!             u2_av = 0.0_wp

          CASE DEFAULT
             CONTINUE

       END SELECT

    ELSEIF ( mode == 'sum' )  THEN

       SELECT CASE ( TRIM( variable ) )

!
!--       Uncomment and extend the following lines, if necessary.
!--       The arrays for storing the user defined quantities (here u2 and
!--       u2_av) have to be declared and defined by the user!
!--       Sample for user-defined output:
!          CASE ( 'u2' )
!             IF ( ALLOCATED( u2_av ) ) THEN
!                DO  i = nxlg, nxrg
!                   DO  j = nysg, nyng
!                      DO  k = nzb, nzt+1
!                         u2_av(k,j,i) = u2_av(k,j,i) + u2(k,j,i)
!                      ENDDO
!                   ENDDO
!                ENDDO
!             ENDIF

          CASE DEFAULT
             CONTINUE

       END SELECT

    ELSEIF ( mode == 'average' )  THEN

       SELECT CASE ( TRIM( variable ) )

!
!--       Uncomment and extend the following lines, if necessary.
!--       The arrays for storing the user defined quantities (here u2_av) have
!--       to be declared and defined by the user!
!--       Sample for user-defined output:
!          CASE ( 'u2' )
!             IF ( ALLOCATED( u2_av ) ) THEN
!                DO  i = nxlg, nxrg
!                   DO  j = nysg, nyng
!                      DO  k = nzb, nzt+1
!                         u2_av(k,j,i) = u2_av(k,j,i) / REAL( average_count_3d, KIND=wp )
!                      ENDDO
!                   ENDDO
!                ENDDO
!             ENDIF

       END SELECT

    ENDIF


 END SUBROUTINE user_3d_data_averaging


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Resorts the user-defined output quantity with indices (k,j,i) to a
!> temporary array with indices (i,j,k) and sets the grid on which it is defined.
!> Allowed values for grid are "zu" and "zw".
!------------------------------------------------------------------------------!
 SUBROUTINE user_data_output_2d( av, variable, found, grid, local_pf, two_d, nzb_do, nzt_do )


    CHARACTER (LEN=*) ::  grid     !< 
    CHARACTER (LEN=*) ::  variable !< 

    INTEGER(iwp) ::  av     !< flag to control data output of instantaneous or time-averaged data
!    INTEGER(iwp) ::  i      !< grid index along x-direction
!    INTEGER(iwp) ::  j      !< grid index along y-direction
!    INTEGER(iwp) ::  k      !< grid index along z-direction
!    INTEGER(iwp) ::  m      !< running index surface elements
    INTEGER(iwp) ::  nzb_do !< lower limit of the domain (usually nzb)
    INTEGER(iwp) ::  nzt_do !< upper limit of the domain (usually nzt+1)

    LOGICAL      ::  found !< 
    LOGICAL      ::  two_d !< flag parameter that indicates 2D variables (horizontal cross sections)

!    REAL(wp) ::  fill_value = -999.0_wp    !< value for the _FillValue attribute

    REAL(wp), DIMENSION(nxl:nxr,nys:nyn,nzb_do:nzt_do) ::  local_pf !< 

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    IF ( av == 0  .OR.  local_pf(nxl,nys,nzb_do) == 0.0_wp  .OR.  two_d )  CONTINUE


    found = .TRUE.

    SELECT CASE ( TRIM( variable ) )

!
!--    Uncomment and extend the following lines, if necessary.
!--    The arrays for storing the user defined quantities (here u2 and u2_av)
!--    have to be declared and defined by the user!
!--    Sample for user-defined output:
!       CASE ( 'u2_xy', 'u2_xz', 'u2_yz' )
!          IF ( av == 0 )  THEN
!             DO  i = nxl, nxr
!                DO  j = nys, nyn
!                   DO  k = nzb_do, nzt_do
!                      local_pf(i,j,k) = u2(k,j,i)
!                   ENDDO
!                ENDDO
!             ENDDO
!          ELSE
!             IF ( .NOT. ALLOCATED( u2_av ) ) THEN
!                ALLOCATE( u2_av(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!                u2_av = REAL( fill_value, KIND = wp )
!             ENDIF
!             DO  i = nxl, nxr
!                DO  j = nys, nyn
!                   DO  k = nzb_do, nzt_do
!                      local_pf(i,j,k) = u2_av(k,j,i)
!                   ENDDO
!                ENDDO
!             ENDDO
!          ENDIF
!
!          grid = 'zu'
!
!--    In case two-dimensional surface variables are output, the user
!--    has to access related surface-type. Uncomment and extend following lines
!--    appropriately (example output of vertical surface momentum flux of u-
!--    component). Please note, surface elements can be distributed over 
!--    several data type, depending on their respective surface properties.
!       CASE ( 'usws_xy' )
!          IF ( av == 0 )  THEN
!
!--           Horizontal default-type surfaces
!             DO  m = 1, surf_def_h(0)%ns
!                i = surf_def_h(0)%i(m)
!                j = surf_def_h(0)%j(m)
!                local_pf(i,j,1) = surf_def_h(0)%usws(m)
!             ENDDO
!
!--           Horizontal natural-type surfaces
!             DO  m = 1, surf_lsm_h%ns
!                i = surf_lsm_h%i(m)
!                j = surf_lsm_h%j(m)
!                local_pf(i,j,1) = surf_lsm_h%usws(m)
!             ENDDO
!
!--           Horizontal urban-type surfaces
!             DO  m = 1, surf_usm_h%ns
!                i = surf_usm_h%i(m)
!                j = surf_usm_h%j(m)
!                local_pf(i,j,1) = surf_usm_h%usws(m)
!             ENDDO
!          ENDIF
!
!          grid = 'zu'
!--       


       CASE DEFAULT
          found = .FALSE.
          grid  = 'none'

    END SELECT


 END SUBROUTINE user_data_output_2d


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Resorts the user-defined output quantity with indices (k,j,i) to a
!> temporary array with indices (i,j,k).
!------------------------------------------------------------------------------!
 SUBROUTINE user_data_output_3d( av, variable, found, local_pf, nzb_do, nzt_do )


    CHARACTER (LEN=*) ::  variable !< 

    INTEGER(iwp) ::  av    !< 
!    INTEGER(iwp) ::  i     !<
!    INTEGER(iwp) ::  j     !<
!    INTEGER(iwp) ::  k     !<
    INTEGER(iwp) ::  nzb_do !< lower limit of the data output (usually 0)
    INTEGER(iwp) ::  nzt_do !< vertical upper limit of the data output (usually nz_do3d)

    LOGICAL      ::  found !< 

!    REAL(wp) ::  fill_value = -999.0_wp    !< value for the _FillValue attribute

    REAL(sp), DIMENSION(nxl:nxr,nys:nyn,nzb_do:nzt_do) ::  local_pf !< 

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    IF ( av == 0  .OR.  local_pf(nxl,nys,nzb_do) == 0.0_wp )  CONTINUE


    found = .TRUE.

    SELECT CASE ( TRIM( variable ) )

!
!--    Uncomment and extend the following lines, if necessary.
!--    The arrays for storing the user defined quantities (here u2 and u2_av)
!--    have to be declared and defined by the user!
!--    Sample for user-defined output:
!       CASE ( 'u2' )
!          IF ( av == 0 )  THEN
!             DO  i = nxl, nxr
!                DO  j = nys, nyn
!                   DO  k = nzb_do, nzt_do
!                      local_pf(i,j,k) = u2(k,j,i)
!                   ENDDO
!                ENDDO
!             ENDDO
!          ELSE
!             IF ( .NOT. ALLOCATED( u2_av ) ) THEN
!                ALLOCATE( u2_av(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!                u2_av = REAL( fill_value, KIND = wp )
!             ENDIF
!             DO  i = nxl, nxr
!                DO  j = nys, nyn
!                   DO  k = nzb_do, nzt_do
!                      local_pf(i,j,k) = u2_av(k,j,i)
!                   ENDDO
!                ENDDO
!             ENDDO
!          ENDIF
!

       CASE DEFAULT
          found = .FALSE.

    END SELECT


 END SUBROUTINE user_data_output_3d


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Calculation of user-defined statistics, i.e. horizontally averaged profiles
!> and time series.
!> This routine is called for every statistic region sr defined by the user,
!> but at least for the region "total domain" (sr=0).
!> See section 3.5.4 on how to define, calculate, and output user defined
!> quantities.
!------------------------------------------------------------------------------!
 SUBROUTINE user_statistics( mode, sr, tn )


    CHARACTER (LEN=*) ::  mode   !< 
    INTEGER(iwp) ::  i    !<
    INTEGER(iwp) ::  j    !<
    INTEGER(iwp) ::  k    !<
    INTEGER(iwp) ::  sr   !< 
    INTEGER(iwp) ::  tn   !< 


    !<KM_FLAGS
    REAL(wp)     ::  CHL
    REAL(wp)     ::  l(nzb+1:nzt), ll(nzb+1:nzt)
    REAL(wp)     ::  dprho_dz, l_stable, l_grid, um, vm, up, vp

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    IF ( sr == 0  .OR.  tn == 0 )  CONTINUE

    IF ( mode == 'profiles' )  THEN

       DO  i = nxl, nxr
          DO  j = nys, nyn
             DO  k = nzb+1, nzt
!


             number_of_particles=prt_count(k,j,i)
             particles => grid_particles(k,j,i)%particles(1:number_of_particles)
             
             !<Volume of sphere * density of P * initial weight
             CHL  =  SUM(particles(1:number_of_particles)%weight_factor) 
             CHL  =  CHL * radius**3.0 * (4.0/3.0) * pi * 1030.0

!--             Sample on how to calculate the profile of the resolved-scale
!--             horizontal momentum flux u*v*
!               <P>
                sums_l(k,pr_palm+1,tn) = sums_l(k,pr_palm+1,tn) +             &
                           (CHL)                                             &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )

            ENDDO
         ENDDO
      ENDDO

      CALL MPI_ALLREDUCE( sums_l(nzb,pr_palm+1,tn), sums(nzb,pr_palm+1), nzt+2-nzb, MPI_REAL,  &
                          MPI_SUM, comm2d, ierr )


      sums(:,pr_palm+1)      =  sums(:,pr_palm+1) / ngp_2dh(sr)
      hom(:,1,pr_palm+1,sr)  =  sums(:,pr_palm+1)


!
!--    Sample on how to calculate horizontally averaged profiles of user-
!--    defined quantities. Each quantity is identified by the index
!--    "pr_palm+#" where "#" is an integer starting from 1. These
!--    user-profile-numbers must also be assigned to the respective strings
!--    given by data_output_pr_user in routine user_check_data_output_pr.
       !$OMP DO
       DO  i = nxl, nxr
          DO  j = nys, nyn
             DO  k = nzb+1, nzt
!


                number_of_particles=prt_count(k,j,i)
                particles => grid_particles(k,j,i)%particles(1:number_of_particles)

                CHL  =  SUM(particles(1:number_of_particles)%weight_factor) 
                CHL  =  CHL * radius**3.0 * (4.0/3.0) * pi * 1030.0

!--             Sample on how to calculate the profile of the resolved-scale
!--             horizontal momentum flux u*v*

!               <P'w'>
                sums_l(k,pr_palm+2,tn) = sums_l(k,pr_palm+2,tn) +             &
                   (CHL-hom(k,1,pr_palm+1,sr)) *0.5*(w(k,j,i)+w(k-1,j,i))     &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )

!               <P'^2>   (Needed for <P'^2> calc)
                sums_l(k,pr_palm+3,tn) = sums_l(k,pr_palm+3,tn) +             &
                    (CHL-hom(k,1,pr_palm+1,sr))**2.0                          &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )
!               <Pc>
                sums_l(k,pr_palm+4,tn) = sums_l(k,pr_palm+4,tn) +             &
                           (number_of_particles)                              &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )
!               <P'N'>
                sums_l(k,pr_palm+5,tn) = sums_l(k,pr_palm+5,tn) +             &
                    (CHL-hom(k,1,pr_palm+1,sr))*(s(k,j,i)-hom(k,1,115,sr))    &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )

!              <P'N'2>
               sums_l(k,pr_palm+7,tn) = sums_l(k,pr_palm+7,tn) +              &
               (CHL-hom(k,1,pr_palm+1,sr))*((s(k,j,i)-hom(k,1,115,sr))**2.0)  &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )


!              <P'2N'>
               sums_l(k,pr_palm+8,tn) = sums_l(k,pr_palm+8,tn) +              &
               ((CHL-hom(k,1,pr_palm+1,sr))**2.0)*(s(k,j,i)-hom(k,1,115,sr))  &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )


!             <P'2w'>
              sums_l(k,pr_palm+9,tn) = sums_l(k,pr_palm+9,tn) +               &
                ((CHL-hom(k,1,pr_palm+1,sr))**2.0) *0.5*(w(k,j,i)+w(k-1,j,i)) &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )


!             <N'2w'>
              sums_l(k,pr_palm+10,tn) = sums_l(k,pr_palm+10,tn) +             &
                ((s(k,j,i)-hom(k,1,115,sr))**2.0) *0.5*(w(k,j,i)+w(k-1,j,i))  &
                                     * rmask(j,i,sr)                          &
                                     * MERGE( 1.0_wp, 0.0_wp,                 &
                                              BTEST( wall_flags_total_0(k,j,i), 0 ) )

             ENDDO
          ENDDO
       ENDDO


!-- Calculate the dissipation rate of the resolved scale flow and of the subgrid scale
    DO  i = nxl, nxr
       DO  j = nys, nyn

!--       Calculate the mixing length (for SGS dissipation)
        DO  k = nzt, nzb+1, -1
           l_grid = ( dx * dy * dzw(k) )**0.33333333333333_wp
           dprho_dz = -( prho(k+1,j,i) - prho(k-1,j,i) ) * dd2zu(k)
           IF ( dprho_dz > 0.0_wp ) THEN
              l_stable = 0.76_wp * SQRT( e(k,j,i) ) / &
                                   SQRT( g / prho(k,j,i) * dprho_dz ) + 1e-5_wp
           ELSE
              l_stable = l_grid
           ENDIF
           l(k) = MIN( l_grid, l_stable )
           ll(k) = l_grid
        ENDDO

        DO  k = nzt, nzb+1, -1

            up = sums(k+1,1)
            um = sums(k-1,1)
            vp = sums(k+1,2)
            vm = sums(k-1,2)

             sums_l(k,pr_palm + 6,tn) = sums_l(k,pr_palm + 6,tn) + 1.8E-6_wp * (&
                + 0.5_wp * ( ( ( v(k,j,i+1)   - v(k,j,i-1)   ) * 0.5_wp )**2 &
                           + ( ( v(k,j+1,i+1) - v(k,j+1,i-1) ) * 0.5_wp )**2 &
                           )&
                + 0.5_wp * ( ( ( w(k,j,i+1)   - w(k,j,i-1)   ) * 0.5_wp )**2 &
                           + ( ( w(k-1,j,i+1) - w(k-1,j,i-1) ) * 0.5_wp )**2 &
                           )&
                + 0.5_wp * ( ( ( u(k,j+1,i)   - u(k,j-1,i)   ) * 0.5_wp )**2 &
                           + ( ( u(k,j+1,i+1) - u(k,j-1,i+1) ) * 0.5_wp )**2 &
                           )&
                   +         ( ( v(k,j+1,i)   - v(k,j,i)     ) )**2 &
                + 0.5_wp * ( ( ( w(k,j+1,i)   - w(k,j-1,i)   ) * 0.5_wp )**2 &
                           + ( ( w(k-1,j+1,i) - w(k-1,j-1,i) ) * 0.5_wp )**2 &
                           )&
         + 0.5_wp * ( ( ( u(k+1,j,i)   - up - u(k-1,j,i)   + um ) * dd2zu(k))**2 &
                    + ( ( u(k+1,j,i+1) - up - u(k-1,j,i+1) + um ) * dd2zu(k))**2 &
                    )&
         + 0.5_wp * ( ( ( v(k+1,j,i)   - vp - v(k-1,j,i)   + vm ) * dd2zu(k))**2 &
                    + ( ( v(k+1,j+1,i) - vp - u(k-1,j+1,i) + vm ) * dd2zu(k))**2 &
                    )&
                 +           ( ( w(k,j,i)     - w(k-1,j,i)   ) * ddzw(k))**2 &
                           )

             sums_l(k,pr_palm + 11,tn) = sums_l(k,pr_palm + 11,tn)              &
                                   + ( 0.19_wp + 0.74_wp*l(k) / ll(k) )         &
                                   * e(k,j,i) * SQRT( e(k,j,i) ) / l(k)

        ENDDO
     ENDDO
  ENDDO


    sums_l(nzt+1,pr_palm + 6,tn) = sums_l(nzt,pr_palm + 6,tn)   ! Neumann boundarycondition
!    sums_l(nzb,pr_palm + 8,tn) = sums_l(nzb+1,pr_palm + 8,tn)   ! Neumann boundarycondition
!    sums_l(nzt+1,pr_palm + 8,tn) = sums_l(nzt,pr_palm + 8,tn)   ! Neumann boundarycondition
!    sums_l(:,pr_palm + 6,tn) = sums_l(:,pr_palm + 7,tn) + sums_l(:,pr_palm + 8,tn)


    ELSEIF ( mode == 'time_series' )  THEN

    ENDIF

 END SUBROUTINE user_statistics


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Reading global restart data that has been defined by the user.
!------------------------------------------------------------------------------!
 SUBROUTINE user_rrd_global( found )


    LOGICAL, INTENT(OUT)  ::  found


    found = .TRUE.


    SELECT CASE ( restart_string(1:length) )

       CASE ( 'global_paramter' )
!          READ ( 13 )  global_parameter

       CASE DEFAULT
 
          found = .FALSE.

    END SELECT


 END SUBROUTINE user_rrd_global


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Reading processor specific restart data from file(s) that has been defined 
!> by the user.
!> Subdomain index limits on file are given by nxl_on_file, etc.
!> Indices nxlc, etc. indicate the range of gridpoints to be mapped from the
!> subdomain on file (f) to the subdomain of the current PE (c). They have been
!> calculated in routine rrd_local.
!------------------------------------------------------------------------------!
 SUBROUTINE user_rrd_local( k, nxlf, nxlc, nxl_on_file, nxrf, nxrc,         &
                            nxr_on_file, nynf, nync, nyn_on_file, nysf,     &
                            nysc, nys_on_file, tmp_3d, found )


    INTEGER(iwp) ::  idum            !<
    INTEGER(iwp) ::  k               !<
    INTEGER(iwp) ::  nxlc            !<
    INTEGER(iwp) ::  nxlf            !<
    INTEGER(iwp) ::  nxl_on_file     !<
    INTEGER(iwp) ::  nxrc            !<
    INTEGER(iwp) ::  nxrf            !<
    INTEGER(iwp) ::  nxr_on_file     !<
    INTEGER(iwp) ::  nync            !<
    INTEGER(iwp) ::  nynf            !<
    INTEGER(iwp) ::  nyn_on_file     !<
    INTEGER(iwp) ::  nysc            !<
    INTEGER(iwp) ::  nysf            !<
    INTEGER(iwp) ::  nys_on_file     !<

    LOGICAL, INTENT(OUT)  ::  found

    REAL(wp), DIMENSION(nzb:nzt+1,nys_on_file-nbgp:nyn_on_file+nbgp,nxl_on_file-nbgp:nxr_on_file+nbgp) :: tmp_3d   !<

!
!-- Next line is to avoid compiler warning about unused variables. Please remove.
    idum = k + nxlc + nxlf + nxrc + nxrf + nync + nynf + nysc + nysf +                             &
           INT( tmp_3d(nzb,nys_on_file,nxl_on_file) )

!
!-- Here the reading of user-defined restart data follows:
!-- Sample for user-defined output

    found = .TRUE.

    SELECT CASE ( restart_string(1:length) )

       CASE ( 'u2_av' )
!          IF ( .NOT. ALLOCATED( u2_av ) ) THEN
!               ALLOCATE( u2_av(nzb:nzt+1,nysg:nyng,nxlg:nxrg) )
!          ENDIF
!          IF ( k == 1 )  READ ( 13 )  tmp_3d
!             u2_av(:,nysc-nbgp:nync+nbgp,nxlc-nbgp:nxrc+nbgp) =         &
!                tmp_3d(:,nysf-nbgp:nynf+nbgp,nxlf-nbgp:nxrf+nbgp)
!
       CASE DEFAULT

          found = .FALSE.

    END SELECT

 END SUBROUTINE user_rrd_local


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Writes global and user-defined restart data into binary file(s) for restart
!> runs.
!------------------------------------------------------------------------------!
 SUBROUTINE user_wrd_global

!    CALL wrd_write_string( 'global_parameter' )
!    WRITE ( 14 )  global_parameter

 END SUBROUTINE user_wrd_global


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Writes processor specific and user-defined restart data into binary file(s) 
!> for restart runs.
!------------------------------------------------------------------------------!
 SUBROUTINE user_wrd_local

!
!-- Here the user-defined actions at the end of a job follow.
!-- Sample for user-defined output:
!    IF ( ALLOCATED( u2_av ) )  THEN
!       CALL wrd_write_string( 'u2_av' )
!       WRITE ( 14 )  u2_av
!    ENDIF

 END SUBROUTINE user_wrd_local


!------------------------------------------------------------------------------!
! Description:
! ------------
!> Execution of user-defined actions at the end of a job.
!------------------------------------------------------------------------------!
 SUBROUTINE user_last_actions

!
!-- Here the user-defined actions at the end of a job might follow.


 END SUBROUTINE user_last_actions

 END MODULE user
