!------------------------------------------------------------------------------!
!                                                                              !
!   PROGRAM : mod_plankton_model.f90                                           !
!                                                                              !
!   PURPOSE : Module for Lagrangian plankton model                             !
!                                                                              !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!


        MODULE plankton_model
          
            USE indices,                                                       &
              ONLY: nzb, nzt

            USE kinds,                                                         &
              ONLY: wp, iwp

            USE pegrid,                                                        &
              ONLY: myid

            USE particle_attributes,                                           &
              ONLY: grid_particles, number_of_particles, particles,            &
                    particle_advection_start, prt_count

            USE arrays_3d,                                                     &
              ONLY: zu

            USE control_parameters,                                            &
              ONLY: simulated_time 

            IMPLICIT NONE
          
            REAL(wp)    ::  D1, G1, K1, growth, death, penetration_depth
            REAL(wp)    ::  time_season_change, time_self_shading
            REAL(wp)    ::  solar 

            REAL(wp),DIMENSION(:),ALLOCATABLE   :: radpen, light, CHL
          
            SAVE

        CONTAINS
        
!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_setup                                                     !
!                                                                              !
!   PURPOSE : Setup for the Lagrangian Plankton Model                          !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        SUBROUTINE LPM_setup 
            IMPLICIT NONE

            INTEGER(iwp)  :: k 

            ALLOCATE( light(nzb:nzt+1) )
            ALLOCATE( radpen(nzb:nzt) )
            ALLOCATE( CHL(nzb:nzt) )

            time_season_change = 172800.0    ! The time when sesason changes 
            time_self_shading  = 180000000.0 ! The time when self shading active
            growth             =      2.0    ! Plankton max growth rate (1/day)
            death              =      0.1    ! Plankton death rate      (1/day)
            penetration_depth  =     10.0    ! Radiation penetration depth (m)

            G1  =  (growth / 86400.0) / 6.2e-5 ! Growth rate in seconds /(N0 or P0)
            D1  =  (death  / 86400.0)
            K1  =  (1.0    / penetration_depth) 

            DO k = nzb, nzt
                CHL(k)    =  0.0
                light(k)  =  exp(K1 * zu(k))
            END DO

        END SUBROUTINE LPM_setup

        END MODULE plankton_model
