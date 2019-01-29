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
          
          USE indices,                                                        &
            ONLY: nzb, nzt

          USE kinds,                                                           &
            ONLY: wp, iwp

          USE pegrid,                                                          &
            ONLY: myid

          USE particle_attributes,                                             &
            ONLY: grid_particles, number_of_particles, particles,              &
                  particle_advection_start, prt_count

          USE control_parameters,                                              &
            ONLY: simulated_time 

          IMPLICIT NONE
          
          REAL(wp)    ::  D1, G1, K1, growth, death, penetration_depth
          REAL(wp)    ::  time_season_change, time_self_shading
          REAL(wp)    ::  solar 

          REAL(wp),DIMENSION(:),ALLOCATABLE   :: radpen, CHL 
          
          SAVE

        CONTAINS
        
        END MODULE plankton_model
