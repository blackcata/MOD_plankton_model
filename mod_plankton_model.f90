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
            REAL(wp)    ::  solar, pt_tend

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
            D1  =  (death  / 86400.0)          ! Death rate in seconds
            K1  =  (1.0 / penetration_depth)   ! Light attenuation (1/m) 

            DO k = nzb, nzt
                CHL(k)    =  0.0
                light(k)  =  exp(K1 * zu(k))
            END DO

        END SUBROUTINE LPM_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_irradiance                                                !
!                                                                              !
!   PURPOSE : Determine the irradiance of each ocena layer                     !
!             Radiation penetration is determined with phytoplankton           !
!                                                                              !
!   REFERENCE : Manniza et al., 2005 GRL                                       !
!                                                                              !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        SUBROUTINE LPM_irradiance(i,j)
            IMPLICIT NONE

            INTEGER(iwp) ::  i, j, k 
            INTEGER(iwp) ::  L_IR, K_IR, L_VIS, K_VIS, L_R, K_R, L_B, K_B
            REAL(wp)     ::  count_z, tot_vol, pi= 3.141592654_wp 

            count_z = 0.0

            DO k = nzt,nzb,-1

                count_z = count_z + 1 

                !<Infrared ray effect
                L_IR = 0.58   !  ( I_IR = I_0 * 0.58  W/m^2) 
                K_IR = 2.86   !  ( 1/m )
                
                IF (simulated_time < time_self_shading ) THEN 
                !< Self Shading Effect Off
                    !<Visibile ray effect
                    L_VIS = 0.42  ! ( I_VIS = I_0 * 0.42 W/m^2 )
                    K_VIS = 0.044 ! ( 1/m ) 
                    
                    radpen(k)  =  L_IR  * exp(zu(k) * K_IR) & 
                               +  L_VIS * exp(zu(k) * K_VIS)  
                ELSE 
                !< Self Shading Effect On    

                    !<Calculating the chlorophyll concentration
                    number_of_particles=prt_count(k,j,i)
                    particles => grid_particles(k,j,i)%particles(1:number_of_particles)
                    tot_vol= SUM( (4.0/3.0)*pi*particles(1:number_of_particles)%radius**3.0 )
                    tot_vol= tot_vol*1030.0*1.0e6

                    IF (k == nzt) THEN 
                        CHL(k)  =  tot_vol
                    ELSE
                        CHL(k)  = (tot_vol + CHL(k-1)*(count_z-1)) / count_z
                    END IF

                    !<Red light effect
                    L_R  =  L_VIS / 2.0  ! 
                    K_R  =  0.225  +  0.037 * CHL(k) ** 0.629

                    !<Blue light effect
                    L_B  =  L_VIS / 2.0 
                    K_B  =  0.0232 +  0.074 * CHL(k) ** 0.674

                    radpen(k)  =  L_IR * exp(zu(k) * K_IR) &
                               +  L_R  * exp(zu(k) * K_R)  & 
                               +  L_B  * exp(zu(k) * K_B)  
                END IF 

            END DO 

        END SUBROUTINE LPM_irradiance

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_pt_tend                                                   !
!                                                                              !
!   PURPOSE : Determine the irradiance of each ocena layer                     !
!                                                                              !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        SUBROUTINE LPM_pt_tend(k)
            IMPLICIT NONE

            INTEGER(iwp)    :: k
            REAL(wp)        :: pi = 3.141592654_wp

            !<400W/m2 * max(0, sin(2 pi T) + 0.25)
            solar = 0.96e-4_wp *                                               &
                  max(0.0,sin(2.0_wp*pi*simulated_time/86400.0_wp) + 0.25_wp)

            IF (k == nzt) THEN 
                IF (simulated_time < time_season_change) THEN 
                !<COOLING daily average 81 W/m2 (WINTER)
                    pt_tend  =  - 0.63e-4_wp*radpen(k)                         &
                                + solar * (radpen(k) - radpen(k-1))
                ELSE
                !<HEATING daily average 81 W/m2 (SUMMER)
                    pt_tend  =  - 0.24e-4_wp*radpen(k)                         &
                                + solar * (radpen(k) - radpen(k-1))
                END IF
            ELSEIF (k == nzt) THEN 
                pt_tend  =  solar * (radpen(k+1) - radpen(k))
            ELSE
                pt_tend  =  solar * 0.5 * (radpen(k+1) - radpen(k-1))
            ENDIF
                
        END SUBROUTINE LPM_pt_tend

        END MODULE plankton_model
