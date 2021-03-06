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
              ONLY:  s, ddzw, zw

            USE control_parameters,                                            &
              ONLY: simulated_time, dt_3d, rho_surface, initializing_actions

            USE statistics,                                                    &
              ONLY: hom 

            IMPLICIT NONE
          
            LOGICAL     ::  nutrient_interaction, dirunal_variation
            LOGICAL     ::  simple_penetration

            REAL(wp)    ::  cpw, Q0_heat, Q0_shift, Q0_shift_summer, Q0_weight
            REAL(wp)    ::  Q0_cool_summer, Q0_cool_winter
            REAL(wp)    ::  D1, G1, K1, growth, death, penetration_depth
            REAL(wp)    ::  time_season_change, time_self_shading
            REAL(wp)    ::  solar, pt_tend, s_tend

            REAL(wp),DIMENSION(:),ALLOCATABLE   :: radpen, CHL
          
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

              IF ( initializing_actions == 'read_restart_data' ) THEN 
                  CONTINUE
              ELSE
                  ALLOCATE ( radpen(nzb:nzt) ) 
                  ALLOCATE ( CHL(nzb:nzt) )
              END IF 

              simple_penetration      =  .TRUE.
              nutrient_interaction    =  .FALSE.
              dirunal_variation       =  .FALSE.

              cpw  =  4218.0_wp ! Heat capacity of water at constant pressure (J/kg/K)

              Q0_weight       =  1.0    ! Weight of Surface buoyancy flux 
              Q0_heat         =  400    ! Surface heating buoyancy flux (W/m^2)
              Q0_cool_summer  =  100    ! Surface cooling buoyancy flux (W/m^2)
              Q0_cool_winter  =  263    ! Surface cooling buoyancy flux (W/m^2)
              Q0_shift        =  163    ! Surface buoyancy flux shift (W/m^2)
              Q0_shift_summer =  80.029 ! Surface buoyancy flux shift (W/m^2)

              time_season_change = 172800.0    ! The time when sesason changes 
              time_self_shading  = 180000000.0 ! The time when self shading active
              growth             =      0.5    ! Plankton max growth rate (1/day)
              death              =      0.1    ! Plankton death rate      (1/day)
              penetration_depth  =     10.0    ! Radiation penetration depth (m)

              IF ( dirunal_variation ) growth  =  growth * 2.0_wp

              G1  =  (growth / 86400.0) / 6.2e-5 ! Growth rate in seconds /(N0 or P0)
              D1  =  (death  / 86400.0)          ! Death rate in seconds
              K1  =  (1.0 / penetration_depth)   ! Light attenuation (1/m) 

              DO k = nzb, nzt
                  CHL(k)    =  0.0
                  radpen(k) =  0.0
            END DO

        END SUBROUTINE LPM_setup

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_irradiance                                                !
!                                                                              !
!   PURPOSE : Determine the irradiance of each ocean layer                     !
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
            REAL(wp)     ::  L_IR, K_IR, L_VIS, K_VIS, L_R, K_R, L_B, K_B
            REAL(wp)     ::  count_z, tot_vol, pi= 3.141592654_wp 

            count_z = 0.0

            DO k = nzt,nzb,-1

                count_z = count_z + 1 

                !<Infrared ray effect
                L_IR = 0.58   !  ( I_IR = I_0 * 0.58  W/m^2) 
                K_IR = 2.86   !  ( 1/m )
                
                IF (simulated_time < time_self_shading) THEN

                    IF (simple_penetration) THEN
                    !< Simple penetration without dividing visible & infrared ray
                        radpen(k)  =  exp(K1 * zw(k))
                    ELSE
                    !< Self Shading Effect Off
                        !<Visibile ray effect
                        L_VIS = 0.42   ! ( I_VIS = I_0 * 0.42 W/m^2 )
                        K_VIS = 0.0434 ! ( 1/m ) 
                        
                        radpen(k)  =  L_IR  * exp(zw(k) * K_IR)                 &
                                   +  L_VIS * exp(zw(k) * K_VIS)  
                    END IF 

                END IF 

            END DO 

        END SUBROUTINE LPM_irradiance

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_pt_tend                                                   !
!                                                                              !
!   PURPOSE : Determine the temperature tendency by radiation & phytoplankton  !
!                                                                              !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        SUBROUTINE LPM_pt_tend(k)
            IMPLICIT NONE

            INTEGER(iwp)    :: k
            REAL(wp)        :: pi = 3.141592654_wp, cooling, heating

            !<Dirunal variation setup
            IF (dirunal_variation) THEN
                IF (simulated_time < time_season_change) THEN
                !<COOLING daily average Q0 = -81 W/m2 (WINTER)
                    cooling  =  - Q0_cool_winter
                    heating  =  Q0_heat*sin(2.0_wp*pi*simulated_time/86400.0_wp)&
                                - Q0_shift
                ELSE
                !<HEATING daily average Q0 = +81 W/m2 (SUMMER)
                   cooling  =  - Q0_cool_summer - Q0_shift_summer
                   heating  =  Q0_heat*sin(2.0_wp*pi*simulated_time/86400.0_wp) &
                                - Q0_shift_summer
                END IF

                solar  =  Q0_weight * max(heating, cooling) / (cpw*rho_surface)
            ELSE 
                !<COOLING daily average -81 W/m2 (WINTER)
                IF (simulated_time < time_season_change) THEN
                    cooling  =  - Q0_cool_winter
                    heating  =  Q0_heat*sin(2.0_wp*pi*simulated_time/86400.0_wp)&
                                - Q0_shift
                    solar  =  Q0_weight * max(heating, cooling) / (cpw*rho_surface)
                !<HEATING daily average +81 W/m2 (SUMMER)
                ELSE
                    solar  = 1.5*81.3_wp  / (cpw*rho_surface)
                END IF

            END IF 

            !<Calculate the potential temperature tendency with radiation fluxes
            IF (k == nzt) THEN 
                pt_tend  =  solar * radpen(k) * ddzw(k)
            ELSEIF (k == nzb) THEN 
                pt_tend  =  - solar * radpen(k-1) * ddzw(k)
            ELSE
                pt_tend = 0.0
            END IF
           

        END SUBROUTINE LPM_pt_tend

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_s_tend                                                    !
!                                                                              !
!   PURPOSE : Determine the nutrient tendency by radiation & phytoplankton     !
!                                                                              !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!
        SUBROUTINE LPM_s_tend(i,j,k)
            IMPLICIT NONE

            INTEGER(iwp)    :: i, j, k
            REAL(wp)        :: PHY_CONC, tot_vol, pi = 3.141592654_wp
            
            IF (nutrient_interaction) THEN

                IF (simulated_time > particle_advection_start) THEN
                !<Calculating the chlorophyll concentration
                    number_of_particles=prt_count(k,j,i)
                    particles => grid_particles(k,j,i)%particles(1:number_of_particles)
                    tot_vol   = SUM( (4.0/3.0)*pi*particles(1:number_of_particles)%radius**3.0 )
                    PHY_CONC  = tot_vol*1030.0
                ELSE
                    PHY_CONC  = 0.0
                END IF

                s_tend  =  ( -G1 * s(k,j,i) * radpen(k) + D1 ) * PHY_CONC
            ELSE
                s_tend  =  0.0
            END IF


        END SUBROUTINE LPM_s_tend

!------------------------------------------------------------------------------!
!                                                                              !
!   SUBROUTINE : LPM_phy_tend                                                  !
!                                                                              !
!   PURPOSE : Determine the phytoplankton particle tendency                    !
!                      by the nutrient and the radiation                       !
!                                                             2019.01.30 K.Noh !
!                                                                              !
!------------------------------------------------------------------------------!

          SUBROUTINE LPM_phy_tend(ip,jp,kp)
             IMPLICIT NONE
 
             INTEGER(iwp) ::  ip  !< index of particle grid box, x-direction
             INTEGER(iwp) ::  jp  !< index of particle grid box, y-direction
             INTEGER(iwp) ::  kp  !< index of particle grid box, z-direction
             INTEGER(iwp) ::  n   !< particle index
             INTEGER(iwp) ::  nb  !< index of sub-box particles are sorted in
             INTEGER(iwp) ::  pn  !< the number of particles want to track

             INTEGER(iwp) ::  subbox_start   !< start index for loop over subbox
             INTEGER(iwp) ::  subbox_end     !< end index for loop over subboxes in particle advection 
             INTEGER(iwp) ::  particle_start !< start index for particle loop
             INTEGER(iwp) ::  particle_end   !< end index for particle loop

             INTEGER(iwp), DIMENSION(0:7)  ::  start_index !< start particle index for current sub-box
             INTEGER(iwp), DIMENSION(0:7)  ::  end_index   !< start particle index for current sub-box

             REAL(wp)     :: net_growth !< parameters for plankton growth

             number_of_particles = prt_count(kp,jp,ip)
             particles => grid_particles(kp,jp,ip)%particles(1:number_of_particles)

             start_index = grid_particles(kp,jp,ip)%start_index
             end_index   = grid_particles(kp,jp,ip)%end_index

             subbox_start  =  1
             subbox_end    =  1 
             
             DO  nb = subbox_start,subbox_end

                particle_start   =  1
                particle_end     =  number_of_particles

                DO  n = particle_start, particle_end
                !<Phytosynthesis is only active when the radiation is available
                    IF (solar >= 0.0) THEN 
                        net_growth  =  G1 * s(kp,jp,ip) * radpen(kp) - D1
                    ELSE 
                        net_growth  =  - D1
                    END IF 
                    
                    IF (simulated_time > particle_advection_start) THEN 
                        particles(n)%weight_factor=particles(n)%weight_factor*  &
                                            (1.0 + dt_3d*net_growth)
                    END IF
                ENDDO

             ENDDO

          END SUBROUTINE LPM_phy_tend
        END MODULE plankton_model
