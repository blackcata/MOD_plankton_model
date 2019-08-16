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
              ONLY: simulated_time, dt_3d, rho_surface

            IMPLICIT NONE
          
            LOGICAL     ::  nutrient_interaction, dirunal_variation
            LOGICAL     ::  simple_penetration, par_interpolation

            REAL(wp)    ::  cpw, Q0_heat, Q0_shift
            REAL(wp)    ::  Q0_cool_summer, Q0_cool_winter
            REAL(wp)    ::  D1, G1, K1, growth, death, penetration_depth
            REAL(wp)    ::  time_season_change, time_self_shading
            REAL(wp)    ::  solar, pt_tend, s_tend
            REAL(wp)    ::  alpha_ip, w2_thres

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

            simple_penetration    =  .FALSE.
            nutrient_interaction  =  .FALSE.
            dirunal_variation     =  .TRUE.
            par_interpolation     =  .TRUE. 

            alpha_ip  =  0.01 ! Interpolation coefficient for vertical velocity 
            w2_thres  =  5e-5 ! Threshold for vertical variance (w'2) (m^2/s^2)
            cpw  =  4218.0_wp ! Heat capacity of water at constant pressure (J/kg/K)

            Q0_heat         =  400    ! Surface heating buoyancy flux (W/m^2)
            Q0_cool_summer  =  100    ! Surface cooling buoyancy flux (W/m^2)
            Q0_cool_winter  =  263    ! Surface cooling buoyancy flux (W/m^2)
            Q0_shift        =  163    ! Surface buoyancy flux shift (W/m^2)

            time_season_change = 172800.0    ! The time when sesason changes 
            time_self_shading  = 180000000.0 ! The time when self shading active
            growth             =      1.0    ! Plankton max growth rate (1/day)
            death              =      0.1    ! Plankton death rate      (1/day)
            penetration_depth  =     10.0    ! Radiation penetration depth (m)

            G1  =  (growth / 86400.0) / 6.2e-5 ! Growth rate in seconds /(N0 or P0)
            D1  =  (death  / 86400.0)          ! Death rate in seconds
            K1  =  (1.0 / penetration_depth)   ! Light attenuation (1/m) 

            DO k = nzb, nzt
                CHL(k)    =  0.0
                radpen(k) =  0.0
                light(k)  =  exp(K1 * zw(k))
            END DO

            IF ( .NOT. dirunal_variation ) growth  =  growth / 2.0_wp

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
            REAL(wp) ::  L_IR, K_IR, L_VIS, K_VIS, L_R, K_R, L_B, K_B
            REAL(wp)     ::  count_z, tot_vol, pi= 3.141592654_wp 

            count_z = 0.0

            DO k = nzt,nzb,-1

                count_z = count_z + 1 

                !<Infrared ray effect
                L_IR = 0.58   !  ( I_IR = I_0 * 0.58  W/m^2) 
                K_IR = 2.86   !  ( 1/m )
                
                IF (simulated_time < time_self_shading ) THEN 

                    IF ( simple_penetration) THEN 
                    !< Simple penetration without dividing visible & infrared ray
                        radpen(k)  =  light(k)
                    ELSE
                    !< Self Shading Effect Off
                        !<Visibile ray effect
                        L_VIS = 0.42   ! ( I_VIS = I_0 * 0.42 W/m^2 )
                        K_VIS = 0.0434 ! ( 1/m ) 
                        
                        radpen(k)  =  L_IR  * exp(zw(k) * K_IR)                 &
                                   +  L_VIS * exp(zw(k) * K_VIS)  
                    END IF 

                ELSE 
                !< Self Shading Effect On    

                    !<Calculating the chlorophyll concentration
                    number_of_particles=prt_count(k,j,i)
                    particles => grid_particles(k,j,i)%particles(1:number_of_particles)
                    tot_vol   = SUM( (4.0/3.0)*pi*particles(1:number_of_particles)%radius**3.0 )
                    tot_vol   = tot_vol*1030.0*1.0e6

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

                    radpen(k)  =  L_IR * exp(zw(k) * K_IR) &
                               +  L_R  * exp(zw(k) * K_R)  & 
                               +  L_B  * exp(zw(k) * K_B)
                                
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
            IF ( dirunal_variation ) THEN 
                !<COOLING daily average -81 W/m2 (WINTER)
                IF ( simulated_time < time_season_change ) THEN 
                    cooling  =  - Q0_cool_winter
                    heating  =  Q0_heat*sin(2.0_wp*pi*simulated_time/86400.0_wp)&
                                - Q0_shift
                !<HEATING daily average +81 W/m2 (SUMMER)
                ELSE
                    cooling  =  - Q0_cool_summer
                    heating  =  Q0_heat*sin(2.0_wp*pi*simulated_time/86400.0_wp)
                END IF
                solar    =  max(heating, cooling) /  (cpw * rho_surface)

            ELSE 
                solar  =  Q0_heat / (cpw * rho_surface)
            END IF 

            !<Calculate the potential temperature tendency with radiation fluxes
            IF (k == nzt) THEN 
                pt_tend  =  solar * radpen(k) * ddzw(k)
            ELSEIF (k == nzb) THEN 
                pt_tend  =  - solar * radpen(k-1) * ddzw(k)
            ELSE
                IF ( simulated_time < time_season_change) THEN 
                    pt_tend  =  (solar + Q0_cool_winter/(cpw * rho_surface))    &
                             *  (radpen(k) - radpen(k-1)) * ddzw(k)
                ELSE
                    pt_tend  =  (solar + Q0_cool_summer/(cpw * rho_surface))    &
                             *  (radpen(k) - radpen(k-1)) * ddzw(k)
                END IF 
            ENDIF
                

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
            
            IF ( nutrient_interaction ) THEN 

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
 
             INTEGER(iwp) :: ip  !< index of particle grid box, x-direction
             INTEGER(iwp) :: jp  !< index of particle grid box, y-direction
             INTEGER(iwp) :: kp  !< index of particle grid box, z-direction
             INTEGER(iwp) ::  n  !< particle index
             INTEGER(iwp) ::  nb !< index of sub-box particles are sorted in
             INTEGER(iwp) ::  pn !< the number of particles want to track

             INTEGER(iwp), DIMENSION(0:7)  ::  start_index !< start particle index for current sub-box
             INTEGER(iwp), DIMENSION(0:7)  ::  end_index   !< start particle index for current sub-box

             REAL(wp)     :: net_growth !< parameters for plankton growth
             REAL(wp)     :: w2_av, dw2_av !< mean vertical velocity varicance
             REAL(wp)     :: dw_corr

             number_of_particles = prt_count(kp,jp,ip)
             particles => grid_particles(kp,jp,ip)%particles(1:number_of_particles)

             start_index = grid_particles(kp,jp,ip)%start_index
             end_index   = grid_particles(kp,jp,ip)%end_index

             DO  nb = 0, 7
                DO  n = start_index(nb), end_index(nb)
                !<Phytosynthesis is only active when the radiation is available
                    IF (solar > 0.0) THEN 
                        net_growth  =  G1 * s(kp,jp,ip) * radpen(kp) - D1
                    ELSE 
                        net_growth  =  - D1
                    END IF 
                    
                    IF (simulated_time > particle_advection_start) THEN 
                        particles(n)%radius=particles(n)%radius*               &
                                            (1.0 + dt_3d*net_growth)**(1.0/3.0)
                    END IF
                    
                    IF (par_interpolation) THEN
                        ! No inteprolation correction at bottom & top boundary
                        IF ( kp > 80 .OR. kp < 10 ) dw_corr = 0.0                         
                        
                        ! Calculate mean vertical velocity variance at each depth
                        w2_av   =  (hom(kp,1,32,0) + hom(kp-1,1,32,0))/2.0
                        dw2_av  =  (hom(kp,1,32,0) - hom(kp-1,1,32,0))*ddzw(k)
                        
                        ! Vertical velocity interpolation correction
                        IF (w2_av < w2_thres) THEN
                            ! Pull back to original place 
                            particles(n)%z  =  particles(n)%z                  &
                                            -  particles(n)%speed_z * dt_3d
                                            
                            ! Vertical velocity correction with variance
                            dw_corr  =  alpha * sqrt(abs(dw2_av))
                            IF (particles(n)%speed_z < 0.0 .AND. dw2_av > 0.0) THEN
                                particles(n)%speed_z  =  particles(n)%speed_z  &   
                                                      +  dw_corr
                            ELSEIF (particles(n)%speed_z > 0.0 .AND. dw2_av < 0.0) THEN
                                particles(n)%speed_z  =  particles(n)%speed_z  &
                                                      -  dw_corr
                            END IF
                            
                            ! Update particle position with corrected vertical velocity
                            particles(n)%z  =  particles(n)%z                  &
                                            +  particles(n)%speed_z * dt_3d                            
                        END IF                        
                    END IF
                    
                ENDDO
             ENDDO

          END SUBROUTINE LPM_phy_tend
        END MODULE plankton_model
