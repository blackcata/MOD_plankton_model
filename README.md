# Lagrangian Plankton Model 
This module is purposed to attach the NP Lagrangian particle model (Nutriend & Phytoplankton) to the PALM code, which is the turbulent resolving large eddy simulation(LES) code for the modern meteorological model system. 
This code is integrated with the user codes, which is given by the PALM code. 

Spring bloom is simulated and analyzed by the LPM code and submitted to the Journal of Geophysical Research : Oceans. 

Title : The Route to Spring Phytoplankton Blooms by a Lagrangian Plankton Model (submitted)
Listed co-author(s) :Kyung Min Noh, Yign Noh, Ashley Brereton, and Jong-Seong Kug
Corresponding Author : Yign Noh 

### Install 
1. Write USE plankton_model to user_init.f90, user_actionf90 and user_ user_lpm_advec.f90 file. 

2. Add one line on the end of user_init.f90 
   -  CALL LPM_setup
   
3. Add one line on the end of user_lpm_advec.f90 
   -  CALL LPM_phy_tend(ip, jp, kp)

4. Add 5 lines on the 'pt-tendency' CASE at user_action.f90
  - CALL LPM_irradiance(i,j)
  - DO k = nzb, nzt
  - CALL LPM_pt_tend(k)
  - tend(k,j,i) = tend(k,j,i) + pt_tend
  - END DO
  
 5. Add 4 lines on the 's-tendency' CASE at user_action.f90
  - CALL LPM_irradiance(i,j)
  - DO k = nzt, nzb+1, -1
  -   CALL CALL LPM_s_tend(i,j,k)
  -   tend(k,j,i) = tend(k,j,i) + s_tend
  - END DO
  
6. Modifiy the Makefile 
  - Add mod_plankton_model.f90 at the SOURCE
  - Add mod_plankton_model.o to user_init.o, user_action.o and user_lpm_advec.o
  - Add mod_plankton_model.o's related modules. 
  
    mod_plankton_model.f90: \
         mod_kinds.o \
         modules.o \
         mod_particle_attributes.o
         
6. Run the PALM model with specific cases.
    
If you want more details,look at the EXAMPLE folder and compare what is different. 
