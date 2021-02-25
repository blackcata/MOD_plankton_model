# Lagrangian Plankton Model 
This module is purposed to attach the NP Lagrangian particle model (Nutriend & Phytoplankton) to the PALM code, which is the turbulent resolving large eddy simulation (LES) code for the modern meteorological model system. 
This code is integrated with the PALM code by user code given by the PALM developers. 

Spring bloom mechanism is firstly suggested by Svedrup (1953), describing as the interaction between light and ocean mixing and this theory is called the critical depth hypothesis (CDH). Svedrup hypothesis persisted several decades, however some scientists have focused on the assumption of CDH and proposed other explanations for the onset of spring bloom: critical turbulence theory (CTH) [Huisman(1999), Taylor & Ferrari (2011), and Chiswell (2013)], grazing pressure theory [Behrenfeld(2010)], mesoscale eddy [Mahadevan(2012)]or ocean front [Taylor & Ferrari(2011)]. These theories are investigated by using satellite observation, in-situ data and model results, including large eddy simulation(LES) and global circulation model(GCM). 

We also used the LES model with Lagrangian particle concept to simulate idealized and realistic ocean condition with Lagrangian approach. This code is based on the NP model (Nutrient and Phyotoplankton), which simulated the phytoplankton particle growth by light and nutrient supply. This code includes four different part: light penetration, temperature & nutrient interaction and phytoplankton particle growth. You can choose options of LPM model in LPM_setup subroutine: penetration scheme, nutrient interaction, and dirunal varation. Light penetration scheme is based on the Manniza scheme (2005), which is widely used for the light penetration and heating by chlorophyll-a concentration in many earth system models (ESM).

Spring bloom is simulated and analyzed by the LPM code and published to the Journal of Geophysical Research : Oceans. 

### Title : The Route to Spring Phytoplankton Blooms by a Lagrangian Plankton Model

Listed co-author(s) : Kyung Min Noh, Yign Noh, Ashley Brereton, and Jong-Seong Kug

Corresponding Author : Yign Noh 

### 1. Simulated fluid 
Ocean mixed layer structures are constructed with Goh & Noh (2013) conditions with same heat flux settings. To reproduce the spring bloom, we used two different ocean condition: winter and spring condition, and the point when the ocean condition is changed is controlled by 'time_season_change' in LPM_setup. The Lagrangian plankton particles are well mixed up with deep convection by surface cooling at winter ocean condition.  
- Langmuir circulation forcing : Noh et al. (2004)
- Ocean heat flux condition : Goh & Noh (2013) 
- Light penetration : Manniza et al. (2005)

### 2. Related papers
- Related paper : http://bit.ly/2uC0mWV
- Related presentation : https://bit.ly/37NfUHF

### 3. Simulation setting for phytoplankton 
To compare the simulation results with other papers, phytoplankton growth & death setting is from Taylor & Ferrari (2011), which simulated the spring bloom with large eddy simulation but Eulerian approach. 
- Growth rate : 1.0 / day
- Death rate : 0.1 / day
- Light penetration depth : 10 m
    
If you want more details,look at the EXAMPLE folder and compare what is different. 
