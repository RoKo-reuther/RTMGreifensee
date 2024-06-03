SUBROUTINE derivs (neq, t, y, ydot, yout, ip)

  IMPLICIT NONE

  !=============================================================================
  ! Declarations & Parameter Initialization
  !=============================================================================
    
  !-----------------------------------------------------------------------------
  ! Dimensions
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER :: N_grid = 250, N_spec = 25
  REAL(kind = 8)     :: grid_start = 0.0, grid_stop = 0.5

  !-----------------------------------------------------------------------------
  ! Subroutine Arguments
  !-----------------------------------------------------------------------------
  REAL(kind = 8)     :: t, y(N_grid * N_spec), ydot(N_grid * N_spec), yout(*)
  INTEGER            :: neq, ip(*)
    
  !-----------------------------------------------------------------------------
  ! Grid, Grid Properties & Transport Parameters
  !-----------------------------------------------------------------------------  
  REAL(kind = 8)            :: xmid(N_grid), xint(N_grid + 1)
  REAL(kind = 8)            :: dx(N_grid), dxaux(N_grid + 1)

  REAL(kind = 8)            :: por_mid(N_grid), por_int(N_grid + 1)
  REAL(kind = 8)            :: svf_mid(N_grid), svf_int(N_grid + 1)

  REAL(kind = 8)            :: v_por_int(N_grid + 1), v_svf_int(N_grid + 1)

  REAL(kind = 8)            :: tortuosity_squared(N_grid + 1)
  REAL(kind = 8)            :: Db_int(N_grid + 1)
  REAL(kind = 8)            :: D_O2_int(N_grid + 1)
  REAL(kind = 8)            :: D_CH4_int(N_grid + 1)
  REAL(kind = 8)            :: D_N2_int(N_grid + 1)
  REAL(kind = 8)            :: D_Mn2_int(N_grid + 1)
  REAL(kind = 8)            :: D_Fe2_int(N_grid + 1)
  REAL(kind = 8)            :: D_Ca2_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_H_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_H2CO3_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_H2PO4_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_NO3_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_NH4_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_SO4_int(N_grid + 1)
  REAL(kind = 8)            :: D_TOT_H2S_int(N_grid + 1)

  REAL(kind = 8), PARAMETER :: AFDW(N_grid + 1) = 1
  REAL(kind = 8), PARAMETER :: A(N_grid) = 1
  REAL(kind = 8), PARAMETER :: A_int(N_grid + 1) = 1

  !-----------------------------------------------------------------------------
  ! Species Variables
  !-----------------------------------------------------------------------------
  REAL(kind = 8)            :: O2(N_grid), REAC_O2(N_grid), TRAN_O2(N_grid), FLUX_O2(N_grid + 1)
  REAL(kind = 8)            :: CH4(N_grid), REAC_CH4(N_grid), TRAN_CH4(N_grid), FLUX_CH4(N_grid + 1)
  REAL(kind = 8)            :: N2(N_grid), REAC_N2(N_grid), TRAN_N2(N_grid), FLUX_N2(N_grid + 1)
  REAL(kind = 8)            :: Mn2(N_grid), REAC_Mn2(N_grid), TRAN_Mn2(N_grid), FLUX_Mn2(N_grid + 1)
  REAL(kind = 8)            :: Fe2(N_grid), REAC_Fe2(N_grid), TRAN_Fe2(N_grid), FLUX_Fe2(N_grid + 1)
  REAL(kind = 8)            :: Ca2(N_grid), REAC_Ca2(N_grid), TRAN_Ca2(N_grid), FLUX_Ca2(N_grid + 1)
  REAL(kind = 8)            :: OMa(N_grid), REAC_OMa(N_grid), TRAN_OMa(N_grid), FLUX_OMa(N_grid + 1)
  REAL(kind = 8)            :: OMb(N_grid), REAC_OMb(N_grid), TRAN_OMb(N_grid), FLUX_OMb(N_grid + 1)
  REAL(kind = 8)            :: MnO2(N_grid), REAC_MnO2(N_grid), TRAN_MnO2(N_grid), FLUX_MnO2(N_grid + 1)
  REAL(kind = 8)            :: FeOH3(N_grid), REAC_FeOH3(N_grid), TRAN_FeOH3(N_grid), FLUX_FeOH3(N_grid + 1)
  REAL(kind = 8)            :: FeOH3_P(N_grid), REAC_FeOH3_P(N_grid), TRAN_FeOH3_P(N_grid), FLUX_FeOH3_P(N_grid + 1)
  REAL(kind = 8)            :: FeCO3(N_grid), REAC_FeCO3(N_grid), TRAN_FeCO3(N_grid), FLUX_FeCO3(N_grid + 1)
  REAL(kind = 8)            :: S0(N_grid), REAC_S0(N_grid), TRAN_S0(N_grid), FLUX_S0(N_grid + 1)
  REAL(kind = 8)            :: FeS(N_grid), REAC_FeS(N_grid), TRAN_FeS(N_grid), FLUX_FeS(N_grid + 1)
  REAL(kind = 8)            :: vivianite(N_grid), REAC_vivianite(N_grid), TRAN_vivianite(N_grid), FLUX_vivianite(N_grid + 1)
  REAL(kind = 8)            :: CaCO3(N_grid), REAC_CaCO3(N_grid), TRAN_CaCO3(N_grid), FLUX_CaCO3(N_grid + 1)
  REAL(kind = 8)            :: apatite(N_grid), REAC_apatite(N_grid), TRAN_apatite(N_grid), FLUX_apatite(N_grid + 1)
  REAL(kind = 8)            :: MnCO3(N_grid), REAC_MnCO3(N_grid), TRAN_MnCO3(N_grid), FLUX_MnCO3(N_grid + 1)
  REAL(kind = 8)            :: TOT_H(N_grid), REAC_TOT_H(N_grid), TRAN_TOT_H(N_grid), FLUX_TOT_H(N_grid + 1)
  REAL(kind = 8)            :: TOT_H2CO3(N_grid), REAC_TOT_H2CO3(N_grid), TRAN_TOT_H2CO3(N_grid), FLUX_TOT_H2CO3(N_grid + 1)
  REAL(kind = 8)            :: TOT_H2PO4(N_grid), REAC_TOT_H2PO4(N_grid), TRAN_TOT_H2PO4(N_grid), FLUX_TOT_H2PO4(N_grid + 1)
  REAL(kind = 8)            :: TOT_NO3(N_grid), REAC_TOT_NO3(N_grid), TRAN_TOT_NO3(N_grid), FLUX_TOT_NO3(N_grid + 1)
  REAL(kind = 8)            :: TOT_NH4(N_grid), REAC_TOT_NH4(N_grid), TRAN_TOT_NH4(N_grid), FLUX_TOT_NH4(N_grid + 1)
  REAL(kind = 8)            :: TOT_SO4(N_grid), REAC_TOT_SO4(N_grid), TRAN_TOT_SO4(N_grid), FLUX_TOT_SO4(N_grid + 1)
  REAL(kind = 8)            :: TOT_H2S(N_grid), REAC_TOT_H2S(N_grid), TRAN_TOT_H2S(N_grid), FLUX_TOT_H2S(N_grid + 1)


  !-----------------------------------------------------------------------------
  ! Tableau
  !-----------------------------------------------------------------------------
  INTEGER, PARAMETER        :: N_tableau_components = 7, N_tableau_species = 19

  REAL(kind = 8)            :: component_total(N_tableau_components * N_grid)  !known total amounts of components
  REAL(kind = 8)            :: tableau(N_tableau_species, N_tableau_components)!the tableau ...
  REAL(kind = 8)            :: logK(N_tableau_species)                         !equilibrium constants
  INTEGER                   :: iter_pcfm(N_grid)                               !iterations conducted for pcfm
  INTEGER                   :: iter_newton(N_grid)                             !iterations conducted for each layer
  INTEGER                   :: info_newton(N_grid)                             !exit status of solving soutine for each layer
  REAL(kind = 8)            :: difference(N_grid,N_tableau_components)         !remaining difference between guessed and known total components
  REAL(kind = 8)            :: species_conc(N_grid,N_tableau_species)          !result: species concentrations
  INTEGER                   :: success(N_grid)                                 !convergence reached? 0: no, 1: first round-pcfm, 2: first round-newton, 3: second round-pcfm, 4: second round-newton
  REAL(kind = 8)            :: component_guess(N_tableau_components * N_grid) = 0.0  !guess for component concentrations 

  REAL(kind = 8)            :: H(N_grid)    
  REAL(kind = 8)            :: OH(N_grid)   
  REAL(kind = 8)            :: H2CO3(N_grid)
  REAL(kind = 8)            :: HCO3(N_grid) 
  REAL(kind = 8)            :: CO3(N_grid)  
  REAL(kind = 8)            :: H3PO4(N_grid)
  REAL(kind = 8)            :: H2PO4(N_grid)
  REAL(kind = 8)            :: HPO4(N_grid) 
  REAL(kind = 8)            :: PO4(N_grid)  
  REAL(kind = 8)            :: HNO3(N_grid) 
  REAL(kind = 8)            :: NO3(N_grid)  
  REAL(kind = 8)            :: NH4(N_grid)  
  REAL(kind = 8)            :: NH3(N_grid)  
  REAL(kind = 8)            :: H2SO4(N_grid)
  REAL(kind = 8)            :: HSO4(N_grid) 
  REAL(kind = 8)            :: SO4(N_grid)  
  REAL(kind = 8)            :: H2S(N_grid)  
  REAL(kind = 8)            :: HS(N_grid)   
  REAL(kind = 8)            :: S2(N_grid)
  REAL(kind = 8)            :: pH(N_grid)


  !-----------------------------------------------------------------------------
  ! Reactions & Omegas & ...
  !-----------------------------------------------------------------------------
  REAL(kind = 8)            :: R_OM_deg_a_O2(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_O2(N_grid)
  REAL(kind = 8)            :: R_OM_deg_a_NO3(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_NO3(N_grid)
  REAL(kind = 8)            :: R_OM_deg_a_MnO2(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_MnO2(N_grid)
  REAL(kind = 8)            :: R_OM_deg_a_FeOH3(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_FeOH3(N_grid)
  REAL(kind = 8)            :: R_OM_deg_a_SO4(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_SO4(N_grid)
  REAL(kind = 8)            :: R_OM_deg_a_CH4(N_grid)
  REAL(kind = 8)            :: R_OM_deg_b_CH4(N_grid)
  REAL(kind = 8)            :: R_H2S_ox_by_O2(N_grid)
  REAL(kind = 8)            :: R_SO4_red_by_CH4(N_grid)
  REAL(kind = 8)            :: R_FeOH3_red_by_H2S(N_grid)
  REAL(kind = 8)            :: R_FeOH3_formation(N_grid)
  REAL(kind = 8)            :: R_Fe2_ox_by_MnO2(N_grid)
  REAL(kind = 8)            :: R_NH4_ox_by_O2(N_grid)
  REAL(kind = 8)            :: R_H2S_ox_by_MnO2(N_grid)
  REAL(kind = 8)            :: R_FeS_ox_by_O2(N_grid)
  REAL(kind = 8)            :: R_CH4_ox_by_O2(N_grid)
  REAL(kind = 8)            :: R_precip_FeCO3(N_grid)
  REAL(kind = 8)            :: R_diss_FeCO3(N_grid)
  REAL(kind = 8)            :: R_precip_FeS(N_grid)
  REAL(kind = 8)            :: R_diss_FeS(N_grid)
  REAL(kind = 8)            :: R_precip_vivianite(N_grid)
  REAL(kind = 8)            :: R_diss_vivianite(N_grid)
  REAL(kind = 8)            :: R_precip_CaCO3(N_grid)
  REAL(kind = 8)            :: R_diss_CaCO3(N_grid)
  REAL(kind = 8)            :: R_precip_apatite(N_grid)
  REAL(kind = 8)            :: R_diss_apatite(N_grid)
  REAL(kind = 8)            :: R_precip_MnCO3(N_grid)
  REAL(kind = 8)            :: R_diss_MnCO3(N_grid)

  REAL(kind = 8)            :: omega_FeCO3(N_grid)
  REAL(kind = 8)            :: omega_CaCO3(N_grid)
  REAL(kind = 8)            :: omega_FeS(N_grid)
  REAL(kind = 8)            :: omega_vivianite(N_grid)
  REAL(kind = 8)            :: omega_apatite(N_grid)
  REAL(kind = 8)            :: omega_MnCO3(N_grid)

  REAL(kind = 8)            :: s2p(N_grid)
  REAL(kind = 8)            :: p2s(N_grid)

  REAL(kind = 8)            :: L_O2(N_grid)
  REAL(kind = 8)            :: L_NO3(N_grid)
  REAL(kind = 8)            :: L_MnO2(N_grid)
  REAL(kind = 8)            :: L_FeOH3(N_grid)
  REAL(kind = 8)            :: L_SO4(N_grid)
  REAL(kind = 8)            :: L_PO4(N_grid)

  REAL(kind = 8)            :: FeOH3_tot(N_grid)
  REAL(kind = 8)            :: chi_Fe(N_grid)


  !-----------------------------------------------------------------------------
  ! Common Block: Parameters (Molar Diffusion Coefficients & Reaction Constants)
  !-----------------------------------------------------------------------------
  REAL(kind = 8)            :: precipitation
  REAL(kind = 8)            :: speciation

  REAL(kind = 8)            :: por_0, por_inf, por_xatt
  
  REAL(kind = 8)            :: Dmol_O2
  REAL(kind = 8)            :: Dmol_CH4
  REAL(kind = 8)            :: Dmol_N2
  REAL(kind = 8)            :: Dmol_Mn2
  REAL(kind = 8)            :: Dmol_Fe2
  REAL(kind = 8)            :: Dmol_Ca2
  REAL(kind = 8)            :: Dmol_TOT_H
  REAL(kind = 8)            :: Dmol_TOT_H2CO3
  REAL(kind = 8)            :: Dmol_TOT_H2PO4
  REAL(kind = 8)            :: Dmol_TOT_NO3
  REAL(kind = 8)            :: Dmol_TOT_NH4
  REAL(kind = 8)            :: Dmol_TOT_SO4
  REAL(kind = 8)            :: Dmol_TOT_H2S

  REAL(kind = 8)            :: k_OM_deg_a
  REAL(kind = 8)            :: k_OM_deg_b
  REAL(kind = 8)            :: f_OM_b
  REAL(kind = 8)            :: f_OM_c
  REAL(kind = 8)            :: lambda
  REAL(kind = 8)            :: K_O2
  REAL(kind = 8)            :: K_NO3
  REAL(kind = 8)            :: K_MnO2
  REAL(kind = 8)            :: K_FeOH3
  REAL(kind = 8)            :: K_SO4
  REAL(kind = 8)            :: k_H2S_ox_by_O2
  REAL(kind = 8)            :: k_SO4_red_by_CH4
  REAL(kind = 8)            :: k_FeOH3_red_by_H2S
  REAL(kind = 8)            :: k_FeOH3_formation
  REAL(kind = 8)            :: K_PO4
  REAL(kind = 8)            :: k_Fe2_ox_by_MnO2
  REAL(kind = 8)            :: k_NH4_ox_by_O2
  REAL(kind = 8)            :: k_H2S_ox_by_MnO2
  REAL(kind = 8)            :: k_FeS_ox_by_O2
  REAL(kind = 8)            :: k_CH4_ox_by_O2
  REAL(kind = 8)            :: k_precip_FeCO3
  REAL(kind = 8)            :: k_diss_FeCO3
  REAL(kind = 8)            :: Ksp_FeCO3
  REAL(kind = 8)            :: k_precip_FeS
  REAL(kind = 8)            :: k_diss_FeS
  REAL(kind = 8)            :: Ksp_FeS
  REAL(kind = 8)            :: k_precip_vivianite
  REAL(kind = 8)            :: k_diss_vivianite
  REAL(kind = 8)            :: Ksp_vivianite
  REAL(kind = 8)            :: k_precip_CaCO3
  REAL(kind = 8)            :: k_diss_CaCO3
  REAL(kind = 8)            :: Ksp_CaCO3
  REAL(kind = 8)            :: k_precip_apatite
  REAL(kind = 8)            :: k_diss_apatite
  REAL(kind = 8)            :: Ksp_apatite
  REAL(kind = 8)            :: k_precip_MnCO3
  REAL(kind = 8)            :: k_diss_MnCO3
  REAL(kind = 8)            :: Ksp_MnCO3

  common /myparms/precipitation,speciation,por_0,por_inf,por_xatt, &
                  Dmol_O2,Dmol_CH4,Dmol_N2,Dmol_Mn2,Dmol_Fe2,Dmol_Ca2, &
                  Dmol_TOT_H,Dmol_TOT_H2CO3,Dmol_TOT_H2PO4,Dmol_TOT_NO3, &
                  Dmol_TOT_NH4,Dmol_TOT_SO4,Dmol_TOT_H2S, &
                  k_OM_deg_a,k_OM_deg_b,f_OM_b,f_OM_c,lambda,K_O2,K_NO3, &
                  K_MnO2,K_FeOH3,K_SO4,k_H2S_ox_by_O2,k_SO4_red_by_CH4, &
                  k_FeOH3_red_by_H2S,k_FeOH3_formation,K_PO4,k_Fe2_ox_by_MnO2, &
                  k_NH4_ox_by_O2,k_H2S_ox_by_MnO2,k_FeS_ox_by_O2,k_CH4_ox_by_O2, &
                  k_precip_FeCO3,k_diss_FeCO3,Ksp_FeCO3,k_precip_FeS,k_diss_FeS, &
                  Ksp_FeS,k_precip_vivianite,k_diss_vivianite,Ksp_vivianite, &
                  k_precip_CaCO3,k_diss_CaCO3,Ksp_CaCO3,k_precip_apatite, &
                  k_diss_apatite,Ksp_apatite,k_precip_MnCO3,k_diss_MnCO3,Ksp_MnCO3


  !-----------------------------------------------------------------------------
  ! Common Block: Forcing Functions
  !-----------------------------------------------------------------------------
  REAL(kind = 8)            :: rsed
  REAL(kind = 8)            :: Db_0, Db_inf, Db_L, Db_xatt

  REAL(kind = 8)            :: O2_up       
  REAL(kind = 8)            :: CH4_up      
  REAL(kind = 8)            :: N2_up       
  REAL(kind = 8)            :: Mn2_up      
  REAL(kind = 8)            :: Fe2_up, Fe2_down      
  REAL(kind = 8)            :: Ca2_up      
  REAL(kind = 8)            :: OMa_up      
  REAL(kind = 8)            :: OMb_up      
  REAL(kind = 8)            :: MnO2_up     
  REAL(kind = 8)            :: FeOH3_up    
  REAL(kind = 8)            :: FeOH3_P_up  
  REAL(kind = 8)            :: FeCO3_up    
  REAL(kind = 8)            :: S0_up       
  REAL(kind = 8)            :: FeS_up      
  REAL(kind = 8)            :: vivianite_up
  REAL(kind = 8)            :: CaCO3_up    
  REAL(kind = 8)            :: apatite_up  
  REAL(kind = 8)            :: MnCO3_up    
  REAL(kind = 8)            :: TOT_H_up    
  REAL(kind = 8)            :: TOT_H2CO3_up
  REAL(kind = 8)            :: TOT_H2PO4_up
  REAL(kind = 8)            :: TOT_NO3_up  
  REAL(kind = 8)            :: TOT_NH4_up  
  REAL(kind = 8)            :: TOT_SO4_up  
  REAL(kind = 8)            :: TOT_H2S_up  

  common /myforcs/rsed,Db_0,Db_inf,Db_L,Db_xatt, &
                  O2_up,CH4_up,N2_up,Mn2_up,Fe2_up,Fe2_down,Ca2_up,         &
                  OMa_up,OMb_up,MnO2_up,FeOH3_up,FeOH3_P_up,FeCO3_up ,      &
                  S0_up,FeS_up,vivianite_up,CaCO3_up,apatite_up,MnCO3_up,   &
                  TOT_H_up,TOT_H2CO3_up,TOT_H2PO4_up,TOT_NO3_up,TOT_NH4_up, &
                  TOT_SO4_up,TOT_H2S_up

  !-----------------------------------------------------------------------------
  REAL(kind = 8)            :: nothing_real
  INTEGER                   :: nothing_integer
  INTEGER                   :: i

  !if(ip(1) < 4) call rexit("nout should be at least 4")


  !=============================================================================
  ! Executable Statements
  !=============================================================================

  !-----------------------------------------------------------------------------
  ! Initialization of State Variables
  !-----------------------------------------------------------------------------
  i = 1
  ! Solutes
  O2        = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  CH4       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  N2        = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  Mn2       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  Fe2       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  Ca2       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  ! Solids
  OMa       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  OMb       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  MnO2      = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  FeOH3     = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  FeOH3_P   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  FeCO3     = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  S0        = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  FeS       = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  vivianite = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  CaCO3     = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  apatite   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  MnCO3     = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  ! Lump Sums (tableau totals)
  TOT_H     = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_H2CO3 = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_H2PO4 = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_NO3   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_NH4   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_SO4   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1
  TOT_H2S   = y(((i-1) * N_grid + 1) : (i * N_grid)); i = i + 1

  !-----------------------------------------------------------------------------
  ! Setup Grid
  !-----------------------------------------------------------------------------
  call setup_grid(grid_start, grid_stop, N_grid, xmid, xint, dx, dxaux)

  !-----------------------------------------------------------------------------
  ! Setup Porosity
  !-----------------------------------------------------------------------------
  call pexp(xmid, por_0, por_inf, por_xatt, por_mid, N_grid)
  call pexp(xint, por_0, por_inf, por_xatt, por_int, N_grid + 1)

  svf_mid = 1.0 - por_mid
  svf_int = 1.0 - por_int

  !-----------------------------------------------------------------------------
  ! Setup advective velocities
  !-----------------------------------------------------------------------------
  call setup_v_por_int(rsed, por_0, por_inf, por_int, v_por_int, N_grid + 1)
  call setup_v_svf_int(rsed, por_0, por_int, v_svf_int, N_grid + 1)

  !-----------------------------------------------------------------------------
  ! Setup Bioturbation
  !-----------------------------------------------------------------------------
  call psig(xint, Db_0, Db_inf, Db_L, Db_xatt, Db_int, N_grid + 1)

  !-----------------------------------------------------------------------------
  ! Solute diffusion coefficients
  !-----------------------------------------------------------------------------
  tortuosity_squared = 1.0 - log(por_int**2.0)

  D_O2_int           = Dmol_O2        / tortuosity_squared + Db_int
  D_CH4_int          = Dmol_CH4       / tortuosity_squared + Db_int
  D_N2_int           = Dmol_N2        / tortuosity_squared + Db_int
  D_Mn2_int          = Dmol_Mn2       / tortuosity_squared + Db_int
  D_Fe2_int          = Dmol_Fe2       / tortuosity_squared + Db_int
  D_Ca2_int          = Dmol_Ca2       / tortuosity_squared + Db_int
  D_TOT_H_int        = Dmol_TOT_H     / tortuosity_squared + Db_int
  D_TOT_H2CO3_int    = Dmol_TOT_H2CO3 / tortuosity_squared + Db_int
  D_TOT_H2PO4_int    = Dmol_TOT_H2PO4 / tortuosity_squared + Db_int
  D_TOT_NO3_int      = Dmol_TOT_NO3   / tortuosity_squared + Db_int
  D_TOT_NH4_int      = Dmol_TOT_NH4   / tortuosity_squared + Db_int
  D_TOT_SO4_int      = Dmol_TOT_SO4   / tortuosity_squared + Db_int
  D_TOT_H2S_int      = Dmol_TOT_H2S   / tortuosity_squared + Db_int

  !-----------------------------------------------------------------------------
  ! Transport
  !-----------------------------------------------------------------------------

  ! tran1d (N, C, BCup, BCdown, VALup, VALdown, ablup, abldown, &
  !       & Dint, vint, AFDW, VF, VFint, A, Aint, dx, dxaux, dC, JF)

  ! Type of upper and lower boundary conditions
  ! Flux     = 1
  ! Convect  = 2
  ! Value    = 3
  ! ZeroGrad = 4

  call tran1d(N_grid, O2, 3, 4, O2_up, nothing_real, nothing_real,             &
              & nothing_real, D_O2_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_O2, FLUX_O2)

  call tran1d(N_grid, CH4, 3, 4, CH4_up, nothing_real, nothing_real,           &
              & nothing_real, D_CH4_int, v_por_int, AFDW, por_mid, por_int, A, &
              & A_int, dx, dxaux, TRAN_CH4, FLUX_CH4)
          
  call tran1d(N_grid, N2, 3, 4, N2_up, nothing_real, nothing_real,             &
              & nothing_real, D_N2_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_N2, FLUX_N2)

  call tran1d(N_grid, Mn2, 3, 4, Mn2_up, nothing_real, nothing_real,           &
              & nothing_real, D_Mn2_int, v_por_int, AFDW, por_mid, por_int, A, &
              & A_int, dx, dxaux, TRAN_Mn2, FLUX_Mn2)

  call tran1d(N_grid, Fe2, 3, 4, Fe2_up, Fe2_down, nothing_real,               &
              & nothing_real, D_Fe2_int, v_por_int, AFDW, por_mid, por_int, A, &
              & A_int, dx, dxaux, TRAN_Fe2, FLUX_Fe2)

  call tran1d(N_grid, Ca2, 3, 4, Ca2_up, nothing_real, nothing_real,           &
              & nothing_real, D_Ca2_int, v_por_int, AFDW, por_mid, por_int, A, &
              & A_int, dx, dxaux, TRAN_Ca2, FLUX_Ca2)

  call tran1d(N_grid, OMa, 1, 4, OMa_up, nothing_real, nothing_real,           &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_OMa, FLUX_OMa)

  call tran1d(N_grid, OMb, 1, 4, OMb_up, nothing_real, nothing_real,           &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_OMb, FLUX_OMb)

  call tran1d(N_grid, MnO2, 1, 4, MnO2_up, nothing_real, nothing_real,         &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_MnO2, FLUX_MnO2)

  call tran1d(N_grid, FeOH3, 1, 4, FeOH3_up, nothing_real, nothing_real,       &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_FeOH3, FLUX_FeOH3)

  call tran1d(N_grid, FeOH3_P, 1, 4, FeOH3_P_up, nothing_real, nothing_real,   &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_FeOH3_P, FLUX_FeOH3_P)

  call tran1d(N_grid, FeCO3, 1, 4, FeCO3_up, nothing_real, nothing_real,       &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_FeCO3, FLUX_FeCO3)

  call tran1d(N_grid, S0, 1, 4, S0_up, nothing_real, nothing_real,             &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_S0, FLUX_S0)

  call tran1d(N_grid, FeS, 1, 4, FeS_up, nothing_real, nothing_real,           &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_FeS, FLUX_FeS)

  call tran1d(N_grid, vivianite, 1, 4, vivianite_up, nothing_real, nothing_real, &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,      &
              & A_int, dx, dxaux, TRAN_vivianite, FLUX_vivianite)

  call tran1d(N_grid, CaCO3, 1, 4, CaCO3_up, nothing_real, nothing_real,       &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_CaCO3, FLUX_CaCO3)

  call tran1d(N_grid, apatite, 1, 4, apatite_up, nothing_real, nothing_real,   &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_apatite, FLUX_apatite)

  call tran1d(N_grid, MnCO3, 1, 4, MnCO3_up, nothing_real, nothing_real,       &
              & nothing_real, Db_int, v_svf_int, AFDW, svf_mid, svf_int, A,    &
              & A_int, dx, dxaux, TRAN_MnCO3, FLUX_MnCO3)

  call tran1d(N_grid, TOT_H, 3, 4, TOT_H_up, nothing_real, nothing_real,          &
              & nothing_real, D_TOT_H_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_H, FLUX_TOT_H)

  call tran1d(N_grid, TOT_H2CO3, 3, 4, TOT_H2CO3_up, nothing_real, nothing_real,      &
              & nothing_real, D_TOT_H2CO3_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_H2CO3, FLUX_TOT_H2CO3)

  call tran1d(N_grid, TOT_H2PO4, 3, 4, TOT_H2PO4_up, nothing_real, nothing_real,      &
              & nothing_real, D_TOT_H2PO4_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_H2PO4, FLUX_TOT_H2PO4)

  call tran1d(N_grid, TOT_NO3, 3, 4, TOT_NO3_up, nothing_real, nothing_real,        &
              & nothing_real, D_TOT_NO3_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_NO3, FLUX_TOT_NO3)

  call tran1d(N_grid, TOT_NH4, 3, 4, TOT_NH4_up, nothing_real, nothing_real,        &
              & nothing_real, D_TOT_NH4_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_NH4, FLUX_TOT_NH4)

  call tran1d(N_grid, TOT_SO4, 3, 4, TOT_SO4_up, nothing_real, nothing_real,        &
              & nothing_real, D_TOT_SO4_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_SO4, FLUX_TOT_SO4)

  call tran1d(N_grid, TOT_H2S, 3, 4, TOT_H2S_up, nothing_real, nothing_real,        &
              & nothing_real, D_TOT_H2S_int, v_por_int, AFDW, por_mid, por_int, A,  &
              & A_int, dx, dxaux, TRAN_TOT_H2S, FLUX_TOT_H2S)


  !-----------------------------------------------------------------------------
  ! Speciation
  !-----------------------------------------------------------------------------

  IF (int(speciation) == 1) THEN

    ! enter tableau row-wise
    tableau = transpose(reshape([   &
         1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
        -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
         0.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
        -1.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
        -2.0,  1.0,  0.0,  0.0,  0.0,  0.0,  0.0, &
         1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0, &
         0.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0, &
        -1.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0, &
        -2.0,  0.0,  1.0,  0.0,  0.0,  0.0,  0.0, &
         1.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0, &
         0.0,  0.0,  0.0,  1.0,  0.0,  0.0,  0.0, &
         0.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0, &
        -1.0,  0.0,  0.0,  0.0,  1.0,  0.0,  0.0, &
         2.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0, &
         1.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0, &
         0.0,  0.0,  0.0,  0.0,  0.0,  1.0,  0.0, &
         0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0, &
        -1.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0, &
        -2.0,  0.0,  0.0,  0.0,  0.0,  0.0,  1.0  &
        ], shape(transpose(tableau))))

    logK = [0.0, -8.0, 0.0, -3.35, -10.68, -0.84, 0.0, -4.21, -13.53, &
            -4.37, 0.0, 0.0, -6.25, -7.1, -1.1, 0.0, 0.0, -4.0, -15.0]

    i = 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) = -TOT_H;     i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_H2CO3; i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_H2PO4; i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_NO3;   i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_NH4;   i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_SO4;   i = i + 1
    component_total(((i-1) * N_grid + 1) : (i * N_grid)) =  TOT_H2S;   i = i + 1

    call solve_tableau(                                   &  
      & component_total,                                  &
      & tableau, logK,                                    &
      & N_tableau_components, N_tableau_species, N_grid,  &
      & iter_pcfm,                                        &
      & iter_newton, info_newton,                         &
      & difference, species_conc,                         &
      & success, component_guess                          &
      & )

    H     = species_conc(:,1)
    OH    = species_conc(:,2)
    H2CO3 = species_conc(:,3)
    HCO3  = species_conc(:,4)
    CO3   = species_conc(:,5)
    H3PO4 = species_conc(:,6)
    H2PO4 = species_conc(:,7)
    HPO4  = species_conc(:,8)
    PO4   = species_conc(:,9)
    HNO3  = species_conc(:,10)
    NO3   = species_conc(:,11)
    NH4   = species_conc(:,12)
    NH3   = species_conc(:,13)
    H2SO4 = species_conc(:,14)
    HSO4  = species_conc(:,15)
    SO4   = species_conc(:,16)
    H2S   = species_conc(:,17)
    HS    = species_conc(:,18)
    S2    = species_conc(:,19)

  ELSE 
  
    H     = 1e-4
    OH    = 1e-4
    H2CO3 = TOT_H2CO3 * 1.828513e-01
    HCO3  = TOT_H2CO3 * 8.167667e-01
    CO3   = TOT_H2CO3 * 3.820305e-04
    H3PO4 = TOT_H2PO4 * 8.941165e-06
    H2PO4 = TOT_H2PO4 * 6.185775e-01
    HPO4  = TOT_H2PO4 * 3.814118e-01
    PO4   = TOT_H2PO4 * 1.825552e-06
    HNO3  = TOT_NO3   * 4.265795e-09
    NO3   = TOT_NO3   * 1.000000e+00
    NH4   = TOT_NH4   * 9.944080e-01
    NH3   = TOT_NH4   * 5.591967e-03
    H2SO4 = TOT_SO4   * 7.943219e-16
    HSO4  = TOT_SO4   * 7.943219e-06
    SO4   = TOT_SO4   * 9.999921e-01
    H2S   = TOT_H2S   * 5.000000e-01
    HS    = TOT_H2S   * 5.000000e-01 
    S2    = TOT_H2S   * 5.000000e-08
  
  END IF

  pH    = -log10(H * 1e-3)


  !-----------------------------------------------------------------------------
  ! Reactions
  !-----------------------------------------------------------------------------
  REAC_O2        = 0.0
  REAC_CH4       = 0.0
  REAC_N2        = 0.0
  REAC_Mn2       = 0.0
  REAC_Fe2       = 0.0
  REAC_Ca2       = 0.0
  REAC_OMa       = 0.0
  REAC_OMb       = 0.0
  REAC_MnO2      = 0.0
  REAC_FeOH3     = 0.0
  REAC_FeOH3_P   = 0.0
  REAC_FeCO3     = 0.0
  REAC_S0        = 0.0
  REAC_FeS       = 0.0
  REAC_vivianite = 0.0
  REAC_CaCO3     = 0.0
  REAC_apatite   = 0.0
  REAC_MnCO3     = 0.0
  REAC_TOT_H     = 0.0
  REAC_TOT_H2CO3 = 0.0
  REAC_TOT_H2PO4 = 0.0
  REAC_TOT_NO3   = 0.0
  REAC_TOT_NH4   = 0.0
  REAC_TOT_SO4   = 0.0
  REAC_TOT_H2S   = 0.0

  ! phase conversion factors
  s2p       = svf_mid / por_mid
  p2s       = por_mid / svf_mid

  ! FeOH3_tot & FeOH3 fraction
  FeOH3_tot = FeOH3 + FeOH3_P
  chi_Fe = 1.0
  DO i = 1, N_grid
    IF (FeOH3_tot(i) > 0.0) THEN
      chi_Fe(i) = FeOH3(i) / FeOH3_tot(i)
    END IF
  END DO

  ! limitation & inhibition terms
  L_O2      = O2        / (K_O2    + O2)
  L_NO3     = TOT_NO3   / (K_NO3   + TOT_NO3)
  L_MnO2    = MnO2      / (K_MnO2  + MnO2)
  L_FeOH3   = FeOH3_tot / (K_FeOH3 + FeOH3_tot)
  L_SO4     = TOT_SO4   / (K_SO4   + TOT_SO4)
  L_PO4     = TOT_H2PO4 / (K_PO4   + TOT_H2PO4)

  ! ---------------------------------------------------------
  ! > OM Degradation
  ! ---------------------------------------------------------
  ! all rates in mol m-3_sf yr-1 

  R_OM_deg_a_O2  = k_OM_deg_a * OMa * L_O2
  REAC_OMa       = REAC_OMa       - R_OM_deg_a_O2
  REAC_O2        = REAC_O2        - s2p * R_OM_deg_a_O2
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + s2p * R_OM_deg_a_O2
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_O2
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_a_O2

  R_OM_deg_b_O2  = k_OM_deg_b * OMb * L_O2
  REAC_OMb       = REAC_OMb       - R_OM_deg_b_O2
  REAC_O2        = REAC_O2        - s2p * R_OM_deg_b_O2
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + s2p * R_OM_deg_b_O2
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_O2
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_b_O2
        
  R_OM_deg_a_NO3 = k_OM_deg_a * OMa * L_NO3 * (1.0 - L_O2)
  REAC_OMa       = REAC_OMa       - R_OM_deg_a_NO3
  REAC_TOT_NO3   = REAC_TOT_NO3   - 4.0/5.0 * s2p * R_OM_deg_a_NO3
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 +  s2p * R_OM_deg_a_NO3
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_NO3
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_a_NO3
  REAC_N2        = REAC_N2        + 2.0/5.0 * s2p * R_OM_deg_a_NO3

  R_OM_deg_b_NO3 = k_OM_deg_b * OMb * L_NO3 * (1.0 - L_O2)
  REAC_OMb       = REAC_OMb       - R_OM_deg_b_NO3
  REAC_TOT_NO3   = REAC_TOT_NO3   - 4.0/5.0 * s2p * R_OM_deg_b_NO3
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 +  s2p * R_OM_deg_b_NO3
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_NO3
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_b_NO3
  REAC_N2        = REAC_N2        + 2.0/5.0 * s2p * R_OM_deg_b_NO3

  R_OM_deg_a_MnO2 = k_OM_deg_a * OMa  * L_MnO2 * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMa        = REAC_OMa       - R_OM_deg_a_MnO2
  REAC_MnO2       = REAC_MnO2      - 2 * R_OM_deg_a_MnO2
  REAC_TOT_H2CO3  = REAC_TOT_H2CO3 + s2p * R_OM_deg_a_MnO2
  REAC_TOT_NH4    = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_MnO2
  REAC_TOT_H2PO4  = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_a_MnO2
  REAC_Mn2        = REAC_Mn2       + 2.0 * s2p * R_OM_deg_a_MnO2

  R_OM_deg_b_MnO2 = k_OM_deg_b * OMb  * L_MnO2 * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMb        = REAC_OMb       - R_OM_deg_b_MnO2
  REAC_MnO2       = REAC_MnO2      - 2.0 * R_OM_deg_b_MnO2
  REAC_TOT_H2CO3  = REAC_TOT_H2CO3 + s2p * R_OM_deg_b_MnO2
  REAC_TOT_NH4    = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_MnO2
  REAC_TOT_H2PO4  = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_b_MnO2
  REAC_Mn2        = REAC_Mn2       + 2.0 * s2p * R_OM_deg_b_MnO2

  R_OM_deg_a_FeOH3 = k_OM_deg_a * OMa  * L_FeOH3 * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMa         = REAC_OMa       - R_OM_deg_a_FeOH3
  REAC_FeOH3       = REAC_FeOH3     - 4.0 * chi_Fe * R_OM_deg_a_FeOH3
  REAC_FeOH3_P     = REAC_FeOH3_P   - 4.0 * (1.0 - chi_Fe) * R_OM_deg_a_FeOH3
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 + s2p * R_OM_deg_a_FeOH3
  REAC_TOT_NH4     = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_FeOH3
  REAC_TOT_H2PO4   = REAC_TOT_H2PO4 + (f_OM_c + 4.0 * lambda * (1.0 - chi_Fe)) * s2p * R_OM_deg_a_FeOH3
  REAC_Fe2         = REAC_Fe2       + 4.0 * s2p * R_OM_deg_a_FeOH3

  R_OM_deg_b_FeOH3 = k_OM_deg_b * OMb  * L_FeOH3 * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMb         = REAC_OMb       - R_OM_deg_b_FeOH3
  REAC_FeOH3       = REAC_FeOH3     - 4.0 * chi_Fe * R_OM_deg_b_FeOH3
  REAC_FeOH3_P     = REAC_FeOH3_P   - 4.0 * (1.0 - chi_Fe) * R_OM_deg_b_FeOH3
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 + s2p * R_OM_deg_b_FeOH3
  REAC_TOT_NH4     = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_FeOH3
  REAC_TOT_H2PO4   = REAC_TOT_H2PO4 + (f_OM_c + 4.0 * lambda * (1.0 - chi_Fe)) * s2p * R_OM_deg_b_FeOH3
  REAC_Fe2         = REAC_Fe2       + 4.0 * s2p * R_OM_deg_b_FeOH3

  R_OM_deg_a_SO4 = k_OM_deg_a * OMa  * L_SO4 * (1.0 - L_FeOH3) * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMa       = REAC_OMa       - R_OM_deg_a_SO4
  REAC_TOT_SO4   = REAC_TOT_SO4   - 0.5 * s2p * R_OM_deg_a_SO4
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + s2p * R_OM_deg_a_SO4
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_SO4
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_a_SO4
  REAC_TOT_H2S   = REAC_TOT_H2S   + 0.5 * s2p * R_OM_deg_a_SO4

  R_OM_deg_b_SO4 = k_OM_deg_b * OMb  * L_SO4 * (1.0 - L_FeOH3) * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMb       = REAC_OMb       - R_OM_deg_b_SO4
  REAC_TOT_SO4   = REAC_TOT_SO4   - 0.5 * s2p * R_OM_deg_b_SO4
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + s2p * R_OM_deg_b_SO4
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_SO4
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_b_SO4
  REAC_TOT_H2S   = REAC_TOT_H2S   + 0.5 * s2p * R_OM_deg_b_SO4

  R_OM_deg_a_CH4 = k_OM_deg_a * OMa  * (1.0 - L_SO4) * (1.0 - L_FeOH3) * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0- L_O2)
  REAC_OMa       = REAC_OMa       - R_OM_deg_a_CH4
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + 0.5 * s2p * R_OM_deg_a_CH4
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_a_CH4
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_a_CH4
  REAC_CH4       = REAC_CH4       + 0.5 * s2p * R_OM_deg_a_CH4

  R_OM_deg_b_CH4 = k_OM_deg_b * OMb  * (1.0 - L_SO4) * (1.0 - L_FeOH3) * (1.0 - L_MnO2) * (1.0 - L_NO3) * (1.0 - L_O2)
  REAC_OMb       = REAC_OMb       - R_OM_deg_b_CH4
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + 0.5 * s2p * R_OM_deg_b_CH4
  REAC_TOT_NH4   = REAC_TOT_NH4   + f_OM_b * s2p * R_OM_deg_b_CH4
  REAC_TOT_H2PO4 = REAC_TOT_H2PO4 + f_OM_c * s2p * R_OM_deg_b_CH4
  REAC_CH4       = REAC_CH4       + 0.5 * s2p * R_OM_deg_b_CH4

  ! ---------------------------------------------------------
  ! > H2S oxidation with O2
  ! ---------------------------------------------------------
  ! mol m-3_pw yr-1
  R_H2S_ox_by_O2 = k_H2S_ox_by_O2 * O2 * TOT_H2S
  REAC_O2        = REAC_O2      - 2.0 * R_H2S_ox_by_O2
  REAC_TOT_H2S   = REAC_TOT_H2S - R_H2S_ox_by_O2
  REAC_TOT_SO4   = REAC_TOT_SO4 + R_H2S_ox_by_O2

  ! ---------------------------------------------------------
  ! > SO4 reduction coupled to AOM
  ! ---------------------------------------------------------
  ! mol m-3_pw yr-1
  R_SO4_red_by_CH4 = k_SO4_red_by_CH4 * SO4 * CH4
  REAC_TOT_SO4     = REAC_TOT_SO4   - R_SO4_red_by_CH4
  REAC_CH4         = REAC_CH4       - R_SO4_red_by_CH4
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 + R_SO4_red_by_CH4
  REAC_TOT_H2S     = REAC_TOT_H2S   + R_SO4_red_by_CH4

  ! ---------------------------------------------------------
  ! > FeOH3 / FeOH3_P reduction coupled to H2S oxidation
  ! ---------------------------------------------------------
  ! mol m-3_sf yr-1
  R_FeOH3_red_by_H2S = k_FeOH3_red_by_H2S * FeOH3_tot * TOT_H2S
  REAC_FeOH3         = REAC_FeOH3     - 2.0 * chi_Fe * R_FeOH3_red_by_H2S
  REAC_FeOH3_P       = REAC_FeOH3_P   - 2.0 * (1.0 - chi_Fe) * R_FeOH3_red_by_H2S
  REAC_TOT_H2S       = REAC_TOT_H2S   - s2p * R_FeOH3_red_by_H2S
  REAC_Fe2           = REAC_Fe2       + 2.0 * s2p * R_FeOH3_red_by_H2S
  REAC_S0            = REAC_S0        + R_FeOH3_red_by_H2S
  REAC_TOT_H2PO4     = REAC_TOT_H2PO4 + s2p * 2.0 * (1.0 - chi_Fe) * lambda * R_FeOH3_red_by_H2S

  ! ---------------------------------------------------------
  ! > FeOH3 / FeOH3_P formation
  ! ---------------------------------------------------------
  ! mol m-3_pw yr-1
  R_FeOH3_formation = k_FeOH3_formation * O2 * Fe2
  REAC_O2           = REAC_O2        - R_FeOH3_formation
  REAC_Fe2          = REAC_Fe2       - 4.0 * R_FeOH3_formation
  REAC_TOT_H2PO4    = REAC_TOT_H2PO4 - 4.0 * lambda * L_PO4 * R_FeOH3_formation
  REAC_FeOH3        = REAC_FeOH3     + 4.0 * (1.0 - L_PO4) * p2s * R_FeOH3_formation
  REAC_FeOH3_P      = REAC_FeOH3_P   + 4.0 * L_PO4 * p2s * R_FeOH3_formation

  ! ---------------------------------------------------------
  ! > Fe2 oxidation by MnO2
  ! ---------------------------------------------------------
  ! mol m-3_svf yr-1
  R_Fe2_ox_by_MnO2 = k_Fe2_ox_by_MnO2 * MnO2 * Fe2
  REAC_Fe2         = REAC_Fe2   - 2.0 * s2p * R_Fe2_ox_by_MnO2
  REAC_MnO2        = REAC_MnO2  - R_Fe2_ox_by_MnO2
  REAC_FeOH3       = REAC_FeOH3 + 2.0 * R_Fe2_ox_by_MnO2
  REAC_Mn2         = REAC_Mn2   + s2p * R_Fe2_ox_by_MnO2

  ! ---------------------------------------------------------
  ! > NH4 oxidation by O2
  ! ---------------------------------------------------------
  ! mol m-3_pw yr-1
  R_NH4_ox_by_O2 = k_NH4_ox_by_O2 * NH4 * O2
  REAC_TOT_NH4   = REAC_TOT_NH4 - R_NH4_ox_by_O2
  REAC_O2        = REAC_O2      - 2.0 * R_NH4_ox_by_O2
  REAC_TOT_NO3   = REAC_TOT_NO3 + R_NH4_ox_by_O2

  ! ---------------------------------------------------------
  ! > H2S oxidation by MnO2
  ! ---------------------------------------------------------
  ! mol m-3_svf yr-1
  R_H2S_ox_by_MnO2 = k_H2S_ox_by_MnO2 * TOT_H2S * MnO2
  REAC_TOT_H2S     = REAC_TOT_H2S - s2p * R_H2S_ox_by_MnO2
  REAC_MnO2        = REAC_MnO2    - R_H2S_ox_by_MnO2
  REAC_Mn2         = REAC_Mn2     + s2p * R_H2S_ox_by_MnO2
  REAC_S0          = REAC_S0      + R_H2S_ox_by_MnO2

  ! ---------------------------------------------------------
  ! > FeS oxidation by O2
  ! ---------------------------------------------------------
  ! mol m-3_svf yr-1
  R_FeS_ox_by_O2 = k_FeS_ox_by_O2 * FeS * O2
  REAC_FeS       = REAC_FeS     - R_FeS_ox_by_O2
  REAC_O2        = REAC_O2      - 2.0 * s2p * R_FeS_ox_by_O2
  REAC_Fe2       = REAC_Fe2     + s2p * R_FeS_ox_by_O2
  REAC_TOT_SO4   = REAC_TOT_SO4 + s2p * R_FeS_ox_by_O2

  ! ---------------------------------------------------------
  ! > CH4 oxidation by O2
  ! ---------------------------------------------------------
  ! mol m-3_pw yr-1
  R_CH4_ox_by_O2 = k_CH4_ox_by_O2 * CH4 * O2
  REAC_CH4       = REAC_CH4       - R_CH4_ox_by_O2
  REAC_O2        = REAC_O2        - 2.0 * R_CH4_ox_by_O2
  REAC_TOT_H2CO3 = REAC_TOT_H2CO3 + R_CH4_ox_by_O2

  ! ---------------------------------------------------------
  ! > Siderite Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_FeCO3 = (Fe2 * CO3) / Ksp_FeCO3

  DO i = 1, N_grid
    IF (omega_FeCO3(i) > 1.0) THEN
      R_precip_FeCO3(i) = k_precip_FeCO3 * (omega_FeCO3(i) - 1.0) * tanh(omega_FeCO3(i) - 1.0)
      R_diss_FeCO3(i)   = 0.0
    ELSE
      R_precip_FeCO3(i) = 0.0
      R_diss_FeCO3(i)   = k_diss_FeCO3 * FeCO3(i) * (1.0 - omega_FeCO3(i)) * tanh(1.0 - omega_FeCO3(i))
    END IF
  END DO

  R_precip_FeCO3   = R_precip_FeCO3 * precipitation
  R_diss_FeCO3     = R_diss_FeCO3   * precipitation

  REAC_Fe2         = REAC_Fe2       - s2p * R_precip_FeCO3 + s2p * R_diss_FeCO3
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 - s2p * R_precip_FeCO3 + s2p * R_diss_FeCO3
  REAC_FeCO3       = REAC_FeCO3     + R_precip_FeCO3       - R_diss_FeCO3

  ! ---------------------------------------------------------
  ! > CaCO3 Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_CaCO3 = (Ca2 * CO3) / Ksp_CaCO3

  DO i = 1, N_grid
    IF (omega_CaCO3(i) > 1.0) THEN
      R_precip_CaCO3(i) = k_precip_CaCO3 * (omega_CaCO3(i) - 1.0) * tanh(omega_CaCO3(i) - 1.0)
      R_diss_CaCO3(i)   = 0.0
    ELSE
      R_precip_CaCO3(i) = 0.0
      R_diss_CaCO3(i)   = k_diss_CaCO3 * CaCO3(i) * (1.0 - omega_CaCO3(i)) * tanh(1.0 - omega_CaCO3(i))
    END IF
  END DO

  R_precip_CaCO3   = R_precip_CaCO3 * precipitation
  R_diss_CaCO3     = R_diss_CaCO3   * precipitation

  REAC_Ca2         = REAC_Ca2       - s2p * R_precip_CaCO3 + s2p * R_diss_CaCO3
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 - s2p * R_precip_CaCO3 + s2p * R_diss_CaCO3
  REAC_CaCO3       = REAC_CaCO3     + R_precip_CaCO3       - R_diss_CaCO3

  ! ---------------------------------------------------------
  ! > FeS Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_FeS = (Fe2 * HS) / (H * Ksp_FeS)

  DO i = 1, N_grid
    IF (omega_FeS(i) > 1.0) THEN
      R_precip_FeS(i) = k_precip_FeS * (omega_FeS(i) - 1.0) * tanh(omega_FeS(i) - 1.0)
      R_diss_FeS(i)   = 0.0
    ELSE
      R_precip_FeS(i) = 0.0
      R_diss_FeS(i)   = k_diss_FeS * FeS(i) * (1.0 - omega_FeS(i)) * tanh(1.0 - omega_FeS(i))
    END IF
  END DO

  R_precip_FeS   = R_precip_FeS * precipitation
  R_diss_FeS     = R_diss_FeS   * precipitation

  REAC_Fe2         = REAC_Fe2       - s2p * R_precip_FeS + s2p * R_diss_FeS
  REAC_TOT_H2S     = REAC_TOT_H2S   - s2p * R_precip_FeS + s2p * R_diss_FeS
  REAC_FeS         = REAC_FeS       + R_precip_FeS       - R_diss_FeS

  ! ---------------------------------------------------------
  ! > Vivianite Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_vivianite = (Fe2 * PO4**(2/3)) / Ksp_vivianite**(1/3)

  DO i = 1, N_grid
    IF (omega_vivianite(i) > 1.0) THEN
      R_precip_vivianite(i) = k_precip_vivianite * (omega_vivianite(i) - 1.0) * tanh(omega_vivianite(i) - 1.0)
      R_diss_vivianite(i)   = 0.0
    ELSE
      R_precip_vivianite(i) = 0.0
      R_diss_vivianite(i)   = k_diss_vivianite * vivianite(i) * (1.0 - omega_vivianite(i)) * tanh(1.0 - omega_vivianite(i))
    END IF
  END DO

  R_precip_vivianite   = R_precip_vivianite * precipitation
  R_diss_vivianite     = R_diss_vivianite   * precipitation

  REAC_Fe2         = REAC_Fe2       - 3.0 * s2p * R_precip_vivianite + 3.0 * s2p * R_diss_vivianite
  REAC_TOT_H2PO4   = REAC_TOT_H2PO4 - 2.0 * s2p * R_precip_vivianite + 2.0 * s2p * R_diss_vivianite
  REAC_vivianite   = REAC_vivianite + R_precip_vivianite             - R_diss_vivianite

  ! ---------------------------------------------------------
  ! > Apatite Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_apatite = (Ca2 * HPO4**(2/3)) / (Ksp_apatite**(1/3) / H**(2/3))

  DO i = 1, N_grid
    IF (omega_apatite(i) > 1.0) THEN
      R_precip_apatite(i) = k_precip_apatite * (omega_apatite(i) - 1.0) * tanh(omega_apatite(i) - 1.0)
      R_diss_apatite(i)   = 0.0
    ELSE
      R_precip_apatite(i) = 0.0
      R_diss_apatite(i)   = k_diss_apatite * apatite(i) * (1.0 - omega_apatite(i)) * tanh(1.0 - omega_apatite(i))
    END IF
  END DO

  R_precip_apatite   = R_precip_apatite * precipitation
  R_diss_apatite     = R_diss_apatite   * precipitation

  REAC_Ca2         = REAC_Ca2       - 3.0 * s2p * R_precip_apatite + 3.0 * s2p * R_diss_apatite
  REAC_TOT_H2PO4   = REAC_TOT_H2PO4 - 2.0 * s2p * R_precip_apatite + 2.0 * s2p * R_diss_apatite
  REAC_apatite     = REAC_apatite   + R_precip_apatite             - R_diss_apatite

  ! ---------------------------------------------------------
  ! > MnCO3 Precipitation / Dissolution
  ! ---------------------------------------------------------
  ! both in mol m-3_sf yr-1
  omega_MnCO3 = (Mn2 * CO3) / Ksp_MnCO3

  DO i = 1, N_grid
    IF (omega_MnCO3(i) > 1.0) THEN
      R_precip_MnCO3(i) = k_precip_MnCO3 * (omega_MnCO3(i) - 1.0) * tanh(omega_MnCO3(i) - 1.0)
      R_diss_MnCO3(i)   = 0.0
    ELSE
      R_precip_MnCO3(i) = 0.0
      R_diss_MnCO3(i)   = k_diss_MnCO3 * MnCO3(i) * (1.0 - omega_MnCO3(i)) * tanh(1.0 - omega_MnCO3(i))
    END IF
  END DO

  R_precip_MnCO3   = R_precip_MnCO3 * precipitation
  R_diss_MnCO3     = R_diss_MnCO3   * precipitation

  REAC_Mn2         = REAC_Mn2       - s2p * R_precip_MnCO3 + s2p * R_diss_MnCO3
  REAC_TOT_H2CO3   = REAC_TOT_H2CO3 - s2p * R_precip_MnCO3 + s2p * R_diss_MnCO3
  REAC_MnCO3       = REAC_MnCO3     + R_precip_MnCO3       - R_diss_MnCO3


  ! ---------------------------------------------------------
  ! > Alkalinity
  ! ---------------------------------------------------------
  REAC_TOT_H = -1.0 * (REAC_TOT_H2PO4 + REAC_TOT_NO3 + 2.0 * REAC_TOT_SO4 &
               - REAC_TOT_NH4 - 2.0 * REAC_Ca2 - 2.0 * REAC_Mn2 - 2.0 * REAC_Fe2)


  !-----------------------------------------------------------------------------
  ! Total change in Concentration
  !-----------------------------------------------------------------------------
  i = 1
  ! Solutes
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_O2 + TRAN_O2; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_CH4 + TRAN_CH4; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_N2 + TRAN_N2; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_Mn2 + TRAN_Mn2; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_Fe2 + TRAN_Fe2; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_Ca2 + TRAN_Ca2; i = i + 1
  ! Solids
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_OMa + TRAN_OMa; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_OMb + TRAN_OMb; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_MnO2 + TRAN_MnO2; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_FeOH3 + TRAN_FeOH3; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_FeOH3_P + TRAN_FeOH3_P; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_FeCO3 + TRAN_FeCO3; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_S0 + TRAN_S0; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_FeS + TRAN_FeS; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_vivianite + TRAN_vivianite; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_CaCO3 + TRAN_CaCO3; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_apatite + TRAN_apatite; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_MnCO3 + TRAN_MnCO3; i = i + 1
  ! Lump Sums (tableau totals)
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_H + TRAN_TOT_H; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_H2CO3 + TRAN_TOT_H2CO3; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_H2PO4 + TRAN_TOT_H2PO4; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_NO3 + TRAN_TOT_NO3; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_NH4 + TRAN_TOT_NH4; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_SO4 + TRAN_TOT_SO4; i = i + 1
  ydot(((i-1) * N_grid + 1) : (i * N_grid)) = REAC_TOT_H2S + TRAN_TOT_H2S; i = i + 1

  !-----------------------------------------------------------------------------
  ! Other Return Values
  !-----------------------------------------------------------------------------
  i = 1

  ! Grid properties (xmid, xint, por_mid, svf_mid, Db_int)
  yout(i : i + N_grid - 1) = xmid           ; i = i + N_grid
  yout(i : i + N_grid)     = xint           ; i = i + N_grid + 1
  yout(i : i + N_grid - 1) = por_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = svf_mid        ; i = i + N_grid
  yout(i : i + N_grid)     = Db_int         ; i = i + N_grid + 1


  ! Speciation
  yout(i : i + N_grid - 1) = H              ; i = i + N_grid
  yout(i : i + N_grid - 1) = OH             ; i = i + N_grid
  yout(i : i + N_grid - 1) = H2CO3          ; i = i + N_grid
  yout(i : i + N_grid - 1) = HCO3           ; i = i + N_grid
  yout(i : i + N_grid - 1) = CO3            ; i = i + N_grid
  yout(i : i + N_grid - 1) = H3PO4          ; i = i + N_grid
  yout(i : i + N_grid - 1) = H2PO4          ; i = i + N_grid
  yout(i : i + N_grid - 1) = HPO4           ; i = i + N_grid
  yout(i : i + N_grid - 1) = PO4            ; i = i + N_grid
  yout(i : i + N_grid - 1) = HNO3           ; i = i + N_grid
  yout(i : i + N_grid - 1) = NO3            ; i = i + N_grid
  yout(i : i + N_grid - 1) = NH4            ; i = i + N_grid
  yout(i : i + N_grid - 1) = NH3            ; i = i + N_grid
  yout(i : i + N_grid - 1) = H2SO4          ; i = i + N_grid
  yout(i : i + N_grid - 1) = HSO4           ; i = i + N_grid
  yout(i : i + N_grid - 1) = SO4            ; i = i + N_grid
  yout(i : i + N_grid - 1) = H2S            ; i = i + N_grid
  yout(i : i + N_grid - 1) = HS             ; i = i + N_grid
  yout(i : i + N_grid - 1) = S2             ; i = i + N_grid

  ! pH
  yout(i : i + N_grid - 1) = pH             ; i = i + N_grid

  ! success
  yout(i : i + N_grid - 1) = success        ; i = i + N_grid

  ! Transport: dC
  yout(i : i + N_grid - 1) = TRAN_O2        ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_CH4       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_N2        ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_Mn2       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_Fe2       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_Ca2       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_OMa       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_OMb       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_MnO2      ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_FeOH3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_FeOH3_P   ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_FeCO3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_S0        ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_FeS       ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_vivianite ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_CaCO3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_apatite   ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_MnCO3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_H     ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_H2CO3 ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_H2PO4 ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_NO3   ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_NH4   ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_SO4   ; i = i + N_grid
  yout(i : i + N_grid - 1) = TRAN_TOT_H2S   ; i = i + N_grid

  ! Transport: flux_up
  yout(i) = FLUX_O2(1)                 ; i = i + 1
  yout(i) = FLUX_CH4(1)                ; i = i + 1
  yout(i) = FLUX_N2(1)                 ; i = i + 1
  yout(i) = FLUX_Mn2(1)                ; i = i + 1
  yout(i) = FLUX_Fe2(1)                ; i = i + 1
  yout(i) = FLUX_Ca2(1)                ; i = i + 1
  yout(i) = FLUX_OMa(1)                ; i = i + 1
  yout(i) = FLUX_OMb(1)                ; i = i + 1
  yout(i) = FLUX_MnO2(1)               ; i = i + 1
  yout(i) = FLUX_FeOH3(1)              ; i = i + 1
  yout(i) = FLUX_FeOH3_P(1)            ; i = i + 1
  yout(i) = FLUX_FeCO3(1)              ; i = i + 1
  yout(i) = FLUX_S0(1)                 ; i = i + 1
  yout(i) = FLUX_FeS(1)                ; i = i + 1
  yout(i) = FLUX_vivianite(1)          ; i = i + 1
  yout(i) = FLUX_CaCO3(1)              ; i = i + 1
  yout(i) = FLUX_apatite(1)            ; i = i + 1
  yout(i) = FLUX_MnCO3(1)              ; i = i + 1
  yout(i) = FLUX_TOT_H(1)              ; i = i + 1
  yout(i) = FLUX_TOT_H2CO3(1)          ; i = i + 1
  yout(i) = FLUX_TOT_H2PO4(1)          ; i = i + 1
  yout(i) = FLUX_TOT_NO3(1)            ; i = i + 1
  yout(i) = FLUX_TOT_NH4(1)            ; i = i + 1
  yout(i) = FLUX_TOT_SO4(1)            ; i = i + 1
  yout(i) = FLUX_TOT_H2S(1)            ; i = i + 1

  ! Transport: flux_down
  yout(i) = FLUX_O2(N_grid + 1)        ; i = i + 1
  yout(i) = FLUX_CH4(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_N2(N_grid + 1)        ; i = i + 1
  yout(i) = FLUX_Mn2(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_Fe2(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_Ca2(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_OMa(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_OMb(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_MnO2(N_grid + 1)      ; i = i + 1
  yout(i) = FLUX_FeOH3(N_grid + 1)     ; i = i + 1
  yout(i) = FLUX_FeOH3_P(N_grid + 1)   ; i = i + 1
  yout(i) = FLUX_FeCO3(N_grid + 1)     ; i = i + 1
  yout(i) = FLUX_S0(N_grid + 1)        ; i = i + 1
  yout(i) = FLUX_FeS(N_grid + 1)       ; i = i + 1
  yout(i) = FLUX_vivianite(N_grid + 1) ; i = i + 1
  yout(i) = FLUX_CaCO3(N_grid + 1)     ; i = i + 1
  yout(i) = FLUX_apatite(N_grid + 1)   ; i = i + 1
  yout(i) = FLUX_MnCO3(N_grid + 1)     ; i = i + 1
  yout(i) = FLUX_TOT_H(N_grid + 1)     ; i = i + 1
  yout(i) = FLUX_TOT_H2CO3(N_grid + 1) ; i = i + 1
  yout(i) = FLUX_TOT_H2PO4(N_grid + 1) ; i = i + 1
  yout(i) = FLUX_TOT_NO3(N_grid + 1)   ; i = i + 1
  yout(i) = FLUX_TOT_NH4(N_grid + 1)   ; i = i + 1
  yout(i) = FLUX_TOT_SO4(N_grid + 1)   ; i = i + 1
  yout(i) = FLUX_TOT_H2S(N_grid + 1)   ; i = i + 1

  ! Reactions: dC
  yout(i : i + N_grid - 1) = REAC_O2 * por_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_CH4 * por_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_N2 * por_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_Mn2 * por_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_Fe2 * por_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_Ca2 * por_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_OMa * svf_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_OMb * svf_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_MnO2 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_FeOH3 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_FeOH3_P * svf_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_FeCO3 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_S0 * svf_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_FeS * svf_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_vivianite * svf_mid ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_CaCO3 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_apatite * svf_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_MnCO3 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_H * por_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_H2CO3 * por_mid ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_H2PO4 * por_mid ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_NO3 * por_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_NH4 * por_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_SO4 * por_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = REAC_TOT_H2S * por_mid   ; i = i + N_grid

  ! Reaction Rates
  yout(i : i + N_grid - 1) = R_OM_deg_a_O2 * svf_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_O2 * svf_mid       ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_a_NO3 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_NO3 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_a_MnO2 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_MnO2 * svf_mid     ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_a_FeOH3 * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_FeOH3 * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_a_SO4 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_SO4 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_a_CH4 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_OM_deg_b_CH4 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_H2S_ox_by_O2 * por_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_SO4_red_by_CH4 * por_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_FeOH3_red_by_H2S * svf_mid  ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_FeOH3_formation * por_mid   ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_Fe2_ox_by_MnO2 * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_NH4_ox_by_O2 * por_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_H2S_ox_by_MnO2 * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_FeS_ox_by_O2 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_CH4_ox_by_O2 * por_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_FeCO3 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_FeCO3 * svf_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_FeS * svf_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_FeS * svf_mid          ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_vivianite * svf_mid  ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_vivianite * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_CaCO3 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_CaCO3 * svf_mid        ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_apatite * svf_mid    ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_apatite * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_precip_MnCO3 * svf_mid      ; i = i + N_grid
  yout(i : i + N_grid - 1) = R_diss_MnCO3 * svf_mid        ; i = i + N_grid

  ! omegas
  yout(i : i + N_grid - 1) = omega_FeCO3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = omega_CaCO3     ; i = i + N_grid
  yout(i : i + N_grid - 1) = omega_FeS       ; i = i + N_grid
  yout(i : i + N_grid - 1) = omega_vivianite ; i = i + N_grid
  yout(i : i + N_grid - 1) = omega_apatite   ; i = i + N_grid
  yout(i : i + N_grid - 1) = omega_MnCO3     ; i = i + N_grid
          
  return
    
END SUBROUTINE derivs


SUBROUTINE initmod (odeparms)

  IMPLICIT NONE

  external odeparms
  INTEGER, PARAMETER :: N_parms = 56
  REAL(kind = 8)     :: parms(N_parms)
  common /myparms/parms
  call odeparms(N_parms, parms)

  return

END SUBROUTINE initmod
  
  
SUBROUTINE forcing_functions (odeforcs)

  IMPLICIT NONE

  external odeforcs
  INTEGER, PARAMETER :: N_forcs = 31
  REAL(kind = 8)     :: forcs(N_forcs)
  common /myforcs/forcs
  call odeforcs(N_forcs, forcs)

  return

END SUBROUTINE forcing_functions