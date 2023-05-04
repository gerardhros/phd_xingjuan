require(data.table)
require(ggplot2)
library(dplyr)
library("ggpubr")
library("RColorBrewer")

# faske test dataset with measured and calculated bc_meas
# read in the input data
d1 <- readxl::read_xlsx('2023 updated data for paper 2.xlsx', sheet = "test")
d1 <- as.data.table(d1)
#d1 <- d1[sitename =='42 Fujian']  #filter(.data =d1, ct == "u");d1[sitename =='40 Jinxian']

#28 0.011,27 0.028, 40 0.019,42 0.007,42 
# minimizing Error 27 Qiyang 38;28 Guiyang 18.8 (19); 40 Jinxian 58.4 (58); 42 Fujian 9.94 (10)
#minimizing logError 27 Qiyang 41;28 Guiyang 17.7 (18); 40 Jinxian 33.2 (33); 42 Fujian 58.4 (58)
# update the fertilizer data (all in kg / ha)
# P fertilizer is calcium superphosphate and K fertilizer is KCl
d1[,pin_i := p2o5in_i * 62 / 142]
d1[,sin_i := p2o5in_i * 13 / 17]
d1[,cain_i := p2o5in_i * 20 / 17]
d1[,kin_i := k2oin_i * 0.83]
d1[,clin_i := k2oin_i * 45.3 / 60]

#total fertilizer input

d1[,n_in_fer := nin_i + nin_o]
d1[,p_in_fer := pin_i + pin_o]
d1[,k_in_fer := kin_i + kin_o]
d1[,ca_in_fer := cain_i + cain_o]
d1[,mg_in_fer := mgin_o]
d1[,na_in_fer := nain_o]
d1[,s_in_fer := sin_i + sin_o]
d1[,cl_in_fer := clin_i + clin_o]
d1[,bc_in_fer := k_in_fer + ca_in_fer + mg_in_fer + na_in_fer]


# calculate crop nutrient uptake of crop 1 (wheat) for N, P and K for wheat (kg / ha)


d1[,c1_n := c1_yield_grain*(1-w2/100) * n_c1_grain * 0.001 + c1_yield_straw *(1-w/100)* n_c1_straw * 0.001]
d1[,c1_p := c1_yield_grain*(1-w2/100) * p_c1_grain * 0.001 + c1_yield_straw*(1-w/100) *  p_c1_straw * 0.001]
d1[,c1_k := c1_yield_grain*(1-w2/100) * k_c1_grain * 0.001 + c1_yield_straw *(1-w/100) * k_c1_straw * 0.001]
d1[,c1_ca := c1_yield_grain*(1-w/100) * ca_c1_grain * 0.001 + c1_yield_straw *(1-w/100) *  ca_c1_straw * 0.001]
d1[,c1_mg := c1_yield_grain*(1-w/100) * mg_c1_grain * 0.001 + c1_yield_straw  *(1-w/100)*  mg_c1_straw * 0.001]
d1[,c1_na := c1_yield_grain*(1-w/100) * na_c1_grain * 0.001 + c1_yield_straw *(1-w/100) *  na_c1_straw * 0.001]
d1[,c1_s := c1_yield_grain*(1-w/100) * s_c1_grain * 0.001 + c1_yield_straw *(1-w/100) *  s_c1_straw * 0.001]
d1[,c1_cl := c1_yield_grain*(1-w/100) * cl_c1_grain * 0.001 + c1_yield_straw  *(1-w/100)*  cl_c1_straw * 0.001]

# calculate crop nutrient uptake of crop 2 (maize) for N, P and K for wheat (kg / ha)

d1[,c2_n := c2_yield_grain*(1-w2/100) * n_c2_grain * 0.001 + c2_yield_straw *(1-w/100)* n_c2_straw * 0.001]
d1[,c2_p := c2_yield_grain*(1-w2/100) * p_c2_grain * 0.001 + c2_yield_straw*(1-w/100) *  p_c2_straw * 0.001]
d1[,c2_k := c2_yield_grain*(1-w2/100) * k_c2_grain * 0.001 + c2_yield_straw *(1-w/100) * k_c2_straw * 0.001]
d1[,c2_ca := c2_yield_grain*(1-w/100) * ca_c2_grain * 0.001 + c2_yield_straw *(1-w/100) *  ca_c2_straw * 0.001]
d1[,c2_mg := c2_yield_grain*(1-w/100) * mg_c2_grain * 0.001 + c2_yield_straw  *(1-w/100)*  mg_c2_straw * 0.001]
d1[,c2_na := c2_yield_grain*(1-w/100) * na_c2_grain * 0.001 + c2_yield_straw *(1-w/100) *  na_c2_straw * 0.001]
d1[,c2_s := c2_yield_grain*(1-w/100) * s_c2_grain * 0.001 + c2_yield_straw *(1-w/100) *  s_c2_straw * 0.001]
d1[,c2_cl := c2_yield_grain*(1-w/100) * cl_c2_grain * 0.001 + c2_yield_straw  *(1-w/100)*  cl_c2_straw * 0.001]


d1[,grain_total := c2_yield_grain + c1_yield_grain ]
d1[,straw_total := c1_yield_straw + c2_yield_straw ]
d1[,yield_total := grain_total + straw_total ]

d1[,up_n := c1_n + c2_n]
d1[,up_p := c1_p + c2_p]
d1[,up_k := c1_k + c2_k]
d1[,up_ca := c1_ca + c2_ca]
d1[,up_mg := c1_mg + c2_mg]
d1[,up_na := c1_na + c2_na]
d1[,up_s := c1_s + c2_s]
d1[,up_cl := c1_cl + c2_cl]

# calculate  N,P,s,cl  Uptake (keq / ha / year)
d1[,up_keq_nh4 := up_n /2 / 14]
d1[,up_keq_no3 := up_n /2 / 14]
d1[,up_keq_p := up_p / 31]
d1[,up_keq_s := up_s/32*2]
d1[,up_keq_cl := up_cl/35.5]

# calculate Base Cation Uptake (keq / ha / year)
d1[,up_keq_k := up_k / 39.0983]
d1[,up_keq_ca := up_ca / 40.078 * 2]
d1[,up_keq_mg := up_mg / 24.305 * 2]
d1[,up_keq_na := up_na / 22.989769]


# N balance (eq / m2 / year)
##no ammonia losses depending on pH

#n fertilizer inputs, n fixation and deposition input,  split over ammonium and nitrate
d1[,nh4in_keq :=  (n_in_fer + nfix) * 0.5 / 14 + depnh4* 10]
d1[,no3in_keq:=  (n_in_fer + nfix) * 0.5 / 14 + depno3* 10]

# nitrogen surplus corrected for nh3 (in kg N /ha and eq / ha)
#d1[,n_sur := (1 - avrate) * (nin_i + nin_o) + nfix + (depnh4 + depno3) * 140 - up_n ]
#d1[,n_surplus := nin_i + nin_o + nfix + (depnh4 + depno3) * 140 - up_n ]
d1[,n_keq_sur := n_sur / 14 ]

# P balance (eq / m2 / year)
d1[,pin_keq  := p_in_fer/31 + depp* 10 ]
d1[,p_sur := p_in_fer  + depp*310 - up_p]
d1[,p_keq_sur := p_sur/31]
d1[,p_keq_le := 0]




# S balance (eq / m2 / year)
d1[,sin_keq := s_in_fer/32*2 + depso4* 10 ]

d1[,s_sur := s_in_fer + depso4/2*320 - up_s]
d1[,s_keq_sur := s_sur/32*2 ]
d1[,s_keq_le := sin_keq - up_keq_s]


# Cl balance (eq / m2 / year) 
d1[,clin_keq := cl_in_fer/35.5 + depcl* 10 ]

d1[,cl_sur := cl_in_fer + depcl*355 - up_cl]
d1[,cl_keq_sur := cl_sur/35.5 ]
d1[,cl_keq_le := clin_keq - up_keq_cl]


# BC balance (eq / m2 / year)
d1[,kin_keq_fer := k_in_fer/39]
d1[,cain_keq_fer := ca_in_fer/40.078 * 2]
d1[,mgin_keq_fer := mgin_o/24.305 * 2]
d1[,nain_keq_fer:= nain_o/22.989769]
d1[,bcin_keq := kin_keq_fer + cain_keq_fer + mgin_keq_fer + nain_keq_fer + depbc*10]
d1[,up_keq_bc := up_keq_k+ up_keq_ca + up_keq_mg + up_keq_na  ]

# H balance (eq / m2 / year) 
d1[,h_c_manure := 10^(-ph_manure)]
d1[,hin_keq_o := 10*h_c_manure*10*manure_mount*(1-water_manure/100)*1000/10000]
#d1[,h_keq_in_i := nin_i/14*0.5 + pin_i/31 + sin_i/32*2 + clin_i/35.5 - cain_i/24.305 * 2 - kin_i/39 - nin_i/14*0.5 ]
d1[,h_keq_in_dep := 0 ]
d1[,hin_keq_i := 0]
d1[,hin_keq := h_keq_in_dep + hin_keq_o + hin_keq_i]
d1[,up_keq_h := 0]
d1 [,h_keq_le := (10^(-ph)*wsur *1000*10)*10]


# function to optimize
# with two arguments co2 pressure and leaching fraction, and d1 as data set being input
testfun <- function(inp = c(pco2,lefr),d1){
  
  # make local copy of argument d1
  d2 <- copy(d1)
  
  # substract the two input arguments from vector inp
  pco2 <- inp[1]
  lefr <- inp[2]
  
  # add here your functions to update bc_calc (here some fake ones)
  # leaching part (eq / m2 / year) 
  d1[,no3_keq_le := n_sur * lefr/14 ]
  d1[, h_c := (10^(-(ph)))]
  d1[, hco3_c := ((10^(-7.8)) * pco2 * 1000 /h_c)]
  d1[, hco3_keq_le := hco3_c * wsur*10]
  d1[, hco3in_keq := bcin_keq + nh4in_keq - no3in_keq - pin_keq -clin_keq - sin_keq]
  d1[, up_keq_hco3 := up_keq_bc + up_keq_nh4 - up_keq_no3 - up_keq_s - up_keq_p - up_keq_cl]
  d1[, bc_keq_le := cl_keq_le + s_keq_le + hco3_keq_le + p_keq_le + no3_keq_le - h_keq_le ]
  d1[, bc_keq_sur := depbc*10+ kin_keq_fer + cain_keq_fer + mgin_keq_fer + nain_keq_fer - up_keq_bc]
  d1[, bc_keq_poolc := depbc*10 + kin_keq_fer + cain_keq_fer + mgin_keq_fer + nain_keq_fer - up_keq_bc - bc_keq_le ]
  
  # retreive the Sum of Squares of difference between calculated and measured BC
  out <- d2[,sum((bc_m - bc_keq_poolc)^2)]
  
  # return out
  return(out)
}

# test function
testfun(inp=c(0.1,0.02),d1=d1)

# use the default solver
#opt <- optim(par = c(0.001,0.01),
             #fn = testfun,
             #d1 = d1)

# there is also a possibility to use optimizer BFGS where you can set minimum and maximum valuse for both input parameters co2p and denfr
# the initial values is given in "par", and the solver tries to adapt these in order to minimize the function fn
# the default solver is Nelder-Mead but that one can not have the minimum and maximum values.
# but note that the BFGS requires the gardient of the function to be minimized.  If you don't pass one it will try to use finite-differences to estimate it. this might generate errors when there is not gradient.
opt <- optim(par = c(0.001,0.001),
             fn = testfun,
             d1 = d1,
             method = 'L-BFGS-B',
             lower = rep(0.001, 2), #2 for 2 length
             upper = rep(0.001, 2))
