# ROTHC equations
# solved
# 12.10.2018
# by A. Mimis
# Panteion Uni
# ###############



# parameters
tree_species <- c("olive","orange","apple","almond","peach")
#production_period <- c(200, 50, 30, 30, 20)
fresh_to_dry <-c(0.486,0.235,0.16,0.941,0.154)
#debris (per tree)
pruning <- c(1.35, 1.27, 0.91, 0.85, 1.45) # input tn/ha/year dry content must multiply by carbon_in_wood
leaves  <- c(1.863, 2.92, 0.9, 0.77, 1.149) # tn/ha/year in dry content must multiply by carbon_in_wood annual_vegetation/(1+s/L)
#leaves  <- c(0.353, 1.109, 1.329, 0.901, 1.329) # tn/ha/year in dry content must multiply by carbon_in_wood spanos

aei8ales  <- c(TRUE, TRUE, FALSE, FALSE, FALSE)
fyllovolo <- c(FALSE, FALSE , TRUE, TRUE, TRUE)

#crop    <- c(6.561 , 9.4 ,7.448 , 4.5168, 4.62) # input Yield tn/ha/year in dry weight
#carbon_in_fruit <- c(0.15090, 0.01926, 0.02025, 0.59410, 0.01293)
crop_losses <- c(0.1, 0.2, 0.2, 0.1, 0.2)

#biomass (per tree)
carbon_in_wood <- 0.45
# y= a*x^b
# y = kg/tree/year and x = age
#apple missing value!
a_biomass <- c(10.3, 24.87, 53.97, 5.08 ,3.39 )
b_biomass <- rep( 0.3, 5)

#biomass_yearly_dev_rate <- c( 0.008, 0.0122, 0.00572, 0.004, 0.002 ) # dry weight spanos
pdensity <- c(300, 400, 350, 300, 600) # plants/ha

Shoot_over_Root <- c(0.3178, 0.3350, 0.3237, 0.3419, 0.3714)

CO2_lost_from_soil <-function(clay){
  
  # x is the ratio CO2/(BIO+HUM)
  x = 1.67 * (1.85 + 1.60 * exp(-0.0786*clay ) )

  # x/(x+1) is evolved to CO2
  # 1/(x+1) is formed as BIO+HUM

  return(x/(x+1))
}

tree_name_to_number<-function(n){
  tree_species <- c("olive","orange","apple","almond","peach")
  if(n=="olive" || n=="Olive"){
    return(1)
  }
  if(n=="olive" || n=="Olive"){
    return(1)
  }
  if(n=="orange" || n=="Orange"){
    return(2)
  }
  if(n=="apple" || n=="Apple"){
    return(3)
  }
  if(n=="almond" || n=="Almond"){
    return(4)
  }
  if(n=="peach" || n=="Peach"){
    return(5)
  }
  
  return(0)  
}

decomp_rate_constants <-function(pool_name){
    if(pool_name == "DPM"){
      k = 10.0
    } else if(pool_name == "RPM"){
      k = 0.3
    } else if(pool_name == "BIO"){
      k = 0.66
    } else if(pool_name == "HUM"){
      k = 0.02
    }else{
      print("Problem in pool names")
    }
  
    return(k)
}

# tmp - average monthly temperature
temp_modyfying_factor<-function( tmp ){
  
  a = rep(0,12)

  for (i in 1:12){
    a[i] = 47.9/( 1+ exp(106/(tmp[i]+18.3) ) )
  }
  return(a)
}

check_limits<-function( acc, max_SDM ){
  
  if(acc<max_SDM){
    acc = max_SDM}

  if(acc>0.0){
    acc = 0.0}   

  return(acc)
}


moisture_modyfying_factor<-function( rainfall, evaporation, clay ){
  
  max_SMD <- -( 20.0 + 1.3*clay - 0.01*(clay**2) )[1]

  r_m_0p75_e <- rep(0.0,12)
  acc_SMD <- rep(0.0,12)
  for(i in 1:12){
    r_m_0p75_e[i] <- rainfall[i] - 0.75 * evaporation[i]}

  first_pos = FALSE   
  for(i in 1:12){
    if (! first_pos){
      if (r_m_0p75_e[i]<0.0){
        acc_SMD[i] <- r_m_0p75_e[i]
        acc_SMD[i] <- check_limits( acc_SMD[i], max_SMD )
        first_pos <- TRUE}
    }else{
      acc_SMD[i] <- acc_SMD[i-1] +r_m_0p75_e[i]
      acc_SMD[i] <- check_limits( acc_SMD[i], max_SMD )  }          
  }
  # cat("_0_")
  # cat(max_SMD)
  # cat("_1_")
  # cat(acc_SMD)

  b <- rep(1.0,12)
  for(i in 1:12){        
    if (acc_SMD[i] > (0.444*max_SMD) ){
      b[i] <- 1.0
    }else{
      b[i] <- 0.2 + ( 1.0-0.2 )*(max_SMD-acc_SMD[i])/(max_SMD-0.444*max_SMD)}
  }
  
  return(b)
}

# soil_veg - Boolean
# if it is True , soil is vegetated and c = 0.6
# if it is False , soil is vegetated and c = 1.0
plant_ret_modyfying_factor<-function( soil_veg ){
  
  c <- rep(0,12)
  for(i in 1:12){
    if(soil_veg[i]){
      c[i] <- 0.6
    }else{
      c[i] <- 1.0} # i had 0.1 by mistake!
  }
  return(c)
}

# an active compartment containts y (tonnes C per ha)
decomp_active_compart<-function( y, pool_name, a, b, c ){
  
  k <- decomp_rate_constants( pool_name)

  t <- 1.0/12.0

  m <- a*b*c
  #m = 0.3561
  #print "m"
  #print m

  after <- y*(1- exp(-k*m*t) )

  return(after)
}

# ipc - incoming plant carbon
# DPM/RPM = 1.44 i.e. 59% DPM and 41% RPM
rothc <- function( initial_pools, ipc, farmyard_manure, temperature, rainfall, evaporation, clay, soil_vegetated ){
  DPM <- initial_pools[1]
  RPM <- initial_pools[2]
  BIO <- initial_pools[3]
  HUM <- initial_pools[4]
  CO2 <- initial_pools[5]   
  
  a <- temp_modyfying_factor( temperature )
  b <- moisture_modyfying_factor( rainfall, evaporation, clay )
  c <- plant_ret_modyfying_factor( soil_vegetated )
  #print a
  #print b
  #print c

  for(i in 1:12){
    # from its pool the amount that is decomposed
    DPM_dec <- decomp_active_compart ( DPM, "DPM", a[i], b[i], c[i])
    RPM_dec <- decomp_active_compart ( RPM, "RPM", a[i], b[i], c[i])
    BIO_dec <- decomp_active_compart ( BIO, "BIO", a[i], b[i], c[i])
    HUM_dec <- decomp_active_compart ( HUM, "HUM", a[i], b[i], c[i]) 

    #print [DPM_dec, RPM_dec, BIO_dec, HUM_dec]
    total_dec <- DPM_dec+RPM_dec+BIO_dec+HUM_dec

    CO2 <- CO2 + CO2_lost_from_soil(clay) * total_dec
    BIO_p_HUM <-  (1.0- CO2_lost_from_soil(clay)) * total_dec

    # loss-gain material in compartments
    DPM <- DPM - DPM_dec
    RPM <- RPM - RPM_dec
    BIO <- BIO - BIO_dec + 0.46* BIO_p_HUM
    HUM <- HUM - HUM_dec + 0.54* BIO_p_HUM

    # new plant carbon in the first two compartments
    DPM <-  DPM + 0.59* ipc[i] 
    RPM <-  RPM + 0.41* ipc[i] 
    DPM <-  DPM + 0.49 * farmyard_manure[i] 
    RPM <-  RPM + 0.49 * farmyard_manure[i]         
    HUM <- HUM + 0.02 * farmyard_manure[i] 

    #print i
    #print "DPM, RPM, BIO, HUM, CO2"
    #print [DPM, RPM, BIO, HUM, CO2]
    #cat(DPM, RPM, BIO, HUM, CO2,"\n")
  }

  return( c(DPM, RPM, BIO, HUM, CO2) )
}

# y= a*x^b
# y = kg/tree/year and x = age
carbon_biomass_trunk_climatree_version<-function(a, b, age){
  
  c_trunk <- (a*(age**b))/1000.0

  return(c_trunk)
}


monthly_leaves<-function(tree_){
  
  c_in_leaves=rep(0.0,12)
  #equal ammount each month
  if(fyllovolo[tree_]){
    c_in_leaves = rep(leaves[tree_]* carbon_in_wood/12.0,12)}

  return(c_in_leaves)
}

# crop losses
# "olive" - Oct-Dec
# "orange" - Nov-July
# "apple" - Sept-Oct
# "almond" - Aug-Oct
# "peach" - May-Sept
monthly_crop_losses<-function(tree_, yield=0){
  total_amount<-0
  
  if(yield>0.001){
    total_amount = yield * crop_losses[tree_]* fresh_to_dry[tree_]*carbon_in_wood
  }else{
    total_amount = crop[tree_] * crop_losses[tree_]*carbon_in_wood
  }
  c_in_crop_losses = rep(0,12)

  if(tree_==1){
    c_in_crop_losses[10]= total_amount/3.0
    c_in_crop_losses[11] = total_amount/3.0
    c_in_crop_losses[12] = total_amount/3.0}
  
  if(tree_==2){
    c_in_crop_losses[1] = total_amount/9.0
    c_in_crop_losses[2] = total_amount/9.0
    c_in_crop_losses[3] = total_amount/9.0
    c_in_crop_losses[4] = total_amount/9.0
    c_in_crop_losses[5] = total_amount/9.0
    c_in_crop_losses[6] = total_amount/9.0
    c_in_crop_losses[7] = total_amount/9.0
    c_in_crop_losses[11] = total_amount/9.0
    c_in_crop_losses[12] = total_amount/9.0}
  
  if(tree_ == 3){
    c_in_crop_losses[9] = total_amount / 2.0
    c_in_crop_losses[10] = total_amount / 2.0}

  if(tree_== 4){
    c_in_crop_losses[8] = total_amount/3.0
    c_in_crop_losses[9] = total_amount/3.0
    c_in_crop_losses[10] = total_amount/3.0}

  if(tree_== 5){
    c_in_crop_losses[5]= total_amount/5.0
    c_in_crop_losses[6] = total_amount/5.0
    c_in_crop_losses[7] = total_amount/5.0
    c_in_crop_losses[8] = total_amount/5.0
    c_in_crop_losses[9] = total_amount/5.0}

  return(c_in_crop_losses)
}

# best period for pruning Feb-March
monthly_pruning<-function(tree_){
  c_in_pruning = rep(0,12)
  total_amount = pruning[tree_]* carbon_in_wood

  c_in_pruning[2] = total_amount/2.0
  c_in_pruning[3] = total_amount/2.0

  return(c_in_pruning)
}

# interpolation between now and future values
l_inter <- function( vnow, vfuture, i, number_of_years ){
  
  a<-(vfuture- vnow)/(1.0*number_of_years-1.0)
  b<-vnow-a
  
  return(1.0*a*i+b) 
}

# perc_age - couple of values giving percentage of trees @ a specific age
climatree_model<-function( number_of_years, surface, tree_, temperature, ftemperature = rep(NA,12), 
                           temp_future_change=0.0, rainfall, frainfall=rep(NA,12), 
                           evaporation, fevaporation = rep(NA,12), 
                           clay, rothc_initial_pools, soil_vegetated, 
                           yield=0, pdensity_=0, keep_res=TRUE, farmyard_manure=rep(0,12),
                           perc_age = data.frame(age=c(0),perc=c(1)) ){
  
  #current_pools <- rep(0,5) # DPM, RPM, BIO, HUM, CO2
  current_pools <- rothc_initial_pools #<-rep(0,5)
  
  pool_biomass_trunk <- 0
  pool_biomass_roots <- 0
  pool_debris <- rep(0,12)
  pool_soil <- 0

  df.pools = data.frame()
  # j is the year of the simulation
  for(j in 1:number_of_years){
    
    pool_biomass_trunk<-0.0
    if(pdensity_>0.001){
      #surface*
      for(k in 1:nrow(perc_age)){
          pool_biomass_trunk <- pool_biomass_trunk + perc_age$perc[k] * pdensity_ * carbon_biomass_trunk_climatree_version(a_biomass[tree_],
                                b_biomass[tree_], perc_age$age[k]+j ) * carbon_in_wood}
    }else{
      #surface*
      for(k in 1:nrow(perc_age)){
        pool_biomass_trunk <- pool_biomass_trunk + perc_age$perc[k] * pdensity_[tree_]* carbon_biomass_trunk_climatree_version(a_biomass[tree_],
                              b_biomass[tree_], perc_age$age[k]+j ) * carbon_in_wood}
    }
    #C_r=(Y/HI)*S:R*0.45
    pool_biomass_roots <- (pool_biomass_trunk*Shoot_over_Root[tree_]) * carbon_in_wood
    #C_e = 0.09*(Y/HI)*0.45 root exudates
    #C_w = 0.07*(Y/HI)*0.45 root weeds
    root_debris <- rep((0.09* pool_biomass_trunk + 0.07*pool_biomass_trunk)*carbon_in_wood/12.0,12)
    leaves_debris <- monthly_leaves(tree_)
    crop_debris <- monthly_crop_losses(tree_, yield)
    pruning_debris <-rep(0,12)
    if(keep_res){
      pruning_debris <- monthly_pruning(tree_)
    }
    
    #pool_debris <- surface * (pruning[tree_] + leaves[tree_] + crop[tree_]*crop_losses[tree_] + root_debris)* carbon_in_wood
    #*surface
    pool_debris <- (leaves_debris + crop_debris + pruning_debris)
    
    #*surface
    incoming_plant_carbon <- (root_debris + leaves_debris + crop_debris + pruning_debris)
    #cat(sum(incoming_plant_carbon),"\n")
    #climatic data are inteprolated between now and future values
    c_temp<-rep(0.0,12)
    c_rain<-rep(0.0,12)
    c_evap<-rep(0.0,12)
    if( !is.na(ftemperature)[1] ){
        c_temp <- l_inter( temperature, ftemperature, j,  number_of_years)
        c_rain <- l_inter( rainfall, frainfall, j,  number_of_years)
        c_evap <- l_inter( evaporation, fevaporation, j,  number_of_years)
    }else if( abs(temp_future_change)>0.1) {
        c_temp <- l_inter( temperature, temperature+temp_future_change, j,  number_of_years)
        c_rain<-rainfall
        c_evap<-evaporation
    }else{
        c_temp<-temperature
        c_rain<-rainfall
        c_evap<-evaporation
    }
      
    current_pools <- rothc(current_pools, incoming_plant_carbon, farmyard_manure, c_temp, c_rain, c_evap, clay, soil_vegetated )
    
    df.tmp <-data.frame( j, pool_biomass_trunk, pool_biomass_roots,sum(pool_debris),sum(current_pools[1:4]) )
    #print "CO2  ", str( current_pools[5])
    
    df.pools<-rbind(df.pools, df.tmp)
  }
  names(df.pools)<-c("year","biomass_trunk", "biomass_roots", "debris", "soil")
    
  return(df.pools)
}



