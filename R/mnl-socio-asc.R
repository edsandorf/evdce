rm(list = ls(all = TRUE)) # Clear the workspace
set.seed(12334)
# Loading R packages
library(apollo)       # Tools for Choice Model Estimation and Application

# Initialize Apollo
apollo_initialise()

# Set core controls
apollo_control = list(modelName  = "MNL_socdem_ASC",
                      modelDescr = "MNL windmils",
                      indivID    = "id_individual",
                      outputDirectory = "../ModelOutputs/")

# Importing data               
database <- read.csv(file="../DataAndDesigns/Data_windmills.csv")

# Eliminating NA in choice
database <- subset(database, !is.na(database$choice))

# Starting values of the parameters
apollo_beta = c(b_asc_alt1                =   0.00,
                b_asc_alt2                =   0.50,
                b_asc_alt3                =   0.50,
                b_medium_farms            =   0.25,
                b_small_farms             =   0.50,
                b_medium_height           =   0.25,
                b_low_height              =   0.50,
                b_red_kite                =  -0.05,
                b_min_distance            =   0.50,
                b_cost                    =  -0.50,
                # Interaction terms
                delta_asc2_age            =   0.00,
                delta_asc2_female         =   0.00, 
                delta_asc2_educ           =   0.00, 
                delta_asc3_age            =   0.00,  
                delta_asc3_female         =   0.00,  
                delta_asc3_educ           =   0.00, 
                delta_mf_age              =   0.00,
                delta_mf_female           =   0.00,
                delta_mf_educ             =   0.00,
                delta_sf_age              =   0.00,
                delta_sf_female           =   0.00,
                delta_sf_educ             =   0.00,
                delta_mh_age              =   0.00,
                delta_mh_female           =   0.00,
                delta_mh_educ             =   0.00,
                delta_lh_age              =   0.00,
                delta_lh_female           =   0.00,
                delta_lh_educ             =   0.00,
                delta_rk_age              =   0.00,
                delta_rk_female           =   0.00,
                delta_rk_educ             =   0.00,
                delta_md_age              =   0.00,
                delta_md_female           =   0.00,
                delta_md_educ             =   0.00,
                delta_ct_age              =   0.00,
                delta_ct_female           =   0.00,
                delta_ct_educ             =   0.00)

# Vector of parameters to be kept fixed at their starting value 
apollo_fixed = c("b_asc_alt1")

# Group and validate inputs
apollo_inputs = apollo_validateInputs()

# Define model and likelihood function 
apollo_probabilities = function(apollo_beta, 
                                apollo_inputs, 
                                functionality = "estimate"){
  
  ## Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  ## Create list of probabilities P
  P = list()
  ## List of utilities: these must use the same names as in mnl_settings
  V = list()
  V[['alt1']]  =  (    b_asc_alt1                               
                   + (b_medium_farms    
                      +   delta_mf_age     * age 
                      +   delta_mf_female  * female
                      +   delta_mf_educ    * education) * alt1_farm2          
                   + (b_small_farms   
                      +   delta_sf_age     * age 
                      +   delta_sf_female  * female
                      +   delta_sf_educ    * education) * alt1_farm3   
                   + (b_medium_height    
                      +   delta_mh_age     * age 
                      +   delta_mh_female  * female
                      +   delta_mh_educ    * education) * alt1_height2 
                   + (b_low_height     
                      +   delta_lh_age     * age 
                      +   delta_lh_female  * female
                      +   delta_lh_educ    * education) * alt1_height3  
                   + (b_red_kite  
                      +   delta_rk_age      * age 
                      +   delta_rk_female   * female
                      +   delta_rk_educ     * education) * alt1_redkite 
                   + (b_min_distance    
                      +   delta_md_age      * age 
                      +   delta_md_female   * female
                      +   delta_md_educ     * education) * alt1_distance
                   + (b_cost             
                      +   delta_ct_age      * age 
                      +   delta_ct_female   * female
                      +   delta_ct_educ     * education) * alt1_cost      )
  V[['alt2']]  =  (       b_asc_alt2      
                      +   delta_asc2_age      * age 
                      +   delta_asc2_female   * female
                      +   delta_asc2_educ     * education
                   + (b_medium_farms    
                      +   delta_mf_age     * age 
                      +   delta_mf_female  * female
                      +   delta_mf_educ    * education) * alt2_farm2          
                   + (b_small_farms   
                      +   delta_sf_age     * age 
                      +   delta_sf_female  * female
                      +   delta_sf_educ    * education) * alt2_farm3   
                   + (b_medium_height    
                      +   delta_mh_age     * age 
                      +   delta_mh_female  * female
                      +   delta_mh_educ    * education) * alt2_height2 
                   + (b_low_height     
                      +   delta_lh_age     * age 
                      +   delta_lh_female  * female
                      +   delta_lh_educ    * education) * alt2_height3  
                   + (b_red_kite  
                      +   delta_rk_age      * age 
                      +   delta_rk_female   * female
                      +   delta_rk_educ     * education) * alt2_redkite 
                   + (b_min_distance    
                      +   delta_md_age      * age 
                      +   delta_md_female   * female
                      +   delta_md_educ     * education) * alt2_distance
                   + (b_cost             
                      +   delta_ct_age      * age 
                      +   delta_ct_female   * female
                      +   delta_ct_educ     * education) * alt2_cost      )
  V[['alt3']] =  (   b_asc_alt3      
                     +   delta_asc3_age      * age 
                     +   delta_asc3_female   * female
                     +   delta_asc3_educ     * education
                  + (b_medium_farms    
                     +   delta_mf_age     * age 
                     +   delta_mf_female  * female
                     +   delta_mf_educ    * education) * alt3_farm2          
                  + (b_small_farms   
                     +   delta_sf_age     * age 
                     +   delta_sf_female  * female
                     +   delta_sf_educ    * education) * alt3_farm3   
                  + (b_medium_height    
                     +   delta_mh_age     * age 
                     +   delta_mh_female  * female
                     +   delta_mh_educ    * education) * alt3_height2 
                  + (b_low_height     
                     +   delta_lh_age     * age 
                     +   delta_lh_female  * female
                     +   delta_lh_educ    * education) * alt3_height3  
                  + (b_red_kite  
                     +   delta_rk_age      * age 
                     +   delta_rk_female   * female
                     +   delta_rk_educ     * education) * alt3_redkite 
                  + (b_min_distance    
                     +   delta_md_age      * age 
                     +   delta_md_female   * female
                     +   delta_md_educ     * education) * alt3_distance
                  + (b_cost             
                     +   delta_ct_age      * age 
                     +   delta_ct_female   * female
                     +   delta_ct_educ     * education) * alt3_cost      )
  
  ## Define settings for MNL model component
  mnl_settings = list(
    alternatives =    c(alt1   = 1, 
                        alt2   = 2, 
                        alt3   = 3),
    avail        = list(alt1   = 1, 
                        alt2   = 1, 
                        alt3   = 1),
    choiceVar    = choice,
    V            = V
  )
  
  ## Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

model = apollo_estimate(apollo_beta, 
                        apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs,
                        estimate_settings = list(writeIter         = FALSE, 
                                                 silent            = FALSE,
                                                 estimationRoutine = "bgw"))
# Printing output    
apollo_modelOutput(model,
                   modelOutput_settings = list(
                     printOutliers = 10,
                     printPVal       = 2))
#
# Saving output in ../ModelOutputs/" directory 
apollo_saveOutput(model)
