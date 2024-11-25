# Preliminaries
rm(list = ls(all = TRUE))             # Clear workspace

# Loading R packages
## Apollo: choice models in R
suppressPackageStartupMessages(library(apollo))                       

# Initialize Apollo
apollo_initialise()

# Set core controls
apollo_control = list(modelName  = "LC_MXL_3Class",
                      modelDescr = "LC MXL_3Class ",
                      indivID    = "id_individual",
                     # nCores     = 7,
                      outputDirectory = "../ModelOutputs/",
                     noValidation    = TRUE,
                     noDiagnostics   = TRUE)

# Importing data               
database <- read.csv(file="../DataAndDesigns/Data_windmills.csv",
                     header = TRUE)

# Eliminating NA in choice
database <- subset(database, !is.na(database$choice))

# Starting values of the parameters
apollo_beta = c(
  # Class 1
  cl1_asc_alt1            =   0.000000 ,
  cl1_asc_alt2            =   0.560583 ,
  cl1_asc_alt3            =   0.545758 ,
  cl1_b_medium_farms      =   0.276174 ,
  cl1_b_small_farms       =   0.317038 ,
  cl1_b_medium_height   =   0.483379 ,
  cl1_b_low_height      =   0.935015 ,
  cl1_b_red_kite          =  -0.044793 ,
  cl1_b_min_distance      =   0.508801 ,
  cl1_b_cost              =  -0.755394 ,

  # Class 2
  cl2_asc_alt1            =  0.000000 ,
  cl2_asc_alt2            =  1.273039 ,
  cl2_asc_alt3            =  1.293037 ,
  cl2_b_medium_farms      = -0.564934   ,
  cl2_b_small_farms       =  0.154396  ,
  cl2_b_medium_height   = -0.779586   ,
  cl2_b_low_height      =  0.444429  ,
  cl2_b_red_kite          = -0.063537  ,
  cl2_b_min_distance      =  0.199549  ,
  cl2_b_cost              = -0.326024  ,
  # Class 2
  cl3_asc_alt1            =   0.000000 ,
  cl3_asc_alt2            =  -0.474119 ,
  cl3_asc_alt3            =  -1.103430 ,
  cl3_b_medium_farms      =   0.028382  ,
  cl3_b_small_farms       =  -0.625295 ,
  cl3_b_medium_height   =   0.326533  ,
  cl3_b_low_height      =  -0.264425 ,
  cl3_b_red_kite          =   0.061798 ,
  cl3_b_min_distance      =  -0.946585 ,
  cl3_b_cost              =   0.582736 ,
  # Allocation function
  cl1_cst_alloc_fun       =  0.00 ,
  cl1_b_age               =  0.00 ,
  cl1_b_female            =  0.00 ,
  cl1_b_educ              =  0.00 ,
  #
  cl2_cst_alloc_fun       =  -0.128537 ,
  cl2_b_age               =   0.021127 ,
  cl2_b_female            =  -0.828830 ,
  cl2_b_educ              =  -0.301882 ,
  #
  cl3_cst_alloc_fun       =  -3.084182 ,
  cl3_b_age               =  -0.008961 ,
  cl3_b_female            =  -2.167977 ,
  cl3_b_educ              =  -0.074765 
  )


# Vector of parameters to be kept fixed at their starting value 
apollo_fixed = c("cl1_asc_alt1", "cl2_asc_alt1", "cl3_asc_alt1",
                 "cl1_cst_alloc_fun" , "cl1_b_age", "cl1_b_female", "cl1_b_educ"         )
#
#apollo_fixed = c("cl1_asc_alt1", "cl2_asc_alt1", "cl1_cst_alloc_fun"  )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["asc_alt1"         ]] = list(cl1_asc_alt1          , cl2_asc_alt1         , cl3_asc_alt1          )
  lcpars[["asc_alt2"         ]] = list(cl1_asc_alt2          , cl2_asc_alt2         , cl3_asc_alt2          )
  lcpars[["asc_alt3"         ]] = list(cl1_asc_alt3          , cl2_asc_alt3         , cl3_asc_alt3          )
  lcpars[["b_medium_farms"   ]] = list(cl1_b_medium_farms    , cl2_b_medium_farms   , cl3_b_medium_farms    )
  lcpars[["b_small_farms"    ]] = list(cl1_b_small_farms     , cl2_b_small_farms    , cl3_b_small_farms     )
  lcpars[["b_medium_height"]] = list(cl1_b_medium_height , cl2_b_medium_height, cl3_b_medium_height )
  lcpars[["b_low_height"   ]] = list(cl1_b_low_height    , cl2_b_low_height   , cl3_b_low_height    )
  lcpars[["b_red_kite"       ]] = list(cl1_b_red_kite        , cl2_b_red_kite       , cl3_b_red_kite        )
  lcpars[["b_min_distance"   ]] = list(cl1_b_min_distance    , cl2_b_min_distance   , cl3_b_min_distance    )
  lcpars[["b_cost"           ]] = list(cl1_b_cost            , cl2_b_cost           , cl3_b_cost            )
  
  ### Utilities of class allocation model
  V=list()
 V[["class_a"]] = cl1_cst_alloc_fun  + cl1_b_age * age + cl1_b_female * female + cl1_b_educ * education 
 V[["class_b"]] = cl2_cst_alloc_fun  + cl2_b_age * age + cl2_b_female * female + cl2_b_educ * education
 V[["class_c"]] = cl3_cst_alloc_fun  + cl3_b_age * age + cl3_b_female * female + cl3_b_educ * education

  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2, class_c=3), 
    utilities    = V  
  )
  
  lcpars[["pi_values"]] = apollo_classAlloc(classAlloc_settings)
  
  return(lcpars)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### Define settings for MNL model component that are generic across classes
  mnl_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3),
    avail        = list(alt1=1, alt2=1, alt3=1),
    choiceVar    = choice
  )
  
  ### Loop over classes
  for(s in 1:length(pi_values)){
    
    ### Compute class-specific utilities
    V=list()
    
    V[["alt1"]] = (   asc_alt1         [[s]]
                      + b_medium_farms   [[s]] * alt1_farm2   
                      + b_small_farms    [[s]] * alt1_farm3   
                      + b_medium_height[[s]] * alt1_height2 
                      + b_low_height   [[s]] * alt1_height3 
                      + b_red_kite       [[s]] * alt1_redkite 
                      + b_min_distance   [[s]] * alt1_distance
                      + b_cost           [[s]] * alt1_cost     )
    
    V[["alt2"]] = (   asc_alt2         [[s]]
                      + b_medium_farms   [[s]] * alt2_farm2   
                      + b_small_farms    [[s]] * alt2_farm3   
                      + b_medium_height[[s]] * alt2_height2 
                      + b_low_height   [[s]] * alt2_height3 
                      + b_red_kite       [[s]] * alt2_redkite 
                      + b_min_distance   [[s]] * alt2_distance
                      + b_cost           [[s]] * alt2_cost     ) 
    
    V[["alt3"]] = (   asc_alt3         [[s]]
                      + b_medium_farms   [[s]] * alt3_farm2   
                      + b_small_farms    [[s]] * alt3_farm3   
                      + b_medium_height[[s]] * alt3_height2 
                      + b_low_height   [[s]] * alt3_height3 
                      + b_red_kite       [[s]] * alt3_redkite 
                      + b_min_distance   [[s]] * alt3_distance
                      + b_cost           [[s]] * alt3_cost     )
    
    mnl_settings$utilities     = V
    mnl_settings$componentName = paste0("Class_",s)
    
    ### Compute within-class choice probabilities using MNL model
    P[[paste0("Class_",s)]] = apollo_mnl(mnl_settings, functionality)
    
    ### Take product across observation for same individual
    P[[paste0("Class_",s)]] = apollo_panelProd(P[[paste0("Class_",s)]], apollo_inputs ,functionality)
    
  }
  
  ### Compute latent class model probabilities
  lc_settings  = list(inClassProb = P, classProb=pi_values)
  P[["model"]] = apollo_lc(lc_settings, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #
#model=apollo_lcEM(apollo_beta, apollo_fixed, apollo_probabilities, 
#                  apollo_inputs, lcEM_settings = list(EMmaxIterations=100))
#
#
model=apollo_lcEM(apollo_beta, 
                  apollo_fixed, 
                  apollo_probabilities, 
                  apollo_inputs, 
                  lcEM_settings = list(EMmaxIterations=100))


### Estimate model
#model = apollo_estimate(apollo_beta, apollo_fixed, 
#                        apollo_probabilities, apollo_inputs,
#                        estimate_settings = list(writeIter = FALSE, 
#                                                 silent    = FALSE,
#                                                 iterMax   = 500,
#                                                 estimationRoutine = "bgw"))
#
# Saving output in ../ModelOutputs/" directory 
apollo_saveOutput(model)

# Printing output   
options(width = 105)
apollo_modelOutput(model)


