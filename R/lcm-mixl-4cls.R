# Preliminaries
rm(list = ls(all = TRUE))             # Clear workspace

# Loading R packages
## Apollo: choice models in R
library(apollo)
library(janitor)
library(tidyverse)

# Initialize Apollo
apollo_initialise()

# Set core controls
apollo_control = list(modelName  = "LC_MXL_4Class",
                      modelDescr = "LC MXL_4Class ",
                      indivID    = "id_individual",
                      noValidation    = TRUE,
                      noDiagnostics   = TRUE)

# Importing data
database <- read_csv(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/data-windmills.csv"))) |>
  clean_names()

# Eliminating NA in choice
database <- subset(database, !is.na(database$choice))

# Starting values of the parameters
apollo_beta = c(
  # Class 1
  cl1_asc_alt1            =   0.000000 ,
  cl1_asc_alt2            =   0.559646 ,
  cl1_asc_alt3            =   0.535155 ,
  cl1_b_medium_farms      =   0.293328 ,
  cl1_b_small_farms       =   0.335134 ,
  cl1_b_medium_height   =   0.493189 ,
  cl1_b_low_height      =   0.941912 ,
  cl1_b_red_kite          =  -0.043925 ,
  cl1_b_min_distance      =   0.511618 ,
  cl1_b_cost              =  -0.760908 ,

  # Class 2
  cl2_asc_alt1            =  0.000000,
  cl2_asc_alt2            =  1.181473,
  cl2_asc_alt3            =  1.274248,
  cl2_b_medium_farms      = -0.538876  ,
  cl2_b_small_farms       =  0.056321 ,
  cl2_b_medium_height   = -0.732143  ,
  cl2_b_low_height      =  0.475587 ,
  cl2_b_red_kite          = -0.066096 ,
  cl2_b_min_distance      =  0.092536 ,
  cl2_b_cost              = -0.323058 ,
  # Class 3
  cl3_asc_alt1            =   0.000000 ,
  cl3_asc_alt2            =  -0.470323 ,
  cl3_asc_alt3            =  -1.104021 ,
  cl3_b_medium_farms      =   0.020767  ,
  cl3_b_small_farms       =  -0.622125 ,
  cl3_b_medium_height   =   0.331915  ,
  cl3_b_low_height      =  -0.276404 ,
  cl3_b_red_kite          =   0.060245 ,
  cl3_b_min_distance      =  -0.964140 ,
  cl3_b_cost              =   0.584228 ,
  # Class 4
  cl4_asc_alt1            =   0.000000 ,
  cl4_asc_alt2            =   1.576843 ,
  cl4_asc_alt3            =   1.397256 ,
  cl4_b_medium_farms      =  -0.673488  ,
  cl4_b_small_farms       =   0.403032 ,
  cl4_b_medium_height   =  -0.928059  ,
  cl4_b_low_height      =   0.356206 ,
  cl4_b_red_kite          =  -0.057916 ,
  cl4_b_min_distance      =   0.547058 ,
  cl4_b_cost              =  -0.345583 ,
  # Allocation function
  cl1_cst_alloc_fun       =  0.00 ,
  cl1_b_age               =  0.00 ,
  cl1_b_female            =  0.00 ,
  cl1_b_educ              =  0.00 ,
  #
  cl2_cst_alloc_fun       = -1.448501 ,
  cl2_b_age               =  0.027643 ,
  cl2_b_female            = -1.179523 ,
  cl2_b_educ              =  0.173920 ,
  #
  cl3_cst_alloc_fun       = -3.327817,
  cl3_b_age               = -0.006263,
  cl3_b_female            = -2.212194,
  cl3_b_educ              =  0.010997,
  #
  cl4_cst_alloc_fun       =  0.00 ,
  cl4_b_age               =  0.00 ,
  cl4_b_female            =  0.00 ,
  cl4_b_educ              =  0.00
  )

# Vector of parameters to be kept fixed at their starting value
apollo_fixed = c("cl1_asc_alt1", "cl2_asc_alt1", "cl3_asc_alt1", "cl4_asc_alt1",
                 "cl1_cst_alloc_fun" , "cl1_b_age", "cl1_b_female", "cl1_b_educ"         )
#
#apollo_fixed = c("cl1_asc_alt1", "cl2_asc_alt1", "cl1_cst_alloc_fun"  )

# ################################################################# #
#### DEFINE LATENT CLASS COMPONENTS                              ####
# ################################################################# #

apollo_lcPars=function(apollo_beta, apollo_inputs){
  lcpars = list()
  lcpars[["asc_alt1"         ]] = list(cl1_asc_alt1          , cl2_asc_alt1         , cl3_asc_alt1          , cl4_asc_alt1          )
  lcpars[["asc_alt2"         ]] = list(cl1_asc_alt2          , cl2_asc_alt2         , cl3_asc_alt2          , cl4_asc_alt2          )
  lcpars[["asc_alt3"         ]] = list(cl1_asc_alt3          , cl2_asc_alt3         , cl3_asc_alt3          , cl4_asc_alt3          )
  lcpars[["b_medium_farms"   ]] = list(cl1_b_medium_farms    , cl2_b_medium_farms   , cl3_b_medium_farms    , cl4_b_medium_farms    )
  lcpars[["b_small_farms"    ]] = list(cl1_b_small_farms     , cl2_b_small_farms    , cl3_b_small_farms     , cl4_b_small_farms     )
  lcpars[["b_medium_height"]] = list(cl1_b_medium_height , cl2_b_medium_height, cl3_b_medium_height , cl4_b_medium_height )
  lcpars[["b_low_height"   ]] = list(cl1_b_low_height    , cl2_b_low_height   , cl3_b_low_height    , cl4_b_low_height    )
  lcpars[["b_red_kite"       ]] = list(cl1_b_red_kite        , cl2_b_red_kite       , cl3_b_red_kite        , cl4_b_red_kite        )
  lcpars[["b_min_distance"   ]] = list(cl1_b_min_distance    , cl2_b_min_distance   , cl3_b_min_distance    , cl4_b_min_distance    )
  lcpars[["b_cost"           ]] = list(cl1_b_cost            , cl2_b_cost           , cl3_b_cost            , cl4_b_cost            )

  ### Utilities of class allocation model
  V=list()
 V[["class_a"]] = cl1_cst_alloc_fun  + cl1_b_age * age + cl1_b_female * female + cl1_b_educ * education
 V[["class_b"]] = cl2_cst_alloc_fun  + cl2_b_age * age + cl2_b_female * female + cl2_b_educ * education
 V[["class_c"]] = cl3_cst_alloc_fun  + cl3_b_age * age + cl3_b_female * female + cl3_b_educ * education
 V[["class_d"]] = cl4_cst_alloc_fun  + cl4_b_age * age + cl4_b_female * female + cl4_b_educ * education

  ### Settings for class allocation models
  classAlloc_settings = list(
    classes      = c(class_a=1, class_b=2, class_c=3, class_d=4),
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
### Estimate model
#model = apollo_estimate(apollo_beta, apollo_fixed,
#                        apollo_probabilities, apollo_inputs,
#                        estimate_settings = list(writeIter = FALSE,
#                                                 silent    = FALSE,
#                                                 iterMax   = 500,
#                                                 estimationRoutine = "bgw"))
### Estimate model
model=apollo_lcEM(apollo_beta,
                  apollo_fixed,
                  apollo_probabilities,
                  apollo_inputs,
                  lcEM_settings = list(EMmaxIterations=500))
#
# Saving output in ../ModelOutputs/" directory
apollo_saveOutput(model)

# Printing output
options(width = 105)
apollo_modelOutput(model)


