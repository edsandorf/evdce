rm(list = ls(all = TRUE)) # Clear the workspace
set.seed(12334)
# Loading R packages
library(apollo)
library(janitor)
library(tidyverse)
library(dplyr)        # Data manipulation


# initialise Apollo
apollo_initialise()

# Set core controls
apollo_control = list(modelName  = "RP_MXL_uncorrelated_ASC_no_convergence",
                      modelDescr = "RP MXL uncorrelated",
                      indivID    = "id_individual",
                      #nCores     = 7,
                      maxIterations = 10000) # Increase the iteration limit here

# Importing data
database <- read_csv(gzcon(url("https://raw.githubusercontent.com/edsandorf/evdce/refs/heads/main/Data/data-windmills.csv"))) |>
  clean_names()

# Eliminating NA in choice
database <- subset(database, !is.na(database$choice))

database <- subset(database, database$id_individual %in% c(1:5))
# Starting values of the parameters
apollo_beta = c(
  # Alternative specific constants
  asc_alt1   =  0.00 ,
  asc_alt2   =  0.50 ,
  asc_alt3   =  0.50 ,
  # Mean parameters  # Standard daeviations
  mu_mf   =  -1.0 ,   sd_mf =0.0,
  mu_sf   =  -1.0 ,   sd_sf =0.0,
  mu_mt   =  -1.0 ,   sd_mt =0.0 ,
  mu_lt   =  -1.0 ,   sd_lt =0.0,
  mu_rk   =   1.0 ,   sd_rk =0.0,
  mu_md   =  -1.0 ,   sd_md =0.0,
  mu_ct   =   1.0 ,   sd_ct =0.0,
  # Interaction terms
  delta_asc2_age    =   0.00,
  delta_asc2_female =   0.00,
  delta_asc2_educ   =   0.00,
  delta_asc3_age    =   0.00,
  delta_asc3_female =   0.00,
  delta_asc3_educ   =   0.00,
  # Interaction terms
  delta_mf_age    = -0.0, delta_sf_age    = -0.0,
  delta_mf_female = -0.0, delta_sf_female = -0.0,
  delta_mf_educ   = -0.0, delta_sf_educ   = -0.0,
  #
  delta_mt_age    = -0.0, delta_lt_age    = -0.0,
  delta_mt_female =  0.0, delta_lt_female =  0.0,
  delta_mt_educ   =  0.0, delta_lt_educ   =  0.0,
  #
  delta_rk_age    =  -0.0, delta_md_age    = -0.0,
  delta_rk_female =   0.0, delta_md_female =  0.0,
  delta_rk_educ   =   0.1, delta_md_educ   = -0.0,
  #
  delta_ct_age     =  0.0,
  delta_ct_female  = -0.0,
  delta_ct_educ    = -0.0  )

# Vector of parameters to be kept fixed at their starting value
apollo_fixed = c("asc_alt1")

# ################################################################# #
#### DEFINE RANDOM COMPONENTS                                    ####
# ################################################################# #

### Set parameters for generating draws
apollo_draws = list(
  interDrawsType = "sobol",
  interNDraws    = 5000,
  interUnifDraws = c(),
  interNormDraws = c("draws_mf",
                     "draws_sf",
                     "draws_mt",
                     "draws_lt",
                     "draws_rk",
                     "draws_md",
                     "draws_ct"),
  intraDrawsType = "halton",
  intraNDraws    = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)

### Create random parameters
apollo_randCoeff = function(apollo_beta, apollo_inputs){
  randcoeff = list()

  randcoeff[["r_mf"]] =     (mu_mf + sd_mf    * draws_mf
                             + delta_mf_age    * age
                             + delta_mf_female * female
                             + delta_mf_educ   * education)
  randcoeff[["r_sf"]] =     (mu_sf + sd_sf    * draws_sf
                             + delta_sf_age    * age
                             + delta_sf_female * female
                             + delta_sf_educ   * education)
  randcoeff[["r_mt"]] =     (mu_mt + sd_mt    * draws_mt
                             + delta_mt_age    * age
                             + delta_mt_female * female
                             + delta_mt_educ   * education)
  randcoeff[["r_lt"]] =     (mu_lt + sd_lt    * draws_lt
                             + delta_lt_age    * age
                             + delta_lt_female * female
                             + delta_lt_educ   * education)
  randcoeff[["r_rk"]] =     (mu_rk + sd_rk    * draws_rk
                             + delta_rk_age    * age
                             + delta_rk_female * female
                             + delta_rk_educ   * education)
  randcoeff[["r_md"]] =     (mu_md + sd_md    * draws_md
                             + delta_md_age    * age
                             + delta_md_female * female
                             + delta_md_educ   * education  )
  randcoeff[["r_ct" ]] =-exp(mu_ct + sd_ct    * draws_ct
                             + delta_ct_age    * age
                             + delta_ct_female * female
                             + delta_ct_educ   * education)
  return(randcoeff)
}

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){

  ### Function initialisation: do not change the following three commands
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))

  ### Create list of probabilities P
  P = list()

  ### List of utilities: these must use the same names as in mnl_settings, order is irrelevant
  V = list()
  V[["alt1"]] = (    asc_alt1
                     + r_mf * alt1_farm2
                     + r_sf * alt1_farm3
                     + r_mt * alt1_height2
                     + r_lt * alt1_height3
                     + r_rk * alt1_redkite
                     + r_md * alt1_distance
                     + r_ct * alt1_cost     )

  V[["alt2"]] =  (   asc_alt2
                     + delta_asc2_age    * age
                     + delta_asc2_female * female
                     + delta_asc2_educ   * education
                     + r_mf              * alt2_farm2
                     + r_sf              * alt2_farm3
                     + r_mt              * alt2_height2
                     + r_lt              * alt2_height3
                     + r_rk              * alt2_redkite
                     + r_md              * alt2_distance
                     + r_ct              * alt2_cost     )

  V[["alt3"]] = (   asc_alt3
                    + delta_asc3_age    * age
                    + delta_asc3_female * female
                    + delta_asc3_educ   * education
                    + r_mf              * alt3_farm2
                    + r_sf              * alt3_farm3
                    + r_mt              * alt3_height2
                    + r_lt              * alt3_height3
                    + r_rk              * alt3_redkite
                    + r_md              * alt3_distance
                    + r_ct              * alt3_cost     )

  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2, alt3=3),
    avail         = list(alt1=1, alt2=1, alt3=1),
    choiceVar     = choice,
    utilities     = V
  )

  ### Compute probabilities using MNL model
  P[["model"]] = apollo_mnl(mnl_settings, functionality)

  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)

  ### Average across inter-individual draws
  P = apollo_avgInterDraws(P, apollo_inputs, functionality)

  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta,
                        apollo_fixed,apollo_probabilities,
                        apollo_inputs,
                        estimate_settings = list(writeIter = FALSE,
                                                 silent = FALSE))

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #
options(width = 105)
apollo_modelOutput(model)

