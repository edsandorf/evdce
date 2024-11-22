rm(list = ls(all = TRUE)) # Clear the workspace
set.seed(12334)
# Loading R packages
library(apollo)       # Tools for Choice Model Estimation and Application

# Initialize Apollo
apollo_initialise()

# Set core controls
apollo_control = list(modelName       = "MNL",
                      modelDescr      = "MNL windmils",
                      indivID         = "id_individual",
                      nCores          = 7,
                      outputDirectory = "../ModelOutputs/")


# Importing data               
database <- read.csv(file="../DataAndDesigns/Data_windmills.csv")

# Eliminating NA in choice
database <- subset(database, !is.na(database$choice))

# Starting values of the parameters
apollo_beta = c(b_asc_alt1          =   0.00,
                b_asc_alt2          =   0.50,
                b_asc_alt3          =   0.50,
                b_medium_farms      =   0.25,
                b_small_farms       =   0.50,
                b_medium_height     =   0.25,
                b_low_height        =   0.50,
                b_red_kite          =  -0.05,
                b_min_distance      =   0.50,
                b_cost              =  -0.50
)

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
  V[['alt1']]  =  (         b_asc_alt1                               
                            + b_medium_farms     * alt1_farm2          
                            + b_small_farms      * alt1_farm3   
                            + b_medium_height    * alt1_height2 
                            + b_low_height       * alt1_height3  
                            + b_red_kite         * alt1_redkite 
                            + b_min_distance     * alt1_distance
                            + b_cost             * alt1_cost      )
  V[['alt2']]  = (          b_asc_alt2 
                            + b_medium_farms     * alt2_farm2          
                            + b_small_farms      * alt2_farm3   
                            + b_medium_height    * alt2_height2 
                            + b_low_height       * alt2_height3  
                            + b_red_kite         * alt2_redkite 
                            + b_min_distance     * alt2_distance
                            + b_cost             * alt2_cost       )
  V[['alt3']] = (           b_asc_alt3
                            + b_medium_farms     * alt3_farm2          
                            + b_small_farms      * alt3_farm3   
                            + b_medium_height    * alt3_height2 
                            + b_low_height       * alt3_height3  
                            + b_red_kite         * alt3_redkite 
                            + b_min_distance     * alt3_distance
                            + b_cost             * alt3_cost       )
  
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
                                                 silent            = TRUE,
                                                 estimationRoutine = "bgw"))



# Printing output    
apollo_modelOutput(model,
                   modelOutput_settings = list(
                     printOutliers = 10,
                     printPVal       = 2))
#
#
# Here the worst outlier is the individual with ID = 556 with Avg prob per choice = 0.08837992
#
#
apollo_saveOutput(model)

# Compute predicted probabilities for the individual with ID = 556

# Filter data for the individual with ID = 556
individual_data <- subset(database, id_individual == 556)

# Update apollo_inputs with the individual data
apollo_inputs_individual = apollo_validateInputs(database = individual_data)

# Compute predicted probabilities for the individual with ID = 556
predicted_probabilities = apollo_prediction(model, apollo_probabilities, apollo_inputs_individual)

# Print the predicted probabilities
print(predicted_probabilities)

# Compute Avg prob per choice for the individual with ID = 556
mean(predicted_probabilities$chosen)

#
#
# The Avg prob per choice for the individual with ID = 556 is 0.09253523 and not 0.08837992
#
#


