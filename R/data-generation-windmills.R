#' Generating the synthetic data for the discrete choice experiment
#'
#'

# Load packages ---
library(tidyverse)
library(evd)
library(truncnorm)

# Set a seed
set.seed(123456)

# Read in the design
design <- read_rds(file.path("Data", "design-windmills.rds"))


# Ordering according to block number
design_100_rows <- design$design

# Setting parameters
n_individuals         <- 1000 # Number of individuals
n_choices             <- 10  # Number of choice occasions for each individual
n_rows_data           <- n_individuals*n_choices  # Number of rows - matrix X
n_rows_design_matrix  <- length(design_100_rows[,1]) # Number of rows in design matrix
n_outliers            <- 10 # Number of respondents who are outliers
perc_serial_SQ        <- 3 # Percentage of serial SQ choosers

# Creating a data frame using the design matrix,
# Extracting randomly 10 rows for each individual
# to achieve a total of "n_individuals * n_choices" rows.


data_LCM  <- do.call("rbind", replicate(n_individuals,
                                        design_100_rows[sample(nrow(design_100_rows), n_choices), ],
                                        simplify = FALSE))

# Creating new variables
data_LCM$choice_task   <- rep(seq(1,n_choices)    , n_individuals) # choice task
data_LCM$id_individual <- rep(seq(1,n_individuals),each=n_choices) # ID individual

# Generation of hypothetical socio-demographic characteristics
data_LCM$age    <- rep(round(rtruncnorm(n_individuals, a    = 18, b    = 100,
                                        mean = 44, sd   = 15))
                       , each=n_choices)

data_LCM$female    <- rep(rbinom(n_individuals, 1, 0.5), each=n_choices)

data_LCM$education <- rep(sample(c(1, 2, 3),
                                 n_individuals,
                                 replace = TRUE,
                                 prob    = c(0.4, 0.3, 0.3))
                          , each=n_choices)

# Setting hypothetical values for population parameters
# Class 1                           Class 2
cl1_asc_alt1             <- -0.50 ; cl2_asc_alt1             <- -0.50
cl1_asc_alt2             <-  0.50 ; cl2_asc_alt2             <-  0.50
cl1_asc_alt3             <-  0.50 ; cl2_asc_alt3             <-  0.50

cl1_mu_medium_farms    <-  0.25 ; cl1_sd_medium_farms    <-  0.01
cl1_mu_small_farms     <-  0.50 ; cl1_sd_small_farms     <-  0.01
cl1_mu_medium_turbines <-  0.50 ; cl1_sd_medium_turbines <-  0.01
cl1_mu_low_turbines    <-  1.00 ; cl1_sd_low_turbines    <-  0.01
cl1_mu_red_kite        <- -0.05 ; cl1_sd_red_kite        <-  0.001
cl1_mu_min_distance    <-  0.50 ; cl1_sd_min_distance    <-  0.01
cl1_mu_cost            <- -0.90 ; cl1_sd_cost            <-  0.01

cl2_mu_medium_farms    <- -0.50 ; cl2_sd_medium_farms    <-  0.01
cl2_mu_small_farms     <-  0.20 ; cl2_sd_small_farms     <-  0.01
cl2_mu_medium_turbines <- -0.70 ; cl2_sd_medium_turbines <-  0.01
cl2_mu_low_turbines    <-  0.50 ; cl2_sd_low_turbines    <-  0.01
cl2_mu_red_kite        <- -0.07 ; cl2_sd_red_kite        <-  0.001
cl2_mu_min_distance    <-  0.20 ; cl2_sd_min_distance    <-  0.01
cl2_mu_cost            <- -0.30 ; cl2_sd_cost            <-  0.01

# Generation of population individual coefficients
cl1_rand_medium_farms    <- rep(rnorm(n_individuals, cl1_mu_medium_farms   , cl1_sd_medium_farms   ), each=n_choices)
cl1_rand_small_farms     <- rep(rnorm(n_individuals, cl1_mu_small_farms    , cl1_sd_small_farms    ), each=n_choices)
cl1_rand_medium_turbines <- rep(rnorm(n_individuals, cl1_mu_medium_turbines, cl1_sd_medium_turbines), each=n_choices)
cl1_rand_low_turbines    <- rep(rnorm(n_individuals, cl1_mu_low_turbines   , cl1_sd_low_turbines   ), each=n_choices)
cl1_rand_red_kite        <- rep(rnorm(n_individuals, cl1_mu_red_kite       , cl1_sd_red_kite       ), each=n_choices)
cl1_rand_min_distance    <- rep(rnorm(n_individuals, cl1_mu_min_distance   , cl1_sd_min_distance   ), each=n_choices)
cl1_rand_cost            <- rep(rnorm(n_individuals, cl1_mu_cost           , cl1_sd_cost           ), each=n_choices)

# Generation of population individual coefficients
cl2_rand_medium_farms    <- rep(rnorm(n_individuals, cl2_mu_medium_farms   , cl2_sd_medium_farms   ), each=n_choices)
cl2_rand_small_farms     <- rep(rnorm(n_individuals, cl2_mu_small_farms    , cl2_sd_small_farms    ), each=n_choices)
cl2_rand_medium_turbines <- rep(rnorm(n_individuals, cl2_mu_medium_turbines, cl2_sd_medium_turbines), each=n_choices)
cl2_rand_low_turbines    <- rep(rnorm(n_individuals, cl2_mu_low_turbines   , cl2_sd_low_turbines   ), each=n_choices)
cl2_rand_red_kite        <- rep(rnorm(n_individuals, cl2_mu_red_kite       , cl2_sd_red_kite       ), each=n_choices)
cl2_rand_min_distance    <- rep(rnorm(n_individuals, cl2_mu_min_distance   , cl2_sd_min_distance   ), each=n_choices)
cl2_rand_cost            <- rep(rnorm(n_individuals, cl2_mu_cost           , cl2_sd_cost           ), each=n_choices)

# Creating attributes of the SQ alternative
data_LCM$alt1_farm2     <- 0.00
data_LCM$alt1_farm3     <- 0.00
data_LCM$alt1_height2   <- 0.00
data_LCM$alt1_height3   <- 0.00
data_LCM$alt1_redkite   <- 0.00
data_LCM$alt1_distance  <- 0.00
data_LCM$alt1_cost      <- 0.00

data_LCM <-
  data_LCM[,
           c(which(colnames(data_LCM)=="id_individual"),
             which(colnames(data_LCM)=="choice_task"),
             which(colnames(data_LCM)=="alt1_sq"  ),
             which(colnames(data_LCM)=="alt1_farm2"):which(colnames(data_LCM)=="alt1_cost"),
             which(colnames(data_LCM)=="alt2_farm2"):which(colnames(data_LCM)=="alt3_cost"),
             which(colnames(data_LCM)=="age")       :which(colnames(data_LCM)=="education")
             )]

# LV1 in allocation function
cst_LV1         <-    1.50
beta_age_LV1    <-   -0.1
beta_female_LV1 <-    2.0
beta_educ_LV1   <-    1.0

data_LCM$LV1 <- ( cst_LV1 + beta_age_LV1    * data_LCM$age
                          + beta_female_LV1 * data_LCM$female
                          + beta_educ_LV1   * data_LCM$education
                          + 0.5*rnorm(nrow(data_LCM)))

# Allocation probabilities
cst_alloc_prob         <-    0.0
beta_LV1_alloc_prob    <-   -0.5

# Generation of allocation probabilities
latent_class2 <- (    cst_alloc_prob
                     + beta_LV1_alloc_prob     * data_LCM$LV1)

alloc_prob_class1 <- exp(0)             / (exp(0)+exp(latent_class2))
alloc_prob_class2 <- exp(latent_class2) / (exp(0)+exp(latent_class2))

# Class assignment
data_LCM$id_class <- 1
data_LCM$id_class[rep(runif(n_individuals),each=n_choices)  > alloc_prob_class1] <- 2

# Class size:
cat(" Class 1: ", 100*(mean(data_LCM$id_class)-1), "%   \n",
    "Class 2: ", 100*(2-mean(data_LCM$id_class)), "%." )


# Generation of utilities
# Class 1
data_LCM$utility1_cl1 <-   ( cl1_asc_alt1
                           + cl1_rand_medium_farms    * data_LCM$alt1_farm2
                           + cl1_rand_small_farms     * data_LCM$alt1_farm3
                           + cl1_rand_medium_turbines * data_LCM$alt1_height2
                           + cl1_rand_low_turbines    * data_LCM$alt1_height3
                           + cl1_rand_red_kite        * data_LCM$alt1_redkite
                           + cl1_rand_min_distance    * data_LCM$alt1_distance
                           + cl1_rand_cost            * data_LCM$alt1_cost
                           + rgumbel(n_individuals, loc=0, scale=1))

data_LCM$utility2_cl1 <- (   cl1_asc_alt2
                           + cl1_rand_medium_farms    * data_LCM$alt2_farm2
                           + cl1_rand_small_farms     * data_LCM$alt2_farm3
                           + cl1_rand_medium_turbines * data_LCM$alt2_height2
                           + cl1_rand_low_turbines    * data_LCM$alt2_height3
                           + cl1_rand_red_kite        * data_LCM$alt2_redkite
                           + cl1_rand_min_distance    * data_LCM$alt2_distance
                           + cl1_rand_cost            * data_LCM$alt2_cost
                           + rgumbel(n_individuals, loc=0, scale=1))

data_LCM$utility3_cl1 <-(   cl1_asc_alt3
                          + cl1_rand_medium_farms     * data_LCM$alt3_farm2
                          + cl1_rand_small_farms      * data_LCM$alt3_farm3
                          + cl1_rand_medium_turbines * data_LCM$alt3_height2
                          + cl1_rand_low_turbines     * data_LCM$alt3_height3
                          + cl1_rand_red_kite         * data_LCM$alt3_redkite
                          + cl1_rand_min_distance     * data_LCM$alt3_distance
                          + cl1_rand_cost             * data_LCM$alt3_cost
                          + rgumbel(n_individuals, loc=0, scale=1))

# Class 2
data_LCM$utility1_cl2 <- (cl2_asc_alt1
                          + cl2_rand_medium_farms    * data_LCM$alt1_farm2
                          + cl2_rand_small_farms     * data_LCM$alt1_farm3
                          + cl2_rand_medium_turbines * data_LCM$alt1_height2
                          + cl2_rand_low_turbines    * data_LCM$alt1_height3
                          + cl2_rand_red_kite        * data_LCM$alt1_redkite
                          + cl2_rand_min_distance    * data_LCM$alt1_distance
                          + cl2_rand_cost            * data_LCM$alt1_cost
                          + rgumbel(n_individuals, loc=0, scale=1))

data_LCM$utility2_cl2 <- (cl2_asc_alt2
                          + cl2_rand_medium_farms    * data_LCM$alt2_farm2
                          + cl2_rand_small_farms     * data_LCM$alt2_farm3
                          + cl2_rand_medium_turbines * data_LCM$alt2_height2
                          + cl2_rand_low_turbines    * data_LCM$alt2_height3
                          + cl2_rand_red_kite        * data_LCM$alt2_redkite
                          + cl2_rand_min_distance    * data_LCM$alt2_distance
                          + cl2_rand_cost            * data_LCM$alt2_cost
                          + rgumbel(n_individuals, loc=0, scale=1))

data_LCM$utility3_cl2 <-(cl2_asc_alt3
                         + cl2_rand_medium_farms     * data_LCM$alt3_farm2
                         + cl2_rand_small_farms      * data_LCM$alt3_farm3
                         + cl2_rand_medium_turbines * data_LCM$alt3_height2
                         + cl2_rand_low_turbines     * data_LCM$alt3_height3
                         + cl2_rand_red_kite         * data_LCM$alt3_redkite
                         + cl2_rand_min_distance     * data_LCM$alt3_distance
                         + cl2_rand_cost             * data_LCM$alt3_cost
                         + rgumbel(n_individuals, loc=0, scale=1))

# Generation of outliers cl1
outliers <- rep((sample(1:n_individuals, n_outliers) - 1) * n_choices, each = n_choices) + 1:n_choices
data_LCM$utility1_cl1[outliers] <- data_LCM$utility1_cl1[outliers] * -10
data_LCM$utility2_cl1[outliers] <- data_LCM$utility2_cl1[outliers] * -10
data_LCM$utility3_cl1[outliers] <- data_LCM$utility3_cl1[outliers] * -10

## Generation of outliers cl2
outliers <- rep((sample(1:n_individuals, n_outliers) - 1) * n_choices, each = n_choices) + 1:n_choices
data_LCM$utility1_cl2[outliers] <- data_LCM$utility1_cl2[outliers] * -10
data_LCM$utility2_cl2[outliers] <- data_LCM$utility2_cl2[outliers] * -10
data_LCM$utility3_cl2[outliers] <- data_LCM$utility3_cl2[outliers] * -10

# Scale Class 1
data_LCM$cl1.scale.params <- (rep((1+(((1:n_choices)-1)^2 - 0.115 * ((1:n_choices)-1)^3)/20),
                                  each = n_individuals)
                              +  data_LCM$female         * 0.1
                              + (data_LCM$education == 1) * -0.1
                              + (data_LCM$education == 3) *  0.1)
data_LCM$cl1.scale.params <- (data_LCM$cl1.scale.params - min(data_LCM$cl1.scale.params))
data_LCM$cl1.scale.params <- 0.4 * data_LCM$cl1.scale.params/max(data_LCM$cl1.scale.params)
data_LCM$cl1.scale.params <-  data_LCM$cl1.scale.params + (1 - median(data_LCM$cl1.scale.params))

# Scale Class 2
data_LCM$cl2.scale.params <- (rep((1+(((1:n_choices)-1)^2 - 0.11 * ((1:n_choices)-1)^3)/20),
                                  each = n_individuals)
                              +  data_LCM$female         * 0.12
                              + (data_LCM$education == 1) * -0.08
                              + (data_LCM$education == 3) *  0.12)
data_LCM$cl2.scale.params <- (data_LCM$cl2.scale.params - min(data_LCM$cl2.scale.params))
data_LCM$cl2.scale.params <- 0.4 * data_LCM$cl2.scale.params/max(data_LCM$cl2.scale.params)
data_LCM$cl2.scale.params <-  data_LCM$cl2.scale.params + (1 - median(data_LCM$cl2.scale.params))

# Generation of choices
# Class 1
data_LCM$choice_cl1 <-  apply(cbind(data_LCM$utility1_cl1 * data_LCM$cl1.scale.params,
                                    data_LCM$utility2_cl1 * data_LCM$cl1.scale.params,
                                    data_LCM$utility3_cl1 * data_LCM$cl1.scale.params),
                              1,which.max)

# Class 2
data_LCM$choice_cl2 <-  apply(cbind(data_LCM$utility1_cl2 * data_LCM$cl2.scale.params,
                                    data_LCM$utility2_cl2 * data_LCM$cl2.scale.params,
                                    data_LCM$utility3_cl2 * data_LCM$cl2.scale.params),
                              1,which.max)

data_LCM$choice <- data_LCM$choice_cl1
data_LCM$choice[data_LCM$id_class == 2] <- data_LCM$choice_cl2[data_LCM$id_class == 2]

# Serial choosing Alt1 = SQ

# Number of SQ choices for each individual
# that are defined as number of data_LCM$choice == 1
# for each individual identified by data_LCM$id_individual
data_LCM$n_sq_choices <- ave(data_LCM$choice,
                             data_LCM$id_individual,
                             FUN = function(x) sum(x == 1))

# table of number of SQ choices
# before generating serial SQ choices
table_serial_before <- table(data_LCM$n_sq_choices)/10

# LV2 in generating serial SQ choice
# that is data_LCM$choice == 1 in all
# choice occasions for each individual
# identified by data_LCM$choice_task
# This will depend on socio-demographic characteristics
# of the individual
cst_LV2         <-     1.0
beta_age_LV2    <-     0.1
beta_female_LV2 <-    -0.80
beta_educ_LV2   <-    -0.50

data_LCM$LV2 <- abs( cst_LV2 + beta_age_LV2    * data_LCM$age
                             + beta_female_LV2 * data_LCM$female
                             + beta_educ_LV2   * data_LCM$education)

# 3% of the individuals with the highest LV2 will be
# identified as serial SQ choosers
# Generate a binary variable data_LCM$serialSQ_choices
# that is TRUE if data_LCM$LV2 > percentile_(100-perc_serial_SQ)
data_LCM$serialSQ_choices <- data_LCM$LV2 > quantile(data_LCM$LV2, 1-(perc_serial_SQ/100))
# Modify value of data_LCM$choice to 1
# for individuals that are identified by data_LCM$serialSQ_choices
data_LCM$choice[data_LCM$serialSQ_choices == TRUE] <- 1

# Number of SQ choices for each individual
# that are defined as number of data_LCM$choice == 1
# for each individual identified by data_LCM$id_individual
data_LCM$n_sq_choices <- ave(data_LCM$choice,
                             data_LCM$id_individual,
                             FUN = function(x) sum(x == 1))

# table of number of SQ choices
# after generating serial SQ choices
table_serial_after <- table(data_LCM$n_sq_choices)/10

# change the following three lines in one cat()

cat("Number of respondets and number of SQ choices
     before and after generating serial SQ choices\n")
table_serial_before
table_serial_after

# Adding missing choices
#
for(i in 1:n_individuals){
  missing <- 0.07*runif(n_choices)*(1:n_choices)^0.1

  if(data_LCM$female[(((i-1)*n_choices) + 1:n_choices)][1] == 1){
    missing <- (missing + (0.12*runif(n_choices)*(1:n_choices)^0.1) )/2
  }

  if(data_LCM$age[(((i-1)*n_choices) + 1:n_choices)][1] <= quantile(data_LCM$age, 1/3)){
    missing <- (missing + (0.085*runif(n_choices)*(1:n_choices)^0.1) )/2
  }

  if(data_LCM$age[(((i-1)*n_choices) + 1:n_choices)][1] > quantile(data_LCM$age, 2/3)){
    missing <- (missing + (0.06*runif(n_choices)*(1:n_choices)^0.1) )/2
  }
  missing <- missing > 0.07
  switch.first <- runif(1) <= 0.25
  if(switch.first == TRUE) missing[1:2] <- rev(missing[1:2])
  data_LCM$choice[(((i-1)*n_choices) + 1:n_choices)[missing == TRUE]] <- NA
}

# Save the data data_LCM into a file named data_save
# without the following columns
# LV1	id_class,	utility1_cl1,	utility2_cl1,	utility3_cl1,
# utility1_cl2	utility2_cl2	utility3_cl2	cl1.scale.params
# cl2.scale.params	choice_cl1	choice_cl2	n_sq_choices	LV2	serialSQ_choices

write.csv(data_LCM[, !(names(data_LCM) %in% c("X","LV1", "id_class", "utility1_cl1", "utility2_cl1", "utility3_cl1",
                                              "utility1_cl2", "utility2_cl2", "utility3_cl2", "cl1.scale.params",
                                              "cl2.scale.params", "choice_cl1", "choice_cl2", "n_sq_choices",
                                              "LV2", "serialSQ_choices"))]
          ,
            file = "../DataAndDesigns/Data_windmills.csv"
          , row.names = FALSE)
