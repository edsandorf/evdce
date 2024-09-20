#
# Create the design used in the book
#
rm(list = ls(all = TRUE))
library(spdesign)

# Define the list of utility functions ----
#' The disign comprises 3 alternatives (including the SQ) described by 5
#' attributes. The levels of the attributes are detailed below.
#'
#' Size of Wind Farm (farm): Dummy-coded -
#'  1 - Large   (16 - 18 windmills) (SQ Alternative)
#'  2 - Medium  (10 - 12 windmills)
#'  3 - Small   (4 - 6 windmills)
#'
#' Maximum height of turbines (height): Dummy-coded
#'  1 - High    (200m) (SQ Alternative)
#'  2 - Medium  (150m)
#'  3 - Low     (110m)

#'
#' Reduction in Redkite population (redkite): Linear
#'  5%, 7.5%, 10% (SQ Alternative), 12.5%, 15%
#'  This alternative has been differenced with respect to the SQ of 10%
#'  Design levels are then -5, -2.5, 0, 2.5 and 5
#'
#' Maximum distance to residential areas
#'  750m (SQ Alternative), 1000m, 1250m, 1500m, 1750m
#'  This alternative has been difference with respect to the SQ of 750.
#'  Design levels are then 0, 250, 500, 750,
#'
#' Monthly surcharge to electricity bill
#'  €1-€10 in €1 increments (€0 for SQ Alternative)
#'
#' Dummy coding and differencing ensures that the SQ alternative is 0. Note
#' that it is still included to get the correct variance-covariance matrix.
utility <- list(
  alt1 = "b_sq[0] * sq[1]",
  alt2 = "b_farm_dummy[c(0.25, 0.5)] * farm[c(1, 2, 3)] + b_height_dummy[c(0.25, 0.5)] * height[c(1, 2, 3)] + b_redkite[-0.05] * redkite[c(-5, -2.5, 0, 2.5, 5)] + b_distance[0.6] * distance[c(0, 0.25, 0.5, 0.75, 1)] + b_cost[-0.05] * cost[seq(1, 10)]",
  alt3 = "b_farm_dummy               * farm             + b_height_dummy               * height             + b_redkite        * redkite                         + b_distance      * distance                           + b_cost       * cost"
)

# Generate designs ----
design <- generate_design(utility,
                          # rows = 30,
                          rows = 100,
                          model = "mnl",
                          efficiency_criteria = "d-error",
                          algorithm = "rsc",
                          draws = "scrambled-sobol",
                          dudx = "b_cost",
                          control = list(
                            efficiency_threshold = 0.001,
                            max_iter = 10000000
                          ))

# Add a blocking variable to the design with 15 blocks.
design <- block(design, 10)

summary(design)
