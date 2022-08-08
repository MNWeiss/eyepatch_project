left_data <- read.csv("left_data.csv") # if you've saved the data in a different location, you'll need to change the path here
right_data <- read.csv("right_data.csv") # if you've saved the data in a different location, you'll need to change the path here

whale_attributes <- read.csv("whale attributes.csv") # read in the whale attributes file. 

left_aggregated <- aggregate(log(narrowness) ~ ID, data = left_data, FUN = mean) # get the mean log(narrowness) for each individual
right_aggregated <- aggregate(log(narrowness) ~ ID, data = right_data, FUN = mean) # get the mean log(narrowness) for each individual

all_ids <- intersect(left_aggregated$ID,right_aggregated$ID)

left_aggregated <- left_aggregated[match(all_ids,left_aggregated$ID),]
right_aggregated <- right_aggregated[match(all_ids,right_aggregated$ID),]

# devtools::install_github("MNWeiss/aninet")

require(aninet)

# These are "broadscale" association numerators and denominators
# The numerator is the number of days two individuals were photographed in the same group
# The denominators are the number of days at least one of the individuals was photographed
# We calculated this for years in which both of the pair were alive between 1987 and 2020
X <- readRDS("broadscale SRI numerator.rds")
D <- readRDS("broadscale SRI denominator.rds")

# Some data wrangling, matching up the eyepatch and social data
left_aggregated <- left_aggregated[left_aggregated$ID %in% colnames(X),]
right_aggregated <- right_aggregated[right_aggregated$ID %in% colnames(X),]

X <- X[left_aggregated$ID,left_aggregated$ID]
D <- D[left_aggregated$ID,left_aggregated$ID]

left_similarity <- aninet::attribute_similarity(left_aggregated$`log(narrowness)`, type = "absdiff")
right_similarity <- aninet::attribute_similarity(right_aggregated$`log(narrowness)`, type = "absdiff")

# We can also read in the maternal kinship data
maternal_kinship <- readRDS("maternal kinship.rds")
maternal_kinship <- maternal_kinship[left_aggregated$ID,left_aggregated$ID]

# Also can make a matrix to indicate pod membership, so we can control for it in the model
same_pod <- aninet::attribute_similarity(substr(left_aggregated$ID,1,1), type = "discrete")

require(asnipe)

SRI <- X/D
SRI[is.na(SRI)] <- 0

# Now we do a GLMQAP to test for the effects
# G: Generalized
# L: Linear
# M: Model
# Q; Quadratic
# A: Assignment
# P: Procedure
# All this means is we're fitting a GLM (here a binomial GLM) and then testing the coefficients with some permutations
# You can do any number of predictors, but they all need to be matrices of the same dimension as the social network

model <- glmqap(SRI ~ right_similarity + left_similarity + maternal_kinship + same_pod, weights = D, family = "binomial")
print(model)
