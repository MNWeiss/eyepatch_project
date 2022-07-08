# An example analysis
# install.packages("lme4") # Run this line if lme4 package is not installed

require(lme4) # load in lme4 package; this is what we use for mixed effects models

# Read in the eyepatch data

left_data <- read.csv("left_data.csv") # if you've saved the data in a different location, you'll need to change the path here
right_data <- read.csv("right_data.csv") # if you've saved the data in a different location, you'll need to change the path here

# We'll fit models that just have a population mean ("intercept") and individual random effects
# This model can help us say how much variance in narrowness is explained by differences between individuals, as opposed to within-individual variation
# This measure is often called "repeatability"
# Keep in mind that this is just an example; in practice, we might also want to correct for individual age and sex

# We'll log transform our narrowness measure; this is pretty standard for ratio data (which are strictly positive and often skewed), and will help us get normal-ish residuals
# We put our random effects in the model using (1|GROUP), where GROUP would be a variable containing a grouping variable (in our case, whale ID)
left_model <- lmer(log(narrowness) ~ (1|ID), data = left_data)
right_model <- lmer(log(narrowness) ~ (1|ID), data = right_data)

left_var <- as.data.frame(VarCorr(left_model,comp="Variance"))[,4] # variance of ID and residual for lefts
right_var <- as.data.frame(VarCorr(right_model,comp="Variance"))[,4] # same for rights

# calculate the portion of the variance explained by ID
left_repeatability <- left_var[1]/sum(left_var)
right_repeatability <- right_var[1]/sum(right_var)

left_repeatability
# Should be 0.667

right_repeatability
# Should be 0.732

# These repeatabilities are quite good!
# Most of the variation in shape is between individuals, rather than within, so we're getting some good individual level information here!

