# An example analysis
# install.packages("lme4") # Run this line if lme4 package is not installed

require(lme4) # load in lme4 package; this is what we use for mixed effects models

# Read in the eyepatch data

left_data <- read.csv("left_data.csv") # if you've saved the data in a different location, you'll need to change the path here
right_data <- read.csv("right_data.csv") # if you've saved the data in a different location, you'll need to change the path here

whale_attributes <- read.csv("whale attributes.csv") # read in the whale attributes file. 

# We can add some attributes to the left and right datasets

left_data$yob <- whale_attributes$yob[match(left_data$ID,whale_attributes$id)]
left_data$age <- left_data$year - left_data$yob
left_data$sex <- whale_attributes$sex[match(left_data$ID,whale_attributes$id)]

right_data$yob <- whale_attributes$yob[match(right_data$ID,whale_attributes$id)]
right_data$age <- right_data$year - right_data$yob
right_data$sex <- whale_attributes$sex[match(right_data$ID,whale_attributes$id)]

# We'll fit models that predicts narrowness from age and individual ID
# This model can help us say how much variance in narrowness is explained by differences between individuals, as opposed to within-individual variation, while also correcting for age differences.
# This measure is often called "repeatability"

# We'll log transform our narrowness measure; this is pretty standard for ratio data (which are strictly positive and often skewed), and will help us get normal-ish residuals
# We put our random effects in the model using (1|GROUP), where GROUP would be a variable containing a grouping variable (in our case, whale ID)

left_model <- lmer(log(narrowness) ~ log(age+1) + (1|ID), data = left_data)
right_model <- lmer(log(narrowness) ~ log(age+1) + (1|ID), data = right_data)

# Let's first plot the relationship with age

par(mfrow = c(1,2))

plot(narrowness ~ age, data = left_data, main = "Left")
curve(exp(predict(left_model, newdata = data.frame(age = x, ID = "J19"), re.form = NA)), add = T, col = "red",lwd = 2)

plot(narrowness ~ age, data = right_data, main = "Right")
curve(exp(predict(right_model, newdata = data.frame(age = x, ID = "J19"), re.form = NA)), add = T, col = "red",lwd = 2)

# Now let's get the repeatability

left_var <- as.data.frame(VarCorr(left_model,comp="Variance"))[,4] # variance of ID and residual for lefts
right_var <- as.data.frame(VarCorr(right_model,comp="Variance"))[,4] # same for rights

# calculate the portion of the variance explained by ID
left_repeatability <- left_var[1]/sum(left_var)
right_repeatability <- right_var[1]/sum(right_var)

left_repeatability
# Should be 0.682

right_repeatability
# Should be 0.757

# These repeatabilities are quite good!
# Most of the variation in shape is between individuals, rather than within, so we're getting some good individual level information here!

