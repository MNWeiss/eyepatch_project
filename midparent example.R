left_data <- read.csv("left_data.csv") # if you've saved the data in a different location, you'll need to change the path here
whale_attributes <- read.csv("whale attributes.csv") # read in the whale attributes file. 

left_aggregated <- aggregate(log(narrowness) ~ ID, data = left_data, FUN = mean) # get the mean log(narrowness) for each individual

left_aggregated$mom <- whale_attributes$mother[match(left_aggregated$ID, whale_attributes$id)] # get each individual's mom
left_aggregated$dad <- whale_attributes$father[match(left_aggregated$ID, whale_attributes$id)] # get their dads

left_aggregated$mom_narrowness <- left_aggregated$`log(narrowness)`[match(left_aggregated$mom, left_aggregated$ID)] # use the info above to get mom's average narrowness
left_aggregated$dad_narrowness <- left_aggregated$`log(narrowness)`[match(left_aggregated$dad, left_aggregated$ID)] # same thing for dads

left_aggregated$midparent <- (left_aggregated$mom_narrowness + left_aggregated$dad_narrowness)/2 # record the mid-point between mom and dad narrowness

#### Now let's do the actual analysis!

model1 <- lm(`log(narrowness)` ~ midparent, data = left_aggregated) # fit a simple linear model
summary(model1) # look at the model summary

plot(`log(narrowness)` ~ midparent, data = left_aggregated) # plot the data
plot(model1) # diagnostic plots for the linear model
