whale_data <- read.csv("right left values 2.csv")

require(aninet)

# These are "broadscale" association numerators and denominators
# The numerator is the number of days two individuals were photographed in the same group
# The denominators are the number of days at least one of the individuals was photographed
# We calculated this for years in which both of the pair were alive between 1987 and 2020
X <- readRDS("broadscale SRI numerator.rds")
D <- readRDS("broadscale SRI denominator.rds")

whale_data <- whale_data[whale_data$ID %in% colnames(X),]

X <- X[whale_data$ID,whale_data$ID]
D <- D[whale_data$ID,whale_data$ID]

SRI <- X/D
SRI[is.na(SRI)] <- 0

View(SRI)

whale_data$weighted_degree <- rowSums(SRI) # very simple centrality measure, just the sum of each individuals' SRIs to other individuals

narrowness_FA_model <- lm(log(weighted_degree) ~ FA.narrowness + pod, data = whale_data)
plot(narrowness_FA_model)
summary(narrowness_FA_model)
