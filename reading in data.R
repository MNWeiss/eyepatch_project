# Only run this line first time to install Momocs
# install.packages("Momocs")

require(Momocs) # load in the Momocs package

lefts <- list.files(path = "Left/", full.names = T) # get the file names for lefts. Change the path to the path of your lefts folder.
rights <- list.files(path = "Right/", full.names = T) # get the file names for rights. Change the path to the path of your rights folder

left_outlines <- import_jpg(lefts) # import lefts as outlines
right_outlines <- import_jpg(rights) # import rights as outlines

left_outlines <- lapply(left_outlines, coo_alignxax) # align the lefts along the x-axis
right_outlines <- lapply(right_outlines, coo_alignxax) # align the rights along the x-axis

# make a function to calculate narrowness
# this function takes a two column matrix (z) defining the x and y coordinates of the outline
# function returns a single value, the ratio of width to height
get_narrowness <- function(z){
  width <- max(z[,1]) - min(z[,1])
  height <- max(z[,2]) - min(z[,2])
  width/height
}

# get narrowness for lefts
# lapply will apply a function (FUN) to each element of a list, returnin the result.
# So, we will apply our "get_narrowness" function to each element of the left outlines
# We wrap it in the "unlist" function so we get a vector rather than a list as a result
left_narrowness <- unlist(lapply(left_outlines, FUN = get_narrowness))

# And then we'll do the same to the right outlines
right_narrowness <- unlist(lapply(right_outlines, FUN = get_narrowness))

# Use some string manipulation to get the whales' IDs from the file names
left_IDs <- substr(names(left_outlines),1, regexpr("_", names(left_outlines)) - 1)
right_IDs <- substr(names(right_outlines),1, regexpr("_", names(right_outlines)) - 1)

# Same kind of manipulation to get the year each photo was taken
left_year <- as.numeric(substr(names(left_outlines),regexpr("_", names(left_outlines)) + 3, regexpr("_", names(left_outlines)) + 6))
right_year <- as.numeric(substr(names(right_outlines),regexpr("_", names(right_outlines)) +3, regexpr("_", names(right_outlines)) + 6))

# Collect data into data frames
left_data <- data.frame(
  ID = left_IDs,
  year = left_year,
  narrowness = left_narrowness
)
right_data <- data.frame(
  ID = right_IDs,
  year = right_year,
  narrowness = right_narrowness
)

# We can export our datasets to .csv files so we can use them in other analysis

write.csv(left_data, file = "left_data.csv", row.names = F)
write.csv(right_data, file = "right_data.csv", row.names = F)

# N.B. the "file" argument can be changed to save these datasets where ever you want on your machine, and you can name them whatever you want as well.
