## Load the libraries we'll need.
library(datasets)
library(mosaic)

## Orchard sprays and honeybees
data(OrchardSprays)

# Calculate the group means.
# You must load the mosaic package to use the following command
mean(decrease~treatment, data=OrchardSprays)

# The above command shows you the groups and their means.
# You can store these in another variable.

GroupMeans = mean(decrease~treatment, data=OrchardSprays)

# Make a dot plot/strip chart showing the group-wise data
stripchart(decrease~treatment,
	data=OrchardSprays, vertical=TRUE,
	xlab="Concentration of Lime Sulphur (A = highest; H = none)",
	ylab="Decrease in volume of solution")

# Add the group means in a different color and label
# pch tells you the type of points; type in ?points to see the choices
# cex: character expansion.  Makes everything bigger (> 1) or smaller (< 1)
points(GroupMeans, pch=19, col='blue', cex=1.5)


# Fit a model and look at the differences between means
lm2 = lm(decrease~treatment, data=OrchardSprays)
coef(lm2)

# Compare these coefficients to the group means:
# How are they related?
# Hint: try adding the "intercept" to each coefficient
# and compare the result to the corresponding group mean!
rbind(GroupMeans, coef(lm2))

# rbind stands for row bind


# Load the chymotrypson data using RStudio's "Import Dataset" button

# Note the importance of the factor command.
plot(Rate~Conc, data=chymotrypsin)
plot(Rate~factor(Conc), data=chymotrypsin)

