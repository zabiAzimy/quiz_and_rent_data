# loading the library for plotting
library(ggplot2)

# read the data
rent_data <- read.csv("data/Rent-21998.csv")

# looking at the data
head(rent_data)

# number of rows in the dataset
nrow(rent_data) # 117

# looking at the column names and their types
str(rent_data)


# defining model to explain netrent by space

m1 <- lm(netrent ~ space, data = rent_data)

# constructing the model
# Create the quadratic term
rent_data$space2 <- rent_data$space ^ 2


# Fit the quadratic model
m2 <- lm(netrent ~ space + space2, data = rent_data)

# explain netrent by room, year and space
m3 <- lm(netrent ~ rooms + year + space, data = rent_data)
# having a look at summary of m3
summary(m3) # the estimated standard deviation of residuals is 163

# we can also find standard residual error as the following
summary_m3 <- summary(m3)
summary_m3$sigma

# Explain netrent by space and space^2 and year and kitchen
m4 <- lm(netrent ~ space + space2 + year + kitchen, data = rent_data)

# creating a new data and then predicting the netrent for that
new_apartment <- data.frame(
  space = 60,
  space2 = 60^2,
  year = 1975,
  kitchen = "no"
)

# using predict function to make predictions
predicted_rent <- predict(m4, newdata = new_apartment)
predicted_rent  # It gives prediction of 507.6766


# taking a look at all the AIC values of the models
# Calculate AIC for each model
aic_m1 <- AIC(m1)
aic_m2 <- AIC(m2)
aic_m3 <- AIC(m3)
aic_m4 <- AIC(m4)

# Print the AIC values
# among the AIC models we find the lowest AIC, that belongs to model m4
# thus m4 is the preferred model
aic_m1
aic_m2
aic_m3
aic_m4


# looking at the m3 summary we conclude that coefficient for rooms is not significant 
# at 10%
summary(m3)



# part f: The plots

# Example data for plotting
plot_data <- data.frame(space = rent_data$space,
                        netrent = rent_data$netrent,
                        space2 = rent_data$space^2)

# Predictions from models
plot_data$m1_pred <- predict(m1, newdata = plot_data)
plot_data$m2_pred <- predict(m2, newdata = plot_data)

# Sort data by space for smoother plotting
plot_data <- plot_data[order(plot_data$space), ]


# create the scatter plots and regression curves
# Scatter plot with regression curve for model m1
p_m1 <- ggplot(plot_data, aes(x = space, y = netrent)) +
  geom_point() +
  geom_line(aes(y = m1_pred), color = "blue") +
  labs(title = "Scatter Plot with Regression Curve (Model m1)",
       x = "Space",
       y = "Net Rent")

# Scatter plot with regression curve for model m2
p_m2 <- ggplot(plot_data, aes(x = space, y = netrent)) +
  geom_point() +
  geom_line(aes(y = m2_pred), color = "red") +
  labs(title = "Scatter Plot with Regression Curve (Model m2)",
       x = "Space",
       y = "Net Rent")

# displaying the plots
p_m1
p_m2


# part g: 
summary(m4)
# This indicates that, on average, apartments with an upscale kitchen (kitchenyes = yes)
# have a net rent that is approximately $117.0 higher than apartments without an upscale
# kitchen (kitchenyes = no), all else being equal.


# part h: 
# In the context of comparing the four models (m1, m2, m3, and m4) using F-tests,
# we typically use F-tests to assess whether adding additional variables (or terms)
# in a model significantly improves its fit compared to a simpler model. 

# The code needed for F-tests
# F-test for m1 vs. m2
anova(m1, m2)

# F-test for m2 vs. m3
anova(m2, m3)

# F-test for m3 vs. m4
anova(m3, m4)

# m1 vs. m2: Model m1 is preferred because m2 does not provide a significant improvement in fit.
# m2 vs. m3: Model m2 is preferred because m3 does not provide a significant improvement in fit.
# m3 vs. m4: Model m4 is preferred because m4 provides a significant improvement in fit compared to m3.

