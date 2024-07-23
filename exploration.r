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

# looking at the column names
names(rent_data)

# let's plot the histograms and kernel density for some of the varibales
hist(rent_data$netrent, xlab = "Netrent", ylab = "density")

# kernel density
lines(density(rent_data$netrent),
      lwd = 2,
      col = "chocolate3")

# den <- density(rent_data$netrent)

ggplot(rent_data, aes(x = space)) +
  geom_density(fill = "blue", alpha = 0.6, linetype="dashed") +
  labs(title = "Kernel Density Plot of netrent",
       x = "netrent",
       y = "Density")


# density plot for the other variables
ggplot(rent_data, aes(x = netrent)) +
  geom_density(fill = "purple", alpha = 0.6, linetype="dashed") +
  labs(title = "Kernel Density Plot of netrent",
       x = "netrent",
       y = "Density")




# histogram for the netrent
ggplot(data = rent_data, aes(x = netrent)) +
  geom_histogram(fill= "brown", alpha=0.5, bins = 40) +
  geom_density(data = rent_data, aes(x = netrent))



# scatter plot for the area and netrent
ggplot(data = rent_data, aes(x = space, y = netrent)) + 
  geom_point(color = "blue",
             fill="#69b3a2",
             shape=21,
             alpha=0.6,
             size=4,
             stroke = 1)

ggplot(data = rent_data, aes(x = year, y = netrent)) + 
  geom_point(
    fill="#69b3a2",
    shape=21,
    alpha=0.6,
    size=4,
    stroke = 1)
