
# Stock to Flow model

# Load necessary packages
library(tidyverse)
library(sandwich)

# Generate dummy data
set.seed(123)
n = 100
years = seq(2020, 2020 + n - 1)

# cumulative sum to ensure increasing stock
stock = cumsum(rnorm(n, mean = 10, sd = 2))

# flow with some randomness
flow = rnorm(n, mean = 10, sd = 2)

# price based on S2F ratio (2 is a dummy coefficient)
price = 2 * stock / flow + rnorm(n)

# Create a data frame
df <- data.frame(years, stock, flow, price)

# Calculate S2F ratio
df <- df %>%
    mutate(s2f = stock / flow)

# Simple linear regression of price on S2F ratio
fit <- lm(price ~ s2f, data = df)

# Print summary of the model
summary(fit)

# Check assumptions of linear regression
par(mfrow = c(2, 2))
plot(fit)

# If there's heteroscedasticity (unequal variance),
#   we might want to use robust standard errors
coeftest(fit, vcov = vcovHC(fit))
# t test of coefficients:
#           Estimate Std.  Error     t value  Pr(>|t|)
# (Intercept) -0.0197139  0.2022336  -0.0975   0.9225
#     s2f     2.0026179   0.0034025 588.5769   <2e-16 ***
#     ---
#     Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Predict price based on S2F ratio
df$predicted_price <- predict(fit, newdata = df)

# Compute the sequence of years with an increment of 1
year_breaks <- seq(min(df$years), max(df$years), by = 1)

# Plot actual price vs. predicted price
ggplot(df, aes(x = years)) +
    geom_line(aes(y = price, color = "Actual Price")) +
    geom_line(aes(y = predicted_price, color = "Predicted Price")) +
    labs(x = "Year", y = "Price") +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    scale_x_continuous(breaks = year_breaks)

# much cleaner without using the years_break object
# plot actual price vs. predicted price
ggplot(df, aes(x = years)) +
    geom_line(aes(y = price, color = "Actual Price")) +
    geom_line(aes(y = predicted_price, color = "Predicted Price")) +
    labs(x = "Year", y = "Price") +
    theme_minimal() +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(breaks = seq(min(df$years),
                                    max(df$years),
                                    by = 1),
                       limits = c(min(df$years),
                                  max(df$years)))
