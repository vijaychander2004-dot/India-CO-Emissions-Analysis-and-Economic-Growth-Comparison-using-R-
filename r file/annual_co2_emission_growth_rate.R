# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the dataset
file_path <-"C:/Users/vijay/OneDrive/Desktop/CO2 emission project/Data/countries_data.csv"
co2_data <- read.csv(file_path, row.names = 1, check.names = FALSE)

# Convert data to numeric (important if read as character)
co2_data[] <- lapply(co2_data, as.numeric)

# Calculate annual growth rate (% change across columns)
growth_rates <- t(apply(co2_data, 1, function(x) {
  c(NA, diff(x) / head(x, -1) * 100)
}))

# Convert to dataframe
growth_rates <- as.data.frame(growth_rates)

# Add country names
growth_rates$Country <- rownames(co2_data)

# Reshape data (wide → long format)
growth_long <- pivot_longer(growth_rates, 
                            cols = -Country, 
                            names_to = "Year", 
                            values_to = "Growth Rate")

# Convert Year to numeric (optional but better)
growth_long$Year <- as.numeric(growth_long$Year)

# Create faceted plot
ggplot(growth_long, aes(x = Year, y = `Growth Rate`)) +
  geom_line(color = "steelblue") +
  facet_wrap(~ Country, scales = "free_x", ncol = 3) +
  labs(
    title = "Annual CO2 Emissions Growth Rate - Faceted Plot",
    x = "Year",
    y = "Annual CO2 Emissions Growth Rate (%)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

