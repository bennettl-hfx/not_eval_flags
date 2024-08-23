# Combining subsequent deployments and reapplying QC Flags over deployment change
# 2024-07-25, Author: LB

# Load required libraries-------------------------------------------------------
library(qaqcmar)
library(ggplot2)
library(sensorstrings)
library(dplyr)
library(lubridate)
library(plotly)

# Read in test data and drop previous flag columns -----------------------------
# Must be connected to Perennia VPN

depl_1 <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-02-28.rds") %>%
  select(county:temperature_degree_c)

depl_2 <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-09-13.rds") %>%
  select(county:temperature_degree_c)

# Serial Number and Deployment Range column are renamed to store the previous (true) values
# Data must be grouped by sensor depth so that the new temporary serial numbers are by depth (not by sensor type)
# The deployment range must also be manually changed since qc_test_rolling_sd groups by deployment_range (separating the deployments despite wanting them put together)

filtered_test_dat <- rbind(depl_1, depl_2) %>%
  #filter(timestamp_utc > "2018-09-10 00:00:00" & timestamp_utc < "2018-09-18 00:00:00") %>% #Optional filtering to zoom in on the deployment change in the plots
  rename(sensor_serial_number_real = sensor_serial_number, deployment_range_real = deployment_range) %>%
  group_by(sensor_depth_at_low_tide_m) %>%
  mutate(sensor_serial_number = cur_group_id(), deployment_range = "2018-Feb-28 to 2019-Apr-12") # Deployment range must be manually updated

# Apply the rolling sd test to the newly updated data
filtered_test_dat_rsd <- qc_test_rolling_sd(filtered_test_dat)

# Assign plots to the variable p
p <- filtered_test_dat_rsd %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2)

# print the plots
p

# Print a specific plot with ggplotly (and zoom in on the deployment change)
p_2 <- p$temperature_degree_c$rolling_sd

ggplotly(p_2)

