
library(qaqcmar)
library(ggplot2)
library(sensorstrings)
library(dplyr)
library(kableExtra)
library(lubridate)
library(plotly)

# read in example data
path <- system.file("testdata", package = "qaqcmar")

dat <- readRDS(paste0(path, "/test_data_grossrange.RDS")) 

kable(dat[1:5, ])

ss_ggplot_variables(dat) + geom_point(size = 1)

test_dat <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-02-28.rds")
test_dat_2 <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-09-13.rds")


test_dat <- test_dat %>%
  select(county:temperature_degree_c)

test_dat_2 <- test_dat_2 %>%
  select(county:temperature_degree_c)



test_dat_rsd <- qc_test_rolling_sd(test_dat)


test_dat_rsd %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2)
#> $dissolved_oxygen_percent_saturation
#> $dissolved_oxygen_percent_saturation$grossrange

?qc_plot_flags

all_test_dat <- rbind(test_dat, test_dat_2)

test_dat_rsd_2 <- qc_test_rolling_sd(all_test_dat)

test_dat_rsd_2 %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2)


# Creating temporary serial number ------------------------------------

all_test_dat <- rbind(test_dat, test_dat_2)

all_test_dat <- all_test_dat %>%
  rename(sensor_serial_number_real = sensor_serial_number) %>%
  group_by(sensor_depth_at_low_tide_m) %>%
  mutate(sensor_serial_number = cur_group_id())


all_test_dat_rsd <- qc_test_rolling_sd(all_test_dat)
  
p <- all_test_dat_rsd %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2, vars = "temperature_degree_c")

p


updated_p <- p[2]

ggplotly(p)

all_test_dat_rsd_2 <- all_test_dat_rsd %>%
  filter(timestamp_utc > "2018-09-10 00:00:00" & timestamp_utc < "2018-09-18 00:00:00")

all_test_dat_rsd_2 %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2)

# qc_test_rolling_sd() groups by deployment range (can this be changed to cover the combined deployment dates?)

test_dat <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-02-28.rds")
test_dat_2 <- readRDS("R:/data_branches/water_quality/processed_data/qc_data/halifax/beaver_point_2018-09-13.rds")


test_dat <- test_dat %>%
  select(county:temperature_degree_c)

test_dat_2 <- test_dat_2 %>%
  select(county:temperature_degree_c)

filtered_test_dat <- rbind(test_dat, test_dat_2) %>%
  filter(timestamp_utc > "2018-09-10 00:00:00" & timestamp_utc < "2018-09-18 00:00:00") %>%
  rename(sensor_serial_number_real = sensor_serial_number, deployment_range_real = deployment_range) %>%
  group_by(sensor_depth_at_low_tide_m) %>%
  mutate(sensor_serial_number = cur_group_id(), deployment_range = "2018-Sep-10 to 2018-Sep-18")
  
filtered_test_dat_rsd <- qc_test_rolling_sd(filtered_test_dat, period_hours = 12)


p <- filtered_test_dat_rsd %>% 
  qc_pivot_longer(qc_tests = "rolling_sd") %>% 
  qc_plot_flags( qc_tests = "rolling_sd", ncol = 2)

p_2 <- p$temperature_degree_c$rolling_sd

ggplotly(p_2)

# Practice using the spike test ------------------------------------------------

filtered_test_dat_spike <- qc_test_spike(filtered_test_dat, join_column = "sensor_type")

filtered_test_dat_spike %>% 
  qc_pivot_longer(qc_tests = "spike") %>% 
  qc_plot_flags( qc_tests = "spike", ncol = 2)

