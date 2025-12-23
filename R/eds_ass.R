library(sf)
library(tidyverse)

tran <- transectdem |>
  st_drop_geometry() |>
  mutate(
    seg = substr(Transect,1,2),
    yr = year(Date),
    msl_depth = Depth_dem - 45.3 #Correct for MLLW datum at OTB Tide Gage (i.e. MSL = 0.453 m)
  ) |>
  filter(seg == 'S1',
         pa == 1) |>
  summarise(
    min_depth = min(msl_depth, na.rm = T),
    .by = c(yr, Transect)
  )

p <- ggplot2::ggplot(data = tran, aes(x = yr, y=min_depth, group=yr)) +
     ggplot2::geom_boxplot() +
     ggplot2::scale_x_continuous(breaks = c(2000,2005,2010,2015,2020,2025)) +
     ggplot2::geom_quantile(quantiles = c(0.1), aes(group = 1, color= "p10")) +
     ggplot2::geom_quantile(quantiles = c(0.5), aes(group = 2, color= "p50")) +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    legend.position = c(0.05,0.95)
  ) +
  ggplot2::labs(
    y = 'Max. Depth of Seagrass Presence Estimate (cm)',
    title = "2m Target Depth still Applicable for OTB"
  )
p

newdat <- epcdata |>
  filter(
    bay_segment %in% 'OTB'
  ) |>
  mutate(
    date = make_date(yr, mo, day = 1),
    turb_raw = as.numeric(`Turbidity_JTU-NTU`),
    color_raw = as.numeric(`Color_345_F45_PCU`)
  ) |>
  summarise(
    sdm = mean(sd_raw_m, na.rm = T),
    lam = 1.49/sdm,
    sal = mean(Sal_Mid_ppth, na.rm = T),
    temp = mean(Temp_Water_Mid_degC, na.rm = T),
    turb = mean(turb_raw, na.rm = T),
    color = mean(color_raw, na.rm = T),
    .by = c(bay_segment, date, yr, mo)
  ) |>
  inner_join(wqmo, by = c('bay_segment', 'date', 'yr', 'mo')) |>
  inner_join(lddat, by = c('bay_segment', 'date', 'yr', 'mo')) |>
  arrange(date)

