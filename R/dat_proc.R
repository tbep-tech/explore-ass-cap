library(tidyverse)
library(here)
library(sf)
library(tbeptools)

source(here('R/funcs.R'))

# get loading data -------------------------------------------------------

mosdatraw <- rdataload(
  'https://github.com/tbep-tech/load-estimates/raw/refs/heads/main/data/mosdat.RData'
)

lddat <- mosdatraw |>
  filter(
    bay_segment %in%
      c(
        'Old Tampa Bay',
        'Hillsborough Bay',
        'Middle Tampa Bay',
        'Lower Tampa Bay'
      )
  ) |>
  mutate(
    bay_segment = case_when(
      bay_segment == 'Old Tampa Bay' ~ 'OTB',
      bay_segment == 'Hillsborough Bay' ~ 'HB',
      bay_segment == 'Middle Tampa Bay' ~ 'MTB',
      bay_segment == 'Lower Tampa Bay' ~ 'LTB'
    )
  ) |>
  summarise(
    tn_load = sum(tn_load, na.rm = TRUE),
    .by = c(year, month, bay_segment)
  ) |>
  rename(
    yr = year,
    mo = month
  ) |>
  mutate(
    date = make_date(yr, mo, 1)
  )

save(lddat, file = here('data/lddat.RData'))

# # get water quality data from pinellas -----------------------------------

# # get sampling locations for a specific waterbody
# waterbodies <- util_importwqwa('waterbodies')
# waterbodyid <- waterbodies |>
#   dplyr::filter(grepl('Old Tampa Bay', name)) |>
#   dplyr::pull(id)
# samplocs <- util_importwqwa('sampling-locations', waterbodyId = waterbodyid)

# Pinellas data from PDEM dashboard --------------------------------------

pinchlraw <- readxl::read_excel(here('data/data_pinellas_wq_2003_2025.xlsx'))

pinchl <- pinchlraw |>
  dplyr::filter(grepl('^OLD', Segment)) |>
  select(
    Site,
    date = Date,
    Latitude,
    Longitude,
    `Chl-a`,
    temp = Temp_Water,
    sal = Salinity
  ) |>
  mutate(
    date = as.Date(date),
    strata = gsub('\\-.*$', '', Site)
  ) |>
  filter(
    Latitude < 28.5 & Latitude > 27.5 & Longitude < -82.5 & Longitude > -82.8
  ) |>
  filter(!is.na(`Chl-a`))

save(pinchl, file = here('data/pinchl.RData'))
