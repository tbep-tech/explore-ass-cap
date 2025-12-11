library(tidyverse)
library(tbeptools)
library(here)

source(here('R/funcs.R'))

# get water quality data from epc ----------------------------------------

epcwq <- epcdata |> 
  filter(bay_segment == 'OTB') |> 
  select(-bay_segment)

save(epcwq, file = here('data/epcwq.RData'))

# get loading data -------------------------------------------------------

mosdatraw <- rdataload('https://github.com/tbep-tech/load-estimates/raw/refs/heads/main/data/mosdat.RData') 

lddat <- mosdatraw |> 
  filter(bay_segment == 'Old Tampa Bay') |> 
  select(-bay_segment)

save(lddat, file = here('data/lddat.RData'))

# # get water quality data from pinellas -----------------------------------

# # get sampling locations for a specific waterbody
# waterbodies <- util_importwqwa('waterbodies')
# waterbodyid <- waterbodies |> 
#   dplyr::filter(grepl('Old Tampa Bay', name)) |>
#   dplyr::pull(id)
# samplocs <- util_importwqwa('sampling-locations', waterbodyId = waterbodyid)
