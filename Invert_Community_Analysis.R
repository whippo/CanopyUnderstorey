#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# Canopy-Understorey Invert Community Comparison                                 ##
# Data are current as of 2022-10-07                                              ##
# Data source: Ross Whippo - OIMB/FHL/NSF/Samish Nation                          ##
# R code prepared by Ross Whippo                                                 ##
# Last updated 2022-10-08                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# INVERT_SWATH.csv

# Associated Scripts:
# NONE

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# 2022-10-08 Script created


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)
library(vegan)
library(worrms)
library(data.table)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

INVERT_SWATH <- read_csv("Data/INVERT_SWATH.csv", 
                         col_types = cols(Date = col_date(format = "%Y-%M-%D"), 
                                          Habitat = col_factor(levels = c("Subcanopy", "Canopy")), 
                                          Interval = col_factor(levels = c("0-5", "5-10", "10-15", 
                                                                           "15-20", "20-25", "25-30", 
                                                                           "30-35", "35-40"))))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# check species names

Inverts <- INVERT_SWATH %>%
  unite(genusSpecies, Genus, Species, sep = " ")
InvertNames <- unique(Inverts$genusSpecies)
InvertWorms <- wm_records_names(name = c(InvertNames))
InvertWormsTib <- data.table::rbindlist(InvertWorms)

# create wide matrix of data

# canopy
canopy <- Inverts %>%
  filter(Habitat == "Canopy") %>%
  pivot_wider(id_cols = Interval, names_from = Code, values_from = Count)
canopy_mat <- canopy[,2:23]

# subcanopy
subcanopy <- Inverts %>%
  filter(Habitat == "Subcanopy") %>%
  pivot_wider(id_cols = Interval, names_from = Code, values_from = Count)
subcanopy_mat <- subcanopy[,2:25]

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# SPECIES ACCUMULATION                                                         ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

canopy_accum <- specaccum(canopy_mat)
plot(canopy_accum)

subcanopy_accum <- specaccum(subcanopy_mat)
plot(subcanopy_accum)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# EXPORT DATASETS                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Create wide csv for all data
INVERT_SWATH_wide <- Inverts %>%
  unite("surveyID", c(Date:Habitat, Interval)) %>%
  pivot_wider(id_col = surveyID, names_from = genusSpecies, values_from = Count, values_fill = 0)
write_csv(INVERT_SWATH_wide, "INVERT_SWATH_wide.csv")

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####