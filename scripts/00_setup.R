# Load packages
library(tidyverse)
library(tidycensus)
library(janitor)
library(sf)
library(tmaptools)
library(foreach)
library(RCurl)
library(RJSONIO)
library(imputeMulti)
library(papaja)
library(grf)

iheiddown::count_words("thesis.Rmd")