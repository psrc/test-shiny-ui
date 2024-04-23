library(shiny)
library(psrcplot)
library(plotly)
library(bs4Dash)
library(psrc.travelsurvey)
library(odbc)
library(DBI)
library(data.table)
library(tidyverse)
library(DT)
library(here)
library(shinycssloaders)

install_psrc_fonts()

## !special case!
vars.subset <- fread('data/vars.csv') # cat variables that have consistently named aliases for testing
current.vars.subset <- fread('data/summary_out.csv')

source('modules/functions.R') # read functions file first

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

