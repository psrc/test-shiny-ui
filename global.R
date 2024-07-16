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
library(echarts4r)
library(openxlsx)

install_psrc_fonts()

selected_year <- 2023

## !special case!
vars.subset <- fread('data/vars.csv') # cat variables that have consistently named aliases for testing
# current.vars.subset <- fread('data/crosstab_df.csv')
current.vars.subset <- read.xlsx('data/all_summary_tbls.xlsx')

current.vars.subset <- current.vars.subset |> 
  filter(year == selected_year & !is.na(grouping))

source('modules/functions.R') # read functions file first

# run all files in the modules sub-directory
module_files <- list.files('modules', full.names = TRUE)
sapply(module_files, source)

