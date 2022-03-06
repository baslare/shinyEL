require(shiny)
require(shinyjs)
require(shinythemes)
require(tidyverse)
require(ggsci)
require(hexbin)

data <- readRDS('data/data_new.rds')
features <- readRDS('data/features.rds')