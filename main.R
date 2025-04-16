library(tidyverse)
library(data.table)
library(forcats)
library(readxl)
library(rdrobust)

sourcedir <- file.path("D:/GitHub Files/ECON2020/final_project", "source")

# Run the cleaning script
source(file.path("D:/GitHub Files/ECON2020/final_project", "clean.R"))

# Run the analysis script
source(file.path("D:/GitHub Files/ECON2020/final_project", "analysis.R"))
