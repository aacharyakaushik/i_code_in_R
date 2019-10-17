.libPaths("/data/hydro/R_libs35")
.libPaths()


library(data.table)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

options(digits=9)

# source_path = "/home/kraghavendra/hardiness/hardiness_core.R"
source_path = "C:/Users/Kaushik Acharya/Documents/Research Task - Kirti/4_kaushik/hardiness/hardiness_core.R"
source(source_path)

### Seeting the AGweater CSV file to usable format