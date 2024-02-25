library(dplyr)
library(RPostgres)
library(shinythemes)
library(kableExtra)
library(ggplot2)
library(tidyverse)
library(plotly)

data = read.csv("data.csv")
rownames(data) = data[,1]