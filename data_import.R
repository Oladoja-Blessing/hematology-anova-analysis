library(tidyverse)
library(dplyr)

df <- read.csv("Final Data.csv")

df_nw <- df%>%
  group_by(Treatment, Parameter)%>%
  mutate(id_row = row_number())%>%
  ungroup()%>%
  pivot_wider( values_from = "Value", names_from = "Parameter")
