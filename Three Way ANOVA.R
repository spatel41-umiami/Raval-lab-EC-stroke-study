# title: "Working Memory 3 Way Anova"
# author: "Isha"
# date: "12/18/2023"
# output: pdf()

library(dplyr)
library(rstatix)
library(tidyverse)
library(ggpubr)
library(ggplot2)
library(tidyr)

read.csv(file = "WM_Rdf.csv", header = TRUE)
WorkingMemory <- read.csv(file = "WM_Rdf.csv", header = TRUE)
head(WorkingMemory)

names(WorkingMemory) <- c("id","treatment","surgery", "sex", "t1","t2")
  WorkingMemory %>% sample_n_by(surgery, treatment, size = 1)

WorkingMemory <- WorkingMemory %>%
  gather(key = "time", value = "latency", t1, t2) %>% convert_as_factor(id, time)

WorkingMemory %>%
  group_by(surgery, treatment, time) %>%
  get_summary_stats(latency, type = "mean_sd")

WorkingMemory.aov <- anova_test(
  data = WorkingMemory, dv = latency, wid = id,
  within = time, between = c(surgery, treatment, sex))
WorkingMemory.aov

get_anova_table(WorkingMemory.aov)

tinytex::install_tinytex()
