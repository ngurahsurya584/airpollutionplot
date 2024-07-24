library(openair)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)

df <-read.csv("data2.csv", header=TRUE)

View(df)

# drop the invalid number rows
df <- df[!is.na(df$Angstrom), ]

# create plot with heatmap from AAEff and AAEbb where AAEff is on y-axis and AAEbb is on x-axis and fill with color based on BCff/BC
options(vsc.dev.args = list(width = 1500, height = 1000))
ggplot(df, aes(x = AAEbb, y = AAEff, fill = BCff/BC)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "AAEbb",  y = "AAEff", fill = "BCff/BC") +
  theme_minimal() + theme(text=element_text(size=30))

ggplot(df, aes(x = AAEbb, y = AAEff, fill = BCbb/BC)) +
  geom_tile() +
  scale_fill_viridis_c() +
  labs(x = "AAEbb", y = "AAEff", fill = "BCbb/BC") +
  theme_minimal() + theme(text=element_text(size=30))
