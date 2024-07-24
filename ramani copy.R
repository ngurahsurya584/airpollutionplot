library(openair)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
df <-read.csv("data3.csv", header=TRUE)
View(df)
colnames(df)[1] <- "date"
colnames(df)[3] <- "wd"
colnames(df)[4] <- "ws"

df$date <- as.POSIXct(df$date)

# Create a new column for seasons
df$season <- NA

# Define function to assign seasons based on month
assign_season <- function(month) {
  if (month %in% 3:5) {
    return("Spring")
  } else if (month %in% 6:8) {
    return("Summer")
  } else if (month %in% 9:11) {
    return("Autumn")
  } else {
    return("Winter")
  }
}

# Extract month from date-time data
df$month <- as.numeric(format(df$date, "%m"))

# Assign seasons to each row based on the month
df$season <- sapply(df$month, assign_season)

# Remove the intermediate 'month' column if you don't need it anymore
df <- df[, -which(names(df) == "month")]


View(df)

#Start of the code for the barplot
# Create a sequence of breaks for binning
breaks <- seq(1, 1.70, by = 0.05)

# Create a new column for binned Angstrom values
df$Angstrom_binned <- cut(df$Angstrom, breaks = breaks, include.lowest = TRUE, right = FALSE)

dfcp <- df[!is.na(df$Angstrom_binned), ]

freq_table_list <- dfcp %>%
  group_by(season, Angstrom_binned) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  complete(season, Angstrom_binned, fill = list(freq = 0))

options(vsc.dev.args = list(width = 2000, height = 1500))
ggplot(freq_table_list, aes(x = Angstrom_binned, y = freq, fill = season)) +
  geom_bar(stat = "identity", width = 1) +
  facet_wrap(~ season, ncol = 2, scales = "free") +
  labs(x = "Angstrom", y = "Frequency") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 20),
    axis.text.y = element_text(size = 20),
    axis.title = element_text(size = 25),
    strip.text = element_text(size = 25),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    panel.spacing = unit(2, "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )


# End of the code for the barplot

# make the barplot for the angstrom value in 1 year

# Create a sequence of breaks for binning
breaks <- seq(0, 1.70, by = 0.05)

# Create a new column for binned Angstrom values
df$Angstrom_binned <- cut(df$Angstrom, breaks = breaks, include.lowest = TRUE, right = FALSE)

dfcp <- df[!is.na(df$Angstrom_binned) & df$Angstrom >= 1, ]

freq_table <- dfcp %>%
  group_by(Angstrom_binned) %>%
  summarise(freq = n())

options(vsc.dev.args = list(width = 2000, height = 1500))

ggplot(freq_table, aes(x = Angstrom_binned, y = freq)) +
  geom_bar(stat = "identity", width = 1, fill = "skyblue") +
  labs(title = "Angstrom Data (1 Year)",
       x = "Angstrom", y = "Frequency") +
  scale_x_discrete(drop = FALSE) +  # Show all x-axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 32),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
#end of the code for the barplot of angstroms in 1 year

#line plot version of angstrom for 1 year

# Create a sequence of breaks for binning
breaks <- seq(1, 1.70, by = 0.05)

# Create a new column for binned Angstrom values
df$Angstrom_binned <- cut(df$Angstrom, breaks = breaks, include.lowest = TRUE, right = FALSE)

dfcp <- df[!is.na(df$Angstrom_binned) & df$Angstrom >= 1, ]

freq_table <- dfcp %>%
  group_by(Angstrom_binned) %>%
  summarise(freq = n())

options(vsc.dev.args = list(width = 2000, height = 1500))

ggplot(freq_table, aes(x = Angstrom_binned, y = freq, group = 1)) +
  geom_line(color = "red", size = 2) +
  geom_point(color = "red", size = 4) +
  labs(title = "Angstrom Data (1 Year)",
       x = "Angstrom", y = "Frequency") +
  scale_x_discrete(drop = FALSE) +  # Show all x-axis labels
  theme_minimal() +
  theme(
    plot.title = element_text(size = 32, face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 90, hjust = 1, size = 28),
    axis.text.y = element_text(size = 28),
    axis.title = element_text(size = 32),
    plot.margin = unit(c(2, 2, 2, 2), "cm"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
#end of the code for the line plot of angstroms in 1 year

# Start of the code for the polar plot
polarPlot(df, pollutant="BCff", type = "season" )

polarPlot(df, pollutant="BCbb", type = "season" )

windRose(df, ws="ws", wd="wd", type= "season")

# End of the code for the polar plot


# Start of the code for the time series plot
angstrom_clean <- df$Angstrom[!is.na(df$Angstrom) & df$Angstrom != 0]
date_clean <- df$date[!is.na(df$Angstrom) & df$Angstrom != 0]
options(vsc.dev.args = list(width = 1500, height = 800))
plot(date_clean, angstrom_clean, type="l", xlab="year", ylab="BC", main="Angstrom levels over time", , ylim = c(0, max(angstrom_clean)))

# End of the code for the time series plot

