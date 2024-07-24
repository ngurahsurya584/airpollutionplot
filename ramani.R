library(openair)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
df <-read.csv("data.csv", header=TRUE)
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

# Adjust the breaks to start from 1.00 and end at the last multiple of 0.05
breaks <- c(1.00, breaks[breaks >= 1.00])

# Create a new column for binned Angstrom values
df$Angstrom_binned <- cut(df$Angstrom, breaks = breaks, include.lowest = TRUE, right = FALSE)

dfcp <- df[!is.na(df$Angstrom_binned), ]

View(dfcp)

freq_table_list <- dfcp %>%
  group_by(season, Angstrom_binned) %>%
  summarise(freq = n()) %>%
  ungroup() %>%
  split(.$season)

View(freq_table_list)


options(vsc.dev.args = list(width = 2000, height = 1500))

par(mfrow = c(2, 2), mar = c(10, 6, 6, 4), oma = c(3, 3, 3, 3))

barplot_list <- lapply(freq_table_list, function(freq_table) {
  barplot(freq_table$freq, names.arg = freq_table$Angstrom_binned, las = 2, col = "skyblue",
          main = paste("Barplot of Angstrom Data -", freq_table$season[1]),
          xlab = "Angstrom Interval", ylab = "Frequency", space = 0,
          cex.names = 1.7, cex.axis = 2, cex.lab = 2.5, cex.main = 2.5, cex.sub = 2.5,
          mgp = c(8.5, 1, 0))
})


# End of the code for the barplot

# Start of the code for the barplot of angstroms in 1 year
# Create a sequence of breaks for binning
breaks_1year <- seq(1, 1.70, by = 0.05)

# Create a new column for binned Angstrom values
df$Angstrom_binned_year <- cut(df$Angstrom, breaks = breaks_1year, include.lowest = TRUE, right = FALSE)

# Calculate the frequency of each Angstrom bin
freq_table <- df %>%
  filter(!is.na(Angstrom_binned_year)) %>%
  group_by(Angstrom_binned_year) %>%
  summarise(freq = n())



barplot(freq_table$freq, names.arg = freq_table$Angstrom_binned_year, las = 2, col = "skyblue",
        main = paste("Barplot of Angstrom Data -", freq_table$season[1]),
        xlab = "Angstrom Interval", ylab = "Frequency", space = 0,
        cex.names = 1.7, cex.axis = 2, cex.lab = 2.5, cex.main = 2.5, cex.sub = 2.5,
        mgp = c(8.5, 1, 0))


# Create the bar plot using ggplot2
options(vsc.dev.args = list(width = 2000, height = 1500))
bar_plot <- ggplot(freq_table, aes(x = Angstrom_binned_year, y = freq)) +
  geom_bar(stat = "identity", fill = "skyblue", width = 1) +
  labs(title = "Barplot of Angstrom Data",
       x = "Angstrom Interval", y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 17),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size = 25),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

# Display the bar plot
print(bar_plot)




# Start of the code for the polar plot
polarPlot(df, pollutant="BCff", type = "season" )

polarPlot(df, pollutant="BCbb", type = "season" )

windRose(df, ws="ws", wd="wd", type= "season")

# End of the code for the polar plot


# Start of the code for the time series plot
angstrom_clean <- df$Angstrom[!is.na(df$Angstrom) & df$Angstrom != 0]
date_clean <- df$date[!is.na(df$Angstrom) & df$Angstrom != 0]
options(vsc.dev.args = list(width = 1500, height = 800))
plot(date_clean, angstrom_clean, type="l", xlab="year", ylab="BC", main="Angstrom levels over time")

# End of the code for the time series plot

