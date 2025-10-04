# Load CSV file
covid <- read.csv("daily situation.csv", stringsAsFactors = FALSE)

# Convert Date column to proper Date type
covid$Date <- as.Date(covid$Date, format = "%d-%m-%Y")

# Extract month name
covid$Month <- format(covid$Date, "%B")

# Rename columns (shorter names for easier use)
names(covid) <- make.names(names(covid))
names(covid)
# Subset April and May daily new cases
april_cases <- as.numeric(covid$COVID.19.Positive...last.24H[2:31])  # rows 2 to 31
may_cases   <- as.numeric(covid$COVID.19.Positive...last.24H[32:61])

length(april_cases )
length(may_cases ) 
shapiro.test(april_cases)
shapiro.test(may_cases)
par(mfrow=c(1,2))
boxplot(april_cases, main="Boxplot of April(2020)")
boxplot(may_cases, main="Boxplot of May(2020)")
#not normal
shapiro.test(may_cases)
qqnorm(april_cases,main="Q-Q plot of April(2020)")
qqnorm(may_cases,main="Q-Q plot of May(2020)")
qqline(april_cases,col="red")
qqline(may_cases,col="green")
#not normal
var(april_cases)
var(may_cases)
dat <- data.frame(april_cases,may_cases)
st <- stack(dat[,c("april_cases","may_cases")])
bartlett.test(values~ind,st)
#not homogenous
# use non parametric test
wilcox.test(april_cases,may_cases)

install.packages("ggplot2")
library(ggplot2)
covid$COVID.19.Positive...last.24H <- as.numeric(covid$COVID.19.Positive...last.24H)
# Line plot of daily cases over time
covid$Date <- seq.Date(from = as.Date("2020-01-01"), by = "day", length.out = nrow(covid))

library(ggplot2)

ggplot(covid, aes(x = Date, y = Total.Number.Confirmed)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_point(color = "red") +
  labs(title = "COVID-19 Confirmed Cases Over Time",
       x = "Date",
       y = "Total Confirmed Cases") +
  theme_minimal()


plot(covid$COVID.19.Positive...last.24H)
boxplot(covid$COVID.19.Positive...last.24H)
hist(covid$COVID.19.Positive...last.24H,col="red")
str(covid)
summary(covid)

covid$Date <- as.Date(covid$Date, format = "%d-%m-%Y")
covid$Month <- format(covid$Date, "%B")
covid$Year <- format(covid$Date, "%Y")
covid$Month_Year <- paste(covid$Month, covid$Year)

ggplot(covid, aes(x = COVID.19.Positive...last.24H)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "yellow") +
  facet_wrap(~ Month_Year, scales = "free_y") +
  labs(title = "Histogram of Daily COVID-19 Cases by Month",
       x = "Daily New Cases",
       y = "Frequency") +
  theme_minimal()

