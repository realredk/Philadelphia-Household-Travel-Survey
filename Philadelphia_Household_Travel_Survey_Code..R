install.packages("readr")
install.packages(c("dplyr", "knitr", "kableExtra", "ggplot2"))
library(dplyr)
library(knitr)
library(kableExtra)
library(ggplot2)
library(readr)

'''
# Question 1

setwd("Q:/r coding/hw3")
hh_data <- read.csv("1_Household_Public.csv")
sorted_unique_hh_ids <- sort(unique(hh_data$HH_ID))
random_index <- sample(11:20, 1)
selected_hh_id <- sorted_unique_hh_ids[random_index]

print(selected_hh_id)


# Question 3

setwd("Q:/r coding/hw3")
trip <- read.csv("4_Trip_Public.csv")
sumbike <- sum(trip$P_WEIGHT[trip$MODE==2],na.rm = TRUE)
print(sumbike)


# Question 4

setwd("Q:/r coding/hw3")
hh_data <- read.csv("1_Household_Public.csv")
table1 <- table(trip$MODE[hh_data$H_COUNTY==34005])
nonmotorvehicle <- sum(table1[1:4])
privatevehicle <- sum(table1[5:10])
bus <- sum(table1[11:20]) - table1[12]
ferry <- sum(table1[29:30])
railway <- sum(table1[21:28])  # take subway as railway
plane <- table1[12]

new_database <- data.frame(
  nonmotorvehicle = c(nonmotorvehicle),
  privatevehicle = c(privatevehicle),
  bus = c(bus),
  plane = c(plane),
  railway = c(railway),
  ferry = c(ferry)
)

write.csv(new_database, file = "q4.csv", row.names = FALSE)


# Question 5

setwd("Q:/r coding/hw3")
hh_data <- read.csv("1_Household_Public.csv")
veh_data <- read.csv("3_Vehicle_Public.csv")
hhve <- merge(hh_data[c(2,28)],veh_data[c(2,9)], by = "HH_ID")
hhve$AGE <- 2023 - hhve$YEAR
hhve_filtered <- hhve[!(hhve$INCOME %in% c(98, 99)), ]
boxplot(AGE~INCOME,
        data=hhve_filtered,
        outline= FALSE,
        main="Household Income and Age of Household Vehicle",
        xlab="Income Type",
        ylab="Age",
        col="#003366",
        border="#7D7D7D")


# Question 6

setwd("Q:/r coding/hw3")
hh_data <- read.csv("1_Household_Public.csv")
veh_data <- read.csv("3_Vehicle_Public.csv")
trip_data <- read.csv("4_Trip_Public.csv")

Mode <- trip_data[c(2, 38)]
Income <- hh_data[c(2, 28)]

Income_Mode <- merge(Mode, Income, by = "HH_ID") %>%
  filter(INCOME < 11) %>%
  filter(!is.na(MODE_AGG))

mode_income_table <- table(Income_Mode$MODE_AGG, Income_Mode$INCOME)
mode_income_df <- as.data.frame.matrix(mode_income_table)

colnames(mode_income_df) <- c("$0 to $9,999","$10,000 to $24,999","$25,000 to $34,999","$35,000 to $49,999","$50,000 to $74,999","$75,000 to $99,999","$100,000 to $149,999","$150,000 to $199,999","$200,000 to $249,999","$250,000 or more")
rownames(mode_income_df) <- c("Walk","Bike","Private Vehicle","Private Transit","Public Transit","School Bus","Other")

custom_palette <- c("#FF6B6B", "#FFD464", "#FFEC8B", "#3DCD58", "#7ECEF4","#6A8EF2", "#A27EE6", "#B364E1", "#570084", "#A0522D")

# Stacked Bar Plot
ggplot(Income_Mode, aes(x = factor(MODE_AGG), fill = factor(INCOME))) +
  geom_bar(position = "stack") +
  labs(title = "Relationship between Household Income and Mode Choice", x = "Mode", y = "Count", fill = "Income") +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

# Violin Plot
ggplot(Income_Mode, aes(x = factor(MODE_AGG), y = as.numeric(INCOME), fill = factor(INCOME))) +
  geom_violin(scale = "width", trim = FALSE) +
  geom_boxplot(width = 0.1, fill = "white") +
  labs(title = "Violin Plot of Household Income by Mode Choice", x = "Mode", y = "Income Level", fill = "Income") +
  scale_fill_manual(values = custom_palette) +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_discrete(labels = c("Walk","Bike","Private Vehicle","Private Transit","Public Transit","School Bus","Other"))


'''
# Question 7
#a
beta_jobs <- 0.179
additional_boardings_per_job <- beta_jobs
print(additional_boardings_per_job)

#b
average_boardings <- 1793
average_jobs <- 3130
elasticity <- beta_jobs * (average_jobs / average_boardings)
print(elasticity * 100) 

#c
beta_am_peak = 105.822
additional_boardings_per_am_peak = beta_am_peak