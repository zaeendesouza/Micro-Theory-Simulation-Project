#------------------------------------------------------------------------------#
# Subject: Advanced Micro Theory
# Assignment: Preliminary code for simulation (Work in Progress)
# Topic: Spread of Infectious Disease
# Name: Zaeen de Souza
# Date: 04-06-2021


# Things to work on still:
# 1. Changing states, I haven't figured out how to get only some people to die
# 2. Adding recoveries and re-infections
# 3. Adding some kind of vaccine after some point in time - make it harder
# to get infected
# 4. Converting it all to a function - so user can specify parameter values
#------------------------------------------------------------------------------#





#------------------------------------Setup part 0------------------------------#
# This clears the current environment - save before running this!
rm(list = ls())
#------------------------------------------------------------------------------#



# Setup pt.1: Will take just 3-5 mins to install - just needed once. Can comment out after installing.


# install.packages("tidyr", dependencies = T)
# install.packages("ggplot", dependencies = T)
# install.packages("dplyr", dependencies = T)
# install.packages("ggthemes", dependencies = T)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)
library(ggplot2)

# Setup pt.2
# This code defines the characteristics of the 1st agent, who has some sickness,and also sets the population size, and then adds the exposed agent to the broader population.

# Note:
# ID = index of the ith agent
# State = Infection Status (Susceptible = S, Exposed = E, Infected = I, Recovered = R, Dead/Dying = D)
# Meeting = P(Meeting other agents) drawn from a uniform distribution [0,1]
# TimeE = Time exposed - we will use this later on for determining who gets infected, who recovers, and who dies

Population <- 1000

Agent1 <- data.frame(
  ID = 1,
  State = "E",
  Meeting = runif(1, 0, 1),
  TimeE = 1,
  stringsAsFactors = F
)

# Making the rest of the population - already defined agent 1, so will go from i=2..., Population
for (i in 2:Population) {
  Agent2 <- data.frame(
    ID = i,
    State = "S",
    Meeting = runif(1, 0, 1),
    TimeE = 0,
    stringsAsFactors = F)
  Agent1 <- rbind(Agent1, Agent2)
}

# This sets the number of time periods - and then populates a dataframe with Time number of rows - each row is one time period.
Time <- 50
Output <- data.frame(
  S = rep(0, Time),
  E = rep(0, Time),
  I = rep(0, Time),
  R = rep(0, Time),
  D = rep(0, Time)
)


# Here, it loops across time, using the parameter values - Meeting and Population, to look at how agents meet, how many they meet, and how/whether or not their state changes
# if they come in contact with an exposed person.

# Note:
# There is a lot going on in this chunk - basically, it calculates how many other people they will meet - this is the parameter value * P(Meeting Others) + 1, where the + 1 avoids zeros - I don't want those. I also add a decision rule, where i randomize within a cut-off, which determines whether your state changes or not - this is a resistance type of parameter. The cut-off is a parameter we set - hardcoded at 0.2 for now.

for (k in 1:Time) {
  for (i in 1:Population) {
    Mix1 <- Agent1$Meeting[i]
    Meet1 <- round(Mix1 * 5, 0) + 1
    Meet2 <-
      sample(1:Population,
             Meet1,
             replace = TRUE,
             prob = Agent1$Meeting)
    for (j in 1:length(Meet2)) {
      Meet1a <- Agent1[Meet2[j],]
      if (Meet1a$State == "E") {
        Resistance1 <- runif(1, 0, 1)
        if (Resistance1 < 0.2) {
          Agent1$State[i] = "E"
        }
      }
    }
  }
  
  # I use another set of parameters here - a cut off for days exposed and transition to recovery
  StateE1 <- (1:Population)[Agent1$State == "E"]
  Agent1$TimeE[StateE1] = Agent1$TimeE[StateE1] + 1
  StateE2 <- (1:Population)[Agent1$State == "E" & Agent1$TimeE > 16]
  Agent1$State[StateE2] <- "R"
  
  # Indexing and adding those who COULD potentially fall sick. 0.2 is parameter for daily infection rate (its set high for now)
  StateE3 <- (1:Population)[Agent1$State == "E" & Agent1$TimeE > 5]
  
  for (i in StateE3) {
    # Randomising whether or not you get sick if you are exposed
    Resistance2 <- runif(1, 0, 1)
    if (Resistance2 < 0.1) {
      Agent1$State[i] <- "I"
    }
  }
  
  Output$S[k] <- length(Agent1$State[Agent1$State == "S"])
  Output$E[k] <- length(Agent1$State[Agent1$State == "E"])
  Output$I[k] <- length(Agent1$State[Agent1$State == "I"])
  Output$R[k] <- length(Agent1$State[Agent1$State == "R"])
  Output$D[k] <- length(Agent1$State[Agent1$State == "D"])
}




# Plotting the output to see what it looks like. There are a few housekeeping commands before that - we will need to run those to make the plot.

Output$Time <- seq(1:Time)

# Renaming columns to actual names
Output <- Output %>%
  rename(
    Exposed = E,
    Susceptible = S,
    Infected = I,
    Recovered = R)


# Reshaping the data, and ordering it via the factor function, so its easier to plot.
plot_data <- Output %>%
  select(Time, Susceptible, Exposed, Infected, Recovered) %>%
  gather(key = "Parameter", value = "Value", -Time)

plot_data$Parameter <- factor(plot_data$Parameter,
                              levels = c("Susceptible",
                                         "Exposed",
                                         "Infected",
                                         "Recovered"))

# Theme
my_theme <- function(base_size = 12,
                     base_family = "") {
  theme_minimal(base_size = base_size,
                base_family = base_family) %+replace%
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y  =  element_line(
        colour = "#c9c0b7",
        linetype = "dotted",
        size = .5),
      axis.ticks = element_line(size = .5, colour = "#c9c0b7"),
      axis.ticks.x = element_blank(),
      axis.line = element_line(
        size = .5,
        colour = "#c9c0b7",
        linetype = "solid"),
      axis.line.y = element_blank(),
      axis.text.y = element_text(colour = "black", 
                                 margin = margin(r = 2), hjust = 1),
      axis.text.x = element_text(colour = "black"),
      # we done here
      complete = TRUE)
}


ggplot(data = plot_data,
       aes(x = Time,
           y = Value)) +
  geom_line(size = 1.5,
            colour = "#e64173") +
  facet_grid( ~ Parameter) +
  my_theme(base_size = 15) +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = c(0, 0), limits = c(0,1000)) +
  xlab("Number of Days") +
  ylab("Number of People") +
  labs(
    title = "Simulating the Spread of Infectious Disease",
    subtitle = paste("Time Periods =", Time, "\nPopulation Size =", Population),
    caption = "Zaeen de Souza")

ggsave("plot1.pdf", width = 10, height = 8)
