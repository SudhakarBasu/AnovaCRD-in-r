install.packages("agricolae")
library(agricolae)

# Set working directory
setwd("C:/Users/user/Desktop/github/Anova CRD")

# Read the CSV file
CRD <- read.csv("CRD.csv")

# Display the first few rows of the dataset
head(CRD)

# Display the entire dataset
CRD

# Check the structure of the dataset
str(CRD)

# Check the dimensions of the dataset
dim(CRD)

# Check the class of the dataset
class(CRD)
mod1 <- lm(CRD$yield~CRD$treatment)

summary <- summary(mod1)

anova <- anova(mod1)

anova

par(mfrow=c(1,2))
plot(mod1, which =1)
plot(mod1, which =2)

###LSD test

lsd <- LSD.test(CRD$yield, CRD$treatment, 30, 0.8284)
lsd


msd <- lsd$means[,1:2]
ag <- lsd$groups
ag1 <- ag[order(row.names(ag)),]
dt <- data.frame(Trt = row.names(ag1), Avg = msd$`CRD$yield`, Sdv = msd$std, gr = ag1$groups)
dt


library(ggplot2)
ggplot(dt, aes(x = Trt, y = Avg)) +
  geom_bar(stat = "identity", aes(fill = Trt),width = 0.5) +
  geom_errorbar(aes(ymin = Avg - Sdv, ymax = Avg + Sdv), width = 0.1) +
  geom_text(aes(label = gr, y = Avg + Sdv), vjust = -0.5) + theme_classic()

###box plot######
p <- ggplot (CRD, aes(x = reorder(treatment, yield, FUN = median), y = yield, fill = treatment))  + 
  geom_boxplot() +
  stat_boxplot(geom ='errorbar', width = 0.2) +
  theme_classic() +
  labs(title = "Box plot")
print(p)

# Save the results in a text file
sink("CRD.txt")
print(anova)
print("LSD Result")
print(lsd$statistics)
print(lsd$groups)
sink()
