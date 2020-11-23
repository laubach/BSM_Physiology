# Molly McDermott
# edited 11/2/20
# Analysis of BSM field experiment - female physiology

#load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(car)

#read in data files
phys <- read.csv("BSM_Female_Phys_2019-2020.csv")
phys$Band <- as.factor(phys$Band)
phys$Date <- as.Date(phys$Date, "%m/%d/%Y")

HL <- read.csv("Blood slide CO 2019-2020.csv")
HL$Band.number <- as.factor(HL$Band.number)
HL$Date <- as.Date(HL$Date, "%m/%d/%Y")

df <- phys %>%
  left_join(HL, by = c("Band" = "Band.number", "Date"))


#convert from narrow to wide format
phys_wide <- df %>%
  mutate(capture = ifelse(ChickAge < 8, 1, 2)) %>%
  pivot_wider(id_cols = c(Site, Nest, Band, Tagged, BroodTrt), 
              names_from = capture,
              values_from = c(Date, Time, BroodSize, ChickAge, BloodGluc, 
                              R1_coll, R1_date, R1_mm, Mass, H.L.ratio))

#write.csv(phys_wide, "BSM_Female_Phys_wide_2019-2020.csv")
#phys_wide <- read.csv("BSM_Female_Phys_wide_2019-2020.csv")

#### EXPLORATORY ANALYSIS ####

#histogram of feather regrowth corrected by days between collection and measurement, excluding birds where feathers were not collected or where feathers were collected prior to first measurement
phys_wide$Days <- as.numeric(format(phys_wide$Date_2, "%j")) - as.numeric(format(phys_wide$Date_1, "%j"))

summary(phys_wide$R1_mm_2)
phys_wide$R1_mm_2[phys_wide$R1_coll_1 != "Y"] <- NA
summary(phys_wide$R1_mm_2)

phys_wide$R1_mm_day <- phys_wide$R1_mm_2/phys_wide$Days
hist(phys_wide$R1_mm_day, xlab = "R1 regrowth per day (mm)")

library(chron)
df$Time <- times(paste0(df$Time, ":00"))
plot(df$Time, df$BloodGluc, xlab = "Time (fraction of day)", ylab = "Blood Glucose (ug/ml)")
phys_wide$Gluc_diff <- phys_wide$BloodGluc_2 - phys_wide$BloodGluc_1
hist(phys_wide$Gluc_diff, xlab = "Difference in blood glucose (ug/ml)")

phys_wide$Mass_diff <- phys_wide$Mass_2 - phys_wide$Mass_1
hist(phys_wide$Mass_diff, xlab = "Mass difference between second and first capture (g)")

phys_wide$HL_diff <- phys_wide$H.L.ratio_2 - phys_wide$H.L.ratio_1
hist(phys_wide$HL_diff, xlab = "Difference in H:L ratio between second and first capture")


#correlation matrices
library(corrplot)
C <- phys_wide %>%
  select(R1_mm_day, Gluc_diff, Mass_diff, HL_diff)

corrplot(cor(C, use = "complete.obs"))
plot(phys_wide$Gluc_diff ~ phys_wide$Mass_diff, xlab = )


#boxplots
phys_wide <- phys_wide[-26,]
phys_wide$trt <- as.factor(paste(phys_wide$Tagged, phys_wide$BroodTrt, sep = "_"))
summary(phys_wide$trt)
#drop bird that was tagged but not part of BSM

plot(phys_wide$trt, phys_wide$Mass_diff, xlab = "Treatment", ylab = "Difference in mass (g)")
plot(phys_wide$trt, phys_wide$HL_diff, xlab = "Treatment", ylab = "Difference in H:L ratio")
plot(phys_wide$trt, phys_wide$R1_mm_day, xlab = "Treatment", ylab = "R1 regrowth per day (mm)")
plot(phys_wide$trt, phys_wide$Gluc_diff, xlab = "Treatment", ylab = "Difference in blood glucose (ug/ml)")

# reaction norm plots
df$Trt_Stage <- "Before"
df$Trt_Stage[df$ChickAge > 8] <- "After"
df$Trt_Stage <- as.factor(df$Trt_Stage)
levels(df$Trt_Stage)
df$Trt_Stage <- factor(df$Trt_Stage, levels = c("Before", "After"))
levels(df$Trt_Stage)

df <- df[-49,]
df$trt <- as.factor(paste(df$Tagged, df$BroodTrt, sep = "_"))
summary(df$trt)

#mass ind var
ggplot(data=df, aes(x=Trt_Stage, y=Mass, group=Band, color = trt)) +
  geom_line(size=1, aes(linetype = trt)) +
  geom_point() +
  ylab("Mass(g)") +
  xlab("") +
  theme_bw()

#HL ind var
ggplot(data=df, aes(x=Trt_Stage, y=H.L.ratio, group=Band, color = trt)) +
  geom_line(size=1, aes(linetype = trt)) +
  geom_point() +
  ylab("H:L ratio") +
  xlab("") +
  theme_bw()

#Glucose ind var
ggplot(data=df, aes(x=Trt_Stage, y=BloodGluc, group=Band, color = trt)) +
  geom_line(size=1, aes(linetype = trt)) +
  geom_point() +
  ylab("Blood Glucose (ug/mL)") +
  xlab("") +
  theme_bw()

#### LINEAR MODELS ####


