library(tidyverse)
library(readxl)
library(janitor)




# INPUTS ------------------------------------------------------------------
# example data
sr_m <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 2)
sr_f <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 3)
asfr <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 4)
pop_m <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 5)
pop_f <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 6)

pop_female <- pop_f
pop_male <- pop_m
survival_ratio_male <- sr_m
survival_ratio_female <- sr_f

mig_m <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 7)
mig_f <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 8)
mig_mL <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 9)
mig_mU <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 10)
mig_fL <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 11)
mig_fU <- read_xlsx("Li_2010_WPP_BY1950_MIG_Canada.xlsx", sheet = 12)

# pull out srb
srb <- asfr %>% filter(Age == "SRB") %>% select(-Age)
asfr <- asfr %>% filter(Age != "SRB")
srb <- srb %>% 
  pivot_longer(everything(), names_to = "year", values_to = "srb")

age_min <- 0
age_max <- 100
age_interval <- 5
year_min <- 1950
year_max <- 2050
year_interval <- 5



# DEFINE AGES AND YEARS ---------------------------------------------------

ages <- seq(age_min, age_max, by = age_interval)
years <- seq(year_min, year_max, by = year_interval)

age_labels <- c(paste(ages[1:(length(ages)-1)], ages[2:length(ages)]-1, sep = "-"), paste0(age_max, "+"))
year_labels <- paste(years[1:(length(years)-1)], years[2:length(years)], sep = "-")

ages_fertility <- seq(15, 45, by = age_interval)
age_labels_fertility <- c(paste(ages_fertility[1:(length(ages_fertility)-1)], 
                                ages_fertility[2:length(ages_fertility)]-1, sep = "-"),
                          paste0(ages_fertility[length(ages_fertility)], "-49"))


# GET IN TIDY FORM --------------------------------------------------------


# assume inputs are of the excel form but we need to make it long and put it all together

colnames(pop_m)[1] <- "age"
colnames(pop_f)[1] <- "age"

df_pop <- pop_m %>% 
  mutate(age = ages) %>% 
  pivot_longer(-age, names_to = "year", values_to = "population") %>% 
  mutate(sex = "m") %>% 
  bind_rows(pop_f %>% 
              mutate(age = ages) %>% 
              pivot_longer(-age, names_to = "year", values_to = "population") %>% 
              mutate(sex = "f")) %>% 
  mutate(year = as.numeric(year))

colnames(sr_m)[1] <- "age"
colnames(sr_f)[1] <- "age"


df_sr <- sr_m %>% 
  mutate(age = ages) %>% 
  pivot_longer(-age, names_to = "year_original", values_to = "sr") %>% 
  mutate(sex = "m") %>% 
  bind_rows(sr_f %>% 
              mutate(age = ages) %>% 
              pivot_longer(-age, names_to = "year_original", values_to = "sr") %>% 
              mutate(sex = "f")) %>% 
  rowwise() %>% 
  mutate(year = as.numeric(years[which(year_labels==year_original)])) %>% 
  select(age, year, sr, sex)

colnames(asfr)[1] <- "age"
df_asfr <- asfr %>% 
  mutate(age = ages_fertility) %>% 
  pivot_longer(-age, names_to = "year_original", values_to = "asfr") %>% 
  rowwise() %>% 
  mutate(year = as.numeric(years[which(year_labels==year_original)])) %>% 
  select(age, year, asfr)

df_srb <- srb %>% 
  pivot_longer(everything(),names_to = "year_original", values_to = "srb") %>% 
  rowwise() %>% 
  mutate(year = as.numeric(years[which(year_labels==year_original)])) %>% 
  select(year, srb)


# Calculate survivors -----------------------------------------------------

# same as below
test <- df_pop %>% 
  left_join(df_sr) %>% 
  mutate(cohort = year - age) %>% 
  arrange(sex, year, age) %>% 
  mutate(implied_survivors = population*lead(sr)) %>% 
  group_by(sex, cohort) %>% 
  arrange(sex, cohort, age) %>% 
  mutate(diff = population - lag(implied_survivors)) %>% 
  arrange(year, sex, age) 



# Replicate CohortMigFlow -------------------------------------------------


# try copying vba code

pop_m_mat <- as.matrix(pop_m %>% select(-age))
pop_f_mat <- as.matrix(pop_f %>% select(-age))
sr_m_mat <- as.matrix(sr_m %>% select(-age))
sr_f_mat <- as.matrix(sr_f %>% select(-age))
asfr_mat <- as.matrix(asfr %>% select(-age))
srb_mat <- as.matrix(srb)


nages <- length(ages)
nyears <- length(years)

net_mig_m <- matrix(NA, nrow = nages, ncol = nyears)
net_mig_f <- matrix(NA, nrow = nages, ncol = nyears)

for(i in 2:(nages-1)){
  for(j in 2:(nyears)){
    net_mig_m[i,j] <- pop_m_mat[i, j] -  pop_m_mat[i-1, j-1]*sr_m_mat[i,j-1]
    net_mig_f[i,j] <- pop_f_mat[i, j] -  pop_f_mat[i-1, j-1]*sr_f_mat[i,j-1]
  }
}

# last age group
for(j in 2:nyears){
  net_mig_m[nages,j] <- pop_m_mat[nages, j] -  (pop_m_mat[nages, j-1] + pop_m_mat[nages-1, j-1])*sr_m_mat[nages,j-1]
  net_mig_f[nages,j] <- pop_f_mat[nages, j] -  (pop_f_mat[nages, j-1] + pop_f_mat[nages-1, j-1])*sr_f_mat[nages,j-1]
}

# births
all_births <- rep(NA, length(years))
fertility_index <- which(ages %in% ages_fertility)
for(j in 2:nyears){
  births_this_year <- 0
  for(i in fertility_index){
    print(i)
    these_births <- age_interval*(0.5*(pop_f_mat[i,j-1]+pop_f_mat[i,j])*asfr_mat[i-(min(fertility_index)-1), j-1])/1000
    births_this_year <- births_this_year+these_births
    print(births_this_year)
  }
  all_births[j] <- births_this_year
}

births_m <- all_births[2:length(all_births)]*(srb_mat/(1+srb_mat))
births_f <- all_births[2:length(all_births)]*(1/(1+srb_mat))

for(j in 2:nyears){
  net_mig_m[1,j] <- pop_m_mat[1, j] - births_m[j-1]*sr_m_mat[1,j-1]
  net_mig_f[1,j] <- pop_f_mat[1, j] - births_f[j-1]*sr_f_mat[1,j-1]
}

mig_upper_m <- matrix(NA, nrow = nages, ncol = nyears)
mig_lower_m <- matrix(NA, nrow = nages, ncol = nyears)
mig_rectangle_m <- matrix(NA, nrow = nages, ncol = nyears)

mig_upper_f <- matrix(NA, nrow = nages, ncol = nyears)
mig_lower_f <- matrix(NA, nrow = nages, ncol = nyears)
mig_rectangle_f <- matrix(NA, nrow = nages, ncol = nyears)

for(i in 2:(nages-1)){
  for(j in 2:(nyears)){
    
    mig_upper_m[i,j] <- net_mig_m[i,j]/(2*sr_m_mat[i,j-1]^0.5)
    #mig_lower_m[i-1,j] <- mig_upper_m[i,j]
    mig_lower_m[i-1,j] <- net_mig_m[i,j] - mig_upper_m[i,j]
    
    mig_upper_f[i,j] <- net_mig_f[i,j]/(2*sr_f_mat[i,j-1]^0.5)
    #mig_lower_f[i-1,j] <- mig_upper_f[i,j]
    mig_lower_f[i-1,j] <- net_mig_f[i,j] - mig_upper_f[i,j]
    
  }
}

# first age group
for(j in 2:(nyears)){
  mig_upper_m[1,j] <- net_mig_m[1,j]/(sr_m_mat[1,j-1]^0.5)
  mig_upper_f[1,j] <- net_mig_f[1,j]/(sr_f_mat[1,j-1]^0.5)
}

# last age group
for(j in 2:(nyears)){
  mig_lower_m[nages-1,j] <- mig_upper_m[nages-1,j]
  mig_lower_f[nages-1,j] <- mig_upper_f[nages-1,j]
  
  mig_upper_m[nages,j] <- net_mig_m[nages,j]*0.5
  mig_upper_f[nages,j] <- net_mig_f[nages,j]*0.5
  mig_lower_m[nages,j] <- net_mig_m[nages,j]*0.5
  mig_lower_f[nages,j] <- net_mig_f[nages,j]*0.5
}

for(i in 1:nages){
  for(j in 1:nyears){
    mig_rectangle_m[i,j] <- mig_upper_m[i,j] + mig_lower_m[i,j]
    mig_rectangle_f[i,j] <- mig_upper_f[i,j] + mig_lower_f[i,j]
  }
}


# compare -----------------------------------------------------------------

round(as.matrix(mig_mL %>% select(-Age)) - mig_lower_m[,2:nyears], 1)
round(as.matrix(mig_mU %>% select(-Age)) - mig_upper_m[,2:nyears], 2)
round(as.matrix(mig_m %>% select(-Age)) - mig_rectangle_m[,2:nyears], 2)

round(as.matrix(mig_fL %>% select(-Age)) - mig_lower_f[,2:nyears], 2)
round(as.matrix(mig_fU %>% select(-Age)) - mig_upper_f[,2:nyears], 2)
round(as.matrix(mig_f %>% select(-Age)) - mig_rectangle_f[,2:nyears], 2)

mig_m %>% 
  pivot_longer(-Age, names_to = "year_original", values_to = "mig") %>% 
  rowwise() %>% 
  mutate(year = as.numeric(years[which(year_labels==year_original)]),
         age = as.numeric(ages[which(age_labels==Age)])) %>% 
  select(age, year, mig) %>% 
  mutate(cohort = year - age) %>% 
  ggplot(aes(age, mig, color = factor(cohort))) + geom_line() 
  

plot(mig_rectangle_m[2,2:20], type = "o")
lines(as.matrix(mig_m[,-1])[2,2:20] , col = 2)


plot(mig_rectangle_m[2:20,3], type = "o")
lines(as.matrix(mig_m[,-1])[2:20,2] , col = 2)
