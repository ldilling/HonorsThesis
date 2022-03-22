library(xtable)
library(eRm)
library(difR)
library(ltm)
library(lme4)
library(MCMCpack)
library(ggplot2)
library(brms)
library(arm)
library(tidyverse)
library(hrbrthemes)
library(moderndive)
police<-read.csv('~/Dropbox (University of Michigan)/Honors Thesis/Police Contracts R.csv')
teacher<-read.csv('~/Dropbox (University of Michigan)/Honors Thesis/Teacher Contracts R.csv')

#Police Summary Statistics
mean(police$LEBOR)
mean(police$AFL.CIO)
mean(police$FOP)
mean(police$NAPA)

#Teacher Summary Statistics
mean(teacher$AFL.CIO)
mean(teacher$NEA)
mean(teacher$NEA & teacher$AFL.CIO)

#Create Item Time Map
#All Items same time period, so it is a vector of time one for each column
police_time_map <- matrix(1,nrow = 1, ncol = 192)
teacher_time_map <- matrix(1,nrow = 1,ncol = 141)

#Subseting Dataframes as the IRT function only takes numeric variables
#Remove City, State, Union Name, and Federation Name Details
subset_police <-subset(police, select= -c(Department.ID, State, Union, Labor.Federation))

subset_teacher <- subset(teacher, select = -c(X, State, Union, Federation))

#Simple IRT Model for Contract Provision Analysis
?MCMCdynamicIRT1d

police_out <-MCMCdynamicIRT1d(subset_police, police_time_map, burnin = 1000, mcmc = 20000, thin = 1 )

teacher_out <-MCMCdynamicIRT1d(subset_teacher, teacher_time_map, burnin = 1000, mcmc = 20000, thin = 1)

names(attributes(police_out))
police_out_sum<-summary(police_out)
names(police_out_sum) 

class(police_out_sum$statistics)
dim(police_out_sum$statistics)
head(police_out_sum$statistics)
head(police_out_sum$quantiles)
rownames(police_out_sum$statistics)


police_out_sum$names <-police$Department.ID
police_out_sum$names

#Creating Objects for Police Values
police_data <-as_tibble(police_out_sum$statistics, rownames='var')
head(police_data)
police_data<- police_data %>%
  mutate(
    var_class = ifelse(
      grepl("theta",var), 
      'theta', 
      ifelse(
        grepl('alpha',var),
        'alpha',
        ifelse(
          grepl('beta', var),
          'beta',
          ifelse(
            grepl('tau',var), 
            'tau', 
            NA
          )))))
length(police$Department.ID)
police_data %>%
  group_by(var_class) %>%
  summarise(count = n())

#Create Police Theta and Tau Frame
theta_tau = police_data %>%
  filter(
    var_class %in% c( 'theta' , 'tau'
    )
  )

#Create Police Alpha and Beta Frame
alpha_beta = police_data %>%
  filter(
    var_class %in% c( 'alpha', 'beta'
    )
  )

#Creating Tibble for Teacher Values
teacher_out_sum <- summary(teacher_out)
teacher_out_sum$district <- teacher$X
teacher_data <-as_tibble(teacher_out_sum$statistics, rownames = 'var')

teacher_data<- teacher_data %>%
  mutate(
    var_class = ifelse(
      grepl("theta",var), 
      'theta', 
      ifelse(
        grepl('alpha',var),
        'alpha',
        ifelse(
          grepl('beta', var),
          'beta',
          ifelse(
            grepl('tau',var), 
            'tau', 
            NA
          )))))
teacher_data %>%
  group_by(var_class) %>%
  summarise(count = n())

#Create Teacher Theta and Tau 
teacher_theta_tau = teacher_data %>%
  filter(
    var_class %in% c( 'theta' , 'tau'
    )
  )

#Create Teacher Alpha and Beta
teacher_alpha_beta = teacher_data %>%
  filter(
    var_class %in% c( 'alpha', 'beta')
  )

#Adding Department Id to the Thetas and Taus
theta_tau <- theta_tau %>%
  mutate(
    Department = rep(police$Department.ID,2)
  )


#Add District to Teacher Theta Tau and Provisions to Alphas and Betas
teacher_theta_tau <- teacher_theta_tau %>%
  mutate(
    district = rep(teacher$X,2)
  )

T_Provisions <-names(subset_teacher)
teacher_alpha_beta <- teacher_alpha_beta %>%
  mutate(
    t_provisions = rep(T_Provisions,2)
  )

#Ordering by Police Theta value
theta_tau = theta_tau %>%
  arrange(
    Mean
  ) %>%
  mutate(
    Department_2 = factor(Department,unique(Department))
  )
#Standard Error For Thetas
theta_tau = theta_tau %>%
  mutate(
    lower = Mean - 1.96*`Time-series SE`,
    upper = Mean + 1.96*`Time-series SE`
  )

#Create Confidence Limit for Thetas and Taus
theta_tau = theta_tau %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', lower, upper
    )
  )

#Plot Theta Means/Values by Department make a table of the theta for each dept
theta_tau %>%
  filter(var_class == 'theta') %>%
  ggplot(aes(x=Mean, y=Department_2))+
  geom_point(color = '#00C49A')+
  geom_errorbarh(aes(xmin=lower, xmax=upper,))+
  ylab('Department')+
  xlab("Theta")+
  ggtitle('Mean Police Latent Union Strength Scores')

#Table for Police Theta Values
theta_table <- theta_tau %>%
  filter(var_class == 'theta') %>%
  select(Department, Theta = Mean, SE = `Time-series SE`, CI)

xtable(theta_table, digits = 3)

#Add Provision Names Back onto Alphas and Betas
Provisions <-names(subset_police)
alpha_beta <- alpha_beta %>%
  mutate(
    provisions = rep(Provisions,2)
  )

#Order Provisions by Highest Alpha Value
alpha_beta = alpha_beta %>%
  arrange(
    Mean
  ) %>%
  mutate(
  provisions_2 = factor(provisions, unique(provisions))
)

#Create Standard Errors and Confidence Levels for Alphas
alpha_beta = alpha_beta %>%
  mutate(
    alpha_lower = Mean - 1.96*`Time-series SE`,
    alpha_upper = Mean + 1.96*`Time-series SE`
  )

alpha_beta = alpha_beta %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', alpha_lower, alpha_upper
    )
  )

#Plot Alpha Values for their discriminatory power for the provisions
alpha_beta %>%
  filter(var_class == 'alpha') %>%
  ggplot(aes(x=Mean, y=provisions_2))+
  geom_point()+
  geom_errorbarh(aes(xmin=alpha_lower, xmax=alpha_upper))

#Table of Alpha Values
police_alpha_table <- alpha_beta %>%
  filter(var_class == 'alpha') %>%
  select(Provisions = provisions_2, Alpha = Mean, SE = `Time-series SE`, CI)

xtable(police_alpha_table, digits = 3)

#Create SE and CI for Betas
alpha_beta = alpha_beta %>%
  mutate(
    beta_lower = Mean - 1.96*`Time-series SE`,
    beta_upper = Mean + 1.96*`Time-series SE`
  )

alpha_beta = alpha_beta %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', beta_lower, beta_upper
    )
  )

#Plot for Beta values for each police department
alpha_beta %>%
  filter(var_class == 'beta') %>%
  ggplot(aes(x=Mean, y=provisions_2))+
  geom_point()+
  geom_errorbarh(aes(xmin=beta_lower, xmax= beta_upper))

#Police Beta Tables
police_beta_table <- alpha_beta %>%
  filter(var_class == 'beta') %>%
  select(Provisions = provisions_2, Beta = Mean, SE = `Time-series SE`, CI)

xtable(police_beta_table, digits = 3)

#Create CI for Teacher Alpha, Beta, and Thetas
teacher_alpha_beta = teacher_alpha_beta %>%
  mutate(
    t_alpha_lower = Mean - 1.96*`Time-series SE`,
    t_alpha_upper = Mean + 1.96*`Time-series SE`
  )
teacher_alpha_beta = teacher_alpha_beta %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', t_alpha_lower, t_alpha_upper
    )
  )

teacher_alpha_beta = teacher_alpha_beta %>%
  mutate(
    t_beta_lower = Mean - 1.96*`Time-series SE`,
    t_beta_upper = Mean + 1.96*`Time-series SE`
  )
teacher_alpha_beta = teacher_alpha_beta %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', t_beta_lower, t_beta_upper
    )
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    t_theta_lower = Mean - 1.96*`Time-series SE`,
    t_theta_upper = Mean + 1.96*`Time-series SE`
  )
teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    CI = sprintf(
      '(%5.2f, %5.2f)', t_theta_lower, t_theta_upper
    )
  )

#Order Teacher Data by Theta, Alpha, and Beta
teacher_theta_tau = teacher_theta_tau %>%
  arrange(
    Mean
  ) %>%
  mutate(
    district_2 = factor(district, unique(district))
  )

teacher_alpha_beta = teacher_alpha_beta %>%
  arrange(
    Mean
  ) %>%
  mutate(
    t_provisions_2 = factor(t_provisions, unique(t_provisions)))

#Teacher Theta Plot and Tables
teacher_theta_tau %>%
  filter(var_class == 'theta') %>%
  ggplot(aes(x=Mean, y=district_2))+
  geom_point(color = '#00C49A')+
  geom_errorbarh(aes(xmin = t_theta_lower, xmax = t_theta_upper))+
  ylab('School District')+
  xlab('Theta Value')+
  ggtitle('Mean Teacher Latent Union Strength Scores')


teacher_theta_table <- teacher_theta_tau %>%
  filter(var_class == 'theta') %>%
  select(District = district_2, Theta = Mean, SE = `Time-series SE`, CI)

xtable(teacher_theta_table)

#Teacher Alpha Plot and Table
teacher_alpha_beta %>%
  filter(var_class == 'alpha') %>%
  ggplot(aes(x = Mean, y = t_provisions_2))+
  geom_point()+
  geom_errorbarh(aes(xmin = t_alpha_lower, xmax = t_alpha_upper))+
  ggtitle('Teacher Alpha Means')+ 
  xlab('Alpha Value')+
  ylab('Contract Provision')

teacher_alpha_table <- teacher_alpha_beta %>%
  filter(var_class =='alpha') %>%
  select(Provisions = t_provisions_2, Alpha = Mean, SE = `Time-series SE`, CI)

xtable(teacher_alpha_table)

#Teacher Beta Plot and Table
teacher_alpha_beta %>%
  filter(var_class == 'beta') %>%
  ggplot(aes(x = Mean, y = t_provisions_2))+
  geom_point()+
  geom_errorbarh(aes(xmin = t_beta_lower, xmax = t_beta_upper))+
  ggtitle("Teacher Beta Means")+
  ylab("Contract Provisions")+
  xlab("Beta Value")

teacher_beta_table <- teacher_alpha_beta %>%
  filter(var_class =='beta')%>%
  select(Provisions = t_provisions_2, Beta = Mean, SE = `Time-series SE`, CI)

xtable(teacher_beta_table)

#Add on Labor Federation and State Law Data to the Police and Teacher Theta_Taus
theta_tau = theta_tau %>%
  mutate(
    afl_cio = rep(police$AFL.CIO,2)
  )
theta_tau = theta_tau %>%
  mutate(
    fop = rep(police$FOP,2)
  )

theta_tau = theta_tau %>%
  mutate(
    napo = rep(police$NAPA, 2)
  )

theta_tau = theta_tau %>%
  mutate(
    lebor = rep(police$LEBOR,2)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    afl_cio = rep(teacher$AFL.CIO,2)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    nea = rep(teacher$NEA,2)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    duty_to_bargain = rep(teacher$Districts.Obligated.to.Bargain,2)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    legal_cb = rep(teacher$Legal.Collective.Bargaining)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    legal_strike = rep(teacher$Legal.Strikes,2)
  )

teacher_theta_tau = teacher_theta_tau %>%
  mutate(
    wage_bargaining = rep(teacher$Wage.Bargaining.Permissible, 2)
  )

teacher_theta_tau = teacher_theta_tau %>% 
mutate(
  fringe_bargaining = rep(teacher$Fringe.Benefit.Bargaining.Permissible, 2)
)
#Reorder Police And Techer Theta_Taus Alphabetically
theta_tau %>%
  arrange(Department)

teacher_theta_tau %>%
  arrange(district)
#Creating a Matching Labor Federation Variable
theta_tau = theta_tau %>%
  mutate(
    matching_fed = case_when(afl_cio == 1 & teacher_theta_tau$afl_cio == 1 ~ 1, 
                             afl_cio == 1 & teacher_theta_tau$afl_cio == 0 ~ 0,
                             afl_cio == 0 & teacher_theta_tau$afl_cio == 0 ~ 0,
                             afl_cio == 0 & teacher_theta_tau$afl_cio == 1 ~ 0)
  )


#Pulling Apart Thetas and Taus for Regressions
p_theta = theta_tau %>%
  filter(
    var_class %in% c('theta')
  )

t_theta = teacher_theta_tau %>%
  filter(
    var_class %in% c('theta')
  )

#Ordering both alphabetically
p_theta %>% 
  arrange(Department)

t_theta %>%
  arrange(district)

#Regression of Union Strength on Matching Labor Federation
p_reg <- lm(p_theta$Mean ~ p_theta$matching_fed + p_theta$lebor
            + t_theta$duty_to_bargain + t_theta$legal_strike + 
              t_theta$wage_bargaining)
p_reg

t_reg <- lm(t_theta$Mean ~ p_theta$matching_fed + t_theta$duty_to_bargain 
            + t_theta$legal_strike +  t_theta$wage_bargaining)
t_reg

#Regression of Union Strength on Individual Federation Membership

p_afl_reg <-lm(p_theta$Mean ~ p_theta$afl_cio +p_theta$lebor
               + t_theta$duty_to_bargain + t_theta$legal_strike + 
                 t_theta$wage_bargaining)
p_afl_reg

p_fop_reg <-lm(p_theta$Mean ~ p_theta$fop +p_theta$lebor
               + t_theta$duty_to_bargain + t_theta$legal_strike + 
                 t_theta$wage_bargaining)
p_fop_reg

p_napo_reg <- lm(p_theta$Mean ~ p_theta$napo +p_theta$lebor
                 + t_theta$duty_to_bargain + t_theta$legal_strike + 
                   t_theta$wage_bargaining)
p_napo_reg

t_afl_reg <- lm(t_theta$Mean ~ t_theta$afl_cio +t_theta$duty_to_bargain 
                + t_theta$legal_strike +  t_theta$wage_bargaining)
t_afl_reg

t_nea_reg <-lm(t_theta$Mean ~ t_theta$nea + t_theta$duty_to_bargain 
               + t_theta$legal_strike +  t_theta$wage_bargaining)
t_nea_reg


#Regression Tables for Latex
xtable(p_reg)

xtable(t_reg)

xtable(p_afl_reg)

xtable(p_fop_reg)

xtable(p_napo_reg)

xtable(t_afl_reg)

xtable(t_nea_reg)

#Distribution of Contract Restrictiveness
alpha_beta %>% 
  filter(var_class == 'alpha') %>%
ggplot(aes(x=Mean))+
  geom_density(fill="#69b3a2", color="#e9ecef")+
  xlab('Estimated Provision Difficulty')+
  ylab('Density')+
  ggtitle('Police Contract Provision Item Difficulty')

teacher_alpha_beta %>%
  filter(var_class == 'alpha')%>%
  ggplot(aes(x=Mean))+
  geom_density(fill="#69b3a2", color="#e9ecef")+
  xlab('Estimated Provision Difficulty')+
  ylab('Density')+
  ggtitle('Teacher Contract Provision Item Difficulty')

#Distribution of Item Discrimination 

alpha_beta %>%
  filter(var_class == 'beta')%>%
  ggplot(aes(x=Mean))+
  geom_density(fill="#69b3a2", color="#e9ecef")+
  xlab('Estimated Provision Prevalence')+
  ylab('Density')+
  ggtitle('Police Contract Provision Discrimination')

teacher_alpha_beta %>%
  filter(var_class == 'beta')%>%
  ggplot(aes(x=Mean))+
  geom_density(fill="#69b3a2", color="#e9ecef")+
  xlab('Estimated Provision Prevalence')+
  ylab('Density')+
  ggtitle('Teacher Contract Provision Discrimination')
  