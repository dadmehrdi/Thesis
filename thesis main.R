
# Load required packages using p_load function
pacman::p_load(ggplot2, ipumsr, dplyr, MASS, plotly, ggeasy, stargazer, usethis, tictoc, pryr, smoothie, Gini, dineq, sjlabelled, expss, readxl, broom, GGally, coefplot, dotwhisker, labelled, broom.helpers, ivreg, arrow, fields, labelled, olsrr, dgumbel, gridExtra, ggstance, gumbel, tidyverse, data.table, lubridate, purrr, stringr, ggplot2, tidyr, dplyr, readr, plotly, broom, gridExtra, ggpubr, forcats, knitr, RColorBrewer, rmarkdown, shiny, shinydashboard, devtools, testthat, stringi, scales, magrittr, ggthemes, broom.helpers, patchwork, cowplot, reshape2, ggsci, tibble, dtplyr, ggrepel, pander, ggpmisc, foreach, pheatmap, kableExtra, leaflet, ggmap, rvest, jsonlite, XML, ggforce, glue, hrbrthemes, flexdashboard, lattice, nycflights13, igraph, DoseFinding, effectsize, epiR, graphlayouts, Guerry, lava, maxLik, plotrix, projpred, psychotools, RNetCDF, ROCR, rpact, sn, coronavirus, rmarkdown, tidyverse, ggthemes, plotly, xfun, magick, jsonlite, magrittr, tidycensus, rvest, httr, tidygraph, igraph, ggraph, gapminder, leaflet, gssr, gtrendsR, ggplot2, PWFSLSmoke, AirSensor, AirMonitorPlots, MazamaSpatialUtils, lubridate, VennDiagram, janitor, skimr, DataExplorer, stringr, nycflights13, pandoc)


setwd("/Users/dadmehr/R")
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")
ddi <- read_ipums_ddi("usa_00037.xml")
main_data <- read_ipums_micro(ddi)


m_main_data <- subset(main_data, INCTOT !=9999999 & MARST %in% c(1,2) & MARRINYR != 2 & YEAR==2021)
jm_main_data <- subset(main_data, INCTOT !=9999999 & MARRINYR == 2 & YEAR==2021)
co_main_data <- subset(main_data, INCTOT !=9999999 & COUPLETYPE == 3 & MARST == 6 & YEAR==2021)
co_2021 <- subset(main_data, INCTOT !=9999999 & YEAR==2021 & COUPLETYPE == 3 & MARST == 6)

main_data <- main_data %>% mutate(mom = case_when(MOMLOC > 0 ~ 1, !MOMLOC > 0 ~ 0))
main_data <- main_data %>% mutate(dad = case_when(POPLOC > 0 ~ 1, !POPLOC > 0 ~ 0))
main_data <- main_data %>% mutate(parent = case_when(abs(dad+mom) > 0 ~ 1, !abs(dad+mom) > 0 ~ 0))



cut_by <- 0.1

m_income_grid <<- sort(unique(as.integer(c(quantile(m_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
m_age_grid <<- sort(unique(as.integer(c(quantile(m_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
m_race_grid <<- sort(unique(as.integer(c(quantile(m_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
m_edu_grid <<- sort(unique(as.integer(c(quantile(m_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

m_income_grid <- m_income_grid[-1]
m_age_grid <- m_age_grid[-1]
m_race_grid <- m_race_grid[-1]
m_edu_grid <- m_edu_grid[-1]

jm_income_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
jm_age_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
jm_race_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
jm_edu_grid <<- sort(unique(as.integer(c(quantile(jm_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

jm_income_grid <- jm_income_grid[-1]
jm_age_grid <- jm_age_grid[-1]
jm_race_grid <- jm_race_grid[-1]
jm_edu_grid <- jm_edu_grid[-1]

co_income_grid <<- sort(unique(as.integer(c(quantile(co_main_data$INCTOT, probs = seq(0, 1, by = cut_by))))))
co_age_grid <<- sort(unique(as.integer(c(quantile(co_main_data$AGE, probs = seq(0, 1, by = cut_by))))))
co_race_grid <<- sort(unique(as.integer(c(quantile(co_main_data$RACE, probs = seq(0, 1, by = 0.2))))))
co_edu_grid <<- sort(unique(as.integer(c(quantile(co_main_data$EDUCD, probs = seq(0, 1, by = 0.2))))))

co_income_grid <- co_income_grid[-1]
co_age_grid <- co_age_grid[-1]
co_race_grid <- co_race_grid[-1]
co_edu_grid <- co_edu_grid[-1]


m_race_grid <<- jm_race_grid <<- co_race_grid <<- race_gridrace_grid <<- c(1,2,6,9)

m_edu_grid <<- jm_edu_grid <<- co_edu_grid <<- edu_grid <<- c(63,81,101,116)

n_age <<- length(jm_age_grid)
n_income <<- length(jm_income_grid)
n_edu <<- length(jm_edu_grid)
n_race <<- length(jm_race_grid)

n_year=2022
if (n_year!=-1){data <- subset(main_data,main_data$YEAR == n_year)}

draw(2022)
co_draw(2022)
draw_not_jm(2022)

draw(2020)
co_draw(2020)
draw_not_jm(2020)

draw(2008)
co_draw(2019)
draw_not_jm(2008)


store_jm <- funk(2021)

store_njm <- funk_not_jm(2021)
store_co <- co_funk(2021)

pair_data <- pair_data_jm <- store_jm$pair_data
pair_data_njm <- store_njm$pair_data
pair_data_co <- store_co$pair_data

#store_co_19 <- co_funk(2019)

#out by 8 x 8

plot(density(store_co$pair_data$MV_log,bw = 0.13),col="blue",xlab = "Estimation",lwd=1.5)
points(density(store_njm$pair_data$MV_log,bw = 0.13),type ="l",col="red",lwd=1.5)
points(density(store_jm$pair_data$MV_log,bw = 0.13),type ="l",col="black",lwd=1.5)
legend("topright", legend=c("Cohabitation","Old Marriage","New Marriage"),
       cex=0.8,
       lty = c(1,1,1),
       col = c("blue","red","black"))

plot(density(store_jm$pair_data$tau,bw = 0.01),xlab = "Estimation",xlim =c(-0.15,0.15),lwd=1.5)
points(density(store_njm$pair_data$tau,bw = 0.01),type ="l",col="red",lwd=1.5)
points(density(store_co$pair_data$tau,bw = 0.01),type = "l",col="blue",lwd=1.5)
legend("topright", legend=c("Cohabitation","Old Marriage","New Marriage"),
       cex=0.8,
       lty = c(1,1,1),
       col = c("blue","red","black"))

#out 6 by 9

plot(density(store_reg_jm$fit$residuals),col="black",xlab = "Residuals")
points(density(store_reg_njm$fit$residuals),type ="l",col="red")
points(density(store_reg_co$fit$residuals),type ="l",col="blue")
legend("topright", legend=c("Cohabitation","Old Marriage","New Marriage"),
       cex=0.8,
       lty = c(1,1,1),
       col = c("blue","red","black"))








dgumbel(-median(store_jm$pair_data$MV_log), location=0, scale=1, log = FALSE, grad=FALSE)
dgumbel(-median(store_co$pair_data$MV_log), location=0, scale=1, log = FALSE, grad=FALSE)
#dgumbel(-median(store_co_19$pair_data$MV_log), location=0, scale=1, log = FALSE, grad=FALSE)

(median(store_co$pair_data$MV_log)-median(store_co_19$pair_data$MV_log))/median(store_co_19$pair_data$MV_log)*100


store_reg_jm <- reg(2021)
store_reg_njm <- reg_not_jm(2021)
store_reg_co <- co_reg(2021)

summary(store_reg_jm$fit)
anova(store_reg_jm$fit)
summary(store_reg_njm$fit)
summary(store_reg_co$fit)



stargazer(store_reg_jm$fit, store_reg_jm$fit_female, store_reg_jm$fit_male, title="MV jm Results", align=TRUE)
stargazer(store_reg_co$fit, store_reg_co$fit_female, store_reg_co$fit_male, title="MV co Results", align=TRUE)
stargazer(store_reg_njm$fit, store_reg_njm$fit_female, store_reg_njm$fit_male, title="MV njm Results", align=TRUE)

stargazer(store_reg_jm$fit, store_reg_jm$fit_female, store_reg_jm$fit_male,
          store_reg_co$fit, store_reg_co$fit_female, store_reg_co$fit_male, 
          store_reg_njm$fit, store_reg_njm$fit_female, store_reg_njm$fit_male, 
          title="all in all", align=TRUE)


stargazer(store_reg_jm$fit, store_reg_co$fit, store_reg_njm$fit, title="3 MV Results", align=TRUE)

pair_data_jm_age_gap <- subset(pair_data_jm,abs(AGE.x-AGE.y) > 19)
pair_data_njm_age_gap <- subset(pair_data_njm,abs(AGE.x-AGE.y) > 19)
pair_data_co_age_gap <- subset(pair_data_co,abs(AGE.x-AGE.y) > 19)

dim(pair_data_jm_age_gap)[1]/dim(pair_data_jm)[1]*100
dim(pair_data_njm_age_gap)[1]/dim(pair_data_njm)[1]*100
dim(pair_data_co_age_gap)[1]/dim(pair_data_co)[1]*100

dim(subset(pair_data_jm,income.y>income.x))[1]/dim(pair_data_jm)[1]*100
dim(subset(pair_data_njm,income.y>income.x))[1]/dim(pair_data_njm)[1]*100
dim(subset(pair_data_co,income.y>income.x))[1]/dim(pair_data_co)[1]*100

dim(subset(pair_data_jm,abs(income.y-income.x)>50))[1]/dim(pair_data_jm)[1]*100
dim(subset(pair_data_njm,abs(income.y-income.x)>50))[1]/dim(pair_data_njm)[1]*100
dim(subset(pair_data_co,abs(income.y-income.x)>50))[1]/dim(pair_data_co)[1]*100

summary(fit)



fit_225_1 <- lm(INCTOT ~ EDUCD , data = data)
summary(fit_225_1)

fit_225_2 <- lm(INCTOT ~ as.factor(EDUCD) , data = data)
summary(fit_225_2)



fit_dif <- lm(1000*MV_log ~ 
           abs(age_dif) + abs(income_dif) + abs(edu_dif)
          + factor(NEW_RACE.x) + factor(NEW_RACE.y)
          + factor(CITY.x)
          , data = pair_data_jm
          , weights = pair_data_jm$HHW)
summary(fit_dif)


fit_jm <- lm(1000*MV_log ~ 
               AGE.x + AGE.y
             + income.x + income.y
             + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y)
             + factor(NEW_RACE.x) + factor(NEW_RACE.y)
             + factor(CITY.x)
             #+ factor(major_city.x)
             , data = pair_data_jm
             , weights = pair_data_jm$HHW)
summary(fit_jm)

fit_jm_int <- lm(1000*MV_log ~ 
               as.integer(NEW_AGE.x) + as.integer(NEW_AGE.y)
             + as.integer(NEW_INCTOT.x) + as.integer(NEW_INCTOT.y)
             + as.integer(NEW_EDUCD.x) + as.integer(NEW_EDUCD.y)
             + factor(NEW_RACE.x) + factor(NEW_RACE.y)
             , data = pair_data_jm
             , weights = pair_data_jm$HHW)

summary(fit_jm_int)

fit_jm_int_dif <- lm(1000*MV_log ~ 
                 + abs(new_age_dif) + abs(new_income_dif) + abs(new_edu_dif)
                 + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                 , data = pair_data_jm
                 , weights = pair_data_jm$HHW)
summary(fit_jm_int_dif)

stra <- stargazer(fit_dif, fit_jm, fit_jm_int,fit_jm_int_dif, title="JM Results", align=TRUE)

# now for co
fit_dif_co <- lm(1000*MV_log ~ 
                abs(age_dif) + abs(income_dif) + abs(edu_dif)
              + factor(NEW_RACE.x) + factor(NEW_RACE.y)
              + factor(major_city.x)
              , data = pair_data_co
              , weights = pair_data_co$HHW)
summary(fit_dif_co)


fit_jm_co <- lm(1000*MV_log ~ 
               AGE.x + AGE.y
             + income.x + income.y
             + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y)
             + factor(NEW_RACE.x) + factor(NEW_RACE.y)
             + factor(major_city.x)
             , data = pair_data_co
             , weights = pair_data_co$HHW)
summary(fit_jm_co)

fit_jm_int_co <- lm(1000*MV_log ~ 
                   as.integer(NEW_AGE.x) + as.integer(NEW_AGE.y)
                 + as.integer(NEW_INCTOT.x) + as.integer(NEW_INCTOT.y)
                 + as.integer(NEW_EDUCD.x) + as.integer(NEW_EDUCD.y)
                 + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                 , data = pair_data_co
                 , weights = pair_data_co$HHW)

summary(fit_jm_int_co)

fit_jm_int_dif_co <- lm(1000*MV_log ~ 
                       + abs(new_age_dif) + abs(new_income_dif) + abs(new_edu_dif)
                     + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                     , data = pair_data_co
                     , weights = pair_data_co$HHW)
summary(fit_jm_int_dif_co)

stra <- stargazer(fit_dif_co, fit_jm_co, fit_jm_int_co,fit_jm_int_dif_co, title="CO Results", align=TRUE)

# now for njm
fit_dif_njm <- lm(1000*MV_log ~ 
                   abs(age_dif) + abs(income_dif) + abs(edu_dif)
                 + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                 , data = pair_data_njm
                 , weights = pair_data_njm$HHW)
summary(fit_dif_njm)


fit_jm_njm <- lm(1000*MV_log ~ 
                  AGE.x + AGE.y
                + income.x + income.y
                + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y)
                + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                , data = pair_data_njm
                , weights = pair_data_njm$HHW)
summary(fit_jm_njm)

fit_jm_int_njm <- lm(1000*MV_log ~ 
                      as.integer(NEW_AGE.x) + as.integer(NEW_AGE.y)
                    + as.integer(NEW_INCTOT.x) + as.integer(NEW_INCTOT.y)
                    + as.integer(NEW_EDUCD.x) + as.integer(NEW_EDUCD.y)
                    + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                    , data = pair_data_njm
                    , weights = pair_data_njm$HHW)

summary(fit_jm_int_njm)

fit_jm_int_dif_njm <- lm(1000*MV_log ~ 
                          + abs(new_age_dif) + abs(new_income_dif) + abs(new_edu_dif)
                        + factor(NEW_RACE.x) + factor(NEW_RACE.y)
                        , data = pair_data_njm
                        , weights = pair_data_njm$HHW)
summary(fit_jm_int_dif_njm)

stra <- stargazer(fit_dif_njm, fit_jm_njm, fit_jm_int_njm,fit_jm_int_dif_njm, title="NJM Results", align=TRUE)


stra <- stargazer(fit_dif, fit_dif_co, fit_dif_njm,fit_jm, fit_jm_co, fit_jm_njm, title="All Results", align=TRUE)


fit_age_cut_jm <- lm(MV_log ~ 
            factor(cut_age_dif)
          + income.x + income.y
          + EDUCD.x + EDUCD.y
          + RACE.x + RACE.y
          , data = pair_data
          , weights = pair_data$HHW)
summary(fit_age_cut_jm)

fit_age_cut_co <- lm(MV_log ~ 
            factor(cut_age_dif)
          + income.x + income.y
          + EDUCD.x + EDUCD.y
          + RACE.x + RACE.y
          , data = pair_data_co
          , weights = pair_data_co$HHW)
summary(fit_age_cut_co)

fit_age_cut_njm <- lm(MV_log ~ 
                       factor(cut_age_dif)
                     + income.x + income.y
                     + EDUCD.x + EDUCD.y
                     + RACE.x + RACE.y
                     , data = pair_data_njm
                     , weights = pair_data_njm$HHW)
summary(fit_age_cut_njm)



fit <- lm(MV_log ~ 
            NEW_AGE.x + NEW_AGE.y
          + NEW_INCTOT.x + NEW_INCTOT.y
          + NEW_EDUCD.x + NEW_EDUCD.y
          + NEW_RACE.x + NEW_RACE.y
          
          + PROPTX99.x + PROPTX99.y
          + ELDCH.x + ELDCH.y
          + YNGCH.x + YNGCH.y
          + VALUEH.x + VALUEH.y
          
          + factor(NEW_RACE.x) * factor(NEW_AGE.x) + factor(NEW_RACE.y) * factor(NEW_AGE.y)
          + factor(NEW_RACE.x) * factor(NEW_INCTOT.x) + factor(NEW_RACE.y) * factor(NEW_INCTOT.y)
          + factor(NEW_RACE.x) * factor(NEW_EDUCD.x) + factor(NEW_RACE.y) * factor(NEW_EDUCD.y)
          
          + NUMPREC.x + NUMPREC.y
          + COSTELEC.x + COSTELEC.y
          + COSTGAS.x + COSTGAS.y
          + NCHILD.x + NCHILD.y

          + factor(CLASSWKRD.x) + factor(CLASSWKRD.y)
          + factor(LABFORCE.x) + factor(LABFORCE.y)
          #+ CITYPOP.x
          + factor(CITY.x)
          + factor(STATEICP.x)
          + factor(MIGRATE1.x) + factor(MIGRATE1.y)
          + factor(VETSTAT.x) + factor(VETSTAT.y)
          + factor(EMPSTAT.x) + factor(EMPSTAT.y)
          + factor(PWTYPE.x) + factor(PWTYPE.y)
          + factor(OCC1950.x) + factor(OCC1950.y)
          + factor(BPL.x) + factor(BPL.y)
          + factor(YNGCH.x) + factor(YNGCH.y)
          + factor(ELDCH.x) + factor(ELDCH.y)
          
          #+ factor(EDUCD.x) + factor(EDUCD.y)
          #+ factor(RACE.x) + factor(RACE.y)
          
          , data = pair_data_jm
          , weights = pair_data_jm$HHW
          )

summary(fit)

summary(store_reg_jm$fit) 
summary(store_reg_njm$fit) 


ggcoef_model(fit)

fit_tau_jm <- lm(1000*tau ~ 
                AGE.x + AGE.y
              + income.x + income.y
              + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
              + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
              , data=pair_data_jm
              , weights = pair_data_jm$HHW)
summary(fit_tau_jm)


fit_theta_jm <- lm(theta ~ 
                   AGE.x + AGE.y
                 + income.x + income.y
                 + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
                 + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
                 , data=pair_data_jm
                 , weights = pair_data_jm$HHW)
summary(fit_theta_jm)





# pair_data$theta_prime <- pair_data$theta/(1-pair_data$theta+0.01)
# pair_data$c_m <- 1/(pair_data$dad.y+0.01)
# pair_data$c_f <- 1/(pair_data$dad.x+0.01)
# pair_data$c_t <- pair_data$c_m+pair_data$c_f
# pair_data$LHS <- pair_data$c_m - pair_data$theta_prime*pair_data$c_f
# pair_data$RHS <- - pair_data$theta_prime*pair_data$c_t
# 
# 

pair_data <- funk(2019)$pair_data
fit_wild_jm <- lm(LHS ~ c_t + RHS + 0
                     , data=pair_data)
summary(fit_wild_jm)

fit_wild_jm <- lm(LHS_2 ~ c_t_2 + RHS_2 + 0
                  , data=pair_data)
summary(fit_wild_jm)

fit_wild_co <- lm(LHS ~ c_t + RHS + 0
                  , data=co_funk(2019)$pair_data)
summary(fit_wild_co)

fit_wild_njm <- lm(LHS ~ c_t + RHS + 0
                  , data=funk_not_jm(2019)$pair_data)
summary(fit_wild_njm)



fit_tau_jm_dif <- lm(1000*tau ~ 
                   age_dif + income_dif
                 + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
                 + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
                 , data=pair_data_jm
                 , weights = pair_data_jm$HHW)
summary(fit_tau_jm_dif)


fit_tau_njm <- lm(1000*tau ~ 
                AGE.x + AGE.y
              + income.x + income.y
              + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
              + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
              , data=pair_data_njm
              , weights = pair_data_njm$HHW)
summary(fit_tau_njm)

fit_tau_njm_dif <- lm(1000*tau ~ 
                       age_dif + income_dif
                     + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
                     + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
                     , data=pair_data_njm
                     , weights = pair_data_njm$HHW)
summary(fit_tau_njm_dif)


fit_tau_co <- lm(1000*tau ~ 
                AGE.x + AGE.y
              + income.x + income.y
              + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
              + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
              , data=pair_data_co
              , weights = pair_data_co$HHW)
summary(fit_tau_co)

fit_tau_co_dif <- lm(1000*tau ~ 
                        age_dif + income_dif
                      + factor(NEW_EDUCD.x) + factor(NEW_EDUCD.y) 
                      + factor(NEW_RACE.x) + factor(NEW_RACE.y) 
                      , data=pair_data_co
                      , weights = pair_data_co$HHW)
summary(fit_tau_njm_dif)

stra <- stargazer(fit_tau_jm, fit_tau_njm, fit_tau_co,fit_tau_jm_dif, fit_tau_njm_dif, fit_tau_co_dif, title="Tau Results", align=TRUE)

ggcoef_model(fit_tau)



fit_tau <- lm(tau ~ 
            NEW_AGE.x + NEW_AGE.y
          + NEW_INCTOT.x + NEW_INCTOT.y
          + NEW_EDUCD.x + NEW_EDUCD.y
          + NEW_RACE.x + NEW_RACE.y
        
          
          
          , data=pair_data)



summary(fit_tau)






sum(lower.tri(Mu_age, diag = FALSE)*Mu_age)/sum(Mu_age)
sum(upper.tri(Mu_age, diag = FALSE)*Mu_age)/sum(Mu_age)
abs(sum(lower.tri(Mu_age, diag = FALSE)*Mu_age)/sum(Mu_age)-sum(upper.tri(Mu_age, diag = FALSE)*Mu_age)/sum(Mu_age))


sum(lower.tri(Mu_M_age, diag = FALSE)*Mu_M_age)/sum(Mu_M_age)
sum(upper.tri(Mu_M_age, diag = FALSE)*Mu_M_age)/sum(Mu_M_age)
abs(sum(lower.tri(Mu_M_age, diag = FALSE)*Mu_M_age)/sum(Mu_M_age)-sum(upper.tri(Mu_M_age, diag = FALSE)*Mu_M_age)/sum(Mu_M_age))




# TRANWORK      Means of transportation to work
# 0                                                     N/A
# 10                                     Auto, truck, or van
# 11                                                    Auto
# 12                                                  Driver
# 13                                               Passenger
# 14                                                   Truck
# 15                                                     Van
# 20                                              Motorcycle
# 31                                                     Bus
# 32                                      Bus or trolley bus
# 33                                        Bus or streetcar
# 34 Light rail, streetcar, or trolley (Carro público in PR)
# 35 Streetcar or trolley car (publico in Puerto Rico, 2000)
# 36                                      Subway or elevated
# 37                   Long-distance train or commuter train
# 38                                                 Taxicab
# 39                                               Ferryboat
# 50                                                 Bicycle
# 60                                             Walked only
# 70                                                   Other
# 80                                          Worked at home


# TRANTIME     Travel time to work

# TRANTIME reports the total amount of time, in minutes, 
# that it usually took the respondent to get from home to work last week.

count(pair_data,TRANTIME.x>0)
count(pair_data,TRANTIME.y>0)
count(pair_data,TRANTIME.x, TRANWORK.x==50)

count(pair_data,MOMLOC.x)


travel_pair_data <- subset(pair_data,TRANTIME.x != 0 & TRANTIME.y!=0
                           & TRANWORK.x == 10 & TRANWORK.y == 10)
dim(travel_pair_data)




next_fit_0f0 <- lm(TRANTIME.x ~ theta,
                   data=pair_data,#subset(pair_data,TRANTIME.x>0),
                   weights = pair_data$HHW)
summary(next_fit_0f0)

next_fit_0m0 <- lm(TRANTIME.y ~ theta 
                   , data=pair_data,#subset(pair_data,TRANTIME.y>0),
                   weights = pair_data$HHW)
summary(next_fit_0m0)

next_fit_0f <- lm(TRANTIME.x ~ theta + TRANTIME.y
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                 + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                 #+ as.factor(DEGFIELD.x)
                 #+ as.factor(DEPARTS.x)
                 #+ as.factor(TRANWORK.x)
                 , data=pair_data,weights = pair_data$HHW)
summary(next_fit_0f)


next_fit_0m <- lm(TRANTIME.y ~ theta + TRANTIME.x
                  + income.x + income.y
                  + AGE.x + AGE.y
                  + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                  + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                  #+ as.factor(DEGFIELD.y)
                  #+ as.factor(DEPARTS.y)
                  #+ as.factor(TRANWORK.y)
                  , data=pair_data,weights = pair_data$HHW)
summary(next_fit_0m)


plot(density(pair_data$theta,bw = .01),
     col="red",xlab = "Estimated bargaining power",title(main=""))
points(density(pair_data_co$theta,bw = .07),type = "l",col="blue",lty = 2)
points(density(pair_data_njm$theta,bw = .07),type = "l",col="black",lty = 3)
abline(v=.5, col="gray")
legend("topright", legend=c("New marriages","Cohabitation","All marriages","0.5 line"),
       cex=0.8,
       lty = c(1,2,3,1),
       col = c("red","blue","black","gray"))
# out 6 x 9

stargazer(next_fit_0f0, next_fit_0m0, next_fit_0f, next_fit_0m, title="commute time vs mu + control", align=TRUE)


###### new marriage


next_fit_xparent0 <- lm(parent.x ~  theta
                    , data=pair_data,weights = pair_data$HHW)
summary(next_fit_xparent0)

next_fit_yparent0 <- lm(parent.y ~  theta
                    , data=pair_data,weights = pair_data$HHW)
summary(next_fit_yparent0)

next_fit_xparent1 <- lm(parent.x ~  theta
                 #+ as.factor(dad.x)
                 #+ as.factor(dad.y)
                 #+ as.factor(mom.y)
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 
                 #+ as.factor(NEW_INCTOT.x) + as.factor(NEW_INCTOT.y)
                 #+ as.factor(NEW_AGE.x) + as.factor(NEW_AGE.y)
                 
                 + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                 + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                 
                 , data=pair_data,weights = pair_data$HHW)
summary(next_fit_xparent1)
next_fit_yparent1 <- lm(parent.y ~  theta
                 #+ as.factor(dad.x) 
                 #+ as.factor(dad.y)
                 #+ as.factor(mom.x) 
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                 + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                 , data=pair_data,weights = pair_data$HHW)
summary(next_fit_yparent1)

stargazer(next_fit_xparent0, next_fit_yparent0, next_fit_xparent1, next_fit_yparent1, title="tau vs parents", align=TRUE)




######## done


next_fit_2 <- lm(tau*100 ~  
                   as.factor(dad.x) + as.factor(dad.y)
                 + as.factor(mom.x) + as.factor(mom.y)
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(RACE.x) + as.factor(RACE.y)
                 + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                 
                 , data=pair_data,weights = pair_data$HHW)
summary(next_fit_2)

next_fit_3 <- lm(tau*100 ~ TRANTIME.x + TRANTIME.y
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_3)

next_fit_4 <- lm(tau*100 ~ TRANTIME.x + TRANTIME.y
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(RACE.x) + as.factor(RACE.y)
                 + as.factor(EDUCD.x) + as.factor(EDUCD.y)
          
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_4)


next_fit_5 <- lm(tau*100 ~ TRANTIME.x + TRANTIME.y
                 
                 + as.factor(dad.x) + as.factor(dad.y)
                 + as.factor(mom.x) + as.factor(mom.y)
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(RACE.x) + as.factor(RACE.y)
                 + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                 
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_5)

stargazer(next_fit_1, next_fit_2, next_fit_3, next_fit_4, next_fit_5, title="tau vs exclusive", align=TRUE)


###### same for cohabitation



next_fit_2 <- lm(tau*100 ~ TRANTIME.x + TRANTIME.y
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_2)



next_fit_test <- lm(TRANTIME.y ~ tau + income.x + income.y
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_test)


next_fit_3 <- lm(tau*100 ~ 
                    as.factor(dad.x) + as.factor(dad.y)
                 + as.factor(mom.x) + as.factor(mom.y)
                 
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(RACE.x) + as.factor(RACE.y)
                 + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_3)

next_fit_4 <- lm(tau*100 ~ TRANTIME.x + TRANTIME.y
                 + income.x + income.y
                 + AGE.x + AGE.y
                 + as.factor(RACE.x) + as.factor(RACE.y)
                 + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                 , data=pair_data,weights = pair_data_njm$HHW)
summary(next_fit_4)

next_njm_fit_1 <- lm(tau*100 ~as.factor(dad.x) + as.factor(dad.y)
                   + as.factor(mom.x) + as.factor(mom.y)
               , data=pair_data_njm, weights = pair_data$HHW)
summary(next_njm_fit_1)

next_njm_fit_2 <- lm(tau*100 ~  TRANTIME.x + TRANTIME.y
                     , data=pair_data_njm,weights = pair_data_njm$HHW)
summary(next_njm_fit_2)




next_co_fit_1 <- lm(tau*100 ~ as.factor(dad.x) + as.factor(dad.y)
                    + as.factor(mom.x) + as.factor(mom.y)
                   , data=pair_data_co,weights = pair_data_co$HHW)
summary(next_co_fit_1)

next_co_fit_2 <- lm(tau*100 ~ TRANTIME.y + TRANTIME.x
                    , data=pair_data_co,weights = pair_data_co$HHW)
summary(next_co_fit_2)




next_co_fit_3 <- lm(tau*100 ~ as.factor(dad.x) + as.factor(dad.y)
                    + as.factor(mom.x) + as.factor(mom.y)
                    
                    + income.x + income.y
                    + AGE.x + AGE.y
                    + as.factor(RACE.x) + as.factor(RACE.y)
                    + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                    
                    , data=pair_data_co,weights = pair_data_co$HHW)
summary(next_co_fit_3)

next_co_fit_4 <- lm(tau*100 ~ TRANTIME.y + TRANTIME.x
                    
                    + income.x + income.y
                    + AGE.x + AGE.y
                    + as.factor(RACE.x) + as.factor(RACE.y)
                    + as.factor(EDUCD.x) + as.factor(EDUCD.y)
                    
                    
                    , data=pair_data_co,weights = pair_data_co$HHW)
summary(next_co_fit_4)




stargazer(next_fit_1,next_co_fit_1,
          next_fit_2,next_co_fit_2,
          next_fit_3,next_co_fit_3,
          next_fit_4,next_co_fit_4,
          title="all in all", align=TRUE)

#plot(density(travel_pair_data$TRANTIME.y-travel_pair_data$TRANTIME.x))




store_jm <- funk(2021)
pair_data <- pair_data_jm <- store_jm$pair_data

#############3
# gains vs wps
MV_female_log_wps <- lm(MV_female_log ~ WPS_index.x
                 , data=pair_data, weights = pair_data_co$HHW)
summary(MV_female_log_wps)



MV_male_log_wps <- lm(MV_male_log ~ WPS_index.x
                        , data=pair_data, weights = pair_data_co$HHW)
summary(MV_male_log_wps)

MV_female_log_wps_control <- lm( MV_female_log ~ WPS_index.x
                       + income.x + income.y
                       + AGE.x + AGE.y
                       + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                       + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                      , data=pair_data, weights = pair_data_co$HHW)
summary(MV_female_log_wps_control)


MV_male_log_wps_control <- lm( MV_male_log ~ WPS_index.x
                       + income.x + income.y
                       + AGE.x + AGE.y
                       + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                       + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                       , data=pair_data, weights = pair_data_co$HHW)
summary(MV_male_log_wps_control)


# bargaining vs wps
fit_wps <- lm(theta ~ WPS_index.x
                 , data=pair_data, weights = pair_data_co$HHW)
summary(fit_wps)
fit_wps_control <- lm(theta ~ WPS_index.x
                         + income.x + income.y
                         + AGE.x + AGE.y
                         + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                         + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                         , data=pair_data,weights = pair_data_co$HHW)
summary(fit_wps_control)


stargazer(MV_female_log_wps, MV_male_log_wps,
          MV_female_log_wps_control,MV_male_log_wps_control,
          fit_wps, fit_wps_control, title="WPS Results", align=TRUE)



# cohabitation
fit_wps_co <- lm(theta ~ WPS_index.x
                 , data=pair_data_co, weights = pair_data_co$HHW)
summary(fit_wps_co)
fit_wps_control_co <- lm(theta ~ WPS_index.x
                         + income.x + income.y
                         + AGE.x + AGE.y
                         + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                         + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
          , data=pair_data_co,weights = pair_data_co$HHW)
summary(fit_wps_control_co)


summary(fit_wps_co)
fit_wps_control_co <- lm(theta ~ WPS_index.x
                         + income.x + income.y
                         + AGE.x + AGE.y
                         + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                         + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                         , data=pair_data_co,weights = pair_data_co$HHW)
summary(fit_wps_control_co)



fit_njm <- lm(theta ~ WPS_index.x
              , data=pair_data_njm,weights = pair_data_njm$HHW)
summary(fit_njm)

fit_wps_control_njm <- lm(theta ~ WPS_index.x
                          + income.x + income.y
                          + AGE.x + AGE.y
                          + as.factor(NEW_RACE.x) + as.factor(NEW_RACE.y)
                          + as.factor(NEW_EDUCD.x) + as.factor(NEW_EDUCD.y)
                          , data=pair_data_njm,weights = pair_data_njm$HHW)
summary(fit_wps_control_njm)





fit_wps <- lm(theta ~ WPS_index.x
          , data=pair_data)
summary(fit)

fit_wps_co <- lm(theta ~ WPS_index.x
          , data=pair_data_co)
summary(fit_co)

fit_wps_njm <- lm(theta ~ WPS_index.x
          , data=pair_data_njm)
summary(fit_njm)



stargazer(fit_wps_control, fit_wps_control_co, fit_wps_control_njm,fit_wps, fit_wps_co, fit_wps_njm, title="WPS Results", align=TRUE)






###########3
fit <- lm(MV_female_log ~ WPS_index.x
          , data=pair_data)
summary(fit)

fit_co <- lm(MV_female_log ~ WPS_index.x
             , data=pair_data_co)
summary(fit_co)

fit_njm <- lm(MV_female_log ~ WPS_index.x
              , data=pair_data_njm)
summary(fit_njm)






fit <- lm(tau ~ f_income_share
          , data=pair_data)
summary(fit)


fit <- lm(MV_log ~ f_income_share
          , data=pair_data)
summary(fit)


fit <- lm(MV_log ~ WPS_index.x
          , data=pair_data)
summary(fit)

fit_co <- lm(MV_log ~ WPS_index.x
             , data=pair_data_co)
summary(fit_co)

fit_njm <- lm(MV_log ~ WPS_index.x
              , data=pair_data_njm)
summary(fit_njm)


chisq.test(main_data$EDUCD, main_data$INCTOT)

fisher.test(main_data$EDUCD, main_data$INCTOT)


