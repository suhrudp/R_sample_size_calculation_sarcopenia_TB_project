# Study size
# The sample size required to run a linear regression analysis to 
# predict hand grip strength (continuous) with the NPLR (continuous) 
# with 4 adjusting variables is 72 patients. The alpha error rate 
# assumed is 5%, with a power of 90%. This sample size is calculated 
# to detect a moderate coefficient of determination for the entire 
# model (R² = 0.2, Cohen’s f² = 0.25).[1,2]
# Power analyses were performed using R, version 4.3.1 using the 
# `wp.regression` function from the `WebPower` package.[3,4]

# 1.	Cohen, J. (1988). Statistical Power Analysis for the 
# Behavioral Sciences (2nd ed.). Routledge.  
# https://doi.org/10.4324/9780203771587 
# 2.	Selya AS, Rose JS, Dierker LC, Hedeker D and Mermelstein 
# RJ (2012) A practical guide to calculating Cohen's f2, a measure of 
# local effect size, from PROC MIXED. Front. Psychology 3:111. 
# https://doi.org/10.3389/fpsyg.2012.00111 
# 3.	R Core Team (2023). _R: A Language and Environment for 
# Statistical Computing_. R Foundation for Statistical Computing, 
# Vienna, Austria. https://www.R-project.org/
# 4.	Zhang Z, Mai Y (2023). _WebPower: Basic and Advanced 
# Statistical Power Analysis_. R package version 0.9.3. 
# https://CRAN.R-project.org/package=WebPower

# Statistical Methods
# Continuous data are summarized as medians (interquartile ranges) 
# while categorical data are summarized as frequencies (percentages). 
# Continuous data are assessed for normality by visualizing histograms 
# and Q-Q plots. Evaluation of the association between two categorical 
# variables is performed using Pearson’s Chi-square test or Fisher’s 
# exact test as appropriate and between two continuous variables is 
# performed using Spearman’s rank correlation test. Evaluation of the 
# difference between two continuous groups is performed using the 
# Wilcoxon rank sum test.
# We used a linear regression approach to evaluate the association 
# between the neutrophil- platelet-lymphocyte ratio (NPLR) and 
# dominant handgrip-strength. To overcome the limitation of excluding 
# outliers and therefore excluding potentially important information, 
# we used a robust linear regression model using the median-of-medians 
# (MM) estimator.
# p-values less than 0.05 are considered to be statistically 
# significant. Data are analyzed using R, version 4.3.1.[1] The 
# ‘robustbase` package is used to perform robust linear regression 
# analyses and the `visreg` package is used to visualize the 
# regression.[2]

# References
# 1.	R Core Team (2023). _R: A Language and Environment for 
# Statistical Computing_. R Foundation for Statistical Computing, 
# Vienna, Austria. https://www.R-project.org/
# 2.	Martin Maechler, Peter Rousseeuw, Christophe Croux, 
# Valentin Todorov, Andreas Ruckstuhl, Matias Salibian-Barrera, 
# Tobias Verbeke, Manuel Koller, c("Eduardo", "L. T.") Conceicao and 
# Maria Anna di Palma (2023). robustbase: Basic Robust Statistics R 
# package version 0.99-0. URL 
# http://CRAN.R-project.org/package=robustbase

# setting seed for reproducibility
set.seed(13)

# set working directory
setwd("your/working/directory")

# load libraries
library(readxl) # read excel workbooks
library(tidyverse) # for ggplot, dplyr and so on
library(ggsci) # better themes for ggplot
library(ggstatsplot) # to get information laden plots
library(gtsummary) # for automated tables
library(flextable) # to print tables
library(DataExplorer) # to get a detailed exploratory data analysis report
library(robustbase) # to perform robust linear regression
library(visreg) # to visualize regression functions
library(quantreg) # to perform quantile regression
library(boot) # to perform data resampling using bootstrapping
library(performance) # to assess model performance eg, for regressions and calculate r squared
library(gridExtra) # to arrange 2 ggplot objects

# import data
df <- read_excel("your/working/directory/data.xlsx")
df %>% colnames()
attach(df)

# exploratory data analysis
# DataExplorer::create_report(df)

# summary statistics table
table1 <- df[,c(3:6,28)] %>%
            tbl_summary(by = "AKT",
                        type = list(where(is.numeric) ~ "continuous"),
                        missing_text = "Missing Data") %>%
            add_overall() %>%
            add_p()

table2 <- df[,c(8:31,36:45)] %>%
            tbl_summary(by = "AKT",
                        type = list(c("Total Number of Symptoms", 
                                      "Duration of Symptoms (weeks)") ~ "continuous"),
                        missing_text = "Missing Data") %>%
            add_overall() %>%
            add_p()

table3 <- df[,c(28,47:58)] %>%
            tbl_summary(by = "AKT",
                        type = list("BTB score" ~ "continuous"),
                        missing_text = "Missing Data") %>%
            add_overall() %>%
            add_p()

# table1 %>% as_flex_table() %>% save_as_docx(path = "Table1.docx")
# table2 %>% as_flex_table() %>% save_as_docx(path = "Table2.docx")
# table3 %>% as_flex_table() %>% save_as_docx(path = "Table3.docx")

## barplots for symptoms
# reshape data to longer format
df_long <- df %>%
            pivot_longer(cols = 11:19, names_to = "symptom", values_to = "response")

# calculate the percentages
df_long <- df_long %>%
  mutate(AKT = recode(AKT, `Yes` = "Treated", `No` = "Not yet treated"))

df_long <- df_long %>%
            group_by(AKT, symptom, response) %>%
            summarise(n = n()) %>%
            mutate(percentage = n / sum(n) * 100)

# create bar plots with percentages and facet by AKT
plotbar1 <- ggplot(df_long, aes(x = response, y = percentage, fill = response)) +
              geom_bar(stat = "identity") +
              facet_grid(AKT ~ symptom, scales = "free") +
              labs(x = "Symptoms", y = "Percentage", fill = "Symptoms") +
              scale_fill_jco() +
              theme_minimal() +
              theme(plot.background = element_rect(fill = "white", colour = "white"),
                    panel.background = element_rect(fill = "white", colour = "white"))
ggsave(plot = plotbar1,
       filename = "Plot2.png",
       height = 5,
       width = 10,
       units = "in",
       dpi = 900)

## unadjusted analysis
# robust linear regression
mod1 <- lmrob(`Grip Strength in Dominant Hand` ~ NPLR)
tbl_regression(mod1)
plot1 <- visreg(mod1, gg = T)
plot1 <- plot1 + 
          labs(x = "Neutrophil-Platelet-Lymphocyte Ratio", 
               y = "Unadjusted Grip Strength in Dominant Hand", 
               title = "Unadjusted Robust Linear Regression",
               subtitle = "beta = -1.66, 95% CI: -2.74 to -0.57, p = 0.004") +
          scale_x_continuous(breaks = seq(0,0.2,0.01)) + 
          scale_y_continuous(breaks = seq(20,80,5)) +
          theme_minimal() +
          theme(plot.background = element_rect(fill = "white", colour = "white"),
            panel.background = element_rect(fill = "white", colour = "white"))
check_model(mod1)
R2 <- r2(mod1) # calculate r squared
R2 <- R2$R2
f2 <- R2/(1-R2)

# also performing quantile regression (median) to check validity of results (considering severe outliers)
mod1q <- rq(`Grip Strength in Dominant Hand` ~ NPLR, data = df, tau = 0.5)
tbl_regression(mod1q)
plot1q <- visreg(mod1q, gg = T)
plot1q <- plot1q + 
            labs(y = "Grip Strength in Dominant Hand") +
            scale_x_continuous(breaks = seq(0,0.6,0.05)) + 
            scale_y_continuous(breaks = seq(20,80,5))

# perform vanilla linear regression dropping the outliers to check whether an association still exists
dfdrop <- df[-52,]
mod1d <- lm(`Grip Strength in Dominant Hand` ~ NPLR, data = dfdrop)
tbl_regression(mod1d)
plot1d <- visreg(mod1d, gg = T)
plot1d <- plot1d + 
            labs(y = "Grip Strength in Dominant Hand") +
            scale_x_continuous(breaks = seq(0,0.1,0.01)) + 
            scale_y_continuous(breaks = seq(20,80,5))
check_model(mod1d)

# lastly, perform bootstrapped linear regression with the dropped dataframe with 5000 resamples
mod1b <- lm(`Grip Strength in Dominant Hand` ~ NPLR, data = dfdrop)

boot_fn <- function(data, indices) {
  test <- data[indices,] 
  mod1b <- lm(`Grip Strength in Dominant Hand` ~ NPLR, data = test)
  return(coef(mod1b))
} # define a function to obtain coefficients

boot_results <- boot(data = dfdrop, statistic = boot_fn, R = 5000) # set up bootstrapping with 5000 resamples
print(boot_results) # print bootstrap results
boot_ci <- boot.ci(boot_results, type = "perc", index = 2) # index = 2 for t2 not t1
print(boot_ci)

# compare all the models to see which one is the best for the data
compare_performance(mod1, mod1q, mod1d, rank = T)

## adjusted analysis (for age, sex, bmi, and current treatment status)
# robust linear regression - primary objective (calculated sample size for this objective)
df$Age <- df$`Age (years)`
df$BMI <- df$`BMI (kg/m²)`
mod2 <- lmrob(`Grip Strength in Dominant Hand` ~ NPLR + Age + Sex + BMI + AKT, data = df)
tableuv1 <- df[,c(55,61,4,62,38,28)] %>% tbl_uvregression(y = "Grip Strength in Dominant Hand", method = lmrob)
tablemv1 <- tbl_regression(mod2)
tablemerged1 <- tbl_merge(tbls = list(tableuv1, tablemv1), tab_spanner = c("Unadjusted", "Adjusted"))
# tablemerged1 %>% as_flex_table() %>% save_as_docx(path = "TableRegNPLR.docx")
plot2 <- visreg(mod2, "NPLR", gg = T)
plot2 <- plot2 + 
          labs(x = "Neutrophil-Platelet-Lymphocyte Ratio", 
               y = "Adjusted Grip Strength in Dominant Hand", 
               title = "Robust Linear Regression Adjusted for Age, Sex, BMI, and Treatment Status",
               subtitle = "beta = -0.22, 95% CI: -0.48 to 0.04, p = 0.11") +
          scale_x_continuous(breaks = seq(0,0.9,0.01)) + 
          scale_y_continuous(breaks = seq(65,90,2)) +
          theme_minimal() +
          theme(plot.background = element_rect(fill = "white", colour = "white"),
                panel.background = element_rect(fill = "white", colour = "white"))
plot3 <- grid.arrange(plot1, plot2, nrow = 1)
ggsave(plot = plot3, 
       filename = "Plot1.png", 
       height = 6, 
       width = 18, 
       units = "in", 
       dpi = 600)
check_model(mod2)
R2 <- r2(mod2) # calculate r squared
R2 <- R2$R2
f2 <- R2/(1-R2)

# correlation between sarcopenia parameters and inflammatory indices
dfcor <- df[,c(10,21,22,52:59,36:38)] # get only the data for the correlation matrix
plotcor <- ggcorrmat(dfcor, type = "np")
ggsave(plot = plotcor, 
       filename = "Plot3.png", 
       height = 10, 
       width = 12, 
       units = "in", 
       dpi = 600)
