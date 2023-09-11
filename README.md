# R_sarcopenia_in_TB_project
A project that I worked during medical school to study the factors associated with sarcopenia (as assessed by hand-grip strength) in patients with pulmonary tuberculosis.

I have provided 2 R scripts for this study:
1. The sample size calculation markdown (for power analysis and deciding the optimal sample size for the study)
2. The script for the main analysis. This contains a few approaches to handling outliers and performing linear regression analysis (I settled on using a robust linear regression, but two other approaches I tried were performing quantile regression [median regression specifically] and removing the outliers [overtly influential observations]). I considered using RANSAC regression; however, I decided to keep it simple since robust linear regression showed a pretty good model performance.

P.S. Please convert the .R file to a .Rmd file (for the sample size calculation) to run it directly as a R Markdown file.
