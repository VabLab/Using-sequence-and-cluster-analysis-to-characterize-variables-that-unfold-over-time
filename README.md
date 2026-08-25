# Using-sequence-and-cluster-analysis-to-characterize-variables-that-unfold-over-time
Code for the "Using sequence and cluster analysis to characterize variables that unfold over time: implementation and practical considerations for epidemiologists" publication

Link to paper: https://academic.oup.com/aje/article/195/6/1707/8111980  

# Authors
 Lucia Pacca , Kristina V Dang, Leah Koenig , Catherine d P Duarte , S Amina Gaye , Amal Harrati and Anusha M Vable

# Abstract
Characterizing longitudinal trajectories of variables that unfold over time (eg, social, health, or environmental variables) is a persistent challenge, but can be accomplished with sequence and cluster analysis, data-driven approaches that can differentiate timing, order, and duration of events. We present practical guidance on implementing sequence and cluster analysis for epidemiologists with the goal of providing clear advice on decision points and tradeoffs. We introduce the three main steps of sequence and cluster analysis: (1) coding trajectories of ordered events (data cleaning); (2) measuring dissimilarity between trajectories (sequence analysis); and (3) grouping similar trajectories (cluster analysis). Each of these steps presents researchers with several decision points, such as data cleaning rules, options for evaluating sequence dissimilarity, and choices of clustering algorithms. After outlining each of the sequence analysis steps, we provide an applied example of sequence analysis in which we create and group transition-to-retirement trajectories from age 51 to 75 years for a sample of 9189 Health and Retirement Study participants using self-reported employment information, then estimate the association between transition-to-retirement groups and self-rated health. We seek to provide an initial guide for epidemiologists through analytic decisions and implementation challenges of sequence analysis as this approach is increasingly implemented and undergoes methodological advances.

# Repository Content
**01 - Data Cleaning and Analysis.do:** This .do file prepares the HRS data for the applied example by cleaning and constructing employment and retirement trajectories from ages 51–75 and obtaining self-rated health at ages 76–77. It prepares the employment sequences for subsequent sequence analysis, merges the resulting trajectory clusters with the outcome data, estimates associations between trajectory clusters and self-rated health, and prepares data for a sensitivity analysis distinguishing death from other unreported states.

**02 - Sequence Analysis:** This R file conducts the sequence and cluster analyses of employment and retirement trajectories from ages 51–75. It uses Hamming distance to calculate sequence dissimilarities, compares hierarchical clustering and partitioning around medoids (PAM), and generates the final seven-cluster solution and corresponding sequence plots. It also conducts a sensitivity analysis using Optimal Matching.

# Contact Information
lucia.pacca88@gmail.com
