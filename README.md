## Purpose of this tutorial

This data-science tutorial and work sample (programmed in R) was developed for a start-up team of software engineers and business analysts engaged in a machine learning training. The team members were invited to enroll in their choice of courses from the [Harvard Professional Certificate in Data Science] (https://www.edx.org/professional-certificate/harvardx-data-science), and this tutorial served both as a guide and as a work sample for the capstone project of the curriculum. It is therefore designed for an audience which already has some knowledge of data science, as the core concepts are only briefly summarised. The models and the workflow, however, are developed in details.

## About the machine learning challenge

This tutorial develops two machine learning algorithms for a movie recommendation system: a linear model and a matrix factorisation model. Their goal is to predict movie ratings from a set of features. These algorithms are trained on a dataset of 10 million ratings (the movielens dataset). This dataset contains ratings of about 10,000 movies by 72,000 users. It is publicly available [here](https://grouplens.org/datasets/movielens/10m/).

## Content of the repository

The clean datasets required to run the .rmd file of the tutorial are available in `/data`. This tutorial consists of three files:

* The main document, `movielens-tutorial.Rmd`, takes the reader through a typical data-science workflow and explains step-by-step the various step of data preparation, visualisation, analysis and development of a machine learning learning algorithm in R.

* The .pdf document corresponding to this r-markdown document.

* The R code extraction from the .rmd file using `knitr::purl()`. Code blocks which are not evaluated in the .rmd file are marked as comments in the R script file.