## ----include=FALSE------------------------------------------------------------
# knitr::opts_chunk$set(
#   collapse = TRUE,
#   comment = "#>"
# )

## ----setup, include=FALSE-----------------------------------------------------
library(dplyr)
library(prettyglm)
#library(tidymodels)

## ----load data, echo=TRUE-----------------------------------------------------
data('titanic')
head(titanic) %>%
  knitr::kable(table.attr = "style='width:30%;'" ) %>% 
  kableExtra::kable_classic(full_width = T, position = "center")

## ----preprocessing, echo=TRUE-------------------------------------------------
# Easiest way to convert multiple columns to a factor.
columns_to_factor <- c('Pclass',
                       'Sex',
                       'Cabin', 
                       'Embarked',
                       'Cabintype')
titanic  <- titanic  %>%
  dplyr::mutate_at(columns_to_factor, list(~factor(.)))

## ----build model, echo=TRUE---------------------------------------------------
survival_model <- stats::glm(Survived ~ Pclass + Sex + Age + Fare + Embarked + SibSp + Parch + Cabintype, 
                             data = titanic, 
                             family = binomial(link = 'logit'))

## ----visualise coefficients, echo=TRUE----------------------------------------
pretty_coefficients(survival_model)

## ----visualise coefficients type iii, echo=TRUE-------------------------------
pretty_coefficients(survival_model, type_iii = 'Wald')

## ----visualise rels, echo=TRUE------------------------------------------------
pretty_relativities(feature_to_plot= 'Embarked',
                    model_object = survival_model)

