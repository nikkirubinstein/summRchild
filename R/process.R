######################################################################
#
# process.R
#
# This file is used to generate a survey to estimate experimental risk
#
# Created by RLadies Melbourne
# 22/2/2022
#
######################################################################
run_survey <- function(){
# Load libraries ----------------------------------------------------------
library(jsonlite) ## version 1.7.3
library(dplyr)
library(magrittr)
library(tidyr)
library(stringr)
library(shiny)
library(shinysurveys)

# Set WD to source file location  -------------------------------------------------------
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read in JSON file -------------------------------------------------------
dat <- jsonlite::fromJSON("https://raw.githubusercontent.com/summerscope/summer-child/main/questions.json", flatten = TRUE) %>%
  dplyr::mutate(qn_text = text, .keep = "unused")

dat_long <- dat %>%
  dplyr::mutate(dplyr::across(dplyr::ends_with("score"), ~ as.numeric(.x)),
                dplyr::across(dplyr::ends_with("multiplier"), ~ as.numeric(.x))) %>%
  tidyr::pivot_longer(cols = starts_with("answers"),
               names_to = c("Response", ".value"),
               names_pattern = "answers\\.(\\w+)\\.(\\w+)"
  ) %>%
  dplyr::filter (!is.na(text))


# Create required data structure for survey ---------------------------------------------------------
# change column names as per shinysurveys
survey_qns <- dat_long %>%
  dplyr::mutate(input_type = ifelse(Response == "Ok", "instructions", "mc"),
         question = stringr::str_replace(qn_text,"To continue, type 'Ok'", ""),
         option = text,
         input_id = id,
         required = TRUE,
         dependence = NA,
         dependence_value = NA,
         page = stringr::str_extract(question, "Section #[0-9]+"),
         .keep = "unused") %>%
  tidyr::fill(page) %>% as.data.frame()

# determine questions with dependencies
dependencies <- survey_qns %>%
  dplyr::select(input_id, nextq) %>%
  dplyr::group_by(input_id) %>%
  dplyr::summarise(num_nextq = dplyr::n_distinct(nextq)) %>%
  dplyr::filter(num_nextq > 1)
dependencies <- survey_qns %>%
  dplyr::filter(input_id %in% dependencies$input_id) %>%
  dplyr::select(input_id, option, nextq)

# assign dependencies
for (qn in sort(unique(dependencies$input_id), decreasing = FALSE)){
  question_range <- dependencies %>%
    dplyr::filter(input_id == qn) %>%
    dplyr::mutate(nextq = as.numeric(stringr::str_extract(nextq, "([0-9]+)")))
  next_option <- question_range %>%
    dplyr::filter(nextq == min(nextq)) %>%
    dplyr::select(option)
  survey_qns <- survey_qns %>%
    dplyr::mutate(
      dependence = ifelse(input_id %in% paste0("Q", min(question_range$nextq):(max(question_range$nextq - 1))),
                          qn,
                          dependence),
      dependence_value = ifelse(input_id %in% paste0("Q", min(question_range$nextq):(max(question_range$nextq - 1))),
                                next_option,
                                dependence_value)
    )
}

# Launch survey --------------------------------------------------------------
ui <- shiny::fluidPage(
  shinysurveys::surveyOutput(df = survey_qns,
               survey_title = "Sweet Summer Child Score (SSCS)",
               survey_description = "SSCS is a scoring mechanism for latent risk. It will help you quickly and efficiently scan for the possibility of harm to people and communities by a socio-technical system. Note that harms to animals and the environment are not considered.
               Please note that all questions are mandatory and you will not be able to submit the survey if there are questions left uncompleted.")
)

server <- function(input, output, session) {
  shinysurveys::renderSurvey()
  shiny::observeEvent(input$submit, {
    shiny::showModal(shiny::modalDialog(
      title = "Congrats, you completed your first shinysurvey!",
      "You can customize what actions happen when a user finishes a survey using input$submit."
    ))
    response_data <- shinysurveys::getSurveyData()
    print(response_data %>%
            dplyr::left_join(survey_qns,
                      by = c("question_id" = "input_id",
                             "response" = "option"  )) %>%
            dplyr::select(question_id, multiplier, score) %>%
            dplyr::summarise(risk = sum(multiplier, na.rm = TRUE) * sum(score, na.rm = T)) %>%
            dplyr::pull(risk) %>%
            cat()
    )
  })
}

shiny::shinyApp(ui, server)
}
