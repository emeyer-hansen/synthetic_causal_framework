# Title: How to Apply the Synthetic Causal Framework

# Author: Emil Meyer-Hansen (MSc., Political Science, Aarhus University, Denmark)
# Email: emil098meyerhansen@gmail.com
# File Created: 2024-9-10
# Notes: This file is an addendum to the author's PhD application 'Does Sociosexuality Affect Political Beliefs? The Development, Validation, and Application of a Novel Causal Framework to solve
# the Non-Manipulable Nature of Sociosexuality using Synthetic Respondents' at Aarhus University (submitted 2023-9-5).
# This file was originally uploaded to the author's github: https://github.com/emeyer-hansen

# System specifications:
# OS: Windows 10 x64 (build 19045)
# R: Version 4.4.1 (2024-06-14 ucrt)
# RStudio: 2024.04.2+764 (Chocolate Cosmos)




# Setup -------------------------------------------------------------------

require(pacman) # v0.5.1
pacman::p_load(
  tidyverse, # v2.0.0
  openai, # v0.4.1
  lme4, # v1.1.35.4
  scales, # v1.3.0
  scrutiny # v0.4.0
)


# Seed for RNG
SEED <- 1
set.seed(SEED)


# Formatting
digits <- 3
big_mark <- ","


# User's Unique OpenAI API Key
OPENAI_API_KEY <- ""


# Maximum length of GPT output
max_tokens <- list(
  life_history = 500, # The larger this value, ceteris paribus, the longer the life-history.
  outcome = 20        # Max length of the outcome response.
)


# GPT's Response randomness (scale: 0-1)
temperature <- list(
  life_history = .5, # Moderate randomness, ~50% reliability
  outcome = .1 # Low randomness, ~90% reliability
)
temperature <- lapply(temperature, function(x) x*2) # Converts temperature to GPT's 0-2 scale.


# Pause duration between calling the GPT model to avoid stressing the system
pause_duration <- 3.1


# GPT Models ----------------------------------------------------------

# Load overview of GPT models
gpt_models <- readr::read_csv("gpt_models.csv")
# View(gpt_models)


gpt_models$model[min(gpt_models$price_avg_per_1m_tokens_dollar_us, na.rm = TRUE) == gpt_models$price_avg_per_1m_tokens_dollar_us]
# At the time of writing, the GPT-model "gpt-4o-mini" is the cheapest on average


selected_gpt_model <- "gpt-4o-mini"





# Simulated Data ----------------------------------------------------------

# Load simulated US respondents, whose characteristics are determined using census data to be in proportion to the US population.
df <- readr::read_csv("simulated_100sample_us.csv")
# View(df)
# dim(df)


# Background characteristics
# colnames(df)
# "state"            "sex"              "race"             "age"              "income_household" "education"        "political_party"


# How many respondents to use
sample_size <- 5
df <- df[1:sample_size,]


# Initialize treatment column
df$treatment <- NA


# Respondent IDs
df$id <- 1:nrow(df)





# Life History ------------------------------------------------------------

# Initialize Columns
df$life_history_gpt <- NA
df$life_history_id_gpt <- NA
df$life_history_object_gpt <- NA
df$life_history_created_gpt <- NA
df$life_history_model_gpt <- NA
df$life_history_prompt_tokens <- NA
df$life_history_completion_tokens <- NA
df$life_history_logprobs <- NA
df$life_history_finish_reason <- NA
df$life_history_message.refusal <- NA
df$life_history_message.role <- NA


# Construct prompt
messages_life_history <- list()
messages_life_history[[1]] <- list(
  "role" = "system",
  "content" = "You are a human simulator."
)
messages_life_history[[2]] <- list(
  "role" = "user",
  "content" = NA
)


# Call the selected GPT-model through OpenAI's API to generate life-histories for the simulated respondents
pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
for(i in 1:nrow(df)){

  # Pause
  if(i > 1){
    Sys.sleep(pause_duration)
  }

  # Adjust prompt to respondent characteristics
  messages_life_history[[2]][2] <- list(paste0(
    "You are to act as a character and must remain in character for the purposes of a roleplay scenario. 'You' and 'your' thus refer to your character from now on. Your character has the following characteristics:
    Country: United States of America (USA)
    State: ", df$state[i], "
    Race: ", df$race[i], "
    Sex: ", df$sex[i], "
    Age: ", df$age[i], "
    Household Income: ", df$income_household[i], "
    Education: ", df$education[i], "
    Political Party Affiliation: ", df$political_party[i], "
    Be creative but realistic and compose a brief life-history for your character based on the provided characteristics:"
  ))

  # Call GPT model (and retry if encountering an error)
  while(TRUE){
    tryCatch(
      expr = {
        gpt_output <- openai::create_chat_completion(
          model = selected_gpt_model,
          messages = messages_life_history,
          max_tokens = max_tokens$life_history,
          temperature = temperature$life_history,
          openai_api_key = OPENAI_API_KEY
        )
      },
      error = function(e){
        print(paste("Error encountered. Error message:", e))
        print(" Going to sleep for 10 seconds ...")
        Sys.sleep(time = 10)
      }
    )
    break
  }


  # Store Life History
  df$life_history_gpt[i] <- gpt_output$choices$message.content


  # Store Utility info
  df$life_history_id_gpt[i] <- gpt_output$id
  df$life_history_object_gpt[i] <- gpt_output$object
  df$life_history_created_gpt[i] <- gpt_output$created
  df$life_history_model_gpt[i] <- gpt_output$model
  df$life_history_prompt_tokens[i] <- gpt_output$usage$prompt_tokens
  df$life_history_completion_tokens[i] <- gpt_output$usage$completion_tokens
  df$life_history_logprobs[i] <- gpt_output$choices$logprobs
  df$life_history_finish_reason[i] <- gpt_output$choices$finish_reason
  df$life_history_message.refusal[i] <- gpt_output$choices$message.refusal
  df$life_history_message.role[i] <- gpt_output$choices$message.role


  # Update progress bar
  setTxtProgressBar(pb, i)
}


# Convert Unix Timestamp to Datatime
df$life_history_datetime <- as_datetime(df$life_history_created_gpt)


# Examine a random life-history
glimpse(df$life_history_gpt)


# I recommend inspecting these to assess there were issues with the GPT model:
glimpse(df$life_history_finish_reason) # "stop" is fine; "length" indicates that response exceeded max_tokens
glimpse(df$life_history_message.refusal) # this should typically equal "NA"






# Outcomes ----------------------------------------------------------------

# Randomized treatment assignment
df$treatment <- rbinom(n = nrow(df), size = 1, prob = .5)


# Initialize Columns
df$outcome_gpt <- NA
df$outcome_id_gpt <- NA
df$outcome_object_gpt <- NA
df$outcome_created_gpt <- NA
df$outcome_model_gpt <- NA
df$outcome_prompt_tokens <- NA
df$outcome_completion_tokens <- NA
df$outcome_logprobs <- NA
df$outcome_finish_reason <- NA
df$outcome_message.refusal <- NA
df$outcome_message.role <- NA


# Generate treatment and control prompt (taken from Aarøe & Petersen, 2014)
prompt_treatment <- paste0(
  "One day you decide to participate in a survey. You are informed that the responsible researchers are professional and impartial and care about your honest opinions. You first answer questions regarding your background and are then greeted with the following scenario:
  “Imagine a man who is currently on social welfare. He has never had a regular job, but he is fit and healthy. He is not motivated to get a job.’’

  You take a second to properly imagine this man before clicking to the next page, which reads:
  'To what extent do you disagree or agree that the eligibility requirements for social welfare should be tightened for persons like him?'

  You see that your response options are:
  1 Strongly disagree
  2 Disagree
  3 Neither agree nor disagree
  4 Agree
  5 Strongly agree
  99 Don't know

  Your short answer:"
)
prompt_control <- paste0(
  "One day you decide to participate in a survey. You are informed that the responsible researchers are professional and impartial and care about your honest opinions. You first answer questions regarding your background and are then greeted with the following scenario:
  'Imagine a man who is currently on social welfare. He has always had a regular job, but has now been the victim of a work-related injury. He is very motivated to get back to work again'

  You take a second to properly imagine this man before clicking to the next page, which reads:
  'To what extent do you disagree or agree that the eligibility requirements for social welfare should be tightened for persons like him?'

  You see that your response options are:
  1 Strongly disagree
  2 Disagree
  3 Neither agree nor disagree
  4 Agree
  5 Strongly agree
  99 Don't know

  Your short answer:"
)


# Call the selected GPT-model through OpenAI's API to generate outcomes for the simulated respondents
pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
for(i in 1:nrow(df)){

  # Pause
  if(i > 1){
    Sys.sleep(pause_duration)
  }


  # Adjust prompt to respondent life-history
  message_history <- list(
    list(
      "role" = "system",
      "content" = "You are a human survey respondent."
    ),
    list(
      "role" = "assistant",
      "content" = df$life_history_gpt[i]
    ),
    list(
      "role" = "user",
      "content" = ifelse(
        df$treatment[i] == 1, prompt_treatment, prompt_control
      )
    )
  )

  # Call GPT model (and retry if encountering an error)
  while(TRUE){
    tryCatch(
      expr = {
        gpt_output <- openai::create_chat_completion(
          model = selected_gpt_model,
          messages = message_history,
          max_tokens = max_tokens$outcome,
          temperature = temperature$outcome,
          openai_api_key = OPENAI_API_KEY
        )
      },
      error = function(e){
        print(paste("Error encountered. Error message:", e))
        print(" Going to sleep for 10 seconds ...")
        Sys.sleep(time = 10)
      }
    )
    break
  }


  # Store Outcome
  df$outcome_gpt[i] <- gpt_output$choices$message.content


  # Store Utility info
  df$outcome_id_gpt[i] <- gpt_output$id
  df$outcome_object_gpt[i] <- gpt_output$object
  df$outcome_created_gpt[i] <- gpt_output$created
  df$outcome_model_gpt[i] <- gpt_output$model
  df$outcome_prompt_tokens[i] <- gpt_output$usage$prompt_tokens
  df$outcome_completion_tokens[i] <- gpt_output$usage$completion_tokens
  df$outcome_logprobs[i] <- gpt_output$choices$logprobs
  df$outcome_finish_reason[i] <- gpt_output$choices$finish_reason
  df$outcome_message.refusal[i] <- gpt_output$choices$message.refusal
  df$outcome_message.role[i] <- gpt_output$choices$message.role


  # Update progress bar
  setTxtProgressBar(pb, i)
}


# Convert Unix Timestamp to Datatime
df$outcome_datetime <- as_datetime(df$outcome_created_gpt)


# Examine outcomes
glimpse(df$outcome_gpt)


# I recommend inspecting these to assess there were issues with the GPT model:
glimpse(df$outcome_finish_reason) # "stop" is fine; "length" indicates that response exceeded max_tokens
glimpse(df$outcome_message.refusal) # this should typically equal "NA"




# Counterfactual Outcome --------------------------------------------------

# Initialize Columns
df$outcome_counterfactual_gpt <- NA
df$outcome_counterfactual_id_gpt <- NA
df$outcome_counterfactual_object_gpt <- NA
df$outcome_counterfactual_created_gpt <- NA
df$outcome_counterfactual_model_gpt <- NA
df$outcome_counterfactual_prompt_tokens <- NA
df$outcome_counterfactual_completion_tokens <- NA
df$outcome_counterfactual_logprobs <- NA
df$outcome_counterfactual_finish_reason <- NA
df$outcome_counterfactual_message.refusal <- NA
df$outcome_counterfactual_message.role <- NA


# Call the selected GPT-model through OpenAI's API to generate counterfactual outcomes for the simulated respondents
pb <- txtProgressBar(min = 0, max = nrow(df), style = 3)
for(i in 1:nrow(df)){

  # Pause
  if(i > 1){
    Sys.sleep(pause_duration)
  }


  # Adjust prompt to respondent life-history
  message_history <- list(
    list(
      "role" = "system",
      "content" = "You are a human survey respondent."
    ),
    list(
      "role" = "assistant",
      "content" = df$life_history_gpt[i]
    ),
    list(
      "role" = "user",
      "content" = ifelse(
        df$treatment[i] == 0, prompt_treatment, prompt_control
      )
    )
  )


  # Call GPT model (and retry if encountering an error)
  while(TRUE){
    tryCatch(
      expr = {
        gpt_output <- openai::create_chat_completion(
          model = selected_gpt_model,
          messages = message_history,
          max_tokens = max_tokens$outcome,
          temperature = temperature$outcome,
          openai_api_key = OPENAI_API_KEY
        )
      },
      error = function(e){
        print(paste("Error encountered. Error message:", e))
        print(" Going to sleep for 10 seconds ...")
        Sys.sleep(time = 10)
      }
    )
    break
  }


  # Store Counterfactual Outcome
  df$outcome_counterfactual_gpt[i] <- gpt_output$choices$message.content


  # Store Utility info
  df$outcome_counterfactual_id_gpt[i] <- gpt_output$id
  df$outcome_counterfactual_object_gpt[i] <- gpt_output$object
  df$outcome_counterfactual_created_gpt[i] <- gpt_output$created
  df$outcome_counterfactual_model_gpt[i] <- gpt_output$model
  df$outcome_counterfactual_prompt_tokens[i] <- gpt_output$usage$prompt_tokens
  df$outcome_counterfactual_completion_tokens[i] <- gpt_output$usage$completion_tokens
  df$outcome_counterfactual_logprobs[i] <- gpt_output$choices$logprobs
  df$outcome_counterfactual_finish_reason[i] <- gpt_output$choices$finish_reason
  df$outcome_counterfactual_message.refusal[i] <- gpt_output$choices$message.refusal
  df$outcome_counterfactual_message.role[i] <- gpt_output$choices$message.role


  # Update progress bar
  setTxtProgressBar(pb, i)
}


# Convert Unix Timestamp to Datatime
df$outcome_counterfactual_datetime <- as_datetime(df$outcome_counterfactual_created_gpt)


# Examine a random counterfactual outcome
glimpse(df$outcome_counterfactual_gpt)


# I recommend inspecting these to assess there were issues with the GPT model:
glimpse(df$outcome_counterfactual_finish_reason) # "stop" is fine; "length" indicates that response exceeded max_tokens
glimpse(df$outcome_counterfactual_message.refusal) # this should typically equal "NA"




# Difference-in-means estimation ------------------------------------------

# Convert (counterfactual) outcomes to numerical data
df$outcome_gpt <- as.numeric(unlist(stringr::str_extract_all(df$outcome_gpt, "\\d")))
df$outcome_counterfactual_gpt <- as.numeric(unlist(stringr::str_extract_all(df$outcome_counterfactual_gpt, "\\d")))


# Distinguish whether treatment/control condition that reflects the outcome and counterfactual outcome
df$outcome_control <- ifelse(
  df$treatment == 0, df$outcome_gpt,
  ifelse(
    df$treatment == 1, df$outcome_counterfactual_gpt, NA
  )
)
df$outcome_treatment <- ifelse(
  df$treatment == 1, df$outcome_gpt,
  ifelse(
    df$treatment == 0, df$outcome_counterfactual_gpt, NA
  )
)


# Individual Treatment Effect (ITE)
df$ite <- df$outcome_treatment-df$outcome_control


# Convert dataframe to long format
df_long <- df[,c("id", "outcome_control", "outcome_treatment")] %>% pivot_longer(
  cols = c(outcome_control, outcome_treatment),
  names_to = "treatment",
  values_to = "outcome",
  values_drop_na = TRUE
)
df_long$treatment <- ifelse(
  df_long$treatment == "outcome_control", "Control",
  ifelse(
    df_long$treatment == "outcome_treatment", "Treatment", NA
  )
)
df_long$id <- as.factor(df_long$id)
df_long$treatment <- factor(df_long$treatment, levels = c("Control", "Treatment"))


# Normalize outcome to 0-1 scale
df_long$outcome <- scales::rescale(df_long$outcome, to = c(0, 1))


# Average Treatment Effect (ATE)
summary(lme4::lmer(
  outcome ~ treatment + (1 | id),
  data = df_long
))



# Total costs
df$input_tokens_used <- rowSums(df[,str_detect(colnames(df), "prompt_tokens")])
df$output_tokens_used <- rowSums(df[,str_detect(colnames(df), "completion_tokens")])

df$input_tokens_cost <- (df$input_tokens_used/1000000)*gpt_models$price_input_per_1m_tokens_dollar_us[gpt_models$model == selected_gpt_model]
df$output_tokens_cost <- (df$output_tokens_used/1000000)*gpt_models$price_output_per_1m_tokens_dollar_us[gpt_models$model == selected_gpt_model]

paste0("Total costs (US$): ", format(scrutiny::round_down(sum(c(df$input_tokens_cost, df$output_tokens_cost)), digits = digits), big.mark = big_mark))





# References --------------------------------------------------------------

# Aarøe, L. & M. B. Petersen (2014): ’Crowding Out Culture: Scandinavians and Americans Agree on Social Welfare in the Face of Deservingness Cues’, The Journal of Politics, 76(3): 684-697. DOI: 10.1017/S002238161400019X

# Douglas B., M. Maechler, B. Bolker & S. Walker (2015): 'Fitting Linear Mixed-Effects Models Using lme4', Journal of Statistical Software, 67(1): 1-48. DOI: 10.18637/jss.v067.i01

# Jung, L. (2024): scrutiny: Error Detection in Science, R package version 0.4.0, on the Comprehensive R Archive Network: (https://CRAN.R-project.org/package=scrutiny).

# Rinker, T. W. & D. Kurkiewicz (2017): pacman: Package Management for R. version 0.5.0, on GitHub: (http://github.com/trinker/pacman).

# Rudnytskyi, I. (2023): openai: R Wrapper for OpenAI API, R package version 0.4.1, on the Comprehensive R Archive Network: (https://CRAN.R-project.org/package=openai).

# Wickham, H., M. Averick, J. Bryan, W. Chang, L. D. McGowan, R. François, G. Grolemund, A. Hayes, L. Henry, J. Hester, M. Kuhn, T, L. Pedersen, E. Miller, S. M. Bache, K. Müller, J. Ooms, D. Robinson, D. P. Seidel, V. Spinu, K. Takahashi, D. Vaughan, C. Wilke, K. Woo & H. Yutani (2019): 'Welcome to the tidyverse', Journal of Open Source Software, 4(43): 1686. DOI: 10.21105/joss.01686

# Wickham, H., T. Pedersen & D. Seidel (2023): scales: Scale Functions for Visualization, R package version 1.3.0, on the Comprehensive R Archive Network: (https://CRAN.R-project.org/package=scales).
