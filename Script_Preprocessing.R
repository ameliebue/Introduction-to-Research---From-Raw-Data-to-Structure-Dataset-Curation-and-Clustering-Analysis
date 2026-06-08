# From Raw Data to Structure: Dataset Curation and Clustering Analysis
# Introduction to Research - Universitat Politècnica de Catalunya
# Amelie Buescher
# 19.03.2026


library(tidyverse)
# Load the original ESS dataset
raw_dataset <- read_csv("ESS10e03_3.csv")

## STEP 1: FEATURE SELECTION
#Step 1.1: Select variables from the topic "Digital social contacts in work and family life"
digital_all_vars <- c(
  
  # Internet access
  "acchome","accwrk","accmove","accoth","accnone","accref","accdk","accna",
  
  # Digital familiarity
  "fampref","famadvs","fampdf",
  
  # Perceptions of online/mobile communication
  "mcclose","mcinter","mccoord","mcpriv","mcmsinf",
  
  # Child aged 12+
  "chldo12","gndro12a","gndro12b","ageo12","hhlio12","closeo12","ttmino12",
  "speako12","scrno12","phoneo12","como12","c19spo12","c19mco12",
  
  # Parent
  "livpnt","pntmofa","agepnt","hhlipnt","closepnt","ttminpnt",
  "speakpnt","scrnpnt","phonepnt","compnt","c19sppnt","c19mcpnt",
  
  # Work and work-life balance
  "stfmjob","trdawrk","jbprtfp","pfmfdjba","dcsfwrka","wrkhome",
  "c19whome","c19wplch","wrklong","wrkresp","c19whacc",
  "mansupp","manhlp",
  
  # Manager communication
  "manwrkpl","manspeak","manscrn","manphone","mancom",
  
  # Colleagues
  "teamfeel","wrkextra","colprop","colhlp",
  "colspeak","colscrn","colphone","colcom",
  
  # Communication at work
  "c19spwrk","c19mcwrk","mcwrkhom"
)

#Reduce dataset to selected variables
dataset_reduced <- raw_dataset %>%
  select(any_of(digital_all_vars))

#Step 1.2: Recode ESS-specific missing values
dataset_missingValues <- dataset_reduced %>%
  mutate(
    # Missing-Codes: 7 / 8 / 9
    across(any_of(c(
      "fampref","famadvs","fampdf",
      "livpnt"
    )),
    ~ replace(., . %in% c(7, 8, 9), NA)),
    # Missing-Codes: 77 / 88 / 99
    across(any_of(c(
      "mcclose","mcinter","mccoord","mcpriv","mcmsinf",
      "chldo12"
    )),
    ~ replace(., . %in% c(77, 88, 99), NA)),
    # Missing-Codes: 6 / 7 / 8 / 9
    across(any_of(c(
      "gndro12a","gndro12b",
      "hhlio12","closeo12","c19mco12",
      "pntmofa","hhlipnt","closepnt","c19mcpnt",
      "trdawrk","pfmfdjba","dcsfwrka","c19wplch",
      "manhlp","colhlp"
    )),
    ~ replace(., . %in% c(6, 7, 8, 9), NA)),
    # Missing-Codes: 66 / 77 / 88 / 99
    across(any_of(c(
      "speako12","scrno12","phoneo12","como12","c19spo12",
      "speakpnt","scrnpnt","phonepnt","compnt","c19sppnt",
      "stfmjob","jbprtfp","wrkhome","c19whome","wrklong",
      "wrkresp","c19whacc","mansupp",
      "manwrkpl","manspeak","manscrn","manphone","mancom",
      "teamfeel","wrkextra","colprop",
      "colspeak","colscrn","colphone","colcom",
      "c19spwrk","c19mcwrk","mcwrkhom"
    )),
    ~ replace(., . %in% c(66, 77, 88, 99), NA)),
    # Missing-Codes: 6666 / 7777 / 8888 / 9999
    across(any_of(c(
      "ttmino12","ttminpnt","ageo12","agepnt"
    )),
    ~ replace(., . %in% c(6666, 7777, 8888, 9999), NA))
  )

#Step 1.3: Calculate proportion of missing values per variable
missing_per_var <- dataset_missingValues %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "missing_share"
  ) %>%
  arrange(desc(missing_share))

#Keep variables with at most 30% missing values
vars_to_keep <- missing_per_var %>%
  filter(missing_share <= 0.30) %>%
  pull(variable)

#Document variables with more than 30% missing
removed_vars <- missing_per_var %>%
  filter(missing_share > 0.30)

#Reduce dataset to selected variables
dataset_AfterMissingValuesFilter <- dataset_missingValues %>%
  select(all_of(vars_to_keep))



## STEP 2: INSTANCE SELECTION
#Step 2.1: Remove cases with more than 30% missing values
dataset_NA_RowsFilter <- dataset_AfterMissingValuesFilter %>%
  mutate(row_missing = rowMeans(is.na(.))) %>%
  filter(row_missing <= 0.30) %>%
  select(-row_missing)

#Step 2.2: Output summary
cat("Number of variables before filtering:", ncol(dataset_missingValues), "\n")
cat("Number of variables after variable filtering:", ncol(dataset_AfterMissingValuesFilter), "\n")
cat("Removed variables:", ncol(dataset_missingValues) - ncol(dataset_AfterMissingValuesFilter), "\n\n")

cat("Number of cases before filtering:", nrow(dataset_AfterMissingValuesFilter), "\n")
cat("Number of cases after filtering:", nrow(dataset_NA_RowsFilter), "\n")
cat("Removed cases:", nrow(dataset_AfterMissingValuesFilter) - nrow(dataset_NA_RowsFilter), "\n\n")

cat("Variables with more than 30% missing:\n")
print(removed_vars)

#Step 2.3: Save cleaned dataset
write_csv(dataset_NA_RowsFilter, "ess_after_missing_filter.csv")





##STEP 3: Missing Value Imputation 
library(dplyr)
library(caret)
dataset_Imputation <- dataset_NA_RowsFilter %>%
  mutate(across(
    everything(),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

#Check if missing values remain
colSums(is.na(dataset_Imputation))




##STEP 4: Near-Zero-Variance
#Step 4.1: categorize variables (based on codebook)
binary_vars <- intersect(c(
  "acchome","accwrk","accmove","accoth","accnone","accref","accdk","accna"
), names(dataset_Imputation))
ordinal_vars <- intersect(c(
  "fampref","famadvs","fampdf",
  "mcclose","mcinter","mccoord","mcpriv","mcmsinf"
), names(dataset_Imputation))
numeric_vars <- intersect(c(
  "chldo12", "ageo12", "agepnt"
), names(dataset_Imputation))
nominal_vars <- intersect(c(
  "livpnt"
), names(dataset_Imputation))

#Step 4.2: set Datatyp
dataset_Typed <- dataset_Imputation %>%
  mutate(
    across(all_of(binary_vars), ~ factor(.)),
    across(all_of(ordinal_vars), ~ factor(., ordered = TRUE)),
    across(all_of(nominal_vars), ~ factor(.)),
    across(all_of(numeric_vars), ~ as.numeric(.))
  )

#Step 4.3: calculate Near-Zero-Variance 
# numeric
nzv_num_vars <- character(0)
if (length(numeric_vars) > 0) {
  nzv_num <- nearZeroVar(
    dataset_Typed %>% select(all_of(numeric_vars)),
    saveMetrics = TRUE
  )
  nzv_num_vars <- rownames(nzv_num[nzv_num$nzv, ])
}

# categorical 
cat_vars <- c(binary_vars, ordinal_vars, nominal_vars)
nzv_cat_vars <- character(0)
if (length(cat_vars) > 0) {
  nzv_cat <- nearZeroVar(
    dataset_Typed %>% select(all_of(cat_vars)),
    saveMetrics = TRUE
  )
  nzv_cat_vars <- rownames(nzv_cat[nzv_cat$nzv, ])
}

#Step 4.4: merge all variables
nzv_all <- c(nzv_num_vars, nzv_cat_vars)

#Step 4.5: remove variables
dataset_cleaned <- dataset_Typed %>%
  select(-all_of(nzv_all))



# STEP 5: Outlier Detection
# choose only ordinale variables 
valid_ordinal <- intersect(ordinal_vars, names(dataset_cleaned))

# Convert ordinal factors to numeric values for calculation purposes
ordinal_numeric <- dataset_cleaned %>%
  select(all_of(valid_ordinal)) %>%
  mutate(across(everything(), ~ as.numeric(as.character(.))))

#Step 5.1: Straightlining
dataset_Outlier <- dataset_cleaned %>%
  mutate(
    sd_response = apply(ordinal_numeric, 1, sd, na.rm = TRUE)
  )
# very low Variation = potential straightliner
straightliners <- dataset_Outlier %>%
  filter(sd_response < 0.1)
cat("Number Straightliner:", nrow(straightliners), "\n")

#Step 5.2: Extreme Responders
# Calculate the proportion of extreme responses (lowest and highest categories: 1 and 5)
extreme_share <- apply(ordinal_numeric, 1, function(x) {
  mean(x %in% c(1, 5), na.rm = TRUE)
})
# Define a threshold (here: more than 80% extreme responses) and identify corresponding observations
extreme_cases <- dataset_cleaned[extreme_share > 0.8, ]
cat("Number Extreme Responders:", nrow(extreme_cases), "\n")




#STEP 6: Reverse Coding 
reverse_vars <- intersect(
  c("mcinter", "mcpriv", "mcmsinf"),
  names(dataset_cleaned)
)

dataset_cleaned <- dataset_cleaned %>%
  mutate(
    across(
      all_of(reverse_vars),
      ~ 10 - as.numeric(as.character(.))
    )
  )

#Safe cleaned dataset BEFORE preprocessing
write_csv(dataset_cleaned, "Dataset_Cleaned.csv")
View(dataset_cleaned)




##STEP 7: Standardization
valid_numeric <- intersect(numeric_vars, colnames(dataset_cleaned))

dataset_preprocessed <- dataset_cleaned %>%
  mutate(
    across(all_of(valid_numeric), ~ scale(.)[,1])
  )


##STEP 8: Feature Engineering
valid_mc <- intersect(
  c("mcclose","mcinter","mccoord","mcpriv","mcmsinf"),
  colnames(dataset_preprocessed)
)

dataset_preprocessed <- dataset_preprocessed %>%
  mutate(
    mc_perception_index =
      rowMeans(
        mutate(select(., all_of(valid_mc)),
               across(everything(), ~ as.numeric(as.character(.)))),
        na.rm = TRUE
      )
  )

# Final preprocessed dataset
dim(dataset_preprocessed)

#Safe preprocessed dataset
write_csv(dataset_preprocessed, "Dataset_Preprocessed.csv")
View(dataset_preprocessed)