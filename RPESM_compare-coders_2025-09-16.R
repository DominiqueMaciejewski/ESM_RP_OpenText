#############################################################
######################## Comparing codes ####################
#############################################################

# LAST UPDATED: SEPTEMBER 16, 2025
# Added sorting the responses alphabetically because otherwise there may not be a full agreement if order of responses was not the same

# UPDATED: JULY 11, 2025
# Added the exact response process answer to the consensus file
# Added which codes are matching and which are unique to each coder

### Library
library(dplyr)
library(tidyr) #NEW
library(writexl)
library(here)
library(stringr) #NEW
library(purrr) #NEW

### Preparation ----

# Set participant ID and coders
participant_ID <- "XXX"      # Change to your participant's ID
namecoder1 <- "XXX"        # Change to coder 1 name
namecoder2 <- "XXX"       # Change to coder 2 name

#  Read in coded responses 
coder1_file <- paste0(namecoder1, "/Act_", participant_ID, ".csv")
coder2_file <- paste0(namecoder2, "/Act_", participant_ID, ".csv")

coder1_ID <- read.csv(here(coder1_file))
coder2_ID <- read.csv(here(coder2_file))

# Rename variables and merge 
coder1_ID <- coder1_ID %>%
  select(X, Code, Comments..general,Comments..depth) %>%
  rename(ID = X, Code_coder1 = Code, Comments_general_coder1 = Comments..general, Comments_depth_coder1=Comments..depth)

coder2_ID <- coder2_ID %>%
  select(Code,Comments..general,Comments..depth) %>%
  rename(Code_coder2 = Code, Comments_general_coder2 = Comments..general, Comments_depth_coder2=Comments..depth)

coder12_ID <- cbind(coder1_ID, coder2_ID)

### Agreement ---- NEW SORTING RESPONSES

# Function to sort and trim codes
sort_and_trim <- function(code_string) {
  if (is.na(code_string)) return(NA)
  code_string %>%
    str_split(";") %>%
    unlist() %>%
    str_trim() %>%
    sort() %>%
    paste(collapse = "; ")
}

# Apply sorting to both coder columns and compute overall agreement
coder12_ID <- coder12_ID %>%
  mutate(
    sorted_coder1 = map_chr(Code_coder1, sort_and_trim),
    sorted_coder2 = map_chr(Code_coder2, sort_and_trim),
    overall_agreement = sorted_coder1 == sorted_coder2
  )


# Identify which responses overlap and which are unique
coder12_ID <- coder12_ID %>%
  mutate(
    list1 = str_split(Code_coder1, ";") %>% map(~str_trim(.)),
    list2 = str_split(Code_coder2, ";") %>% map(~str_trim(.)),
    
    # Overlapping codes
    overlap = map2(list1, list2, ~ intersect(.x, .y)) %>% map_chr(~ paste(sort(.x), collapse = "; ")),
    
    # Unique to column 1
    unique_coder1 = map2(list1, list2, ~ setdiff(.x, .y)) %>% map_chr(~ paste(sort(.x), collapse = "; ")),
    
    # Unique to column 2
    unique_coder2 = map2(list2, list1, ~ setdiff(.x, .y)) %>% map_chr(~ paste(sort(.x), collapse = "; "))
  ) %>%
  select(-c(list1, list2, sorted_coder1, sorted_coder2))


# Calculate agreement 

## Overall (if all codes match exactly)
coder12_ID$overall_agreement_percentage <- mean(coder12_ID$overall_agreement, na.rm = TRUE) * 100
print(paste0("Agreement Percentage: ", coder12_ID$overall_agreement_percentage, "%"))

## Partial (taking into account that there are multiple codes)

# Function to compute Jaccard similarity
jaccard_similarity <- function(c1, c2) {
  s1 <- strsplit(c1, ";")[[1]] %>% trimws()
  s2 <- strsplit(c2, ";")[[1]] %>% trimws()
  
  intersection <- length(intersect(s1, s2))
  union <- length(union(s1, s2))
  
  if (union == 0) return(NA)
  return(intersection / union)
}

# Apply Jaccard similarity to each row
coder12_ID$partial_agreement <- mapply(jaccard_similarity, coder12_ID$Code_coder1, coder12_ID$Code_coder2)

# Mean partial agreement percentage
coder12_ID$partial_agreement_percentage <- mean(coder12_ID$partial_agreement, na.rm = TRUE) * 100
print(paste0("Partial Agreement Percentage: ", coder12_ID$partial_agreement_percentage, "%"))

# add columns Consensus coder (taken from the overlapping responses) and comments consensus
coder12_ID$consensus_coder <- coder12_ID$overlap
coder12_ID$comments_consensus <- ""

### Extract RP data ----

## NEW: extract RP from the .RData file

# Read in data
load("./RPESM_MPath_Qualtrics_clean_2025-05-09.RData") # DON'T CHANGE

# Extract the response process answer for the participant
target_participant <- participant_ID

data_RP <- data %>%
  filter(participant_ID == target_participant) %>%
  select(happy_RP, relaxed_RP, down_RP, irritated_RP, nervous_RP) %>%
  filter(if_any(ends_with("_RP"), ~ !is.na(.)))

data_long <- data_RP %>%
  pivot_longer(
    cols = everything(),       # all 5 columns
    names_to = "Emotion", # new column to hold the original column names
    values_to = "RP"        # new column to hold the values
  ) %>%
  filter(!is.na(RP)) %>%
  mutate(Emotion = sub("_RP", "", Emotion))

coder12_ID <- cbind(coder12_ID$ID, data_long, coder12_ID[,2:16])
colnames(coder12_ID)[1] <- "ID" # Rename the first column to ID

# Convert to UTF-8, otherwise characters like ï for geïrriteerd are not properly written
coder12_ID$RP <- iconv(coder12_ID$RP, from = "", to = "UTF-8", sub = "byte")

### Save ----

# save as excel file 
coder12_file <- paste0("Consensus/Consensus_", participant_ID, ".xlsx")

write_xlsx(coder12_ID, coder12_file)

