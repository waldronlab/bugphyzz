# Function returns tokens
tokens <- function(df){
  tibble_dat <- as_tibble(df)
  tibble_dat$abstract <- gsub("-", "", tibble_dat$abstract)

  tibble_dat <- tibble_dat %>%
    unnest_tokens(output = word, input = abstract, strip_punct = T) %>%
    anti_join(stop_words, by = "word") %>%
    filter(str_detect(word, "[:alpha:]")) %>%
    distinct()
  return(tibble_dat)
}

# Function replaces synonyms with standard title
# Example: temp <- standardize(dat, "gram", words[[1]], colnames(words)[[1]])
# \\bword\\b
standardize <- function(df, standard, synonyms = standard){
  regString <- paste("\\b", standard, "\\b", sep = "")
  df[[3]] <- gsub("[[:punct:][:blank:]]+", " ", df[[3]])
  df[[3]] <- tolower(df[[3]])

  for (i in 1:length(synonyms)){
    df[[3]] <- gsub(synonyms[i], standard, df[[3]])
  }

  df <- tokens(df) %>% filter(str_detect(word, regString))
  return(df)
}

# Function modifies the bergey's df and substitutes words
sub_word <- function(df, standard, synonyms = standard){
  df[[3]] <- gsub("[[:punct:][:blank:]]+", " ", df[[3]])
  df[[3]] <- tolower(df[[3]])
  for (i in 1:length(synonyms)){
    df[[3]] <- gsub(synonyms[i], standard, df[[3]])
  }
  return(df)
}

# Function retrieves bacteria that need manual curation
# Also writes to csv
extractDuplicate <- function(df){
  df <- df[duplicated(df$genus)|duplicated(df$genus, fromLast=TRUE),]
  df <- df[order(df$genus)]
  return(df)
}

extract_accurate <- function(df){
  df <- df[!(duplicated(df$genus)|duplicated(df$genus, fromLast=TRUE)),]
  df <- df[order(df$genus)]
  return(df)
}
