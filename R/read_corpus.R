
#' Process Files
#'
#' For usage in the "getbook" function.
#' This, very simply, reads in a JSON file.
#' @import jsonlite
#' @param filename Takes in the location of the JSON file
#' @return JSON file as a dataframe
#' @export
proc <- function(filename){
  data <- fromJSON(filename)
  return(data)
}

#' Count Dependencies
#'
#' For usage in the "create character data" function.
#' This function extracts a dependency list from a JSON file,
#' and creates a counter from the terms in the list.
#' @param dep_list list from a read JSON file
#' @return list containing character data
#' @export
get_counter_from_dependency_list <- function(dep_list) {
  counter <- dep_list %>%
    group_by(w) %>%
    summarise(count = n())
  return(counter)
}

#' Create Character Data
#'
#' The key function that will later be called to run "getbook."
#' This function takes in a given .book file and creates character data from the .book file.
#' Create a data dictionary for this--but essentially returns character's coreference ID,
#' number of times the character appears, their name, inferred gender, things they've done
#' (as an "agent"), things done upon them (as a "patient"), things they possess (whether
#' that's a material thing or quality, like a necklace, family, or intelligence),
#' modifiers/descriptors of the character.
#' @param data Large list (list of dfs, dependency parsing list) containing data of the JSON file per text
#' @param printTop Top x number of actions/modifiers/possessions to return
#' @return Large dataframe containing character level data per text
#' @export
create_character_data <- function(data, printTop) {

  chara_data <- data$characters
  character_data <- list()

  for (character in 1:nrow(chara_data)) {

    agentList <- chara_data$agent[[character]]
    patientList <- chara_data$patient[[character]]
    possList <- chara_data$poss[[character]]
    modList <- chara_data$mod[[character]]

    coref_id <- chara_data$id[[character]]
    chara_count <- chara_data$count[[character]] #number times the character appears

    chara_main_gender <- "unknown"
    chara_gender_distribution <- "unknown"

    if (!is.null(chara_data$g)) { #basically, if pronoun analysis exists
      chara_gender_distribution <- chara_data$g$inference[character, ] #again: distribution of pronoun usage
      chara_main_gender <- chara_data$g$argmax[[character]] #most frequently used set of pronouns
    }

    names <- chara_data$mentions$proper[[character]] #best way to 'name' characters since this is in descending order. believe n refers to the name itself, c refers to the number of times it appears
    max_used_name <- ""

    # Making empty lists for items
    agent_items <- list() #actions done by character
    patient_items <- list() #actions done upon character
    poss_items <- list() #things the character 'has'--body parts, family members
    mod_items <- list() #modifiers--descriptors applied to the character

    # Print out information about named characters
    if (length(names) > 0 &&  #this is where the difference is to adjust for the bug on Diaz + Farming of Bones. Check this, and apply for this select number of books to analyze farming of bones through diaz books
        nrow(agentList) != 0 &&
        nrow(patientList) != 0 &&
        nrow(possList) != 0 &&
        nrow(modList) != 0) {

      max_used_name <- names$n[[1]]

      agent_counter <- get_counter_from_dependency_list(agentList)
      patient_counter <- get_counter_from_dependency_list(patientList)
      poss_counter <- get_counter_from_dependency_list(possList)
      mod_counter <- get_counter_from_dependency_list(modList)

      agent_items <- agent_counter %>% arrange(desc(count)) %>% head(printTop) %>% as.list()
      patient_items <- patient_counter %>% arrange(desc(count)) %>% head(printTop) %>% as.list()
      poss_items <- poss_counter %>% arrange(desc(count)) %>% head(printTop) %>% as.list()
      mod_items <- mod_counter %>% arrange(desc(count)) %>% head(printTop) %>% as.list()

      character_data[[character]] <- list(
        coref = coref_id,
        chara_count = chara_count,
        max_used_name = max_used_name,
        chara_main_gender = chara_main_gender,
        #chara_gender_distribution = chara_gender_distribution,
        agentList = agent_items,
        patientList = patient_items,
        possList = poss_items,
        modList = mod_items
      )
    }
  }

  return(character_data)
}


#' Flatten Lists
#'
#' For usage in the "getbook function." Standardizes list elements to ensure lists have the
#' same structure before conversion into a dataframe.
#' @param x list containing character data
#' @return cleaned list rest for conversion into df
#' @export
flatten_list <- function(x) {
  list(
    coref = x$coref,
    chara_count = x$chara_count,
    max_used_name = x$max_used_name,
    chara_main_gender = x$chara_main_gender,
    agentList = paste(x$agentList, collapse = ", "),
    patientList = paste(x$patientList, collapse = ", "),
    possList = paste(x$possList, collapse = ", "),
    modList = paste(x$modList, collapse = ", ")
  )
}

#' Get Book Data
#'
#' This function essentially applies the create_character_data function to all files
#' within your corpus, extracting only the .book files
#' @param filepath Filepath containing .book files (scripts parsed by BookNLP on corpus)
#' @param outputpath Filepath under which output will be stored
#' @return Returns a list of dataframes, where each dataframe corresponds to atttributes of all characters within a text
#' @export
getbook <- function(filepath, outputpath = NULL) {
  local_directory <- filepath
  book_files <- list.files(path = local_directory, pattern = "\\.book", full.names = TRUE)

  output_dir <- outputpath
  list_of_char_analyses <- list()

  for (file_path in book_files) {
    book_data <- proc(file_path)

    book_character_results <- book_data %>%
      create_character_data(10) %>%
      lapply(flatten_list) %>%
      bind_rows() %>%
      na.omit() %>%
      rename("Actions Done" = "agentList",
             "Actions Done On" = "patientList",
             "Posessions" = "possList",
             "Modifiers" = "modList")

    list_of_char_analyses[[basename(file_path)]] <- book_character_results

    if (!is.null(outputpath)) {
      new_file_name <- file.path(outputpath, paste0(basename(file_path), "_processed.csv"))
      write.csv(book_character_results, new_file_name, row.names = FALSE)
    }
  }

  # Return the compiled list of character analyses
  return(list_of_char_analyses)
}


#' Get Tokens
#'
#' Function to read in the .tokens files associated with each BookNLP parsed text.
#' The .tokens file simply contains details (part of speech, lemmatized form, link to character)
#' about each token (word or punctuation mark) in the text.
#' This function aims to simply gather all the .tokens files, and store them in one object ("token_files").
#' The output to this function can then be used if somebody is interested in the plain text of a document,
#' rather than the BookNLP parse of the document.
#' @param filepath File path of directory containing BookNLP parses/document ending in file type ".tokens"
#' @return List of dataframes
#' @export
gettokens <- function(filepath){
  local_directory <- filepath
  token_files <- list.files(path = local_directory, pattern = "\\.tokens", full.names = TRUE)
  read_tokens_file <- function(file_path){
    read.csv(file_path, sep = '\t')
  }
  token_files <- map(token_files, read_tokens_file)
}




