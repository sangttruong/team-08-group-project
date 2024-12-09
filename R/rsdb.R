
#' Expand Dataframe
#'
#' Data wrangling function that helps make variables in a character dataframe legible
#' for further data processing (e.g., by splitting up individual words and their associated
#' weights into further variables).
#' Note that this function is designed for individual document use, not corpus-wide use.
#' This function is later passed into grab_racialmods and grab_sentiment to help expand
#' the initial output from parsed_books, and generate descriptive output from the corpus.
#'
#' @param example_text A single parsed text, e.g., one entry from the list output of getbook().
#' @param col_name The column that should be expanded (e.g., "Modifiers", "Actions", "Possessions", etc.)
#' @return An expanded (wider) version of the inputted text.
#'
#' @export
expand_text <- function(example_text, col_name){

  if(!col_name %in% names(example_text)){
    stop("Select a variable that exists in a df outputted by getbook().")
  }

  col_sym <- sym(col_name)

  expanded_text <- example_text %>%
    select(max_used_name, chara_count, chara_main_gender, !!col_sym) %>%
    mutate(
      Words = ifelse(
        str_detect(!!col_sym, "c\\("), # For characters with multiple modifiers
        str_extract(!!col_sym, "c\\(.*?\\)"),
        str_extract_all(!!col_sym, "[a-zA-Z]+") # For characters with a single modifier
      )
    ) %>%
    mutate(
      Counts = ifelse(
        str_detect(!!col_sym, "c\\C"),
        str_extract(!!col_sym, "(?<=\\), ).*"),
        str_extract_all(!!col_sym, "\\d+")
      )
    ) %>%
    mutate(
      Words = gsub("c\\(|\\)", "", Words),
      Counts =  gsub("c\\(|\\)", "", Counts)
    )

  return(expanded_text)
}



#' Grabbing Racial Modifiers
#'
#'This function grabs the modifiers used to describe the characters in a single text
#'(e.g., one dataframe from the list output of "getbooks").
#'It widens the dataframe for further wrangling, so that users can see whether/how many
#'racializing/ethnographic terms (from the built-in dataframe "rsdb") are used
#'as a modifier to describe any of the characters in a given parsed text.
#'Note that this function takes in a single text at a time:
#'it is not built to be passed onto an entire corpus.
#'
#'@param example_text A dataframe containing character, such as one entry in the list output passed from "getbooks."
#'@param db A referential database containing a glossary of racializing terms. This currently links to the built in dataframe, "rsdb."
#'@return A dataframe containing the character, the racializing term used to describe them (if any),
#'and the number of times this modifier was used.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#'
#' @export
grab_racialmods <- function(example_text, db = "rsdb"){

  expanded_text <- example_text %>%
    expand_text(col_name = "Modifiers")%>%
    mutate(
      Words = gsub("c\\(|\\)", "", Words),
      Counts =  gsub("c\\(|\\)", "", Counts)
    ) %>%
    mutate(Words_split = str_split(Words, ", \\s*") %>%
             lapply(function(x) gsub("\"", "", x))
    )

  expanded_words_df <- expanded_text %>%
    mutate(row_id = row_number()) %>%
    unnest_wider(Words_split, names_sep = "_")

  modifiers_long <- expanded_words_df %>%
    pivot_longer(
      cols = starts_with("Words_split_"), # all modifier columns
      names_to = "Modifier_Column",       # which modifier (1, 2, ... 10) it was
      values_to = "Modifier"              # the actual modifiers
    ) %>%
    filter(!is.na(Modifier)) %>% #note not all characters will have 10 modifiers
    select(max_used_name, Modifier)

  if(db == "rsdb"){

    overlapping_modifiers <- modifiers_long %>%
      filter(Modifier %in% rsdb$Slur |
            Modifier %in% rsdb$Represents)

    summary_results <- overlapping_modifiers %>%
      group_by(max_used_name) %>%
      summarize(
       ` Racializing Modifiers` = paste(unique(Modifier), collapse = ", "), # putting overlapping modifiers into a list
        `Total Overlaps` = n() # Count total overlapping modifiers
      ) %>%
      rename("Character Name" = "max_used_name")

    if(nrow(summary_results) != 0){
      return(summary_results)
    }else{
      print("No racializing modifiers referenced in the RSDB were used to describe any of the ten major characters from this text.")
    }


  }else{
    print("RSDB is the only database available at this time.")
  }

}


