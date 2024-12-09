
#' Sentiment Analysis of Characters -----
#'
#' This function takes in a list of character dataframes (e.g., a list output produced by running
#' "getbooks" on BookNLP-parses), and outputs a modified list of dataframes, in which additional
#' variables describe the average sentiment of a set of words associated with a given character
#' (e.g., actions done on/by, modifiers of the character, etc.), as well as the standard deviation
#' of the sentiments associated with that set of words (e.g., the spread of sentiment used
#' to characterize a character).
#' Note that a "positive" sentiment score indicates that a character is described positively,
#' and a "negative" sentiment score indicates that a character is described negatively.
#'
#' @param parsedbooks A output (type list) generated from running getbook() on a corpus.
#' @param colname The variable (column) in parsedbooks upon which sentiment analysis is conducted.
#' Takes on one of four values ("Actions Done", "Actions Done On", "Possessions", or "Modifiers").
#' @param method The method of sentiment analysis to be conducted.
#'
#' @return A list of modified dataframes containing character data for each text.
#'
#' @import syuzhet
#' @import dplyr
#' @import stringr
#' @export
grab_sentiment <- function(parsedbooks, colname, method = "syuzhet"){

  method_sym <- sym(method)
  sentiment_scores <- list()

  for (i in 1:length(parsedbooks)){

    book <- parsedbooks[[i]]

    words <- expand_text(book, col_name = colname) %>%
      mutate(
        words = str_extract_all(Words, "[a-zA-Z]+"),
        counts = str_extract_all(Counts, "\\d+")
      ) %>%
      rowwise() %>%
      mutate(
        sentimentscores = list(
          get_sentiment(unlist(words), method = as.character(method_sym))
        ),
        weightedsentiment = sum(unlist(sentimentscores) * as.numeric(unlist(counts)),
                                na.rm = TRUE) / sum(as.numeric(unlist(counts)), na.rm = TRUE),
        sentiment_sd = sqrt(
          sum(as.numeric(unlist(counts)) * (unlist(sentimentscores) - weightedsentiment)^2, na.rm = TRUE) /
            sum(as.numeric(unlist(counts)), na.rm = TRUE)
        )
      ) %>%
      ungroup() %>%
      select(max_used_name, chara_count, chara_main_gender,
             words, counts, sentimentscores, weightedsentiment, sentiment_sd) %>%
      mutate(chara_level = as.numeric(rownames(.)))

    sentiment_scores[[i]] <- words
  }

  return(sentiment_scores)
}


#' Plotting Character Sentiment ---------
#'
#' This function plots the sentiments associated with characters for each document within a corpus.
#' In particular, it looks at either the average or spread of sentiment for each character,
#' in relation to that character's majorness or minorness. It effectively allows for users to
#' visualize general changes in how characters are described as they become more or less major.
#'
#' @param sentiment_list The output of a grab_sentiment run. Takes on the form of a list of dataframes, where each df covers a document's characters and the sentiments they are associated with
#' @param measure Either "mean" or "sd." Represents which measure of sentiment is plotted in the resulting visualization (the spread of, or average, sentiment of the character).
#' @param charalevel The number of characters whose descriptions to return. (1 corresponds to returning only the major character; 10 corresponds to returning the 10th most major characters.)
#' @return A ggplot. Note that the x-axis can (and likely should) be constrained to fit a smaller number of characters than there are total in the corpus.
#' @import ggplot2
#' @export
plot_sentiment <- function(sentiment_list, measure, charalevel){

  combined_df <- sentiment_list[[1]] %>%
    mutate(text_no = as.factor(1))
  for (i in 1:(length(sentiment_list) - 1)){
    combined_df <- sentiment_list[[i]] %>%
      mutate(text_no = as.factor(i)) %>%
      rbind(combined_df)
  }

  ggplot(data = combined_df, aes(x = chara_level,
                                 y = if(measure == "mean"){weightedsentiment}
                                 else if(measure == "sd"){sentiment_sd},
                                 color = text_no)) +
    geom_point() +
    labs(color = "# Texts",
         x = "Character Majorness",
         y = if(measure == "mean"){"Sentiment of Character"}
         else if(measure == "sd"){"Diversity in Character Description"},
         title = paste(
           if(measure == "mean"){"Character Sentiment"}
           else if(measure == "sd"){"Range of Character Description"},
           "vs Majorness of Character"
         )
    ) +
    scale_y_continuous(
      breaks = if(measure == "mean"){c(min(combined_df$weightedsentiment),
                                       0,
                                       max(combined_df$weightedsentiment))}
      else if(measure == "sd"){c(0, max(combined_df$sentiment_sd))},
      labels = if(measure == "mean"){c("Negative", "Neutral", "Positive")}
      else if(measure == "sd"){c("Low", "High")}
    ) +
    scale_x_continuous(
      limits = c(1, charalevel),
      breaks = c(1, charalevel),
      labels = c("Major", "Minor")
    )
}

