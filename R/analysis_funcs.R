#' Calculate and Visualize TF-IDF Scores
#'
#' This function calculates TF-IDF (Term Frequency-Inverse Document Frequency) scores
#' for terms in a corpus and visualizes the results as a column plot grouped by a metadata variable.
#'
#' @param filepath Character. The path to the directory containing `.tokens` files.
#' @param group Character. The column name in the metadata used to group the corpus (e.g., "time" or "author").
#' @param number_groups Integer. The number of groups to divide the metadata into (default: 2).
#' @param terms Character vector. The terms for which TF-IDF scores are calculated.
#'
#' @return A data frame containing the TF-IDF scores for each term in each group.
#' Additionally, a column plot is displayed showing TF-IDF scores by group.
#'
#' @import dplyr tidyr tidytext tm ggplot2 quanteda stringr
#' @export

tf_idf <- function(filepath, group = "time", number_groups = 2, terms) {
  # Load metadata

  metadata <- read.csv(file.path(filepath, "CHICAGO_CORPUS_NOVELS.csv"))

  # Step 1: Locate texts in the filepath and associate with metadata
  files <- list.files(filepath, pattern = "\\.tokens$", full.names = TRUE)
  filenames <- basename(files)
  filenames_no_extension <- sub("\\.tokens$", "", filenames)

  # Check the column name in your metadata
  if (!"FILENAME" %in% colnames(metadata)) {
    stop("The metadata does not have a column named 'FILENAME'. Please check your metadata file.")
  }
  text_filenames = metadata[["FILENAME"]]
  text_filenames_no_extension <- sub("\\.txt$", "", text_filenames)
  indices <- match(unlist(filenames_no_extension), unlist(text_filenames_no_extension))
  indices_vector <- unlist(indices)
  indices_vector <- indices_vector[!is.na(indices_vector)]

  # Filter the metadata using the indices
  metadata <- metadata[indices_vector, ]

  if (nrow(metadata) == 0) {
    stop("No matching files found in the metadata.")
  }

  # Step 2: Divide corpus into groups based on the specified variable
  if (!(group %in% colnames(metadata))) {
    stop(paste("Group variable", group, "not found in metadata."))
  }

  # Create groups
  if (number_groups > 1) {
    metadata <- metadata %>%
      mutate(group = cut(as.numeric(get(group)), breaks = number_groups, labels = FALSE))
  } else {
    metadata <- metadata %>%
      mutate(group = get(group))
  }

  # Step 3: Process each group and compute tf-idf
  results <- metadata %>%
    group_by(group) %>%
    do({
      group_data <- .
      texts <- lapply(seq_along(group_data$FILENAME), function(i) {
        file <- group_data$FILENAME[i]
        group_value <- group_data$group[i]
        document_id <- sub("\\.txt$", "", file) # Unique identifier per file
        filepath_full <- file.path(filepath, file)
        filepath_tokens <- sub("\\.txt$", ".tokens", filepath_full)

        if (file.exists(filepath_tokens)) {
          tokens <- read.delim(filepath_tokens, header = TRUE) %>%
            pull(word) %>%
            tolower() %>%
            str_remove_all("[[:punct:]]") %>%
            str_remove_all("[[:digit:]]") %>%
            removeWords(stopwords("en"))
          data.frame(word = tokens, document = document_id, group = group_value)
        } else {
          print(paste("File does not exist:", filepath))
          data.frame(word = character(), document = character(), group = character())
        }
      })

      # Combine all token data into one data frame
      word_data <- do.call(rbind, texts)

      # Calculate term frequency and tf-idf
      dtm <- word_data %>%
        count(document, word, name = "n") %>%
        bind_tf_idf(term = word, document = document, n = n) %>%
        filter(word %in% terms)

      # Aggregate tf-idf scores by group
      dtm_grouped <- dtm %>%
        left_join(metadata %>% mutate(document = sub("\\.txt$", "", FILENAME)) %>% select(document, group), by = "document") %>%
        group_by(group, word) %>%
        summarise(tf_idf = mean(tf_idf, na.rm = TRUE)) %>%

        return(dtm_grouped)
    }) %>%
    unnest(cols = c())

  # Step 4: Visualize results as a column plot
  if (nrow(results) > 0) {
    ggplot(results, aes(x = factor(group), y = tf_idf, fill = word)) +
      geom_col(position = "dodge") + # Use geom_col for column plot
      theme_minimal() +
      labs(title = "TF-IDF Scores by Group",
           x = "Group",
           y = "TF-IDF",
           fill = "Term") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    print("No results to visualize. Ensure terms match tokens in the documents.")
  }

  return(results)
}


#' Analyze Collocates in a Corpus
#'
#' This function identifies collocates of specified terms in a corpus by searching
#' for words that occur within a specified window (`horizon`) around the target terms.
#'
#' @param filepath Character. The path to the directory containing `.tokens` files.
#' @param terms Character vector. The terms for which collocates are identified.
#' @param horizon Integer. The number of words before and after the target term to include in the window (default: 5).
#'
#' @return A data frame containing collocates for each term, along with their counts and significance p-values.
#'
#' @import dplyr tidyr stringr
#' @export

grab_collocates <- function(filepath, terms, horizon) {
  # Step 1: Process Corpus
  files <- list.files(filepath, pattern = "\\.tokens$", full.names = TRUE)

  if (length(files) == 0) {
    stop("No .tokens files found in the specified filepath.")
  }

  # Combine all .tokens files into one data frame
  corpus <- lapply(files, function(file) {
    read.delim(file, header = TRUE, quote = "") %>%  # Disable quoting
      select(word) %>%
      mutate(word = tolower(word) %>% str_remove_all("[[:punct:]]")) %>%
      filter(word != "")
  }) %>%
    bind_rows()

  # Check if terms exist in the corpus
  terms <- tolower(terms)
  corpus <- corpus %>%
    mutate(is_term = word %in% terms)

  if (sum(corpus$is_term) == 0) {
    stop("None of the specified terms were found in the corpus.")
  }

  # Step 2: Run Collocates Analysis
  collocate_results <- lapply(terms, function(term) {
    # Subset to find windows around the term
    indices <- which(corpus$word == term)
    collocates <- unlist(lapply(indices, function(idx) {
      lower_bound <- max(1, idx - horizon)
      upper_bound <- min(nrow(corpus), idx + horizon)
      corpus$word[lower_bound:upper_bound]
    }))

    collocates <- collocates[collocates != term] # Exclude the term itself

    # Count occurrences of collocates
    collocate_counts <- table(collocates) %>%
      as.data.frame() %>%
      setNames(c("collocate", "count"))

    # Ensure 'collocate' is a character vector
    collocate_counts <- collocate_counts %>%
      mutate(collocate = as.character(collocate)) %>% # Convert to character
      rowwise() %>%
      mutate(p_value = chisq.test(matrix(c(count, sum(corpus$word != collocate)), ncol = 2))$p.value)

    # Filter results to reduce noise
    collocate_counts <- collocate_counts %>%
      arrange(p_value) %>%
      filter(!str_detect(collocate, "\\d")) %>% # Remove numeric-only collocates
      filter(nchar(collocate) > 2) # Remove short noise words

    return(collocate_counts)
  })

  # Step 3: Parse Results
  results <- lapply(seq_along(terms), function(i) {
    if (!is.null(collocate_results[[i]]) && nrow(collocate_results[[i]]) > 0) {
      # If there are rows in the current collocate result
      data.frame(term = terms[i], collocate_results[[i]])
    } else {
      # Create an empty data frame with all columns having zero rows
      data.frame(term = character(0), word = character(0), frequency = integer(0))
    }
  }) %>%
    bind_rows()

  # Return processed results
  return(results)
}

