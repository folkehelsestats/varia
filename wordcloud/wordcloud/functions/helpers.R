
# These settings must match the input app.
sheet_id <- "108kPND1ySv8XQss6xt0DYo5kxYsAC1aTZpgxoJUuBuU"
worksheet_name <- "words"
credentials_file <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON", "service-task-survey.json")

if (!file.exists(credentials_file)) {
  stop(
    "Google service-account JSON was not found. Set GOOGLE_SERVICE_ACCOUNT_JSON ",
    "to its path, or place service-task-survey.json in this app folder."
  )
}
googlesheets4::gs4_auth(path = credentials_file)

empty_words <- data.frame(name = character(), weight = numeric())

# Google Sheet source ----------------------------------------------------------
get_word_counts <- function() {
  dat <- googlesheets4::read_sheet(ss = sheet_id, sheet = worksheet_name)

  if (!"word" %in% names(dat) || nrow(dat) == 0) {
    return(empty_words)
  }

  ## words <- tolower(trimws(as.character(dat$word)))
  words <- normalize_words_fast(as.character(dat$word))
  words <- words[!is.na(words) & nzchar(words)]

  if (!length(words)) {
    return(empty_words)
  }

  freq <- as.data.frame(table(words), stringsAsFactors = FALSE)
  names(freq) <- c("name", "weight")
  freq$weight <- as.numeric(freq$weight)
  freq <- freq[order(-freq$weight, freq$name), , drop = FALSE]
  rownames(freq) <- NULL
  freq
}

# Uploaded file source ---------------------------------------------------------
# Reads a CSV (comma, semicolon or tab separated; UTF-8 or latin1) as text.
read_csv_flex <- function(path) {
  lines <- readLines(path, warn = FALSE)
  enc <- if (validUTF8(paste(lines, collapse = "\n"))) "UTF-8-BOM" else "latin1"
  first <- lines[1]
  seps <- c(";", ",", "\t")
  counts <- vapply(
    seps,
    function(s) lengths(regmatches(first, gregexpr(s, first, fixed = TRUE))),
    integer(1)
  )
  sep <- seps[which.max(counts)]

  utils::read.csv(
    path,
    sep = sep,
    fileEncoding = enc,
    colClasses = "character",
    check.names = FALSE,
    strip.white = TRUE,
    stringsAsFactors = FALSE
  )
}

to_numeric <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- gsub("[[:space:]]", "", as.character(x))
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

# Turns an uploaded table into the same data.frame(name, weight) that the
# Google Sheet produces. Column 1 = word, column 2 = frequency by default;
# common header names (word/ord, freq/antall/weight ...) are recognised.
# A single-column file is treated as a raw word list and counted.
read_uploaded_words <- function(path, filename) {
  ext <- tolower(tools::file_ext(filename))

  dat <- switch(
    ext,
    csv = , txt = , tsv = read_csv_flex(path),
    xlsx = , xls = as.data.frame(readxl::read_excel(path, sheet = 1)),
    stop("Filtypen .", ext, " støttes ikke. Bruk .csv, .xlsx eller .xls.")
  )

  if (nrow(dat) == 0 || ncol(dat) == 0) {
    stop("Filen inneholder ingen data.")
  }

  if (ncol(dat) == 1) {
    ## words <- tolower(trimws(as.character(dat[[1]])))
    words <- normalize_words_fast(as.character(dat[[1]]))
    words <- words[!is.na(words) & nzchar(words)]
    if (!length(words)) stop("Fant ingen ord i filen.")
    freq <- as.data.frame(table(words), stringsAsFactors = FALSE)
    names(freq) <- c("name", "weight")
  } else {
    nms <- tolower(trimws(names(dat)))
    word_idx <- which(nms %in% c("word", "words", "ord", "name", "navn"))[1]
    freq_idx <- which(nms %in% c(
      "freq", "frequency", "weight", "count", "n",
      "antall", "frekvens", "forekomster", "vekt"
    ))[1]
    if (is.na(word_idx)) word_idx <- 1
    if (is.na(freq_idx) || freq_idx == word_idx) {
      freq_idx <- setdiff(seq_len(ncol(dat)), word_idx)[1]
    }

    freq <- data.frame(
      name = tolower(trimws(as.character(dat[[word_idx]]))),
      weight = to_numeric(dat[[freq_idx]]),
      stringsAsFactors = FALSE
    )
    freq <- freq[!is.na(freq$name) & nzchar(freq$name) &
                   !is.na(freq$weight) & freq$weight > 0, , drop = FALSE]
    if (nrow(freq) == 0) {
      stop("Fant ingen gyldige rader. Forventer ord i første kolonne og frekvens (tall) i andre kolonne.")
    }
    # Sum duplicates of the same word
    freq <- stats::aggregate(weight ~ name, data = freq, FUN = sum)
  }

  freq$weight <- as.numeric(freq$weight)
  freq <- freq[order(-freq$weight, freq$name), , drop = FALSE]
  rownames(freq) <- NULL
  freq
}
