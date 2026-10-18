
###############################################################################
# Main function
###############################################################################
#
#' Normalize Similar Words for Word Clouds
#'
#' Groups similar words (misspellings, inflections, and variants) and replaces
#' them with the most frequently occurring version of the word.
#'
#' The function is designed for word-cloud and text-mining workflows where
#' multiple spellings of the same concept should be treated as a single word.
#'
#' Clustering is based on:
#' \itemize{
#'   \item Relative string distance using \pkg{stringdist}
#'   \item Common prefix similarity
#'   \item Frequency-based canonical word selection
#' }
#'
#' For example:
#'
#' \preformatted{
#' morsom
#' morsomst
#' morsommere
#' morsm
#' }
#'
#' may all be normalized to:
#'
#' \preformatted{
#' morsom
#' }
#'
#' because it is the most frequently occurring variant.
#'
#' To improve scalability on large vocabularies, words are partitioned into
#' blocks based on their prefix before similarity comparisons are performed.
#' This greatly reduces the number of pairwise distance calculations.
#'
#' @param words Character vector of single words to normalize.
#'
#' @param max_dist Numeric scalar between 0 and 1 giving the maximum allowed
#' relative string distance for two words to be considered similar.
#'
#' Relative distance is calculated as:
#'
#' \deqn{
#' distance / max(nchar(word1), nchar(word2))
#' }
#'
#' A value of:
#' \itemize{
#'   \item 0.10 is conservative
#'   \item 0.20 is recommended for survey data
#'   \item 0.30 is aggressive
#' }
#'
#' Default: \code{0.20}
#'
#' @param prefix_ratio_threshold Numeric scalar between 0 and 1 defining how
#' much of the shorter word must match from the beginning of both words.
#'
#' Example:
#'
#' \preformatted{
#' nerd
#' nerdete
#' }
#'
#' Common prefix length = 4
#'
#' Shortest word length = 4
#'
#' Prefix ratio:
#'
#' \deqn{
#' 4 / 4 = 1.00
#' }
#'
#' Since 1.00 exceeds the default threshold of 0.80, the words are grouped
#' together.
#'
#' Default: \code{0.80}
#'
#' @param prefix_length Integer specifying the number of leading characters
#' used to create comparison blocks.
#'
#' Only words sharing the same prefix block are compared.
#'
#' Larger values increase speed but may miss some matches.
#'
#' Smaller values increase recall but require more comparisons.
#'
#' Recommended values:
#' \itemize{
#'   \item 3 for short words
#'   \item 4 (default)
#'   \item 5 for very large vocabularies
#' }
#'
#' Default: \code{4}
#'
#' @param short_word_threshold Relative string distance threshold used for
#' very short words (four characters or fewer).
#'
#' Short words are more likely to be incorrectly merged:
#'
#' \preformatted{
#' mat
#' hat
#' }
#'
#' Therefore a stricter threshold is applied.
#'
#' Default: \code{0.10}
#'
#' @param min_cluster_freq Minimum frequency required for a word to become a
#' cluster center.
#'
#' Rare words occurring fewer times than this threshold may still be assigned
#' to an existing cluster but cannot create a cluster of their own during the
#' first clustering pass.
#'
#' This helps prevent one-off spelling mistakes from becoming canonical words.
#'
#' Example:
#'
#' \preformatted{
#' morsom   100
#' morsm      1
#' }
#'
#' The word \code{"morsm"} cannot become a cluster center.
#'
#' Default: \code{2}
#'
#' @param distance_method Character string specifying the
#' \pkg{stringdist} distance algorithm.
#'
#' Common choices:
#'
#' \itemize{
#'   \item \code{"osa"} Optimal String Alignment (default)
#'   \item \code{"lv"} Levenshtein distance
#'   \item \code{"dl"} Damerau-Levenshtein distance
#'   \item \code{"jw"} Jaro-Winkler distance
#' }
#'
#' The default \code{"osa"} handles:
#' \itemize{
#'   \item insertions
#'   \item deletions
#'   \item substitutions
#'   \item adjacent transpositions
#' }
#'
#' making it suitable for survey responses and typing errors.
#'
#' @param raw Logical.
#'
#' If \code{FALSE} (default), the function returns only the normalized
#' word vector.
#'
#' If \code{TRUE}, the function returns a \code{data.table} containing:
#'
#' \describe{
#'   \item{raw}{Original input word.}
#'   \item{canonical}{Normalized replacement word.}
#' }
#'
#' This is useful for quality assurance and validation of clustering results.
#'
#' @return
#'
#' If \code{raw = FALSE}:
#'
#' A character vector containing normalized words.
#'
#' If \code{raw = TRUE}:
#'
#' A \code{data.table} with columns:
#'
#' \describe{
#'   \item{raw}{Original input word.}
#'   \item{canonical}{Assigned canonical word.}
#' }
#'
#' @examples
#' words <- c(
#'   "nerdete",
#'   "nerdete",
#'   "nerd",
#'   "morsom",
#'   "morsomst",
#'   "morsommere",
#'   "morsm"
#' )
#'
#' normalize_words_fast(words)
#'
#' normalize_words_fast(
#'   words,
#'   raw = TRUE
#' )
#'
#' @export

normalize_words_fast <- function(
    words,
    max_dist = 0.20,
    prefix_ratio_threshold = 0.80,
    prefix_length = 4,
    short_word_threshold = 0.10,
    min_cluster_freq = 2,
    distance_method = "osa",
    raw = FALSE
) {

  ###########################################################################
  # Frequency table
  ###########################################################################

  freq_dt <- data.table(
    word = tolower(trimws(words))
  )[
    ,
    .(freq = .N),
    by = word
  ]

  ###########################################################################
  # Most frequent words first
  ###########################################################################

  setorder(freq_dt, -freq)

  ###########################################################################
  # Blocking key
  ###########################################################################

  freq_dt[
    ,
    block := substr(
      word,
      1,
      pmin(prefix_length, nchar(word))
    )
  ]

  ###########################################################################
  # Initialize
  ###########################################################################

  freq_dt[
    ,
    `:=`(
      cluster = NA_integer_,
      assigned = FALSE
    )
  ]

  cluster_id <- 1L

  ###########################################################################
  # Process block-by-block
  ###########################################################################

  for (current_block in unique(freq_dt$block)) {

    rows <- which(freq_dt$block == current_block)

    block_dt <- copy(freq_dt[rows])

    for (i in seq_len(nrow(block_dt))) {

      if (block_dt$assigned[i]) {
        next
      }

      #####################################################################
      # Allow only sufficiently frequent words to become centers
      #####################################################################

      if (block_dt$freq[i] < min_cluster_freq) {
        next
      }

      center_word <- block_dt$word[i]

      remaining <- which(
        !block_dt$assigned
      )

      candidates <- block_dt$word[
        remaining
      ]

      #####################################################################
      # Relative edit distance
      #####################################################################

      dist <- stringdist(
        center_word,
        candidates,
        method = distance_method
      )

      max_len <- pmax(
        nchar(center_word),
        nchar(candidates)
      )

      rel_dist <- dist / max_len

      threshold <- ifelse(
        max_len <= 4,
        short_word_threshold,
        max_dist
      )

      #####################################################################
      # Prefix similarity
      #####################################################################

      prefix_ratio <- vapply(
        candidates,
        function(x) {
          common_prefix_ratio(
            center_word,
            x
          )
        },
        numeric(1)
      )

      #####################################################################
      # Cluster membership
      #####################################################################

      members <- remaining[
        rel_dist <= threshold |
        prefix_ratio >= prefix_ratio_threshold
      ]

      block_dt[
        members,
        `:=`(
          assigned = TRUE,
          cluster = cluster_id
        )
      ]

      cluster_id <- cluster_id + 1L
    }

    #######################################################################
    # Attach remaining rare words
    #######################################################################

    unassigned <- which(
      is.na(block_dt$cluster)
    )

    assigned <- which(
      !is.na(block_dt$cluster)
    )

    if (
      length(unassigned) > 0 &&
      length(assigned) > 0
    ) {

      centers <- block_dt$word[assigned]

      for (u in unassigned) {

        candidate <- block_dt$word[u]

        dist <- stringdist(
          candidate,
          centers,
          method = distance_method
        )

        rel_dist <- dist / pmax(
          nchar(candidate),
          nchar(centers)
        )

        prefix_ratio <- vapply(
          centers,
          function(x) {
            common_prefix_ratio(
              candidate,
              x
            )
          },
          numeric(1)
        )

        hits <- which(
          rel_dist <= max_dist |
          prefix_ratio >= prefix_ratio_threshold
        )

        if (length(hits) > 0) {

          best <- hits[
            which.min(rel_dist[hits])
          ]

          block_dt$cluster[u] <-
            block_dt$cluster[
              assigned[best]
            ]
        }
      }
    }

    #######################################################################
    # Remaining become own clusters
    #######################################################################

    still_unassigned <- which(
      is.na(block_dt$cluster)
    )

    if (length(still_unassigned) > 0) {

      for (u in still_unassigned) {

        block_dt$cluster[u] <- cluster_id

        cluster_id <- cluster_id + 1L
      }
    }

    freq_dt[rows] <- block_dt
  }

  ###########################################################################
  # Canonical word = most frequent word in cluster
  ###########################################################################

  canonical_lookup <- freq_dt[
    order(cluster, -freq)
  ][
    ,
    .SD[1],
    by = cluster
  ][
    ,
    .(
      cluster,
      canonical = word
    )
  ]

  freq_dt <- merge(
    freq_dt,
    canonical_lookup,
    by = "cluster"
  )

  ###########################################################################
  # Apply lookup
  ###########################################################################

  mapping <- freq_dt[
   ,
     .(
       word,
       canonical
     )
  ]

  result <- mapping[
    data.table(
      raw = words,
      word = tolower(trimws(words))
    ),
    on = "word"
  ]

  if (raw) {

    return(
      result[
       ,
         .(
           raw,
           canonical
         )
      ]
    )

  } else {

    return(result$canonical)

  }

}

###############################################################################
# Common prefix ratio
###############################################################################

common_prefix_ratio <- function(a, b) {

  a_chars <- strsplit(a, "", fixed = TRUE)[[1]]
  b_chars <- strsplit(b, "", fixed = TRUE)[[1]]

  n <- min(length(a_chars), length(b_chars))

  if (n == 0) {
    return(0)
  }

  match_len <- 0L

  for (i in seq_len(n)) {

    if (a_chars[i] != b_chars[i]) {
      break
    }

    match_len <- i
  }

  match_len / n
}
