#' Read Facereader file
#'
#' @param file Path to the FaceReader \code{.txt} file to be read.
#' @param subject A REGEX to extract from the source file a unique subject identifier.
#' If \code{NULL} (default), the function will utilize the file name as the unique
#' identifier (different from \code{source} which is derived from inside the file).
#' @param skip Number of lines to skip before actual data is present in the file.
#' If \code{NULL} (default), the function will attempt to find where the
#' actual data start and estimate based off of front matter how many lines to skip
#' when reading in the file.
#' @param ... Additional arguments to be passed to \code{\link[utils]{read.table}}
#'
#' @return Tidy data frame.
#' @export
#' @importFrom magrittr '%>%'
#' @importFrom dplyr select mutate
#'
#' @examples
#' read_facereader(file = system.file("extdata/Participant 2_Trump_Analysis 1.txt", package="facereadeR"))
#'

read_facereader <-
  function(file,
           subject = NULL,
           skip = NULL,
           ...) {
    src <- read.table(
      file,
      sep = "\t",
      as.is = TRUE,
      strip.white = TRUE,
      fill = TRUE,
      row.names = NULL
    )[1:10,]

    position <- which(src[1] == "Filename:")
    source <- src[4, 2]
    # source <- position

    if (is.null(skip)) {
      start <- NULL

      if (purrr::is_empty(start)) {
        start <- which(src[1] == "Video Time")
        skip <- start - 1

        df <- read_fr(file, skip = skip)

        if (any(class(df) == 'error') | purrr::is_empty(start)) {
          start <- as.numeric(rownames(src[grep("Filename", src[, 1]), ]))
          skip <- start + 1

          df <- read_fr(file, skip = skip)
        }

        if (any(class(df) == 'error')) {
          start <- which(src[1] == "Frame rate:")
          skip <- start + 2

          df <- read_fr(file, skip = skip)
        }

        if (any(class(df) == 'error')) {
          stop("Cannot determine start point. Please specify using \"skip\" argument.")
        }

      }
    } else {
      df <- read.table(
        file,
        sep = "\t",
        skip = skip,
        as.is = TRUE,
        strip.white = TRUE,
        fill = TRUE,
        header = TRUE
      )
    }

    if (is.null(subject)) {
      subject <- file
    } else {
      subject <- stringr::str_extract(source, subject)
    }

    data <- df %>%
      mutate(., subject_nr = subject,
             source = source) %>%
      select(subject_nr, source, dplyr::everything())

    return(data)
  }

#' Clean FaceReader data
#'
#' @param data Data to clean, usually in the form that is provided by \code{\link{read_facereader}}.
#' @param include Include \code{All} variables, or just the emotions (\code{Basic}) to clean.
#'
#' @return Data frame with fit failures removed, numeric values transformed to class `numeric`,
#' and factor values transformed to class `factor`.
#' @export
#'
#' @examples
#'cleaned <- clean_data(data, include = 'Basic')

clean_data <- function(data, include = "All") {
  data <- data
  vars <- names(data)

  if (include == "Basic") {
    emotions <- c(
      'Neutral',
      'Happy',
      'Sad',
      'Angry',
      'Surprised',
      'Scared',
      'Disgusted',
      'Valence',
      'Arousal'
    )

    for (i in seq_along(emotions)) {
      if (emotions[[i]] %in% vars) {
        data[, emotions[i]] <-
          as.numeric(ifelse(data[, emotions[j]] == 'FIT_FAILED', NA, data[, emotions[i]]))
      } else
        next
    }

  } else if (include == 'All') {
    nums <- c(
      'Neutral',
      'Happy',
      'Sad',
      'Angry',
      'Surprised',
      'Scared',
      'Disgusted',
      'Contempt',
      'Valence',
      'Arousal',
      'Y...Head.Orientation',
      'X...Head.Orientation',
      'Z...Head.Orientation'
    )
    facts <- c(
      'Gender',
      'Age',
      'Beard',
      'Moustache',
      'Ethnicity',
      'Mouth',
      'Left.Eye',
      'Right.Eye',
      'Left.Eyebrow',
      'Right.Eyebrow',
      'Gaze.Direction',
      'Glasses',
      'Landmarks',
      'Stimulus',
      'Event.Marker'
    )

    for (i in seq_along(nums)) {
      if (nums[[i]] %in% vars) {
        data[, nums[i]] <-
          as.numeric(ifelse(data[, nums[i]] == 'FIT_FAILED', NA, data[, nums[i]]))
      } else
        next
    }

    for (j in seq_along(facts)) {
      if (facts[[j]] %in% vars) {
        data[, facts[j]] <- as.character(data[, facts[j]])
        data[, facts[j]] <-
          ifelse(data[, facts[j]] == 'FIT_FAILED', NA, data[, facts[j]])
        data[, facts[j]] <- as.factor(data[, facts[j]])

      } else
        next
    }

    for (k in seq_along(facts)) {
      if (facts[[k]] %in% vars) {
        data[, facts[k]] <- as.character(data[, facts[k]])
        data[, facts[k]] <-
          ifelse(data[, facts[k]] == 'FIND_FAILED', NA, data[, facts[k]])
        data[, facts[k]] <- as.factor(data[, facts[k]])

      } else
        next
    }

  } else {
    stop('Please specify what to include in cleaning process.')
  }

  return(data)

}

#' Rename Action Units to their shortened names
#'
#' @param data
#'
#' @return Dataframe with AUs renamed to shortened values.
#' @export
#'
#' @examples
#' @importFrom dplyr select rename_
#' @importFrom purrr set_names

rename_aus <- function(data) {
  old <- data %>% dplyr::select(dplyr::starts_with('Action.')) %>%
    names(.)

  new <- c(
    "AU01",
    "AU02",
    "AU04",
    "AU05",
    "AU06",
    "AU07",
    "AU09",
    "AU10",
    "AU12",
    "AU14",
    "AU15",
    "AU17",
    "AU18",
    "AU20",
    "AU23",
    "AU24",
    "AU25",
    "AU26",
    "AU27",
    "AU43"
  )

  new_names <- purrr::set_names(old, new)

  data <- data %>% dplyr::rename_(.dots = new_names)

}

#' Score Action Units numerically
#'
#' @param data
#'
#' @return Dataframe with AUs recoded into numeric values.
#' @export
#' @importFrom dplyr recode
#' @importFrom purrr map_df
#'
#' @examples

score_aus <- function(data, score0 = FALSE) {
  AUs <- c(
    "AU01",
    "AU02",
    "AU04",
    "AU05",
    "AU06",
    "AU07",
    "AU09",
    "AU10",
    "AU12",
    "AU14",
    "AU15",
    "AU17",
    "AU18",
    "AU20",
    "AU23",
    "AU24",
    "AU25",
    "AU26",
    "AU27",
    "AU43"
  )

  if (score0) {
    data[AUs] <-
      purrr::map_df(data[AUs],
                    ~ dplyr::recode(
                      .x,
                      A = 1,
                      B = 2,
                      C = 3,
                      D = 4,
                      E = 5,
                      NotActive = 0
                    ))
  } else {
    data[AUs] <-
      purrr::map_df(data[AUs],
                    ~ dplyr::recode(
                      .x,
                      A = 1,
                      B = 2,
                      C = 3,
                      D = 4,
                      E = 5
                    ))
  }


  return(data)
}

extract_landmarks <- function(data) {
  xy <- paste0(rep(c('x','y'), times = 2), '_', rep(1:49, each = 2))

  landmarks <- data %>%
  separate(., Landmarks, into = xy, sep=',')
  return(landmarks)
}

