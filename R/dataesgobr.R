dataesgobr <- function(url = "",
                       title = "",
                       description = "",
                       formats,
                       format_links,
                       info,
                       issued,
                       identifier,
                       keywords,
                       publisher,
                       spatial,
                       theme,
                       json,
                       dataframe,
                       create_from_json = !missing(json),
                       create_from_dataframe = !missing(dataframe), ...) {

  stopifnot(is.character(url))

  if (create_from_json) {
    do.call(dataesgobr_from_json, c(json))
  } else if (create_from_dataframe) {
    do.call(dataesgobr_from_dataframe, list(dataframe))
  } else {
    final_languages <- prepare_descriptions(description)
    final_titles <- prepare_titles(title)

    value <- list(
      url = url,
      title = final_titles,
      description = final_languages,
      issued = issued,
      identifier = identifier,
      formats = format_links,
      formats_info = info,
      keywords = keywords,
      publisher = publisher,
      spatial = spatial,
      theme = theme)

    names(value$formats) = formats
    attr(value, "class") <- "dataesgobr"

    value
  }
}

dataesgobr_from_json <- function(json) {
  if ( is.null(nrow(json$distribution)) ) {
    values <- json$distribution[[1]]$format$value
    access <- json$distribution[[1]]$accessURL
    info <- json$distribution[[1]]$title
  } else {
    values <- json$distribution$format$value
    access <- json$distribution$accessURL
    info <- json$distribution$title
  }

  final_languages <- prepare_descriptions(json$description)
  final_titles <- prepare_titles(json["title"])

  value <- list(
    url = as.character(json["_about"]),
    title = final_titles,
    description = final_languages,
    formats = access,
    formats_info = info,
    issued = json$issued,
    identifier = json$identifier,
    keywords = json$keyword,
    publisher = json$publisher,
    spatial = json$spatial,
    theme = json$theme
  )

  names(value$formats) = values
  attr(value, "class") <- "dataesgobr"

  value
}

dataesgobr_from_dataframe <- function(dataframe) {
  if (is.null(nrow(dataframe$distribution))) {
    values <- dataframe$distribution[[1]]$format$value
    access <- dataframe$distribution[[1]]$accessURL
    info <- dataframe$distribution[[1]]$title
  } else {
    values <- dataframe$distribution$format$value
    access <- dataframe$distribution$accessURL
    info <- dataframe$distribution$title
  }

  final_languages <- prepare_descriptions(dataframe$description)
  final_titles <- prepare_titles(dataframe$title)

  value <- list(
    url = as.character(dataframe["_about"]),
    title = final_titles,
    description = final_languages,
    formats = access,
    formats_info = info,
    issued = dataframe$issued,
    identifier = dataframe$identifier,
    keywords = dataframe$keyword,
    publisher = dataframe$publisher,
    spatial = dataframe$spatial,
    theme = dataframe$theme
  )

  names(value$formats) = values
  attr(value, "class") <- "dataesgobr"

  value
}
