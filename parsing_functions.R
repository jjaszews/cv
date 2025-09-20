find_link <- stringr::regex("\\[[^\\]]+\\]\\([^\\)]+\\)")

sanitize_links <- function(text){
  if (exists("PDF_EXPORT", inherits = TRUE) && isTRUE(get("PDF_EXPORT", inherits = TRUE))) {
    stringr::str_extract_all(text, find_link) |>
      purrr::pluck(1) |>
      purrr::walk(function(link_from_text){
        title <- link_from_text |> stringr::str_extract("\\[.+\\]") |> stringr::str_remove_all("\\[|\\]")
        link  <- link_from_text |> stringr::str_extract("\\(.+\\)") |> stringr::str_remove_all("\\(|\\)")
        if (!exists("links", inherits = TRUE)) links <<- character(0)
        links <<- c(links, link)
        text  <<- stringr::str_replace(text, stringr::fixed(link_from_text),
                                       glue::glue("{title}<sup>{length(links)}</sup>"))
      })
  }
  text
}

# Robust to zero rows, missing columns, NA cells
strip_links_from_cols <- function(data, cols_to_strip){
  if (!nrow(data)) return(data)
  cols <- intersect(cols_to_strip, names(data))
  if (!length(cols)) return(data)
  dplyr::mutate(
    data,
    dplyr::across(
      dplyr::all_of(cols),
      ~ ifelse(is.na(.), NA_character_, vapply(., sanitize_links, character(1)))
    )
  )
}

print_section <- function(position_data, section_id){
  position_data |>
    dplyr::ungroup() |>
    dplyr::filter(section == section_id) |>
    dplyr::arrange(dplyr::desc(end)) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    # ensure all description_* columns are character so pivot_longer can stack them
    dplyr::mutate(dplyr::across(dplyr::starts_with("description"),
                                ~ ifelse(is.na(.), NA_character_, as.character(.)))) |>
    tidyr::pivot_longer(
      dplyr::starts_with("description"),
      names_to  = "description_num",
      values_to = "description"
    ) |>
    dplyr::filter(!is.na(description) | description_num == "description_1") |>
    dplyr::group_by(id) |>
    dplyr::mutate(
      descriptions    = list(description),
      no_descriptions = is.na(dplyr::first(description))
    ) |>
    dplyr::filter(description_num == "description_1") |>
    dplyr::ungroup() |>
    dplyr::mutate(
      timeline = ifelse(is.na(start) | start == end, end, glue::glue("{end} - {start}")),
      description_bullets = ifelse(
        no_descriptions, " ", purrr::map_chr(descriptions, ~ paste("-", ., collapse = "\n"))
      )
    ) |>
    strip_links_from_cols(c("title", "description_bullets")) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.), "N/A", .))) |>
    glue::glue_data(
      "### {title}", "\n\n",
      "{loc}", "\n\n",
      "{institution}", "\n\n",
      "{timeline}", "\n\n",
      "{description_bullets}", "\n\n\n"
    )
}

build_skill_bars <- function(skills, out_of = 5){
  bar_color <- "#969696"; bar_background <- "#d9d9d9"
  skills |>
    dplyr::mutate(width_percent = round(100*level/out_of)) |>
    glue::glue_data(
      "<div class = 'skill-bar'",
      "style = \"background:linear-gradient(to right,",
      "{bar_color} {width_percent}%,",
      "{bar_background} {width_percent}% 100%)\" >",
      "{skill}",
      "</div>"
    )
}