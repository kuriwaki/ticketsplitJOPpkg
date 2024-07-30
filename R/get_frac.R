#' Create proportions, by top_party, of split/straight/etc
#' @param var Name of office to subset to, e.g. "USH"
#' @param tbl Vote object, in long form. Can be arrow connection
#'
#' @import dplyr
#' @importFrom stringr str_detect
#' @export
frac_by_top <- function(var, tbl = vdf) {

  # subset to the office
  tbl <- tbl |>
    filter(office == var) |>
    filter(top_party2 %in% c(-1, 1)) |>
    filter(!is.na(party))

  # subset to contested
  if (!str_detect(var, "(USS|ATG|SSI|SOS|CLR|AUD)"))
    tbl <- filter(tbl, ncand == 2)

  # numerator / denominator
  categ_table <- tbl |>
    count(top_party2, party) |>
    transmute(down = party, top_party2, n) |>
    collect() |>
    mutate(tot_n = sum(n), .by = top_party2)

  # fraction
  frac_tbl <- categ_table |>
    mutate(
      frac = n / tot_n,
      tot_n,
      office = var,
      type = case_match(down,
                        1   ~ "Republican",
                        -1  ~ "Democrat",
                        0.5 ~ "Other",
                        0   ~ "Roll-off"),
      type = ifelse(top_party2 == down, "Straight", type),
      type = ifelse(down %in% c(-1, 1) & (top_party2 != down), "Split", type)
    )

  frac_tbl
}
