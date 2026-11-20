# Savant pitch-type codes -> app labels and broad groups (FB / BR / OS).
# The table lives in reference/pitch_type_map.csv; codes not listed there (EP, PO, IN, UN, ...)
# map to NA and are excluded downstream.

pitch_type_table <- function() {
  readr::read_csv(file.path(repo_root(), "reference", "pitch_type_map.csv"),
                  col_types = "ccc", progress = FALSE)
}

map_pitch_type <- function(code) {
  tbl <- pitch_type_table()
  tbl$label[match(code, tbl$savant_code)]
}

pitch_group <- function(label) {
  tbl <- dplyr::distinct(pitch_type_table(), label, group)
  tbl$group[match(label, tbl$label)]
}
