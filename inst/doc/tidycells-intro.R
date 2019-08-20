## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, include = FALSE----------------------------------------------
library(tidycells)

## ---- eval=FALSE---------------------------------------------------------
#  read_cells(file_name)

## ------------------------------------------------------------------------
fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
# this is because extension is intentionally given wrong  
# while filename is actual identifier of the file type
fcsv <- list.files(fold, pattern = "^csv.", full.names = TRUE)[1]

## ---- eval=FALSE---------------------------------------------------------
#  utils::read.csv(fcsv)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(utils::read.csv(fcsv), align = "l", longtable = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  read_cells(fcsv)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(read_cells(fcsv))

## ---- eval=FALSE---------------------------------------------------------
#  read.csv(fcsv) %>% tidyr::gather(measure, val_of_measure, -Kid.Name)

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(read.csv(fcsv) %>% tidyr::gather(measure, val_of_measure, -Kid.Name), align = "l", longtable = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  read_cells()

## ---- out.width = "550px", echo=FALSE, dpi=300---------------------------
knitr::include_graphics("ext/read_cells_out.png")

## ------------------------------------------------------------------------
d1 <- read_cells(fcsv, at_level = "va_classify") %>% 
  read_cells()
d2 <- read_cells(fcsv, at_level = 2) %>% 
  read_cells(at_level = 4) %>% 
  read_cells()
identical(d1, d2)

## ---- out.width = "356px", echo=FALSE, fig.align='center', dpi=300-------
knitr::include_graphics("ext/read_cells.svg")

## ------------------------------------------------------------------------
x <- iris %>% head() 
cd <- x %>% as_cell_df(take_col_names = TRUE)

## ---- eval=TRUE, out.width = "356px"-------------------------------------
plot(cd, adaptive_txt_size = FALSE, txt_size = 2.5)

## ---- eval=FALSE---------------------------------------------------------
#  dummy_dat <- tibble::tibble(name = c("Mr. X", "Mr. Y"), sal = c("1,000","12,000"), age = c(35, 60))
#  dummy_dat

## ---- echo=FALSE---------------------------------------------------------
dummy_dat <- tibble::tibble(name = c("Mr. X", "Mr. Y"), sal = c("1,000","12,000"), age = c(35, 60))
knitr::kable(dummy_dat)

## ------------------------------------------------------------------------
dummy_dat_cells <- dummy_dat %>% 
  as_cell_df(take_col_names = TRUE) 

## ------------------------------------------------------------------------
va_cells <- numeric_values_classifier(dummy_dat_cells)

## ------------------------------------------------------------------------
va_cells <- value_attribute_classify(dummy_dat_cells, 
                                     classifier = numeric_values_classifier())

## ---- fig.width=3, fig.height=2, eval=TRUE, out.width = "256px"----------
plot(va_cells, adaptive_txt_size = FALSE, txt_size = 3)

## ---- eval=TRUE, out.width = "356px"-------------------------------------
# let's resume with iris example
cd <- numeric_values_classifier(cd)
plot(cd, adaptive_txt_size = FALSE, txt_size = 2.5)

## ---- echo=FALSE---------------------------------------------------------
cd <- numeric_values_classifier(cd)

## ------------------------------------------------------------------------
ca <- analyze_cells(cd)

## ---- eval=TRUE, out.width = "356px"-------------------------------------
plot(ca)

## ---- eval=FALSE---------------------------------------------------------
#  dcomp <- compose_cells(ca)
#  # discarding other columns for printing (you can check them!)
#  head(dcomp[1:7])

## ---- echo=FALSE---------------------------------------------------------
dcomp <- compose_cells(ca)
knitr::kable(head(dcomp[1:7]))

## ---- eval= FALSE--------------------------------------------------------
#  # let's check the same for va_cells (earlier dummy example)
#  va_cells %>%
#    analyze_cells() %>%
#    compose_cells(discard_raw_cols = TRUE)

## ---- echo=FALSE---------------------------------------------------------
va_cells %>% 
  analyze_cells() %>% 
  compose_cells(discard_raw_cols = TRUE) %>% 
  knitr::kable()

## ---- fig.width=3, fig.height=2------------------------------------------
cell_composition_traceback(ca, trace_row = 1)
cell_composition_traceback(ca, trace_row = 10)

## ---- eval=FALSE---------------------------------------------------------
#  dcc <- collate_columns(dcomp)
#  head(dcc)

## ---- echo=FALSE---------------------------------------------------------
dcc <- collate_columns(dcomp)
head(dcc) %>% knitr::kable()

## ---- eval=FALSE---------------------------------------------------------
#  # below should match with
#  # read_cells(fcsv) %>% dplyr::select(-table_tag)
#  # * which (the column table_tag) is added to identify
#  # * potential multiple tables.
#  fcsv %>%
#    read.csv() %>%
#    as_cell_df(take_col_names = TRUE) %>%
#    numeric_values_classifier() %>%
#    analyze_cells() %>%
#    compose_cells() %>%
#    collate_columns()

## ---- echo=FALSE---------------------------------------------------------
fcsv %>% 
  read.csv() %>% 
  as_cell_df(take_col_names = TRUE) %>% 
  numeric_values_classifier() %>% 
  analyze_cells() %>% 
  compose_cells() %>% 
  collate_columns() %>% 
  knitr::kable()

## ------------------------------------------------------------------------
rc_part <- read_cells(fcsv, at_level = 2)
# it is a list with `read_cells_stage` attribute
# which indicate the last processed stage in read_cells
str(rc_part)
# sample_based_classifier is another VA classifier
# for details see coresponding documentation
rc_part[[1]] <- rc_part[[1]] %>% 
  numeric_values_classifier() %>% 
  sample_based_classifier(empty_sample = "6")
  

# below should be similar to 
# rc_part[[1]] %>% 
#   analyze_cells() %>% 
#   compose_cells(discard_raw_cols = TRUE)
rc_part %>% read_cells(from_level = 3)

## ---- eval=FALSE---------------------------------------------------------
#  system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE)

## ---- out.width = "451px", echo=FALSE, eval=TRUE, dpi=300----------------
knitr::include_graphics("ext/marks.png")

## ---- eval=FALSE---------------------------------------------------------
#  system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>%
#    read_cells()

## ---- echo=FALSE---------------------------------------------------------
if(rlang::is_installed("tidyxl")){
  system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>% 
    read_cells() %>% 
    knitr::kable()
}

## ---- eval=FALSE---------------------------------------------------------
#  # if you have tidyxl installed
#  d <- system.file("extdata", "marks.xlsx", package = "tidycells", mustWork = TRUE) %>%
#    read_cells(at_level = "make_cells") %>%
#    .[[1]]

## ------------------------------------------------------------------------
# or you may do
d <- system.file("extdata", "marks_cells.rds", package = "tidycells", mustWork = TRUE) %>% 
  readRDS()
d <- numeric_values_classifier(d)
da <- analyze_cells(d)

## ---- echo=TRUE, eval=FALSE----------------------------------------------
#  dc <- compose_cells(da, print_attribute_overview = TRUE)

## ---- out.width = "451px", echo=FALSE, eval=TRUE, dpi=300----------------
knitr::include_graphics("ext/compose_cells_cli1.png")
dc <- compose_cells(da)

## ------------------------------------------------------------------------
# bit tricky and tedious unless you do print_attribute_overview = TRUE in above line
dcfine <- dc %>% 
  dplyr::mutate(name = dplyr::case_when(
    data_block == 1 ~ major_row_left_2_1,
    data_block == 2 ~ major_col_bottom_1_1,
    data_block == 3 ~ major_row_left_1_1
  ),
  sex = dplyr::case_when(
    data_block == 1 ~ major_row_left_1_1,
    data_block == 2 ~ major_col_bottom_2_1,
    data_block == 3 ~ minor_row_right_1_1
  ),
  school = dplyr::case_when(
    data_block == 1 ~ minor_col_top_1_1,
    data_block == 2 ~ minor_corner_topLeft_1_1,
    data_block == 3 ~ minor_col_top_1_1
  )) %>% 
  dplyr::select(school,sex, name, value)


## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(dcfine), align = c(rep("l", 3), "c"))

## ---- eval=FALSE---------------------------------------------------------
#  collate_columns(dc) %>%
#    head()

## ---- echo=FALSE---------------------------------------------------------
knitr::kable(head(collate_columns(dc)), align = c(rep("l", 5), "c"))

## ------------------------------------------------------------------------
dm <- tibble::tibble(fn = list.files(fold, full.names = T))
dm$fn %>% basename()

## ---- echo=FALSE, include=FALSE------------------------------------------
# filter based on file-type
dm <- dm %>%
    dplyr::mutate(original = dm$fn %>%
      purrr::map_chr(~ basename(.x) %>%
        stringr::str_split("\\.") %>%
        purrr::map_chr(1)))
if(!(rlang::is_installed("readxl") | rlang::is_installed("xlsx"))){
  dm <- dm %>% dplyr::filter(original!="xls")
}
if(!(rlang::is_installed("tidyxl"))){
  dm <- dm %>% dplyr::filter(original!="xlsx")
}
if(!(rlang::is_installed("docxtractr"))){
  dm <- dm %>% dplyr::filter(original!="docx" & original!="doc")
}
if(!(rlang::is_installed("tabulizer"))){
  dm <- dm %>% dplyr::filter(original!="pdf")
}
if(!(rlang::is_installed("XML"))){
  dm <- dm %>% dplyr::filter(original!="html")
}

## ------------------------------------------------------------------------
dcomps <- dm$fn %>% purrr::map(read_cells)
dcomps_sel <- dcomps %>%
    purrr::map(~ .x %>%
      dplyr::select(value, collated_1, collated_2))
# all of them are same [intentionaly made. but the file types are totally different]
dcomps_sel %>% 
  purrr::map_lgl(~identical(.x, dcomps_sel[[1]])) %>% 
  all()
# check one sample
dcomps_sel[[1]]


## ---- out.width = "516px", echo=FALSE, dpi=300---------------------------
knitr::include_graphics("ext/v12.png")

## ---- out.width = "516px", echo=FALSE, dpi=300---------------------------
knitr::include_graphics("ext/v34.png")

## ---- out.width = "516px", echo=FALSE, dpi=300---------------------------
knitr::include_graphics("ext/v56.png")

