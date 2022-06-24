#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

#' ------------------------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(dplyr)
library(survey)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

#' # Data preparation
# Download & Read SAS Transport Files
# Demographic (DEMO)

# Demographic ---------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT", tf <- tempfile(), mode="wb")
DEMO_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT", tf <- tempfile(), mode="wb")
DEMO_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT", tf <- tempfile(), mode="wb")
DEMO_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT", tf <- tempfile(), mode="wb")
DEMO_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# Dietary interview ---------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DRXTOT.XPT", tf <- tempfile(), mode="wb")
DIET_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DRXTOT_B.XPT", tf <- tempfile(), mode="wb")
DIET_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DRXTKCAL",
                                       "DRXTPROT",
                                       "DRXTCARB",
                                       "DRXTTFAT",
                                       "DRXTCALC")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DR1TOT_C.XPT", tf <- tempfile(), mode="wb")
DIET_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DR1TOT_D.XPT", tf <- tempfile(), mode="wb")
DIET_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT",
                                       "DR1TCALC")]

# Body measures -------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/BMX.XPT", tf <- tempfile(), mode="wb")
BODY_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/BMX_B.XPT", tf <- tempfile(), mode="wb")
BODY_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/BMX_C.XPT", tf <- tempfile(), mode="wb")
BODY_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BMX_D.XPT", tf <- tempfile(), mode="wb")
BODY_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]

# DEXA ----------------------------------------------------------------------------------------

# Body measures -------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx.xpt", tf <- tempfile(), mode="wb")
DEXA_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 01-02
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_b.xpt", tf <- tempfile(), mode="wb")
DEXA_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 03-04
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_c.xpt", tf <- tempfile(), mode="wb")
DEXA_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]

# 05-06
download.file("https://wwwn.cdc.gov/nchs/data/nhanes/dxa/dxx_d.xpt", tf <- tempfile(), mode="wb")
DEXA_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DXDLALE",
                                       "DXDLLLE",
                                       "DXDRALE",
                                       "DXDRLLE",
                                       "DXXLSBMD",
                                       "DXDTOBMD",
                                       "DXDTOPF")]


# Append Files
DEMO <- dplyr::bind_rows(DEMO_99,
                         DEMO_01,
                         DEMO_03,
                         DEMO_05)

DIET <-  dplyr::bind_rows(DIET_99,
                          DIET_01,
                          DIET_03,
                          DIET_05)

BODY <-  dplyr::bind_rows(BODY_99,
                          BODY_01,
                          BODY_03,
                          BODY_05)

DEXA <-  dplyr::bind_rows(DEXA_99,
                          DEXA_01,
                          DEXA_03,
                          DEXA_05)

# Merge DEMO and DIET files

DEMO_DIET <- dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <- dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

# Merge DEMO_DIET_BODY and DEXA - full data frame

df <- dplyr::left_join(DEMO_DIET_BODY, DEXA, by="SEQN")

# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar
readr::write_csv2(x = df, file = "df.csv")


###### PRECISA COLOCAR O PESO ABAIXO DOS MUTATE ##########

df |>
  # remove duplicates
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting protein, CHO, energy, fat, calcium due to distinct assessments
  dplyr::mutate(DRXTPROT = tidyr::replace_na(DRXTPROT, 0),
                DR1TPROT = tidyr::replace_na(DR1TPROT, 0)) |>
  dplyr::mutate(DR1TCARB = tidyr::replace_na(DR1TCARB, 0),
                DRXTCARB = tidyr::replace_na(DRXTCARB, 0)) |>
  dplyr::mutate(DRXTKCAL = tidyr::replace_na(DRXTKCAL, 0),
                DR1TKCAL = tidyr::replace_na(DR1TKCAL, 0)) |>
  dplyr::mutate(DRXTTFAT = tidyr::replace_na(DRXTTFAT, 0),
                DR1TTFAT = tidyr::replace_na(DR1TTFAT, 0)) |>
  dplyr::mutate(DRXTCALC = tidyr::replace_na(DRXTCALC, 0),
                DR1TCALC = tidyr::replace_na(DR1TCALC, 0)) |>
  # create apendicular lean mass
  dplyr::mutate(APLM = DXDLALE + DXDLLLE + DXDRALE + DXDRLLE,
                APLMBMI = APLM/BMXBMI,
                # create obesity class
                OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                                    BMXBMI < 30 ~ "NORMAL"),
                # create osteoporose class
                OSSO = case_when(DXXLSBMD > 0.915  ~ "NORMAL",
                                 DXXLSBMD >= 0.762 & DXXLSBMD < 0.915 ~ "OSTEOPENIA",
                                 DXXLSBMD <= 0.761 ~ "OSTEOPOROSE"),
                # create protein consumption variable of distinct assessments
                PTN = DR1TPROT + DRXTPROT,
                # create relative protein consumption variable
                PTNKG = PTN/BMXWT,
                # create CHO consumption variable
                CHO = DR1TCARB + DRXTCARB,
                # create FAT consumption variable
                FAT = DRXTTFAT + DR1TTFAT,
                # create CALCIUM consuption variable
                CAL = DRXTCALC + DR1TCALC,
                # create ENERGY consumption variable
                ENERGY = DRXTKCAL + DR1TKCAL,
                # create protein consumption status
                PTN_STATUS = case_when(PTNKG < 0.8 ~ "BAIXO",
                                       PTNKG >= 0.8 & PTNKG < 1.0 ~ "ADEQUADO",
                                       PTNKG >= 1.0 ~ "ELEVADO"))



One <- df |>
  # remove duplicate
  dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # Set 7=Refused and 9=Don't Know To Missing for variables DPQ010 thru DPQ090 ##
  mutate_at(vars(DPQ010:DPQ090), ~ifelse(. >=7, NA, .)) %>%
  mutate(. ,
         # create indicator for overall summary
         one = 1,
         # Create depression score as sum of variables DPQ010 -- DPQ090
         Depression.Score = rowSums(select(. , DPQ010:DPQ090)),
         # Create depression indicator as binary 0/100 variable. (is missing if Depression.Score is missing)
         Depression= ifelse(Depression.Score >=10, 100, 0),
         # Create factor variables
         Gender = factor(RIAGENDR, labels=c("Men", "Women")),
         Age.Group = cut(RIDAGEYR, breaks=c(-Inf,19,39,59,Inf),labels=c("Under 20", "20-39","40-59","60 and over")),
         # Generate 4-year MEC weight (Divide weight by 2 because we are appending 2 survey cycles)
         # Note: using the MEC Exam Weights (WTMEC2YR), per the analytic notes on the
         #       Mental Health - Depression Screener (DPQ_H) documentation
         WTMEC4YR = WTMEC2YR/2 ,
         # Define indicator for analysis population of interest: adults aged 20 and over with a valid depression score
         inAnalysis= (RIDAGEYR >= 20 & !is.na(Depression.Score))
  ) %>%
  # drop DPQ variables
  select(., -starts_with("DPQ"))


#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~WTMEC4YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# Testes Saulo --------------------------------------------------------------------------------
# olhando a base inteira - One <- autor deu esse nome

# tamanho da amostra
count(One)

# ordenando para ver se tem duplicado
One |>
  arrange(SEQN) |> head()

# tamanho da amostra sem duplicados
count(One)

# descritivo
One |>
  select(Depression.Score,
         RIAGENDR,
         SEQN) |>
  summary()

# extraindo a base do subset - o autor chamou de NHANES

id <- as_tibble(NHANES$variables$SEQN)
depresssion <- as_tibble(NHANES$variables$Depression.Score)
sexo <- as_tibble(NHANES$variables$RIAGENDR)

teste <- bind_cols(x = depresssion,y =  sexo, by = id)
head(teste)

# ordenando para ver os dados iguais
teste |>
  arrange(value...3)

# tamanho da amostra
count(teste)

# descritivo
summary(teste) # SEQN


glm(formula = value...1 ~ value...2,data = teste)
glm(formula = Depression.Score ~ RIAGENDR,data = One_sem_duplicados)

# Fim dos testes Saulo ------------------------------------------------------------------------

#' ## Analysis
# Define a function to call svymean and unweighted count
getSummary <- function(varformula, byformula, design){
  # Get mean, stderr, and unweighted sample size
  c <- svyby(varformula, byformula, design, unwtd.count )
  p <- svyby(varformula, byformula, design, svymean )
  outSum <- left_join(select(c,-se), p)
  outSum
}

#' ### Calculate prevalence of depression overall, by gender, by age group, and by age and gender
#' Adults
getSummary(~Depression, ~one, NHANES)
#' By sex
getSummary(~Depression, ~Gender, NHANES)
#' By age
getSummary(~Depression, ~Age.Group, NHANES)
#' By sex and age
getSummary(~Depression, ~Gender + Age.Group, NHANES)

#' ### Compare Prevalence Between Men And Women
svyttest(Depression~Gender, NHANES)$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="20-39"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Gender, subset(NHANES, Age.Group=="60 and over"))$p.value %>% as.numeric


#' ### Pairwise t-testing by age groups for total, men, and women
#' Differences by age group, among all adults
# Full output from svyttest command
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))
# Displaying p-values only
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="40-59"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="20-39" | Age.Group=="60 and over"))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Age.Group=="40-59" | Age.Group=="60 and over"))$p.value %>% as.numeric
#' Differences by age group, among men
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Men" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric
#' Differences by age group, among women
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="40-59")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="20-39" | Age.Group=="60 and over")))$p.value %>% as.numeric
svyttest(Depression~Age.Group, subset(NHANES, Gender=="Women" & (Age.Group=="40-59" | Age.Group=="60 and over")))$p.value %>% as.numeric

