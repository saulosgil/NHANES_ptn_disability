#' Example R code to replicate NCHS Data Brief No.303, Figures 1
#' Prevalence of Depression Among Adults Aged 20 and Over: United States, 2013-2016

#' Brody DJ, Pratt LA, Hughes JP. Prevalence of Depression Among Adults Aged 20 and Over: United
#' States, 2013-2016. NCHS Data Brief. No 303. Hyattsville, MD: National Center for Health Statistics. 2018.

#' Available at: https://www.cdc.gov/nchs/products/databriefs/db303.htm

#' ------------------------------------------------------------------------------------------------------------

# Load survey and dplyr packages
#+ message = FALSE, warning=FALSE
library(tidyverse)
library(survey)
library(GGally)
library(sjPlot)
#'
options(survey.lonely.psu='adjust')

# Display Version Information
cat("R package versions:\n")
for (p in c("base", "survey","dplyr")) {
  cat(p, ": ", as.character(packageVersion(p)), "\n")
}

# tirar notação cientifica
options(scipen = 999)

#' # Data preparation
# Download & Read SAS Transport Files
# Demographic (DEMO)
# Demographic ---------------------------------------------------------------------------------
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DEMO_G.XPT", tf <- tempfile(), mode="wb")
DEMO_11 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT", tf <- tempfile(), mode="wb")
DEMO_13 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DEMO_I.XPT", tf <- tempfile(), mode="wb")
DEMO_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT", tf <- tempfile(), mode="wb")
DEMO_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# Dietary interview ---------------------------------------------------------------------------
#11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DR1TOT_G.XPT", tf <- tempfile(), mode="wb")
DIET_11 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DR1TOT_H.XPT", tf <- tempfile(), mode="wb")
DIET_13 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DR1TOT_I.XPT", tf <- tempfile(), mode="wb")
DIET_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DR1TOT_J.XPT", tf <- tempfile(), mode="wb")
DIET_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "DR1TKCAL",
                                       "DR1TPROT",
                                       "DR1TCARB",
                                       "DR1TTFAT")]
# Body measures -------------------------------------------------------------------------------
# 11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BMX_G.XPT", tf <- tempfile(), mode="wb")
BODY_11 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BMX_H.XPT", tf <- tempfile(), mode="wb")
BODY_13 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BMX_I.XPT", tf <- tempfile(), mode="wb")
BODY_15 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT", tf <- tempfile(), mode="wb")
BODY_17 <- foreign::read.xport(tf)[, c("SEQN",
                                       "BMXWT",
                                       "BMXHT",
                                       "BMXBMI")]
# Disability ----------------------------------------------------------------------------------
#11-12
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PFQ_G.XPT", tf <- tempfile(), mode="wb")
PFQ_11 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PFQ061H",
                                      "PFQ061I",
                                      "PFQ061K",
                                      "PFQ061L")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PFQ_H.XPT", tf <- tempfile(), mode="wb")
PFQ_13 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PFQ061H",
                                      "PFQ061I",
                                      "PFQ061K",
                                      "PFQ061L")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PFQ_I.XPT", tf <- tempfile(), mode="wb")
PFQ_15 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PFQ061H",
                                      "PFQ061I",
                                      "PFQ061K",
                                      "PFQ061L")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PFQ_J.XPT", tf <- tempfile(), mode="wb")
PFQ_17 <- foreign::read.xport(tf)[, c("SEQN",
                                      "PFQ061H",
                                      "PFQ061I",
                                      "PFQ061K",
                                      "PFQ061L")]

# Diabetes ------------------------------------------------------------------------------------
#11-12
  download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/DIQ_G.XPT", tf <- tempfile(), mode="wb")
DM_11 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DIQ_H.XPT", tf <- tempfile(), mode="wb")
DM_13 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/DIQ_I.XPT", tf <- tempfile(), mode="wb")
DM_15 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DIQ_J.XPT", tf <- tempfile(), mode="wb")
DM_17 <- foreign::read.xport(tf)[, c("SEQN",
                                     "DIQ010")]

# HAS -----------------------------------------------------------------------------------------
#11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/BPQ_G.XPT", tf <- tempfile(), mode="wb")
HAS_11 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/BPQ_H.XPT", tf <- tempfile(), mode="wb")
HAS_13 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/BPQ_I.XPT", tf <- tempfile(), mode="wb")
HAS_15 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BPQ_J.XPT", tf <- tempfile(), mode="wb")
HAS_17 <- foreign::read.xport(tf)[, c("SEQN",
                                      "BPQ020")]
# heart conditions ----------------------------------------------------------------------------
#11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/MCQ_G.XPT", tf <- tempfile(), mode="wb")
HEARTH_11 <- foreign::read.xport(tf)[, c("SEQN",
                                         "MCQ160B",
                                         "MCQ160E")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/MCQ_H.XPT", tf <- tempfile(), mode="wb")
HEARTH_13 <- foreign::read.xport(tf)[, c("SEQN",
                                         "MCQ160B",
                                         "MCQ160E")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/MCQ_I.XPT", tf <- tempfile(), mode="wb")
HEARTH_15 <- foreign::read.xport(tf)[, c("SEQN",
                                         "MCQ160B",
                                         "MCQ160E")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/MCQ_J.XPT", tf <- tempfile(), mode="wb")
HEARTH_17 <- foreign::read.xport(tf)[, c("SEQN",
                                         "MCQ160B",
                                         "MCQ160E")]

# Physical activity ---------------------------------------------------------------------------
#11-12
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/PAQ_G.XPT", tf <- tempfile(), mode="wb")
PA_11 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ670",
                                     "PAD675")]
#13-14
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/PAQ_H.XPT", tf <- tempfile(), mode="wb")
PA_13 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ670",
                                     "PAD675")]
#15-16
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2015-2016/PAQ_I.XPT", tf <- tempfile(), mode="wb")
PA_15 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ670",
                                     "PAD675")]
#17-18
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/PAQ_J.XPT", tf <- tempfile(), mode="wb")
PA_17 <- foreign::read.xport(tf)[, c("SEQN",
                                     "PAQ610",
                                     "PAD615",
                                     "PAQ620",
                                     "PAQ625",
                                     "PAD630",
                                     "PAQ640",
                                     "PAD645",
                                     "PAQ655",
                                     "PAD660",
                                     "PAQ670",
                                     "PAD675")]

# Append files --------------------------------------------------------------------------------
DEMO <- dplyr::bind_rows(DEMO_11,
                         DEMO_13,
                         DEMO_15,
                         DEMO_17)



DIET <-  dplyr::bind_rows(DIET_11,
                          DIET_13,
                          DIET_15,
                          DIET_17)

BODY <-  dplyr::bind_rows(BODY_11,
                          BODY_13,
                          BODY_15,
                          BODY_17)

PFQ <-  dplyr::bind_rows(PFQ_11,
                         PFQ_13,
                         PFQ_15,
                         PFQ_17)

DM <-  dplyr::bind_rows(DM_11,
                        DM_13,
                        DM_15,
                        DM_17)

HAS <-  dplyr::bind_rows(HAS_11,
                         HAS_13,
                         HAS_15,
                         HAS_17)

HEARTH <-  dplyr::bind_rows(HEARTH_11,
                            HEARTH_13,
                            HEARTH_15,
                            HEARTH_17)

PA <-  dplyr::bind_rows(PA_11,
                        PA_13,
                        PA_15,
                        PA_17)

# Merges --------------------------------------------------------------------------------------
# Merge DEMO and DIET files

DEMO_DIET <- dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <- dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

#Merge DEMO_DIET_BODY AND PFQ

DEMO_DIET_BODY_PFQ <- dplyr::left_join(DEMO_DIET_BODY, PFQ, by="SEQN")

#Merge DEMO_DIET_BODY_PFQ AND DM

DEMO_DIET_BODY_PFQ_DM <- dplyr::left_join(DEMO_DIET_BODY_PFQ, DM, by="SEQN")

#Merge DEMO_DIET_BODY_PFQ_DM and HAS
DEMO_DIET_BODY_PFQ_DM_HAS <- dplyr::left_join(DEMO_DIET_BODY_PFQ_DM, HAS, by="SEQN")

#Merge DEMO_DIET_BODY_PFQ_DM_HAS AND HEARTH

DEMO_DIET_BODY_PFQ_DM_HAS_HEARTH <- dplyr::left_join(DEMO_DIET_BODY_PFQ_DM_HAS, HEARTH, by="SEQN")

#Merge DEMO_DIET_BODY_PFQ_DM_HAS_HEARTH AND PA

DEMO_DIET_BODY_PFQ_DM_HAS_HEARTH_PA <- dplyr::left_join(DEMO_DIET_BODY_PFQ_DM_HAS_HEARTH, PA, by="SEQN")

# Created new variables -----------------------------------------------------------------------

df <- DEMO_DIET_BODY_PFQ_DM_HAS_HEARTH_PA

###### salvando data.frame para explorar

readr::write_rds(x = df, file = "test.rds")

# Lendo a base --------------------------------------------------------------------------------

df <- read_rds(file = "test.rds")

glimpse(df)

# DataPrep ------------------------------------------------------------------------------------
One <-
  df |>
  # remove duplicates
    dplyr::distinct(SEQN, .keep_all = TRUE) |>
  # adjusting physical activity - where is NA to change to zero
  dplyr::mutate(
    PAQ610 = tidyr::replace_na(PAQ610, 0),
    PAD615 = tidyr::replace_na(PAD615, 0),
    PAQ620 = tidyr::replace_na(PAQ620, 0),
    PAQ625 = tidyr::replace_na(PAQ625, 0),
    PAD630 = tidyr::replace_na(PAD630, 0),
    PAQ640 = tidyr::replace_na(PAQ640, 0),
    PAD645 = tidyr::replace_na(PAD645, 0),
    PAQ655 = tidyr::replace_na(PAQ655, 0),
    PAD660 = tidyr::replace_na(PAD660, 0),
    PAQ670 = tidyr::replace_na(PAQ670, 0),
    PAD675 = tidyr::replace_na(PAD675, 0)
  ) |>
  # created physical activity outcomes
  dplyr::mutate(
    # age_class
    AGE = case_when(RIDAGEYR >= 65 & RIDAGEYR < 80 ~ "< 80 years",
                    RIDAGEYR >= 80 ~ ">= 80 years"),
    # PA work
    PAW = PAQ610 * PAD615 + PAQ625 * PAD630,
    # PA transport
    PAT = PAQ640 * PAD645,
    # PA leisure
    PAL = PAQ655 * PAD660 + PAQ670 * PAD675,
    PATOTAL = PAW + PAT + PAL,
    PA_CLASS = case_when(PATOTAL >= 150 ~ "ATIVO",
                         PATOTAL < 150 ~ "INATIVO")
  ) |>
# adjusting physical functioning (disability) parameters
  dplyr::mutate(WALKING_ROOMS = PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ061I) |>
  dplyr::mutate(EATING = PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ061L) |>
# To create the variable INCAPAZ - PRIMARY OUTCOME
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1),
         STANDINGUP_NOVO = case_when(STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1),
         EATING_NOVO = case_when(EATING == 1 ~ 0,
                                 EATING >=2 & EATING <=4 ~ 1),
         DRESSING_NOVO = case_when(DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ < 1 ~ 0, # no disability
                                    INCAPAZ >= 1 & INCAPAZ <= 16 ~ 1),
         OBESITY = case_when(BMXBMI >= 30 ~ "OBESO",
                             BMXBMI < 30 ~ "NORMAL"),
         ENERGY = DR1TKCAL,
         PTN = DR1TPROT,
         PTNKG = PTN/BMXWT,
         CHO = DR1TCARB,
         FAT = DR1TTFAT,
         ENERGY_KG = ENERGY/BMXWT,
         ENERGY_PT_MODEL = ENERGY - PTN * 4,
         ENERGY_STATUS = case_when(RIAGENDR == 1 & ENERGY < 800 ~ "UNLIKELY",
                                   RIAGENDR == 1 & ENERGY > 4000 ~ "UNLIKELY",
                                   RIAGENDR == 1 & ENERGY >= 800 & ENERGY <=4000 ~ "LIKELY",
                                   RIAGENDR == 2 & ENERGY < 500 ~ "UNLIKELY",
                                   RIAGENDR == 2 & ENERGY > 3500 ~ "UNLIKELY",
                                   RIAGENDR == 2 & ENERGY >= 500 & ENERGY <=3500 ~ "LIKELY"),
         # create protein consumption status
         PTN_STATUS = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                PTNKG >= 1.2  & PTNKG < 1.6 ~ "C_MODERADO",
                                PTNKG >= 1.6 ~ "D_ELEVADO"),
         # create prOtein consumptoin status RDA
         PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                             PTNKG >=0.8 ~ "B_ADEQUADO"),
         # Saulo/Hamilton protein consumption status
         PTN_STATUS_ROSCHEL = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                        PTNKG >= 0.8 & PTNKG < 1.2 ~ "B_ADEQUADO",
                                        PTNKG >= 1.2 ~ "D_ELEVADO"),
         # classes de ptn apenas no grupo LOW (abaixo da RDA)
         PTN_MID_HIGH = case_when(PTNKG < 1.2 ~ "A_ate1.2",
                                  PTNKG >= 1.2 & PTNKG < 1.6 ~ "B_1.2_a_1.6",
                                  PTNKG >= 1.6 ~ "D_acima1.4"),
         # create pritein consumptoin status RDA
         PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                             PTNKG >=0.8 ~ "B_ADEQUADO"),
         # Peso de 8 anos
         MEC8YR = WTMEC2YR * 1/4,
         inAnalysis = (RIDAGEYR >= 65 & !is.na(INCAPAZ_CLASSE) & !is.na(OBESITY) & !is.na(PTN_RDA) & ENERGY_STATUS == "LIKELY")
         )

#' ## Define survey design
# Define survey design for overall dataset
NHANES_all <- svydesign(data=One, id=~SDMVPSU, strata=~SDMVSTRA, weights=~MEC8YR, nest=TRUE)

# Create a survey design object for the subset of interest: adults aged 20 and over with a valid depression score
# Subsetting the original survey design object ensures we keep the design information about the number of clusters and strata
NHANES <- subset(NHANES_all, inAnalysis)

# creating database from subset - the author called of NHANES

id <- as_tibble(NHANES$variables$SEQN)
sexo <- as_tibble(NHANES$variables$RIAGENDR)
raca <- as_tibble(NHANES$variables$RIDRETH1)
idade <- as_tibble(NHANES$variables$RIDAGEYR)
idade_class <- as_tibble(NHANES$variables$AGE)
peso <- as_tibble(NHANES$variables$BMXWT)
energy <- as_tibble(NHANES$variables$ENERGY)
cho <- as_tibble(NHANES$variables$CHO)
fat <- as_tibble(NHANES$variables$FAT)
ptn <- as.tibble(NHANES$variables$PTN)
ptn_kg <- as_tibble(NHANES$variables$PTNKG)
ptn_status <- as_tibble(NHANES$variables$PTN_STATUS)
functioning <- as.tibble(NHANES$variables$INCAPAZ_CLASSE)
obesity <- as_tibble(NHANES$variables$OBESITY)
ptn_rda <- as_tibble(NHANES$variables$PTN_RDA)
energy_kg <- as_tibble(NHANES$variables$ENERGY_KG)
energy_pt_model <- as_tibble(NHANES$variables$ENERGY_PT_MODEL)
diabetes <- as_tibble(NHANES$variables$DIQ010)
has <- as_tibble(NHANES$variables$BPQ020)
heart_failure <- as_tibble(NHANES$variables$MCQ160B)
heart_attack <- as_tibble(NHANES$variables$MCQ160E)
patotal <- as_tibble(NHANES$variables$PATOTAL)
pa_class <- as_tibble(NHANES$variables$PA_CLASS)

# create database to analyse

df_ajust <-
  bind_cols(
    id,
    sexo,
    raca,
    idade,
    idade_class,
    peso,
    energy,
    cho,
    fat,
    ptn,
    ptn_kg,
    ptn_status,
    ptn_rda,
    functioning,
    obesity,
    energy_kg,
    energy_pt_model,
    diabetes,
    has,
    heart_failure,
    heart_attack
  ) |>
  rename(
    "id" = value...1,
    "sexo" = value...2,
    "raca" = value...3,
    "idade" = value...4,
    "idade_class" = value...5,
    "peso" = value...6,
    "energy" = value...7,
    "cho" = value...8,
    "fat" = value...9,
    "ptn" = value...10,
    "ptn_kg" = value...11,
    "ptn_status" = value...12,
    "ptn_rda" = value...13,
    "functioning" = value...14,
    "obesity" = value...15,
    "energy_kg" = value...16,
    "energy_pt_model" = value...17,
    "diabetes" = value...18,
    "has" = value...19,
    "heart_failure" = value...20,
    "heart_attack" = value...21
  )

# writing a df to explore

readr::write_rds(x = df_ajust, file = "df_ajust.rds")

# reading new database

df_ajust <- readr::read_rds("df_ajust.rds")

# Exploratory analysis ------------------------------------------------------------------------

# General Descriptive and distribution analysis

glimpse(df_ajust)

skimr::skim(df_ajust)
