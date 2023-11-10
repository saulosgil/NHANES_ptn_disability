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

#' # Data preparation
# Download & Read SAS Transport Files
# Demographic (DEMO)

# Demographic ---------------------------------------------------------------------------------
# 99-00
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/DEMO.XPT", tf <- tempfile(), mode="wb")
DEMO_99 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/DEMO_B.XPT", tf <- tempfile(), mode="wb")
DEMO_01 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC4YR")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/DEMO_C.XPT", tf <- tempfile(), mode="wb")
DEMO_03 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
                                       "RIAGENDR",
                                       "RIDAGEYR",
                                       "SDMVSTRA",
                                       "SDMVPSU",
                                       "RIDRETH1",
                                       "WTMEC2YR")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DEMO_D.XPT", tf <- tempfile(), mode="wb")
DEMO_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "SDDSRVYR",
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

# QUESTIONARY ---------------------------------------------------------------------------------
# physical functioning ------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/PFQ.XPT", tf <- tempfile(), mode="wb")
Q_99 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ060A",
                                    "PFQ060B",
                                    "PFQ060C",
                                    "PFQ060D",
                                    "PFQ060E",
                                    "PFQ060F",
                                    "PFQ060G",
                                    "PFQ060H",
                                    "PFQ060I",
                                    "PFQ060J",
                                    "PFQ060K",
                                    "PFQ060L")]


# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/PFQ_B.XPT", tf <- tempfile(), mode="wb")
Q_01 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ060A",
                                    "PFQ060B",
                                    "PFQ060C",
                                    "PFQ060D",
                                    "PFQ060E",
                                    "PFQ060F",
                                    "PFQ060G",
                                    "PFQ060H",
                                    "PFQ060I",
                                    "PFQ060J",
                                    "PFQ060K",
                                    "PFQ060L")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/PFQ_C.XPT", tf <- tempfile(), mode="wb")
Q_03 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ061A",
                                    "PFQ061B",
                                    "PFQ061C",
                                    "PFQ061D",
                                    "PFQ061E",
                                    "PFQ061F",
                                    "PFQ061G",
                                    "PFQ061H",
                                    "PFQ061I",
                                    "PFQ061J",
                                    "PFQ061K",
                                    "PFQ061L")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/PFQ_D.XPT", tf <- tempfile(), mode="wb")
Q_05 <- foreign::read.xport(tf)[, c("SEQN",
                                    "PFQ061A",
                                    "PFQ061B",
                                    "PFQ061C",
                                    "PFQ061D",
                                    "PFQ061E",
                                    "PFQ061F",
                                    "PFQ061G",
                                    "PFQ061H",
                                    "PFQ061I",
                                    "PFQ061J",
                                    "PFQ061K",
                                    "PFQ061L")]

# LABORATORY ---------------------------------------------------------------------------------

# CHOLESTEROL AND HDL -------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB13.XPT", tf <- tempfile(), mode="wb")
hdl_chol_99 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXTC",
                                           "LBDHDL")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13_B.XPT", tf <- tempfile(), mode="wb")
hdl_chol_01 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXTC",
                                           "LBDHDL")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13_C.XPT", tf <- tempfile(), mode="wb")
hdl_chol_03 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXTC",
                                           "LBXHDD")]

# 05-06 CHOLESTEROL
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/TCHOL_D.XPT", tf <- tempfile(), mode="wb")
chol_05 <- foreign::read.xport(tf)[, c("SEQN",
                                       "LBXTC")]

# 05-06 HDL
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/HDL_D.XPT", tf <- tempfile(), mode="wb")
hdl_05 <- foreign::read.xport(tf)[, c("SEQN",
                                      "LBDHDD")]


# LDL and triglycerides -----------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB13AM.XPT", tf <- tempfile(), mode="wb")
ldl_tg_99 <- foreign::read.xport(tf)[, c("SEQN",
                                         "LBXTR",
                                         "LBDLDL")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L13AM_B.XPT", tf <- tempfile(), mode="wb")
ldl_tg_01 <- foreign::read.xport(tf)[, c("SEQN",
                                         "LBXTR",
                                         "LBDLDL")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L13AM_C.XPT", tf <- tempfile(), mode="wb")
ldl_tg_03 <- foreign::read.xport(tf)[, c("SEQN",
                                         "LBXTR",
                                         "LBDLDL")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/TRIGLY_D.XPT", tf <- tempfile(), mode="wb")
ldl_tg_05 <- foreign::read.xport(tf)[, c("SEQN",
                                         "LBXTR",
                                         "LBDLDL")]

# C-REACTIVE PROTEIN --------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB11.XPT", tf <- tempfile(), mode="wb")
rcp_99 <- foreign::read.xport(tf)[, c("SEQN",
                                      "LBXCRP")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L11_B.XPT", tf <- tempfile(), mode="wb")
rcp_01 <- foreign::read.xport(tf)[, c("SEQN",
                                      "LBXCRP")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L11_C.XPT", tf <- tempfile(), mode="wb")
rcp_03 <- foreign::read.xport(tf)[, c("SEQN",
                                      "LBXCRP")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/CRP_D.XPT", tf <- tempfile(), mode="wb")
rcp_05 <- foreign::read.xport(tf)[, c("SEQN",
                                      "LBXCRP")]

# GLICATED HEMOGLOBIN -------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10.XPT", tf <- tempfile(), mode="wb")
gh_99 <- foreign::read.xport(tf)[, c("SEQN",
                                     "LBXGH")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10_B.XPT", tf <- tempfile(), mode="wb")
gh_01 <- foreign::read.xport(tf)[, c("SEQN",
                                     "LBXGH")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10_C.XPT", tf <- tempfile(), mode="wb")
gh_03 <- foreign::read.xport(tf)[, c("SEQN",
                                     "LBXGH")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GHB_D.XPT", tf <- tempfile(), mode="wb")
gh_05 <- foreign::read.xport(tf)[, c("SEQN",
                                     "LBXGH")]

# Glucose and insulin -------------------------------------------------------------------------

download.file("https://wwwn.cdc.gov/Nchs/Nhanes/1999-2000/LAB10AM.XPT", tf <- tempfile(), mode="wb")
glic_ins_99 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXIN",
                                           "LBXGLU")]

# 01-02
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2001-2002/L10AM_B.XPT", tf <- tempfile(), mode="wb")
glic_ins_01 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXIN",
                                           "LBXGLU")]

# 03-04
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2003-2004/L10AM_C.XPT", tf <- tempfile(), mode="wb")
glic_ins_03 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXIN",
                                           "LBXGLU")]

# 05-06
download.file("https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/GLU_D.XPT", tf <- tempfile(), mode="wb")
glic_ins_05 <- foreign::read.xport(tf)[, c("SEQN",
                                           "LBXIN",
                                           "LBXGLU")]


# Append Files
DEMO <- dplyr::bind_rows(DEMO_99,
                         DEMO_01,
                         DEMO_03,
                         DEMO_05)

DEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


DIET <-  dplyr::bind_rows(DIET_99,
                          DIET_01,
                          DIET_03,
                          DIET_05)

DIET |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

BODY <-  dplyr::bind_rows(BODY_99,
                          BODY_01,
                          BODY_03,
                          BODY_05)

BODY |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

DEXA <-  dplyr::bind_rows(DEXA_99,
                          DEXA_01,
                          DEXA_03,
                          DEXA_05)

DEXA_WITHOUT_DUPLICATE <- DEXA |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES" NEED TREATMENT!

QUEST <- dplyr::bind_rows(Q_99,
                          Q_01,
                          Q_03,
                          Q_05)

QUEST |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


RCP <- dplyr::bind_rows(rcp_99,
                        rcp_01,
                        rcp_03,
                        rcp_05)

RCP |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

HEMO <- dplyr::bind_rows(gh_99,
                         gh_01,
                         gh_03,
                         gh_05)

HEMO |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

GlU_INS <- dplyr::bind_rows(glic_ins_99,
                           glic_ins_01,
                           glic_ins_03,
                           glic_ins_05)

GlU_INS |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"


LDL_TG <- dplyr::bind_rows(ldl_tg_99,
                           ldl_tg_01,
                           ldl_tg_03,
                           ldl_tg_05)

LDL_TG |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# HDL and TC from 99 to 03
HDL_CHOL <- dplyr::bind_rows(hdl_chol_99,
                             hdl_chol_01,
                             hdl_chol_03)

# Appeding TC due to different links for data
TC_99_03 <- HDL_CHOL |> select(SEQN, LBXTC)

TC <- dplyr::bind_rows(TC_99_03, chol_05)

TC |> dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "none"

# Appeding HDL due to different links for data
HDL_99_01 <- HDL_CHOL |> select(SEQN, LBDHDL)

HDL_03 <- HDL_CHOL |>
  select(SEQN, LBXHDD) |>
  rename(LBDHDL = LBXHDD)

HDL_05 <- hdl_05 |> rename(LBDHDL = LBDHDD)

HDL <- dplyr::bind_rows(HDL_99_01,
                        HDL_03,
                        HDL_05)

HDL_ajustado <- HDL |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES" NEED TREATMENT!


# Merge DEMO and DIET files

DEMO_DIET <-
  dplyr::left_join(DEMO, DIET, by="SEQN")

# Merge DEMO_DIET and BODY

DEMO_DIET_BODY <-
  dplyr::left_join(DEMO_DIET, BODY, by="SEQN")

# Merge DEMO_DIET_BODY and DEXA_ajust

DEMO_DIET_BODY_DEXA <-
  dplyr::left_join(DEMO_DIET_BODY, DEXA_WITHOUT_DUPLICATE, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA and QUEST

DEMO_DIET_BODY_DEXA_QUEST <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA, QUEST, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST and TC

DEMO_DIET_BODY_DEXA_QUEST_TC <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST, TC, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST_TC and HDL

DEMO_DIET_BODY_DEXA_QUEST_TC_HDL <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST_TC, HDL, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST_HDL_TC and RCP

DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST_TC_HDL, RCP, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP and HEMO

DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP, HEMO, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO and GLUC_INS

DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO_GLU_INS <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO, GlU_INS, by="SEQN")

# Merge DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO_GLUC_INS and LDL_TG

DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO_GLU_INS_LDL_TG <-
  dplyr::left_join(DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO_GLU_INS, LDL_TG, by="SEQN")

df_bruto <- DEMO_DIET_BODY_DEXA_QUEST_TC_HDL_RCP_HEMO_GLU_INS_LDL_TG

df <- df_bruto |>
  dplyr::distinct(SEQN, .keep_all = TRUE) # testing duplicade rows - "YES" NEED TREATMENT!


# Created new variables -----------------------------------------------------------------------

###### salvando data.frame para explorar

readr::write_csv2(x = df, file = "df.csv")

# Lendo a base --------------------------------------------------------------------------------

df <- read.csv2(file = "df.csv")

# DataPrep ------------------------------------------------------------------------------------

One_1 <-
  df |>
  # input 0 in NA due to different variable names
  dplyr::mutate(DRXTPROT = tidyr::replace_na(DRXTPROT, 0),
                DR1TPROT = tidyr::replace_na(DR1TPROT, 0),
                DR1TCARB = tidyr::replace_na(DR1TCARB, 0),
                DRXTCARB = tidyr::replace_na(DRXTCARB, 0),
                DRXTKCAL = tidyr::replace_na(DRXTKCAL, 0),
                DR1TKCAL = tidyr::replace_na(DR1TKCAL, 0),
                DRXTTFAT = tidyr::replace_na(DRXTTFAT, 0),
                DR1TTFAT = tidyr::replace_na(DR1TTFAT, 0),
                DRXTCALC = tidyr::replace_na(DRXTCALC, 0),
                DR1TCALC = tidyr::replace_na(DR1TCALC, 0),
                PFQ060A = tidyr::replace_na(PFQ060A, 0),
                PFQ061A = tidyr::replace_na(PFQ061A, 0),
                PFQ060B = tidyr::replace_na(PFQ060B, 0),
                PFQ061B = tidyr::replace_na(PFQ061B, 0),
                PFQ060C = tidyr::replace_na(PFQ060C, 0),
                PFQ061C = tidyr::replace_na(PFQ061C, 0),
                PFQ060D = tidyr::replace_na(PFQ060D, 0),
                PFQ061D = tidyr::replace_na(PFQ061D, 0),
                PFQ060E = tidyr::replace_na(PFQ060E, 0),
                PFQ061E = tidyr::replace_na(PFQ061E, 0),
                PFQ060F = tidyr::replace_na(PFQ060F, 0),
                PFQ061F = tidyr::replace_na(PFQ061F, 0),
                PFQ060G = tidyr::replace_na(PFQ060G, 0),
                PFQ061G = tidyr::replace_na(PFQ061G, 0),
                PFQ060H = tidyr::replace_na(PFQ060H, 0),
                PFQ061H = tidyr::replace_na(PFQ061H, 0),
                PFQ060I = tidyr::replace_na(PFQ060I, 0),
                PFQ061I = tidyr::replace_na(PFQ061I, 0),
                PFQ060J = tidyr::replace_na(PFQ060J, 0),
                PFQ061J = tidyr::replace_na(PFQ061J, 0),
                PFQ060K = tidyr::replace_na(PFQ060K, 0),
                PFQ061K = tidyr::replace_na(PFQ061K, 0),
                PFQ060L = tidyr::replace_na(PFQ060L, 0),
                PFQ061L = tidyr::replace_na(PFQ061L, 0)
)

# adjusting physical function parameters

One_2 <-
  One_1 |>
  dplyr::mutate(MONEY_FUNCTION = PFQ060A + PFQ061A) |>
  dplyr::mutate(WALKING_MILE = PFQ060B + PFQ061B) |>
  dplyr::mutate(WALKING_STEPS = PFQ060C + PFQ061C) |>
  dplyr::mutate(STOOPING = PFQ060D + PFQ061D) |>
  dplyr::mutate(LIFTING = PFQ060E + PFQ061E) |>
  dplyr::mutate(HOUSE_CHORE = PFQ060F + PFQ061F) |>
  dplyr::mutate(PREP_MEALS = PFQ060G + PFQ061G) |>
  dplyr::mutate(WALKING_ROOMS = PFQ060H + PFQ061H) |>
  dplyr::mutate(STANDINGUP = PFQ060I + PFQ061I) |>
  dplyr::mutate(BED_DIFFICULT = PFQ060J + PFQ061J) |>
  dplyr::mutate(EATING = PFQ060K + PFQ061K) |>
  dplyr::mutate(DRESSING = PFQ060L + PFQ061L)

# To create the variable INCAPAZ - PRIMARY OUTCOME
One_3 <-
  One_2 |>
  mutate(WALKING_ROOMS_NOVO = case_when(WALKING_ROOMS == 0 ~ 1000,
                                        WALKING_ROOMS == 1 ~ 0,
                                        WALKING_ROOMS >=2 & WALKING_ROOMS <=4 ~ 1,
                                        WALKING_ROOMS >= 5 & WALKING_ROOMS < 10 ~ 100),
         STANDINGUP_NOVO = case_when(STANDINGUP == 0 ~ 1000,
                                     STANDINGUP == 1 ~ 0,
                                     STANDINGUP >=2 & WALKING_ROOMS <=4 ~ 1,
                                     STANDINGUP >= 5 & STANDINGUP < 10 ~ 100),
         EATING_NOVO = case_when(EATING == 0 ~ 1000,
                                 EATING == 1 ~ 0,
                                 EATING >=2 & EATING <=4 ~ 1,
                                 EATING >= 5 & EATING < 10 ~ 100),
         DRESSING_NOVO = case_when(DRESSING == 0 ~ 1000,
                                   DRESSING == 1 ~ 0,
                                   DRESSING >=2 & WALKING_ROOMS <=4 ~ 1,
                                   DRESSING >= 5 & DRESSING < 10 ~ 100),
         INCAPAZ = WALKING_ROOMS_NOVO + STANDINGUP_NOVO + EATING_NOVO + DRESSING_NOVO,
         INCAPAZ_CLASSE = case_when(INCAPAZ <= 1 ~ 0, # no disability
                                    INCAPAZ > 1 & INCAPAZ <= 16 ~ 1, # disability
                                    INCAPAZ > 12 & INCAPAZ <=300 ~ 3))

# Creating HOMA-IR

# HOMA-IR function

HOMA <- function(LBXGLU, LBXIN){
  x <- LBXGLU / 18
  y <- LBXIN / 22.5
  x * y
}

One_4 <-
  One_3 |>
  # calculate HOMA-IR
  dplyr::mutate(HOMA_IR = HOMA(LBXGLU, LBXIN))

# Creating apendicular lean mass, osteoporose and protein uptake variables

One_5 <-
  One_4 |>
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
                # create RELATIVE ENERGY - ENERGY/KG
                ENERGY_KG = ENERGY/BMXWT,
                # create ENERGY_PT_MODEL
                ENERGY_PT_MODEL = ENERGY - PTN * 4,
                # create energy class for unlikely data
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
                # create pritein consumptoin status RDA
                PTN_RDA = case_when(PTNKG < 0.8 ~ "A_BAIXO",
                                    PTNKG >=0.8 ~ "B_ADEQUADO"),
                # create low ALM
                ALM_STATUS = case_when(RIAGENDR == 2 & APLMBMI < 512 ~ "BAIXO",
                                       RIAGENDR == 2 & APLMBMI >= 521 ~ "ALTO",
                                       RIAGENDR == 1 & APLMBMI < 789 ~ "BAIXO",
                                       RIAGENDR == 1 & APLMBMI >= 789 ~ "ALTO"),
                # create low alm plus obesity
                SARC = case_when(ALM_STATUS == "BAIXO" & OBESITY == "OBESO" ~ "SARCOBESO",
                                 ALM_STATUS == "ALTO" & OBESITY == "OBESO" ~ "OBESO",
                                 ALM_STATUS == "BAIXO" & OBESITY == "NORMAL" ~ "SARC",
                                 ALM_STATUS == "ALTO" & OBESITY == "NORMAL" ~ "NORMAL"),
                # Peso de 8 anos
                MEC8YR = case_when(SDDSRVYR <= 2 ~ 2/4 * WTMEC4YR,
                                  (SDDSRVYR > 2 ~ 1/4 * WTMEC2YR)),
                inAnalysis = (RIDAGEYR >= 65 & !is.na(INCAPAZ_CLASSE) & !is.na(PTN_STATUS) & INCAPAZ_CLASSE != 3 & ENERGY_STATUS == "LIKELY")
)

# Final database

One <- One_5

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
peso <- as_tibble(NHANES$variables$BMXWT)
bmi <- as_tibble(NHANES$variables$BMXBMI)
alm <- as_tibble(NHANES$variables$APLM)
almbmi <- as_tibble(NHANES$variables$APLMBMI)
alm_status <- as_tibble(NHANES$variables$ALM_STATUS)
dmo_lombar <- as_tibble(NHANES$variables$DXXLSBMD)
energy <- as_tibble(NHANES$variables$ENERGY)
cho <- as_tibble(NHANES$variables$CHO)
fat <- as_tibble(NHANES$variables$FAT)
ptn <- as.tibble(NHANES$variables$PTN)
ptn_kg <- as_tibble(NHANES$variables$PTNKG)
ptn_status <- as_tibble(NHANES$variables$PTN_STATUS)
calcio <- as_tibble(NHANES$variables$CAL)
homa <- as.tibble(NHANES$variables$HOMA_IR)
hemoglobina <- as.tibble(NHANES$variables$LBXGH)
glicose <- as.tibble(NHANES$variables$LBXGLU)
hdl <- as_tibble(NHANES$variables$LBDHDL)
ldl <- as.tibble(NHANES$variables$LBDLDL)
colesterol <- as.tibble(NHANES$variables$LBXTC)
pcr <- as.tibble(NHANES$variables$LBXCRP)
functioning <- as.tibble(NHANES$variables$INCAPAZ_CLASSE)
obesity <- as_tibble(NHANES$variables$OBESITY)
osteoporose <- as_tibble(NHANES$variables$OSSO)
sarc <- as_tibble(NHANES$variables$SARC)
ptn_rda <- as_tibble(NHANES$variables$PTN_RDA)
energy_kg <- as_tibble(NHANES$variables$ENERGY_KG)
energy_pt_model <- as_tibble(NHANES$variables$ENERGY_PT_MODEL)

# create database to analyse

df_ajust <-
  bind_cols(id,
            sexo,
            raca,
            idade,
            peso,
            bmi,
            alm,
            almbmi,
            alm_status,
            dmo_lombar,
            energy,
            cho,
            fat,
            ptn,
            ptn_status,
            calcio,
            homa,
            hemoglobina,
            glicose,
            hdl,
            ldl,
            colesterol,
            pcr,
            functioning,
            obesity,
            osteoporose,
            sarc,
            ptn_rda,
            energy_kg,
            energy_pt_model,
            ptn_kg) |>
  rename("id" = value...1,
         "sexo" = value...2,
         "raca" = value...3,
         "idade" = value...4,
         "peso" = value...5,
         "bmi" = value...6,
         "alm" = value...7,
         "almbmi" = value...8,
         "alm_status" = value...9,
         "dmo_lombar" = value...10,
         "energy" = value...11,
         "cho" = value...12,
         "fat" = value...13,
         "ptn" = value...14,
         "ptn_status" = value...15,
         "calcio" = value...16,
         "homa" = value...17,
         "hemoglobina" = value...18,
         "glicose" = value...19,
         "hdl" = value...20,
         "ldl" = value...21,
         "colesterol" = value...22,
         "pcr" = value...23,
         "functioning" = value...24,
         "obesity" = value...25,
         "osteoporose" = value...26,
         "sarc" = value...27,
         "ptn_rda" = value...28,
         "energy_kg" = value...29,
         "energy_pt_model" = value...30,
         "ptn_kg" = value...31)

# writing a df to explore

readr::write_csv2(x = df_ajust, file = "df_ajust.csv")

# reading new database

df_ajust <- read.csv2("df_ajust.csv")

# Exploratory analysis ------------------------------------------------------------------------

# General Descriptive and distribution analysis

glimpse(df_ajust)

skimr::skim(df_ajust)

# TESTING MODELS

## crude logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
rda_crude <- glm(as.factor(functioning) ~ ptn_rda, family = binomial, data = df_ajust)

sjPlot::plot_model(rda_crude,show.values = TRUE)

sjPlot::tab_model(rda_crude, show.intercept = FALSE)

## Adjusted logistic regression -  Consumo de PTN (RDA - >= 0.8 ptn = adequado)
rda_adjusted <- glm(as.factor(functioning) ~ ptn_rda + idade + sexo + raca + energy_pt_model, family = binomial, data = df_ajust)

sjPlot::plot_model(rda_adjusted,show.values = TRUE)

sjPlot::tab_model(rda_adjusted, show.intercept = FALSE)

## crude logistic regression -  Consumo de PTN (STU PHILLIPS)
stu_crude <- glm(as.factor(functioning) ~ ptn_status, family = binomial, data = df_ajust)

sjPlot::plot_model(stu_crude,show.values = TRUE)

sjPlot::tab_model(stu_crude, show.intercept = FALSE)

## Adjusted logistic regression -  Consumo de PTN (STU PHILLIPS)
stu_adjusted <- glm(as.factor(functioning) ~ ptn_status + idade + sexo + raca + energy_pt_model, family = binomial, data = df_ajust)

sjPlot::plot_model(stu_adjusted,show.values = TRUE)

sjPlot::tab_model(stu_adjusted, show.intercept = FALSE)










