# Packages 📦 ====================================================
# importing required packages
library(readxl)    
library(tidyverse)

# Read data 👀 ======================================================

#  + specifying the path name ####
path <- "data/pub_luc_assay.xlsx"
  
# + getting info about all excel sheets ####
sheets <- readxl::excel_sheets(path)

  

# + read sheets in two iterations due to formatting differences ####
tibble1 <- lapply(1:8, function(i) read_excel(path, sheet = i, range = "B32:I40"))
names(tibble1) <- sheets[1:8]

tibble2 <- lapply(9:17, function(i) read_excel(path, sheet = i, range = "C38:K44"))
names(tibble2) <- sheets[9:17]

tibble1 <- enframe(tibble1) %>% 
  unnest()

tibble2 <- enframe(tibble2) %>%
  unnest()

# separate list names into cell lines and experiments
pub_data <- bind_rows(tibble1, tibble2) %>% 
  separate(name, into=c("cell_line", "experiment"), sep=" ", remove=FALSE, extra = "merge")

# Long data ============================================

pub_data_long <- pub_data %>% 
  pivot_longer(
    cols = `no FF`: GFP,
    names_to = "Promoter",
    values_to = "Values")

# Set Promoters as factors =============================

pub_data_long$Promoter <- factor(pub_data_long$Promoter)

pub_data_long$Promoter <- fct_recode(pub_data_long$Promoter, "-2565" = "-2565", 
                                     "-565" = "full -565",
                                     "-465" = "-465",
                                     "-365" = "-365",
                                     "-265" = "-265",
                                     "-133" = "-133",
                                     "-133 \n(77bp intron)" = "-133(77bp intron)",
                                     "GFP" = "GFP",
                                     "no FF"= "no FF")
pub_data_long$Promoter <- fct_relevel(pub_data_long$Promoter,
                                      c("-2565", 
                                        "-565",
                                        "-465", 
                                        "-365",
                                        "-265", 
                                        "-133", 
                                        "-133 \n(77bp intron)",
                                        "GFP",
                                        "no FF"))
