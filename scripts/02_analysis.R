source("scripts/01_import.R")
library(DHARMa)
library(glmmTMB)
library(sjPlot)


pub_data_long

pub_noGFP <- pub_data_long %>% filter(!Promoter %in% c ("GFP")) %>% drop_na()

# Linear models ================================================================

## Promoters as discrete factors #####


glmm1 <- glmmTMB(as.integer(Values) ~ Promoter * cell_line + (1|experiment), data = pub_noGFP, family = nbinom1)

simout  <-  simulateResiduals(glmm1)
plot(simout, asFactor = T) 

# Means summaries ==================================================================

means <- emmeans::emmeans(glmm1, specs = "Promoter", "cell_line", type = "response")


# Model table ==================================================================

tab_model(glmm1)


# Promoters as linear variable #####

linear_pub <- pub_noGFP %>%  
  filter(!Promoter %in% c("no FF", "-133 \n(77bp intron)")) %>% 
  separate(Promoter, c("Length", "intron"), extra = "merge", remove = FALSE, sep = " ") %>% 
  mutate(Length = as.numeric(Length))

linear_pub$obs <- (1:nrow(linear_pub)) 


glmm2 <- glmmTMB(as.integer(Values) ~ Length * cell_line + (1|experiment), data = linear_pub, family = nbinom2)

emmeans::emmeans(glmm2, specs = "Length", "cell_line", type = "response", at = list(Length = c(-2565, -565, -465, -365, -265, -133))) %>% 
  as_tibble -> means2

#______________________________________________________________________________#####

# Combine summary tables ======================================================

combined_summary <- means %>% as_tibble() %>% separate(Promoter, c("Length", "intron"), extra = "merge", remove = FALSE, sep = " ") %>% 
  mutate(Length = as.numeric(Length)) %>% full_join(means2, by = c("Length", "cell_line")) %>% 
  mutate(intron = if_else(intron == "\n(77bp intron)", "short","full")) %>% 
  mutate(intron = replace_na(intron, "full"))


