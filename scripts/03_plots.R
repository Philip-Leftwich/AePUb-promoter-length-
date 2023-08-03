source("scripts/02_analysis.R")
source("scripts/functions.R")

# Packages ===========================================================
# importing required packages
library(showtext)
library(patchwork)
library(ggbeeswarm)
library(scales)
#library(camcorder)
# ___________________________________________________________________ ####

font_add_google("Open Sans", "Sans")
showtext_begin()

# Make image files as I go to check formatting
#camcorder::gg_record(
#  dir = 'figures',
#  width=12, 
#  height=7, 
#  units="in", 
#  dpi=600
#)



# Figures ===========================================================

supp.labs <-as_labeller(c(`aag2`="A) Aag2 cells", 
                          `C636`= "B) C6.36 cells",
                          `u4.4`="c) U4.4 cells"))


combined_summary %>% 
  ggplot(., aes(x = Length, y = log10(response.y), colour = cell_line, fill = cell_line, shape = intron))+
  geom_ribbon(aes(ymin = asymp.LCL.y, ymax = asymp.UCL.y), alpha = 0.2, colour = "white")+
  geom_beeswarm(data = linear_pub, aes(x = Length, y = Values, colour = cell_line,group = cell_line),shape=21, dodge=0.3, fill="white", alpha=0.3, size=0.8)+  
  geom_point(aes(x = Length, y = response.x),position = position_dodge(width = 0.3), size = 3)+
  geom_linerange(aes(min = asymp.LCL.x, max = asymp.UCL.x),position = position_dodge(width = 0.3))+
  facet_wrap(~cell_line,  labeller = labeller(cell_line = supp.labs))+
#  scale_y_log10(labels = label_log())+
  scale_x_continuous(label = label_number(big.mark = "", suffix = "bp"))+
  scale_color_manual(values = c("#3d6721", "#a86826", "#006c89"))+
  scale_fill_manual(values = c("#3d6721", "#a86826", "#006c89"))+
  scale_shape_manual(values = c(21, 24), labels = c("615bp intron",
                                "77bp intron"))+
  labs(x = "", y = "Firefly/Renilla Luciferase ratio", color = "Cell Line", fill = "Cell Line")+
  theme_custom()+
  ylim(0,175)+
  guides(colour = "none",
         fill = "none")+
  theme(strip.text = element_text(hjust = 0),
        legend.position = c(0.92,0.8))

# ___________________________________________________________________ ####

# Export =================================================================

 ggsave("figures/Pub_estimates.png", type="cairo", width=12, height=7, units="in", dpi=600)


# ___________________________________________________________________####

# Session Info ==========================================================

sink("sessionInfo.txt")
sessionInfo()
sink()
 