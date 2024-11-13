library(tidyverse)
library(ggplot2)
library(openxlsx)
library(ggpmisc)
library(ggpubr)
library(patchwork)
library(grid)
library(gridExtra) 
library(cowplot)
library(ggtext)


library(emmeans)
library(effectsize)

# set working directory
setwd("...")


# load full extensive data
expi <- read.xlsx("sleep_experience_master.xlsx")


# colors
paperstages <- expi %>% group_by(paper_label) %>% summarize(nr_stages = length(na.omit(unique(stage))))
paperstages <- paperstages[paperstages$nr_stages > 0,]
set.seed(493)
# col_polychrome <- as.vector(Polychrome::createPalette(length(unique(expi_noreport$paper)),  RColorBrewer::brewer.pal(n = 12, name = "Set3")))
col_brewer <- c(RColorBrewer::brewer.pal(n = 12, name = "Set3"), 
                RColorBrewer::brewer.pal(n = 9, name = "Set1")[-c(5,6)])
col_mokolecom <- c("#696969", "#228b22", "#7f0000", "#808000", "#483d8b", "#008b8b",
                   "#cd853f", "#4682b4", "#9acd32", "#00008b", "#7f007f", #"#8fbc8f",
                   "#b03060", "#ff4500", "#ff8c00", "#ffd700", #"#00ff00",
                   "#00ff7f",
                   "#dc143c", "#00ffff", "#0000ff", "#da70d6", "#ff00ff", "#1e90ff",
                   #"#90ee90", 
                   "#add8e6", "#ff1493", "#7b68ee", "#ffe4b5", "#ffb6c1")

paperstages$color_fill <- sample(col_mokolecom, 
                     size = dim(paperstages)[1], replace = T)
paperstages$color_border <- sample(col_mokolecom, 
                      size = dim(paperstages)[1], replace = T)
# make unique
checko_colo <- paperstages[, c("color_fill", "color_border")]
isdupcol <- duplicated(checko_colo)
while (sum(isdupcol) > 0) {
  paperstages[isdupcol, "color_border"] <- sample(col_mokolecom, size = sum(isdupcol), replace = TRUE)
  checko_colo <- paperstages[, c("color_fill", "color_border")]
  isdupcol <- duplicated(checko_colo)
}


color_fill <- paperstages$color_fill
names(color_fill) <- paperstages$paper_label
color_border <- paperstages$color_border
names(color_border) <- paperstages$paper_label





##### 1) experience with and without explicit recall
e0 <- expi %>% 
  filter(!is.na(expi$per_exp)|!is.na(expi$per_exp_no_recall)|!is.na(expi$per_no_report)) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10),
         per_total = sum(per_exp,per_exp_no_recall, per_no_report, na.rm = T)) %>%
  ggplot(aes(x = stage, y = 100*(per_total))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(fill = paper_label, 
                 colour = paper_label, 
                 size = n_participants), 
             #size = 3, 
             position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.5, seed = 7), 
             shape = 21, stroke = 2) + 
  scale_fill_manual(values = color_fill) + 
  scale_color_manual(values = color_border) +
  
  labs(fill="Paper", color = "Paper") +
  xlab("Stage") +
  ylab("Experience with and without recall [%]") + 
  guides(fill = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1), 
         colour = guide_legend(ncol = 2, override.aes = list(size = 4), order = 1),
         size = guide_legend(ncol = 4, title = "# unique participants", order = 2)) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(2,5)) + 
  theme(legend.title = element_text(size = 18), 
        legend.text = element_text(size = 16), legend.location = "plot")+ ylim(0, 130000)
e0 <- as_ggplot(cowplot::get_legend(e0))

e1 <- expi %>%
  filter(!is.na(per_no_report)) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage, y = 100*per_no_report)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(fill = paper_label, 
                 colour = paper_label, 
                 size = n_participants), 
             #size = 3, 
             position = position_jitterdodge(jitter.width = 0.1,dodge.width = 0.5, seed = 7), 
             shape = 21, stroke = 2) + #geom_jitter(width = 0.1)
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_fill_manual(values = color_fill) + # randomcoloR::distinctColorPalette(length(unique(expi_noreport$paper)))
  scale_color_manual(values = color_border) +
  
  labs(fill="Paper", color = "Paper") +
  xlab("Stage") +
  ylab("No report [%]") + 
  guides(fill = guide_legend(ncol = 1, override.aes = list(size = 3), order = 1), 
         colour = guide_legend(ncol = 1, override.aes = list(size = 3), order = 1),
         size = guide_legend(ncol = 4, title = "# unique participants", order = 2)) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(2,5)) + 
  theme(legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), legend.location = "plot") + ylim(c(0,100))+ 
  scale_x_discrete(labels = c("nrem1" = "N1", "nrem2" = "N2", "nrem3" = "N3", "nrem" = "NREM", "rem" = "REM", "wake" = "W"))

ggsave(e1, filename = "plots/no_report.jpg", width = 9, height = 7, units = "in", dpi = 600)



##### 2) experience with explicit recall
e2 <- expi %>% 
  filter(!is.na(expi$per_exp)) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
ggplot(aes(x = stage, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(fill = paper_label, 
                 colour = paper_label,
                 size = n_participants),
             stroke = 2, shape = 21, 
             position = position_jitterdodge(jitter.width = 0.5,dodge.width = 0.1, seed = 78)) +
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_fill_manual(values = color_fill) +
  scale_color_manual(values = color_border) +
  labs(fill="Paper", color = "Paper", shape = "Time in sleep") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(fill = guide_legend(ncol = 3, override.aes = list(size = 3)), 
         colour = guide_legend(ncol = 3, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(2,5))+ 
  theme(legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), legend.location = "plot")+ theme(axis.text.x = element_blank())+ ylim(c(0,100))
ggsave(e2, filename = "plots/experience_with_recall.jpg", width = 14, height = 7, units = "in", dpi = 600)



##### 3) experience without explicit recall
e3 <- expi %>% 
  filter(!is.na(per_exp_no_recall)) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
ggplot(aes(x = stage, y = 100*(per_exp_no_recall))) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(aes(fill = paper_label, colour = paper_label, size = n_participants), 
             position = position_jitterdodge(jitter.width = 0.5,dodge.width = 0.1, seed = 78), 
             shape = 21, stroke = 2) + #geom_jitter(width = 0.1)
  geom_boxplot(outlier.shape = NA, fill = NA) +
  scale_fill_manual(values = color_fill) +
  scale_color_manual(values = color_border) +
  
  labs(fill="Paper", color = "Paper") +
  xlab("Stage") +
  ylab("Experience without recall [%]")  + 
  guides(fill = guide_legend(ncol = 1, override.aes = list(size = 3)), 
         colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(2,5))+ 
  theme(legend.title = element_text(size = 14), 
        legend.text = element_text(size = 12), legend.location = "plot")+ theme(axis.text.x = element_blank()) + ylim(c(0,100))
ggsave(filename = "plots/experience_without_recall.jpg", width = 9, height = 7, units = "in", dpi = 600)

layout <- "
AED
BED
CED
"
e4 <- e2 + theme(legend.position="none")+ e3+ theme(legend.position="none") + e1+ theme(legend.position="none") + e0 + plot_spacer()+
  plot_layout(axis_titles = "collect", design = layout, widths = c(8,0.7,11)) + 
  plot_annotation(caption = "<span style = 'color:#000000;'>Experience with recall:</span><span style = 'color:#ffffff;'>..</span><span style='color:#9b2f1a;'><i>NREM - REM: <span style = 'color:#ffffff;'>.</span>-28.0%, p < 0.001<span style = 'color:#ffffff;'>..</span>NREM - W: <span style = 'color:#ffffff;'>.</span>-36.7%, p = 0.006</i></span><span style = 'color:#ffffff;'>..</span>REM - W: <span style = 'color:#ffffff;'>oi</span>-8.8%, p = 0.814<span style = 'color:#ffffff;'>..</span> N2 - N3: <span style = 'color:#ffffff;'>1</span>+2.8%, p = 0.454<br>
                  <span style = 'color:#000000;'>Experience without recall:</span><span style = 'color:#ffffff;'>..</span><span style='color:#9b2f1a;'><i>NREM - REM: +17.1%, p < 0.001</span><span style = 'color:#ffffff;'>..</span>NREM - W: +17.3%, p = 0.158</i><span style = 'color:#ffffff;'>..</span>REM - W: <span style = 'color:#ffffff;'>1</span>+0.1%, p = 1<span style = 'color:#ffffff;'>...+</span><span style = 'color:#ffffff;'>...</span> N2 - N3: <span style = 'color:#ffffff;'>oi</span>-7.1%, p = 0.265<br>
                  <span style = 'color:#000000;'>No report:</span><span style = 'color:#ffffff;'>..</span><span style='color:#bb8138;'><i>NREM - REM: +14.7%, p = 0.072</i></span><span style = 'color:#ffffff;'>..</span>NREM - W: +24.3%, p = 0.272<span style = 'color:#ffffff;'>..</span>REM - W: <span style = 'color:#ffffff;'>1</span>+9.6%, p = 0.781<span style = 'color:#ffffff;'>..</span> N2 - N3: +14.8%, p = 0.951")&
  theme(plot.caption = element_markdown(size = 15, face = "italic", color = "gray50"), 
        plot.caption.position = "plot") 
ggsave(e4, filename = "plots/total_stages.jpg", width = 14.1, height = 11.8, units = "in", dpi = 600)


## nrem2 vs3 tests
expt <- t.test(na.omit(expi$per_exp[expi$stage %in% "nrem2"]),
       na.omit(expi$per_exp[expi$stage %in% "nrem3"]), paired = F, var.equal = F, alternative = "greater")
nrect <- t.test(na.omit(expi$per_exp_no_recall[expi$stage %in% "nrem2"]),
               na.omit(expi$per_exp_no_recall[expi$stage %in% "nrem3"]), paired = F, var.equal = F, alternative = "less")
nrept <- t.test(na.omit(expi$per_no_report[expi$stage %in% "nrem2"]),
                na.omit(expi$per_no_report[expi$stage %in% "nrem3"]), paired = F, var.equal = F, alternative = "less")
data.frame(type = c("experience", "without recall", "no report"),
           nrem3minus2 = c(expt$estimate[2]-expt$estimate[1], nrect$estimate[2]-nrect$estimate[1],nrept$estimate[2]-nrept$estimate[1]),
           tvalue = c(expt$statistic, nrect$statistic,nrept$statistic),
           pvalue = c(expt$p.value, nrect$p.value,nrept$p.value))
           


#### years plots
p1 <- expi %>% 
  filter(!is.na(expi$per_exp) &
           !expi$stage_n2n3rw %in% c("nrem1", "nrem", "wake") & 
           !is.na(expi$stage_n2n3rw)) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = timey, y = 100*(per_exp))) +
  geom_point(aes(color = stage_n2n3rw, size = n_participants), 
             stroke = 1, shape = 21, 
             position = position_jitter(width = 0.2, height = 0, seed = 5)) +
  geom_smooth(aes(group = stage_n2n3rw, color = stage_n2n3rw), se = F, method = "lm") +
  
  scale_color_manual(values = c("nrem1" = "#90ee90",
                                "nrem2" = "#8fbc8f",
                                "nrem3" = "#228b22",
                                "nrem" = "#225b22",
                                "nrem2+3" ="#207f8a",
                                "rem"= "#a89bfa",
                                "wake" = "gray60"),
                     labels = c("nrem2+3" = "N2 + N3", "rem" = "REM")) +
  labs(color = "Stage") +
  xlab("Year of publication") +
  ylab("Percentage report type") +
  scale_x_continuous(breaks = c(2000,2005, 2010, 2015, 2020, 2024),limits = c(2000, 2024)) +
  ggtitle("Experience with recall")+ ylim(c(0,100))+ 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5)) +
  labs(caption = "REM - (N2 + N3): +39.5% p < 0.001\n# participants: +0.2% n.s.\nyear: +0.4% n.s.")+ 
  theme(plot.caption = element_text(size = 14, face = "italic", color = "gray50"), plot.caption.position = "panel")

ggsave(p1, filename = "plots/experience_year.jpg", width = 8, height = 7, units = "in", dpi = 600)
p2 <- expi %>% 
  filter(!is.na(expi$per_no_report) &
           !expi$stage_n2n3rw %in% c("nrem1", "nrem", "wake") & 
           !is.na(expi$stage_n2n3rw)) %>%
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = timey, y = 100*(per_no_report))) +
  geom_point(aes(color = stage_n2n3rw, size = n_participants), 
             stroke = 1, shape = 21, 
             position = position_jitter(width = 0.2, height = 0, seed = 5)) +
  geom_smooth(aes(group = stage_n2n3rw, color = stage_n2n3rw), se = F, method = "lm") +
  
  scale_color_manual(values = c("nrem1" = "#90ee90",
                                "nrem2" = "#8fbc8f",
                                "nrem3" = "#228b22",
                                "nrem" = "#225b22",
                                "nrem2+3" ="#207f8a",
                                "rem"= "#a89bfa",
                                "wake" = "gray60"),
                     labels = c("nrem2+3" = "N2 + N3", "rem" = "REM")) +
  labs(color = "Stage") +
  xlab("Year of publication") +
  ylab("Percentage report type") +
  scale_x_continuous(breaks = c(2000,2005, 2010, 2015, 2020, 2024),limits = c(2000, 2024))+
  ggtitle("No report")+ ylim(c(0,100))+ 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+
  labs(caption = "REM - (N2 + N3): -9.6% n.s.\n# participants: +0.4% n.s.\nyear: -1% n.s.")+ 
  theme(plot.caption = element_text(size = 14, face = "italic", color = "gray50"), plot.caption.position = "panel")
ggsave(p2, filename = "plots/noreport_year.jpg", width = 8, height = 7, units = "in", dpi = 600)

p3 <- expi %>% 
  filter(!is.na(expi$per_exp_no_recall) &
           !expi$stage_n2n3rw %in% c("nrem1", "nrem", "wake") & 
           !is.na(expi$stage_n2n3rw)) %>%
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = timey, y = 100*(per_exp_no_recall))) +
  geom_point(aes(color = stage_n2n3rw, size = n_participants), 
             position = position_jitter(width = 0.2, height = 0, seed = 5),
             stroke = 1, shape = 21) +
  geom_smooth(aes(group = stage_n2n3rw, color = stage_n2n3rw), se = F, method = "lm") +
  
  scale_color_manual(values = c("nrem1" = "#90ee90",
                                "nrem2" = "#8fbc8f",
                                "nrem3" = "#228b22",
                                "nrem" = "#225b22",
                                "nrem2+3" ="#207f8a",
                                "rem"= "#a89bfa",
                                "wake" = "gray60"),
                     labels = c("nrem2+3" = "N2 + N3", "rem" = "REM")) +
  labs(color = "Stage") +
  xlab("Year of publication") +
  ylab("Percentage report type") +
  scale_x_continuous(breaks = c(2000,2005, 2010, 2015, 2020, 2024),limits = c(2000, 2024))+
  ggtitle("Experience without recall")+ ylim(c(0,100))+ 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+
  labs(caption = "REM - (N2 + N3): -31.5% p < 0.001\n# participants: -0.7% n.s.\nyear: -0.3% n.s.")+ 
  theme(plot.caption = element_text(size = 14, face = "italic", color = "gray50"), plot.caption.position = "panel")
ggsave(p3, filename = "plots/experience_notexplicitrecall_year.jpg", width = 8, height = 7, units = "in", dpi = 600)

p4 <- p1 + p3 + p2 + 
  plot_layout(guides = "collect", axis_titles = "collect", axes = "collect")&
  theme(legend.position='right')&
  guides(size = guide_legend(title.position="top", nrow = 1, byrow = TRUE,
                             title = "# unique participants", order = 1),
         color = guide_legend(title.position="top", nrow = 2, byrow = TRUE, order = 2, override.aes = list(size = 3)))
ggsave(p4, filename = "plots/supp_year.jpg", width = 18, height = 7, units = "in", dpi = 600)


# year model
data_ym <- expi %>% 
  filter((!is.na(per_exp)|!is.na(per_exp_no_recall)|!is.na(per_no_report)) & stage_n2n3rw%in%c("nrem2+3", "rem")) %>% 
  pivot_longer(cols = c(per_exp, per_exp_no_recall, per_no_report), 
               names_to = "report_type", 
               values_to = "report_percentage") %>% 
  select(c(report_percentage, report_type, n_participants, stage_n2n3rw, year))
#data_ym$report_type <- factor(data_ym$report_type, levels = c("per_exp", "per_exp_no_recall"))
data_ym$stage_n2n3rw <- factor(data_ym$stage_n2n3rw)
data_ym <- na.omit(data_ym)


data_ym$year <- data_ym$year - 2000
af <- c(NULL,NULL, NULL, NULL)
for (rtype in c("per_exp", "per_exp_no_recall", "per_no_report")){
  fity <- lm(report_percentage ~ stage_n2n3rw*(n_participants + year), 
             data = data_ym[data_ym$report_type %in% rtype,])
  summary_fit(fity)
  af <- rbind(af, as.data.frame(summary(fity)$coefficients))
}

data.frame(variable = rownames(af),
           estimate = round(af$Estimate, 3),
           SE = round(af$`Std. Error`, 3),
           p.raw = round(af$`Pr(>|t|)`,3),
           p.BH = round(p.adjust(af$`Pr(>|t|)`, method = "BH"),3),
           p.raw.s = significance_stars(af$`Pr(>|t|)`),
           p.BH.s = significance_stars(p.adjust(af$`Pr(>|t|)`, method = "BH")),
           cohensd = round(t_to_d(af$`t value`, df_error = 172)$d,3),
           effectsize = categorize_effect_size(t_to_d(af$`t value`, df_error = 172)$d))



scols <- c("#8DD3C7", "#FFFFB3", 
           "#BEBADA", "#80B1D3", 
           "#FDB462", "#B3DE69", 
           "#FCCDE5", "#D9D9D9", 
           "#BC80BD", "#CCEBC5", 
           "#fe9e97", "#22c1ce",
           "#8AA29E", "#F1EDEE"
           )  #col_brewer[1:11][-4]


# per waking method
s0 <- expi %>%
  filter(!is.na(expi$per_exp) & !is.na(expi$awa_sig_c2)&
           !expi$stage %in% c("wake", "nrem1", "nrem") &
           expi$awa_sig_c2 %in% c("alarm/buzzer/sound/tone", "name")) %>%
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,awa_sig_c2), fill = awa_sig_c2), show.legend = F) +
  geom_point(aes(#colour = awa_sig_c2,
                 group = interaction(stage_nrw,awa_sig_c2),
                 size = n_participants),
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  
  labs(fill = "Awakening method") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) +
  scale_size_continuous(breaks = c(10,20,30,40),
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+
  theme(legend.title = element_text(size = 16),
        legend.text = element_text(size = 14)) +
  labs(title = "alarm/buzzer/sound/tone - name<br><span style='color:gray50;'><i>-10.1%, p = 0.046</i></span>") +
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel")  +
  scale_fill_manual(values = scols[1:2]) + ylim(c(0,100))+
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM"))

s0 <- as_ggplot(cowplot::get_legend(s0))

s1 <- expi %>% 
  filter(!is.na(expi$per_exp) & !is.na(expi$awa_sig_c2)& 
           !expi$stage %in% c("wake", "nrem1", "nrem") &
           expi$awa_sig_c2 %in% c("alarm/buzzer/sound/tone", "name")) %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,awa_sig_c2), fill = awa_sig_c2)) +
  geom_point(aes(#colour = awa_sig_c2,
    group = interaction(stage_nrw,awa_sig_c2),
    size = n_participants), show.legend = F,
    stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  
  labs(fill = "Awakening method") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  labs(title = "<span style='color:#9b2f1a;'><i>diff: -10.1%, p = 0.046, interaction</i></span>") + 
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel")  +
  scale_fill_manual(values = scols[1:2]) + ylim(c(0,100))+ 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM"))+ theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s1, filename = "plots/awakening_method.jpg", width = 8, height = 7, units = "in", dpi = 600)

tps <- t.test(per_exp~awakening_careful, alternative = "greater", data = expi %>% 
               filter(!is.na(expi$per_exp) & !is.na(expi$awakening_careful) & !expi$stage %in% "wake"), paired = F, var.equal = F)
pshow <- round(tps$p.value,3)
diffshow <-  round(-100*diff(tps$estimate),1)

s2<- expi %>% 
  filter(!is.na(expi$per_exp) & !is.na(expi$awakening_careful) & !expi$stage %in% "wake") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,awakening_careful), fill = awakening_careful)) +
  geom_point(aes(#colour = awakening_careful,
                 group = interaction(stage_nrw,awakening_careful),
                 size = n_participants), show.legend = F,
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Awakening") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))+
  labs(title = "<span style='color:#9b2f1a;'><i>diff: +7%, p = 0.033</i></span>") +  
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel")  +
  scale_fill_manual(values = scols[3:4], labels = c("soft/careful" = "gentle", "other/unknown" = "other/unknown"))+ 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM"))+ 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 0),
    legend.text = element_text(hjust = 0),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s2, filename = "plots/supp_gentle_awakening.jpg", width = 6, height = 7, units = "in", dpi = 600)


s3 <- expi %>% 
  filter(!is.na(expi$per_exp) & 
           !is.na(expi$question_type) & 
           !expi$stage %in% "wake" &
           !expi$question_type %in% "other") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,question_type), fill = question_type)) +
  geom_point(aes(#colour = question_type,
                 group = interaction(stage_nrw,question_type),
                 size = n_participants), show.legend = F,
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Question type") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14))  +
  labs(title = "<span style='color:gray50;'><i>diff: +0.2%, p = 0.999</i></span>") +  
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel")  +
  scale_fill_manual(values = scols[5:6])+ 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM")) + 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s3, filename = "plots/question_type.jpg", width = 8, height = 7, units = "in", dpi = 600)

s4 <- expi %>% 
  filter(!is.na(expi$per_exp) & 
           !is.na(expi$repeated) & 
           !expi$stage %in% "wake" &
           !expi$repeated %in% "both") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,repeated), fill = repeated)) +
  geom_point(aes(#colour = repeated,
                 group = interaction(stage_nrw,repeated),
                 size = n_participants), show.legend = F,
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Awakenings per recording") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  labs(title = "<span style='color:gray50;'><i>diff: +0.5%, p = 0.999</i></span>") + 
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel")  +
  scale_fill_manual(values = scols[7:8])+ 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM")) + 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s4, filename = "plots/repeated_within_sleep.jpg", width = 8, height = 7, units = "in", dpi = 600)


s5 <- expi %>% 
  filter(!is.na(expi$per_exp) & 
           !is.na(expi$setting) & 
           !expi$stage %in% "wake" &
           !expi$setting %in% "both") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,setting), fill = setting)) +
  geom_point(aes(#colour = setting,
                 group = interaction(stage_nrw,setting),
                 size = n_participants), show.legend = F,
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Environment") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  labs(title = "<span style='color:#9b2f1a;'><i>diff: -14.4%, p = 0.046</i></span>") + 
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel") +
  scale_fill_manual(values = scols[9:10])+ 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM")) + 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s5, filename = "plots/environment.jpg", width = 8, height = 7, units = "in", dpi = 600)


s61 <- expi %>% 
  filter(!is.na(expi$per_exp) & 
           !is.na(expi$nap) & 
           !expi$stage %in% "wake") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,nap), fill = nap)) +
  geom_point(aes(#colour = nap,
                 group = interaction(stage_nrw,nap),
                 size = n_participants), show.legend = F,
             stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Sleep") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values = scols[11:12]) +
  labs(title = "<span style='color:gray50;'><i>diff: +0.5%, p = 0.608</i></span>") + 
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel") + 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM")) + 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s61, filename = "plots/nap.jpg", width = 8, height = 7, units = "in", dpi = 600)


s62 <- expi %>% 
  filter(!is.na(expi$per_exp) & 
           !is.na(expi$studyduration) & 
           !expi$stage %in% "wake") %>% 
  mutate(n_participants = pmax(pmin(n_participants, 40),10)) %>%
  ggplot(aes(x = stage_nrw, y = 100*(per_exp))) +
  geom_boxplot(outlier.shape = NA, position = position_dodge(width = 0.8),
               aes(group = interaction(stage_nrw,studyduration), fill = studyduration)) +
  geom_point(aes(#colour = studyduration,
    group = interaction(stage_nrw,studyduration),
    size = n_participants), show.legend = F,
    stroke = 0.5, shape = 21, position = position_dodge(width = 0.8)) +
  labs(fill = "Study days") +
  xlab("Stage") +
  ylab("Experience with recall [%]")+ 
  guides(colour = guide_legend(ncol = 1, override.aes = list(size = 3)),
         size = guide_legend(ncol = 4, title = "# unique participants")) + 
  scale_size_continuous(breaks = c(10,20,30,40), 
                        labels = c("≤10",20,30,"≥40"),
                        range = c(1,5))+ 
  theme(legend.title = element_text(size = 16), 
        legend.text = element_text(size = 14)) +
  scale_fill_manual(values = scols[13:14]) +
  labs(title = "<span style='color:#9b2f1a;'><i>diff: -8.9%, p = 0.021</i></span>") + 
  theme(plot.title = element_markdown(size = 16), plot.title.position = "panel") + 
  scale_x_discrete(labels = c("nrem" = "NREM", "rem" = "REM")) + 
  theme(
    legend.position = c(1, 0.01),
    legend.justification = c("right", "bottom"),
    legend.title = element_text(hjust = 1),
    legend.text = element_text(hjust = 1),
    legend.background = element_rect(fill = "transparent", color = NA) 
  )
ggsave(s62, filename = "plots/studyduration.jpg", width = 8, height = 7, units = "in", dpi = 600)

layout <- "
ABC
DEF
GGG
"
s7 <- s1 + s3 + s4 + s5 + s61 + s62 + s0 +
  plot_layout(guides = "keep", axis_titles = "collect", axes = "collect", design = layout, heights = c(5,5,1))&
  guides(fill = guide_legend(title.position="top", nrow = 1, byrow = TRUE))
ggsave(s7, filename = "plots/total_models.jpg", width = 12, height = 10, units = "in", dpi = 600)





# models
na_per_col(expi[,c("per_exp", 
                   "stage_overall", 
                   #"awakening_careful",
                   "awa_sig_c3" ,
                   "question_type" ,
                   "age_mean" ,   
                   "percent_female" ,
                   "provoked" ,
                   "repeated" , 
                   "setting" , 
                   "studyduration",
                   "nap")])
na_per_row(expi[,c("per_exp", 
                   "stage_overall", 
                   #"awakening_careful",
                   "awa_sig_c3" ,
                   "question_type" ,
                   "age_mean" ,   
                   "percent_female" ,
                   "provoked" ,
                   "repeated" , 
                   "setting" , 
                   "studyduration",
                   "nap")])


# no explicit recall
plot_data <- na.omit(expi[,c("per_exp_no_recall", 
                             "stage_overall")])
full_model <- lm(per_exp_no_recall ~ 
                   stage_overall, plot_data)
contrastlist <- list(list("(Intercept)", ""),
                     list("stage_overall", ""))
write.xlsx(phtest(full_model, contrastlist), file = "models/exp_norecall_full.xlsx")



# no report
plot_data <- na.omit(expi[,c("per_no_report", 
                             "stage_overall")])
full_model <- lm(per_no_report ~ 
                   stage_overall, plot_data)
contrastlist <- list(list("(Intercept)", ""),
                     list("stage_overall", ""))
write.xlsx(phtest(full_model, contrastlist), file = "models/no_report_full.xlsx")


# explicit recall
plot_data <- na.omit(expi[,c("per_exp", 
                             "stage_overall", 
                             #"awakening_careful",
                             "awa_sig_c3" ,
                             "question_type" ,
                             "age_mean" ,   
                             "percent_female" ,
                             #"provoked" ,
                             "repeated" , 
                             "setting" , 
                             "studyduration",
                             "nap")])
full_model <- lm(per_exp ~ 
                   stage_overall + ( 
                     #awakening_careful + 
                     awa_sig_c3 +
                       question_type +
                       age_mean +   
                       percent_female +
                       #provoked +
                       repeated + 
                       setting + 
                       studyduration + nap), plot_data)

summary(full_model)
anova(full_model)
summary_fit(full_model)


contrastlist <- list(list("(Intercept)", ""),
                     list("stage_overall", ""),
                     list("awa_sig_c3", "name - (alarm/buzzer/sound/tone)"),
                     list("question_type", "mind - dream"),
                     list("age_mean", ""),
                     list("percent_female", ""),
                     list("repeated", "repeated - single"),
                     list("setting", "laboratory - at home"),
                     list("studyduration", ""),
                     list("nap", ""))

write.xlsx(phtest(full_model, contrastlist), file = "models/exp_full.xlsx")




# stepaic stage interactions
full_model_aic <- lm(per_exp ~ 
                       stage_overall * ( 
                         #awakening_careful + 
                         awa_sig_c3 +
                           question_type +
                           age_mean +   
                           percent_female +
                           #provoked +
                           repeated + 
                           setting + 
                           studyduration + nap), 
                     plot_data)

stepwise_model <- MASS::stepAIC(full_model_aic,
                                scope = list(lower = formula(per_exp ~ stage_overall + ( 
                                  #awakening_careful + 
                                  awa_sig_c3 +
                                    question_type +
                                    age_mean +   
                                    percent_female +
                                    #provoked +
                                    repeated + 
                                    setting + 
                                    studyduration + nap)),
                                  upper = formula(per_exp ~
                                                    stage_overall * ( 
                                                      #awakening_careful + 
                                                      awa_sig_c3 +
                                                        question_type +
                                                        age_mean +   
                                                        percent_female +
                                                        #provoked +
                                                        repeated + 
                                                        setting + 
                                                        studyduration + nap))),
                                direction = "both", trace = FALSE)

sink(file = "models/exp_stepwise_stages.txt")
anova(stepwise_model)
sink()
write.xlsx(as.data.frame(anova(stepwise_model)), file = "models/exp_stepwise_stages.xlsx", rowNames = TRUE)


#### additional N2 N3 tests
t1 <- t.test(expi$per_exp[expi$stage %in% "nrem2"], 
       expi$per_exp[expi$stage %in% "nrem3"], 
       paired = F, var.equal = F, alternative = "greater")
t2 <- t.test(expi$per_exp_no_recall[expi$stage %in% "nrem2"], 
             expi$per_exp_no_recall[expi$stage %in% "nrem3"], 
             paired = F, var.equal = F, alternative = "less")
t3 <- t.test(expi$per_no_report[expi$stage %in% "nrem2"], 
             expi$per_no_report[expi$stage %in% "nrem3"], 
             paired = F, var.equal = F, alternative = "less")

n2n3stats <- data.frame(outcome = c("experience with recall", "experience without recall", "no report"),
  comparison = "N2 -N3", 
           estimate_diff_per = c(diff(rev(t1$estimate)), diff(rev(t2$estimate)), diff(rev(t3$estimate)))*100,
           SE = c(t1$stderr, t2$stderr, t3$stderr),
           df = c(t1$parameter, t2$parameter, t3$parameter),
           t.ratio = c(t1$statistic, t2$statistic, t3$statistic),
           p_raw = c(t1$p.value, t2$p.value, t3$p.value), 
           p_BH = p.adjust(c(t1$p.value, t2$p.value, t3$p.value), method = "BH"), 
           p_raw_s = sapply(c(t1$p.value, t2$p.value, t3$p.value),significance_stars),
           p_BH_s = sapply(p.adjust(c(t1$p.value, t2$p.value, t3$p.value), method = "BH"),significance_stars),
           cohensd = c(t_to_d(t1$statistic, df_error = t1$parameter)$d, t_to_d(t2$statistic, df_error = t2$parameter)$d, t_to_d(t3$statistic, df_error = t3$parameter)$d), 
           effectsize = sapply(c(t_to_d(t1$statistic, df_error = t1$parameter)$d, t_to_d(t2$statistic, df_error = t2$parameter)$d, t_to_d(t3$statistic, df_error = t3$parameter)$d),categorize_effect_size))
n2n3stats$estimate_diff_per <- round(n2n3stats$estimate_diff_per, 1)
n2n3stats$SE <- round(n2n3stats$SE, 3)
n2n3stats$df <- round(n2n3stats$df, 1)
n2n3stats$t.ratio <- round(n2n3stats$t.ratio, 3)
n2n3stats$p_raw <- round(n2n3stats$p_raw, 3)
n2n3stats$p_BH <- round(n2n3stats$p_BH, 3)
n2n3stats$cohensd <- round(n2n3stats$cohensd, 3)
write.xlsx(n2n3stats, file = "models/n2n3stats.xlsx")





# nr_awakenings scaled
expi$n_awa_normalized <- (expi$n_awakening_row/expi$n_participants)#/nr_nights
plot_data <- na.omit(expi[nr_nights %in% 1,c("per_exp", 
                             "stage_overall", 
                             #"awakening_careful",
                             "n_awa_normalized", "nap")])
awa_mod <- lm(per_exp ~ stage_overall * n_awa_normalized, plot_data[plot_data$nap %in% "night",])

summary(awa_mod)
anova(awa_mod)

sink(file = "models/awa_mod.txt")
anova(awa_mod)
sink()

write.xlsx(as.data.frame(anova(awa_mod)), file = "models/awa_mod.xlsx", rowNames = TRUE)


