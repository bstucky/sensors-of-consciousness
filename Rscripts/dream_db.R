library(ggplot2)
library(tidyverse)
library(ggforce)

library(ggpubr)
library(patchwork)

library(glmmTMB)

# set working directory
setwd("...")

### read in data
dream <- read.csv("dream_database_wong.csv")

# only healthy
dream <- dream[dream$Subject.healthy %in% T, ]

# omit subject/stage where Unknown stage occurred
substage_omit <- dream[dream$Experience %in% "Unknown", c("Subject.ID", "Last.sleep.stage")]
for(i in 1:dim(substage_omit)[1]){
  dream <- dream[!(dream$Subject.ID %in% substage_omit[i,1] & dream$Last.sleep.stage %in% substage_omit[i,2]),]
}

# omit the NA stage
dream <- dream[!dream$Last.sleep.stage %in% "#N/A", ]

# new fully unique identifier, based on subject id and data set id
dream$Subject.ID <- paste(dream$Subject.ID, dream$Set.ID, sep = "_")

study_withoutrecall <- unique(dream$Set.ID[dream$Experience %in% "Without recall"])
study_experience <- unique(dream$Set.ID[dream$Experience %in% "Experience"])
study_noexperience <- unique(dream$Set.ID[dream$Experience %in% "No experience"])


# how many participants and studies
mean(sort(table(dream$Subject.ID),decreasing = T))
length(unique(dream$Subject.ID))
length(unique(dream$Set.ID))

howmany <- dream %>% 
  filter(Last.sleep.stage %in% c("N1", "N2", "N3")) %>% 
  filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
  select(Subject.ID, Set.ID, Last.sleep.stage)
mean(sort(table(howmany$Subject.ID),decreasing = T))
length(unique(howmany$Subject.ID))
length(unique(howmany$Set.ID))

howmany <- dream %>% 
  filter(Last.sleep.stage %in% c("N2")) %>% 
  filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
  select(Subject.ID, Set.ID, Last.sleep.stage)
mean(sort(table(howmany$Subject.ID),decreasing = T))
length(unique(howmany$Subject.ID))
length(unique(howmany$Set.ID))

howmany <- dream %>% 
  filter(Last.sleep.stage %in% c("REM")) %>% 
  filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
  select(Subject.ID, Set.ID, Last.sleep.stage)
mean(sort(table(howmany$Subject.ID),decreasing = T))
length(unique(howmany$Subject.ID))
length(unique(howmany$Set.ID))

# colors
col_brewer <- c(RColorBrewer::brewer.pal(n = 12, name = "Set3"), 
                RColorBrewer::brewer.pal(n = 9, name = "Set1")[-c(5,6)])
col_mokolecom <- c("#696969", 
                   "#9acd32",
                   "#7f0000", 
                   "#808000", 
                   "#483d8b", 
                   "#008b8b",
                   "#cd853f", 
                   "#4682b4", 
                   "#228b22",
                   "#00008b", 
                   "#7f007f", #"#8fbc8f",
                   "#b03060", 
                   "#ff4500", 
                   "#ff8c00", 
                   "#ffd700", #"#00ff00",
                   "#00ff7f",
                   "#00ffff", 
                   "#dc143c", 
                   #"#0000ff", 
                   "#da70d6", 
                   #"#ff00ff", 
                   "#1e90ff",
                   #"#90ee90", 
                   "#add8e6", 
                   "#ff1493", 
                   "#7b68ee", 
                   "#ffe4b5", 
                   "#ffb6c1")
col_mokolecom <- rev(col_mokolecom)[1:19]
set.seed(20)
col_mokolecom <- sample(col_mokolecom)
names(col_mokolecom) <- as.character(1:19)


explabel = c("Experience with recall [%]",
             "No report [%]",
             "Experience without recall [%]")
exptype = c("Experience", "No experience", "Without recall")
collect_dotplot <- list()
collect_hist <- list()
collect_hist_nrem <- list()
collect_hist_rem <- list()
collect_models <- list()
for(expidx in 1:3){
  # select percentage of experience per subject
  dream_psubj <- dream %>% 
    group_by(Subject.ID, Last.sleep.stage) %>% 
    summarize(percent_exp = sum(Experience == exptype[expidx])/n(),
              number_obs = n())
  
  # add back age, sex
  dream_psubj <- merge(dream_psubj, unique(dream[,c("Subject.ID", "Subject.age", "Subject.sex")]),
                       by = "Subject.ID", all.x = T)
  
  
  # nr repreats per sleep stage
  nr_repeats <- table(dream[,c("Subject.ID", "Last.sleep.stage")])
  # nr repeats at least 3, to get percentage of no experience on individual level
  which_to_select <- which(nr_repeats > 2, arr.ind = T)
  wsel <- cbind(id = rownames(nr_repeats)[which_to_select[,1]], stage = colnames(nr_repeats)[which_to_select[,2]])
  
  dream_s <- dream_psubj[NULL,]
  for(i in 1:dim(wsel)[1]){
    dream_s <- rbind(dream_s, dream_psubj[(dream_psubj$Subject.ID %in% wsel[i, 1] & 
                                             dream_psubj$Last.sleep.stage %in% wsel[i, 2]), ])
  }
  
  # rename N3 sleep stage
  dream_s$Last.sleep.stage[dream_s$Last.sleep.stage %in% "N3/NREM3/NREM4"] = "N3"
  
  # add set id back into it
  dream_s$setid <- sapply(strsplit(dream_s$Subject.ID, "_"), \(x)x[2])
  dream_s$setid <- factor(dream_s$setid, levels = 1:19)
  # filter based on what type is available
  if (exptype[expidx] == "Experience"){
    dream_s <- dream_s[dream_s$setid %in% study_experience, ]
    setii <- study_experience
    dreamss <- dream_s
  } else if (exptype[expidx] == "No experience"){
    dream_s <- dream_s[dream_s$setid %in% study_noexperience, ]
    setii <- study_noexperience
  } else if (exptype[expidx] == "Without recall"){
    dream_s <- dream_s[dream_s$setid %in% study_withoutrecall, ]
    setii <- study_withoutrecall
  }
  
  
  # summary of data overall
  dream %>% 
    group_by(Last.sleep.stage) %>% 
    summarize(per_exp = sum(Experience == exptype[expidx])/n())

  # plot individual percentages
  allstudies <- unique(c(study_experience, study_noexperience, study_withoutrecall))
  dummy_data <- dream_s[1:length(allstudies),]
  dummy_data[,] <- NA
  dummy_data$setid <- allstudies
  
  collect_dotplot[[expidx]] <- rbind(dream_s, dummy_data)[sample(1:dim(rbind(dream_s, dummy_data))[1]),] %>% 
    mutate(number_obs = pmax(pmin(number_obs, 20),5)) %>%
    ggplot(aes(x = Last.sleep.stage, y = percent_exp*100)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point(aes(size = number_obs, fill = setid), shape = 21,  #### setid important to see it comes from same study
               position = position_jitter(seed = 423, width = 0.3)) + 
    geom_boxplot(outlier.shape = NA, fill = NA) +
    #geom_sina(aes(size = number_obs, fill = setid, width = 1), shape = 21, adjust = 4) +
    #scale_size_continuous(range = c(1, 8), breaks=pretty_breaks(4)) +
    scale_fill_manual(values = col_mokolecom) +
    theme_nice() + 
    xlab("Stage") + ylab("") +
    ggtitle(explabel[expidx]) +
    labs(size = "# Observations per Participant", 
         color = "Study id") + ylim(c(0,100))+
    scale_x_discrete(na.translate = FALSE) +
    scale_size_continuous(breaks = c(5,10,15,20), 
                          labels = c("≤5",10,15,"≥20"),
                          range = c(3,7))
  
  
  
  mean(dream_s$Subject.age, na.rm = T) # near to the mean
  dream_s$agec <- ">30"
  dream_s$agec[dream_s$Subject.age <= 30] <- "<=30"
  dream_s$agec[is.na(dream_s$Subject.age)] <- NA
  pp1 <- dream_s %>% 
    filter(!is.na(Subject.age) & Last.sleep.stage %in% "N2") %>% 
    ggplot(aes(x = agec, y = percent_exp*100, group = interaction(Last.sleep.stage,agec))) +
    geom_boxplot(outlier.shape = NA) +
    geom_sina(size = 3) + # , aes(colour = agec)) +
    stat_compare_means(method = "t.test", method.args = list(alternative = "less"), 
                       size = 6, label.x.npc = 0.5, hjust = 0.5, vjust = -1) + theme_nice()+
    # labs(colour = "Age [years]") +
    xlab("Age [years]") +
    ylab(explabel[expidx])+ylim(c(0,100))  # +
  # ggtitle("N2 sleep")
  
  pp2 <- dream_s %>% 
    filter(Subject.sex %in% c("Male", "Female") & Last.sleep.stage %in% "N2") %>% 
    ggplot(aes(x = Subject.sex, y = percent_exp*100, group = interaction(Last.sleep.stage,Subject.sex))) +
    geom_boxplot(outlier.shape = NA) +
    geom_sina(size = 3) +  #, aes(colour = Subject.sex))+
    stat_compare_means(method = "t.test", method.args = list(alternative = "greater"), 
                       size = 6, label.x.npc = 0.5, hjust = 0.5, vjust = -1) + theme_nice() +
    # labs(colour = "Sex") +
    xlab("Sex") +
    ylab(explabel[expidx]) +ylim(c(0,100)) # +
  # ggtitle("N2 sleep") +
  # scale_color_manual(values = c("#7CAE00","#C77CFF"))
  ptot <- pp1 + pp2 + plot_layout(axes = "collect")
  ggsave(ptot, filename = paste0("plots/supp_age_gender_", exptype[expidx], ".jpg"), 
         width = 9, height = 9, units = "in", dpi = 900)
  
  
  
  ##### N2 version
  # permutation test trait
  # observed statistic = standard deviation of individual percentages
  obs_stat <-  unlist(dream %>% 
                        filter(Last.sleep.stage %in% "N2") %>% 
                        filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>%  
                        filter(Set.ID %in%setii) %>% 
                        select(c(Subject.ID,Experience, Set.ID)) %>% 
                        group_by(Subject.ID) %>% 
                        summarize(percent_exp = sum(Experience == exptype[expidx])/n(),
                                  number_obs = n()) %>% 
                        summarize(sdo = sd(percent_exp)))
  
  
  n2d <- dream %>% 
    filter(Last.sleep.stage %in% "N2") %>% 
    filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
    filter(Set.ID %in%setii) %>% 
    select(c(Subject.ID,Experience, Set.ID, Subject.age, Subject.sex))%>%
    mutate(agec = case_when(
      Subject.age > 30 ~ ">30",
      Subject.age <= 30 ~ "<=30",
      is.na(Subject.age) ~ NA_character_
    ))
  
  
  set.seed(567)
  
  # Define the number of permutations
  n_perm <- 10000
  
  # Initialize a vector to store the permuted test statistics
  perm_diffs <- numeric(n_perm)
  
  # Conduct the permutation test
  for (i in 1:n_perm) {
    n2d <- n2d %>%
      group_by(Set.ID, agec, Subject.sex) %>%
      mutate(Experience = sample(Experience))
    perm_diffs[i] <- n2d %>% 
      group_by(Subject.ID) %>% 
      summarize(percent_exp = sum(Experience == exptype[expidx])/n()) %>% 
      summarize(sd = sd(percent_exp))
  }
  perm_diffs <- unlist(perm_diffs)
  
  # Calculate the p-value
  p_value <- sum(perm_diffs >= obs_stat)/n_perm
  p_value
  
  
  collect_hist[[expidx]] <- ggplot(data.frame(permutation_diffs = perm_diffs), 
                                   aes(x=permutation_diffs*100)) + 
    geom_density() + theme_nice() +
    annotate("text", x=obs_stat*100, y=-0.03, label=paste0("Observed: ",round(obs_stat*100,1),"%", 
                                                           "  p = ",ifelse(p_value< 0.001, "<0.001", round(p_value,3)), " "),                 , 
             color="#9b2f1a", size = 5, hjust = 1, fontface = "italic") + 
    annotate("point", x = obs_stat*100, y = -Inf, color = "#9b2f1a", size = 3, pch = 21, stroke = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ylab("Density of permutation statistic") +
    xlab("Standard deviation of participant values in N2 [%]") +
    ggtitle(explabel[expidx]) + 
    ylim(c(-0.03,0.45))+
    coord_cartesian(clip = 'off') +
    scale_x_continuous(breaks=scales::pretty_breaks(4))
  
  
  
  
  ###### NREM version
  # permutation test trait
  # observed statistic = standard deviation of individual percentages
  obs_stat <-  unlist(dream %>% 
                        filter(Last.sleep.stage %in% c("N1", "N2", "N3")) %>% 
                        filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>%  
                        filter(Set.ID %in%setii) %>% 
                        select(c(Subject.ID,Experience, Set.ID)) %>% 
                        group_by(Subject.ID) %>% 
                        summarize(percent_exp = sum(Experience == exptype[expidx])/n(),
                                  number_obs = n()) %>% 
                        summarize(sdo = sd(percent_exp)))
  
  
  n2d <- dream %>% 
    filter(Last.sleep.stage %in% c("N1", "N2", "N3")) %>% 
    filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
    filter(Set.ID %in%setii) %>% 
    select(c(Subject.ID,Experience, Set.ID, Subject.age, Subject.sex, Last.sleep.stage))%>%
    mutate(agec = case_when(
      Subject.age > 30 ~ ">30",
      Subject.age <= 30 ~ "<=30",
      is.na(Subject.age) ~ NA_character_
    ))
  
  # n2d <- n2d[n2d$Subject.sex %in% c("Female", "Male") & !is.na(n2d$agec), ] # no influence
  
  set.seed(567)
  
  # Define the number of permutations
  n_perm <- 10000
  
  # Initialize a vector to store the permuted test statistics
  perm_diffs <- numeric(n_perm)
  
  # Conduct the permutation test
  for (i in 1:n_perm) {
    n2d <- n2d %>%
      group_by(Set.ID, agec, Subject.sex, Last.sleep.stage) %>%
      mutate(Experience = sample(Experience))
    perm_diffs[i] <- n2d %>% 
      group_by(Subject.ID) %>% 
      summarize(percent_exp = sum(Experience == exptype[expidx])/n()) %>% 
      summarize(sd = sd(percent_exp))
  }
  perm_diffs <- unlist(perm_diffs)
  
  # Calculate the p-value
  p_value <- sum(perm_diffs >= obs_stat)/n_perm
  p_value
  
  # corrected p value
  p_nrem <- p.adjust(c(0.182, 0.783,0.185, 0,0.007,0), method = "BH")[c(4,6,5)]
  collect_hist_nrem[[expidx]] <- ggplot(data.frame(permutation_diffs = perm_diffs), 
                                   aes(x=permutation_diffs*100)) + 
    geom_density() + theme_nice() +
    annotate("text", x=obs_stat*100, y=-0.06, label=paste0("",round(obs_stat*100,1),"%", 
                                                           "  p = ",ifelse(p_nrem[expidx]< 0.001, "<0.001", round(p_nrem[expidx],3)), " "),                 , 
             color="#9b2f1a", size = 5, hjust = 1, fontface = "italic") + 
    annotate("point", x = obs_stat*100, y = -Inf, 
             color = "#9b2f1a", size = 3, pch = 21, stroke = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ylab("Density of permutation statistic") +
    xlab("Standard deviation of participant values in NREM [%]") +
    ggtitle(explabel[expidx]) + 
    ylim(c(-0.1,0.5))+
    coord_cartesian(clip = 'off') +
    scale_x_continuous(breaks=scales::pretty_breaks(4))
  
  
  
  
  
  
  
  ###### REM version
  # permutation test trait
  # observed statistic = standard deviation of individual percentages
  obs_stat <-  unlist(dream %>% 
                        filter(Last.sleep.stage %in% "REM") %>% 
                        filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>%  
                        filter(Set.ID %in%setii) %>% 
                        select(c(Subject.ID,Experience, Set.ID)) %>% 
                        group_by(Subject.ID) %>% 
                        summarize(percent_exp = sum(Experience == exptype[expidx])/n(),
                                  number_obs = n()) %>% 
                        summarize(sdo = sd(percent_exp)))
  
  
  n2d <- dream %>% 
    filter(Last.sleep.stage %in% "REM") %>% 
    filter(Subject.ID %in%  names(table(Subject.ID))[table(Subject.ID)>2]) %>% 
    filter(Set.ID %in%setii) %>% 
    select(c(Subject.ID,Experience, Set.ID, Subject.age, Subject.sex))%>%
    mutate(agec = case_when(
      Subject.age > 30 ~ ">30",
      Subject.age <= 30 ~ "<=30",
      is.na(Subject.age) ~ NA_character_
    ))
  
  # n2d <- n2d[n2d$Subject.sex %in% c("Female", "Male") & !is.na(n2d$agec), ] # no influence
  
  set.seed(567)
  
  # Define the number of permutations
  n_perm <- 10000
  
  # Initialize a vector to store the permuted test statistics
  perm_diffs <- numeric(n_perm)
  
  # Conduct the permutation test
  for (i in 1:n_perm) {
    n2d <- n2d %>%
      group_by(Set.ID, agec, Subject.sex) %>%
      mutate(Experience = sample(Experience))
    perm_diffs[i] <- n2d %>% 
      group_by(Subject.ID) %>% 
      summarize(percent_exp = sum(Experience == exptype[expidx])/n()) %>% 
      summarize(sd = sd(percent_exp))
  }
  perm_diffs <- unlist(perm_diffs)
  
  # Calculate the p-value
  p_value <- sum(perm_diffs >= obs_stat)/n_perm
  p_value
  
  p_rem <- p.adjust(c(0.182, 0.783,0.185, 0,0.007,0), method = "BH")[c(1,3,2)]
  collect_hist_rem[[expidx]] <- ggplot(data.frame(permutation_diffs = perm_diffs), 
                                   aes(x=permutation_diffs*100)) + 
    geom_density() + theme_nice() +
    annotate("text", x=obs_stat*100, y=-0.06, label=paste0("",round(obs_stat*100,1),"%", 
                                                           "  p = ",ifelse(p_rem[expidx]< 0.001, "<0.001", round(p_rem[expidx],3)), " "),                 , 
             color="gray50", size = 5, hjust = 0.5, fontface = "italic") + 
    annotate("point", x = obs_stat*100, y = -Inf, color = "gray50", size = 3, pch = 21, stroke = 2) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    ylab("Density of permutation statistic") +
    xlab("Standard deviation of participant values in REM [%]") +
    ggtitle(explabel[expidx]) + 
    ylim(c(-0.1,0.5))+
    coord_cartesian(clip = 'off') +
    scale_x_continuous(breaks=scales::pretty_breaks(4))
    
    
  
  # model Experience
  if(exptype[expidx] %in% exptype){#"Without recall"){
    median(dream_s$number_obs)
    ffit <- glmmTMB(percent_exp ~ Last.sleep.stage + (1|setid), 
                    data = dream_s,#dream_s[dream_s$number_obs>median(dream_s$number_obs),], 
                    family = ordbeta(link = "logit"))
    hdh <- DHARMa::simulateResiduals(fittedModel = ffit, plot = F)
    plot(hdh)
    title(main = explabel[expidx], sub = NULL)
    contrastlist <- list(list("(Intercept)", ""),
                         list("Last.sleep.stage", ""))
    collect_models[[exptype[expidx]]] <- phtest(ffit, contrastlist)
  } else {
    ffit <- glmmTMB(percent_exp ~ Last.sleep.stage + number_obs + Subject.age + Subject.sex + (1|setid), 
                    data = dream_s, 
                    family = ordbeta(link = "logit"))
    hdh <- DHARMa::simulateResiduals(fittedModel = ffit, plot = F)
    plot(hdh)
    title(main = explabel[expidx], sub = NULL)
    contrastlist <- list(list("(Intercept)", ""),
                         list("Last.sleep.stage", ""),
                         list("number_obs", ""),
                         list("Subject.age", ""),
                         list("Subject.sex", ""))
    collect_models[[exptype[expidx]]] <- phtest(ffit, contrastlist)
  }
  
}

pfull <- collect_hist[[1]] + collect_hist[[3]] + collect_hist[[2]] + 
  plot_layout(axes = "collect")
ggsave(pfull, filename = paste0("plots/supp_N2_permutation.jpg"), 
       width = 15, height = 6, units = "in", dpi = 900)

pfull_nrem <- collect_hist_nrem[[1]] + collect_hist_nrem[[3]] + collect_hist_nrem[[2]] + 
  plot_layout(axes = "collect")
ggsave(pfull_nrem, filename = paste0("plots/dreamdb_permutation_nrem.jpg"), 
       width = 15, height = 6, units = "in", dpi = 900)

pfull_rem <- collect_hist_rem[[1]] + collect_hist_rem[[3]] + collect_hist_rem[[2]] + 
  plot_layout(axes = "collect")
ggsave(pfull_rem, filename = paste0("plots/dreamdb_permutation_rem.jpg"), 
       width = 15, height = 6, units = "in", dpi = 900)

dfull <- collect_dotplot[[1]] + collect_dotplot[[3]] + collect_dotplot[[2]] + 
  plot_layout(axes = "collect", 
              axis_titles = "collect", 
              guides = "collect") &
  theme(legend.position='bottom')&
  guides(size = guide_legend(title.position="top"),
         fill = guide_legend(title.position="top", title = "Study id", 
                             override.aes = list(size =5), nrow = 1))
ggsave(dfull, filename = paste0("plots/dreamdb_individual_percentages.jpg"), 
       width = 15, height = 8, units = "in", dpi = 900)



fullfull <- (collect_hist_rem[[1]] + ggtitle("Experience with recall") + labs(tag = "A") + theme(plot.tag = element_text(size = 20)) ) + 
  (collect_hist_rem[[3]]+ ggtitle("Experience without recall")) + 
  (collect_hist_rem[[2]] + ggtitle("No report")) + 
  (collect_hist_nrem[[1]]+ ggtitle(NULL)) + 
  (collect_hist_nrem[[3]]+ ggtitle(NULL)) + 
  (collect_hist_nrem[[2]]+ ggtitle(NULL)) + 
  (collect_dotplot[[1]] + ggtitle(NULL) + ylab("Percentage report type")+ labs(tag = "B") + theme(plot.tag = element_text(size = 20)) ) + 
  (collect_dotplot[[3]]+ ggtitle(NULL)+ ylab("Percentage report type")) + 
  (collect_dotplot[[2]]+ ggtitle(NULL)+ ylab("Percentage report type")) +
  plot_layout(axes = "collect", 
              axis_titles = "collect", heights = c(0.5,0.5,1.5),
              guides = "collect", nrow = 3, ncol = 3)&
  theme(legend.position='bottom')&
  guides(size = guide_legend(title.position="top"),
         fill = guide_legend(title.position="top", title = "Study id", 
                             override.aes = list(size =5), nrow = 1))

ggsave(fullfull, filename = paste0("plots/total_dreamdb.jpg"), 
       width = 15, height = 11, units = "in", dpi = 900)

# output models
write.xlsx(collect_models, file = "models/dreamdb_models.xlsx")

