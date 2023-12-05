library(dplyr)
library(tidyr)
library(readr)
library(here)
library(flextable)
library(survival)
library(ggplot2)
library(ggfortify)
library(survminer)
library(ggh4x)
library(officer)
library(knitr)
library(officedown)
library(grid)
library(scales)

source(here("functions_pacs.R"))
source(here("prep_data_pacs.R"))

## VE - Meta Analysis ----
data_meta <- estimates %>%
  filter(cdm_name == "Meta Analysis") %>%
  filter(study == "Meta Analysis") %>%
  mutate(dash = ifelse(i2 <= 0.4 | is.na(i2), FALSE, TRUE))

y_pos <- tibble(outcome_name = c(rev(oth), NA, NA, rev(ate), NA, NA, rev(vte))) %>%
  filter(!is.na(outcome_name)) %>%
  inner_join(data_meta %>% 
               select(outcome_name, nice_outcome_name, group)) %>%
  distinct() %>%
  group_by(group) %>%
  mutate("adjustment_y" = row_number())

y_breaks <- c(1:4, 7:10, 13:15)

y_scale <- list()

breaks_groups <- list("VTE" = 1:3, "ATE" = 1:4, "Others" = 1:4)
labels_groups <- list("VTE" = y_pos %>% filter(group == "VTE") %>% pull(nice_outcome_name),
                      "ATE" = y_pos %>% filter(group == "ATE") %>% pull(nice_outcome_name),
                      "CD + HS" = y_pos %>% filter(group == "CD + HS") %>% pull(nice_outcome_name))


y_scale = list(group == "VTE" ~ scale_y_continuous(breaks = c(1:3), 
                                                   labels = c("Pulmonary embolism","Deep vein thrombosis","Venous thromboembolism"),
                                                   limits = c(0.5,3.5)),
               group == "ATE" ~ scale_y_continuous(breaks = c(1:4), 
                                                   labels = c("Myocardial infarction","Transient ischemic attack","Ischemic stroke","Arterial thrombosis/thromboembolism"),
                                                   limits = c(0.5,4.5)),
               group == "CD + HS" ~ scale_y_continuous(breaks = c(1:4), 
                                                      labels = c("Ventricular arrhythmia or cardiac arrest","Myocarditis pericarditis","Haemorrhagic stroke","Heart failure"),
                                                      limits = c(0.5,4.5)))

x_scale = list(group == "VTE" ~ scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), trans = "log10"),
               group == "ATE" ~ scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), trans = "log10"),
               group == "CD + HS" ~ scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), trans = "log10"))

for (ii in 1:nrow(forest_meta_ve)) {
gg <- data_meta %>%
  filter(comparison != "BNT162b2 - ChAdOx1") %>%
  select(-nice_outcome_name) %>%
  filter(comparison == forest_meta_ve$exposure_name[ii]) %>%
  filter(censoring_method == forest_meta_ve$censoring_method[ii]) %>%
  inner_join(y_pos) %>%
  mutate(outcome_name = factor(outcome_name, 
                               levels = c(vte, ate, oth)),
         group = factor(group, levels = c("VTE", "ATE", "Others"), labels = c("VTE", "ATE", "CD + HS"))) %>%
  ggplot(aes(y = adjustment_y, color = outcome_name, group = dash)) +
  geom_point(aes(x = hr), size = 1.5) +
  geom_linerange(aes(xmin = lower_hr, xmax = upper_hr, linetype = dash), linewidth = 0.8) +
  geom_vline(xintercept = 1) +
  ggplot2::theme_test() +
  facet_grid(group ~ window, scales = "free_y", space = "free") +
  guides(color = guide_legend(title = "Database")) +
  facetted_pos_scales(y = y_scale, x = x_scale) +
  theme_gray() +
  theme(
    axis.title.y = element_blank(),
    legend.position = "none",
    panel.spacing = unit(1, "lines")
  ) +
    # scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), trans = "log10") +
    coord_cartesian(xlim = c(0.1, 2)) +
  # scale_x_continuous(limits = c(0.01, 1), breaks = seq(0, 2, by = 0.5), trans = "log10",
  #                    oob=scales::rescale_none) +
  scale_color_manual(values = c("#01497c", "#2a6f97", "#2a6f97",
                                "#2d6a4f", "#32A251FF", "#32A251FF", "#32A251FF",
                                "#e36414", "#e36414", "#e36414", "#e36414")) +
  xlab("Subdistribution Hazard Ratio")
  
  gt = ggplot_gtable(ggplot_build(gg))
  gt$heights[8] <- gt$heights[8]*0.75
  
  ggsave(
    paste0("Figure1_", forest_meta_ve$exposure_name[ii], "_", forest_meta_ve$censoring_method[ii],".png"),
    plot = gt,
    path = here("figures_pacs"),
    scale = 1,
    width = 3000,
    height = 1800,
    units = "px",
    dpi = 300
  )
}


## VE - Individual + meta ----
for (ii in 1:nrow(forest_groups_ve)) {
  
  switch (forest_groups_ve$group[ii],
          "VTE" = out_group <- vte,
          "ATE" = out_group <- ate,
          "Others" = out_group <- oth
  )
  
  y_pos <- expand_grid(outcome_name = rev(out_group),
                       cdm_name = c("Meta Analysis", "CORIVA", "SIDIAP", "GOLD", "AURUM"))
  ind <- seq(1, nrow(y_pos), 5)
  
  if (length(out_group) == 3) {
    y_pos <- rbind(y_pos[1:(ind[2]-1),],
                   tibble(outcome_name = c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[2]:(ind[3]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[3]:nrow(y_pos),]) %>%
      mutate("adjustment_y" = row_number())
    y_breaks <- seq(3,nrow(y_pos),nrow(y_pos)/length(out_group))
  } else {
    y_pos <- rbind(y_pos[1:(ind[2]-1),],
                   tibble(outcome_name = c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[2]:(ind[3]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[3]:(ind[4]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[4]:nrow(y_pos),]) %>%
      mutate("adjustment_y" = row_number())
    y_breaks <- seq(4,nrow(y_pos),nrow(y_pos)/length(out_group))
  }
  y_pos <- y_pos %>%
    filter(outcome_name != "temp")

  data <- estimates %>%
    filter(group == forest_groups_ve$group[ii]) %>%
    filter(comparison == forest_groups_ve$comparison[ii]) %>%
    filter(censoring_method == forest_groups_ve$censoring_method[ii])  %>%
    inner_join(y_pos,
               by = c("outcome_name", "cdm_name"))  %>%
    mutate(cdm_name = factor(cdm_name, levels = c("AURUM", "GOLD", "SIDIAP", "CORIVA", "Meta Analysis"))) %>%
    mutate(dash = ifelse(i2 <= 0.4 | is.na(i2), FALSE, TRUE))
  
  
  xlim <- c(0.001,100)
  
  outcome_labels <- y_pos %>%
    group_by(outcome_name) %>%
    filter(adjustment_y == min(adjustment_y)) %>%
    arrange(adjustment_y) %>%
    mutate(outcome_name = gsub("_", " ", .data$outcome_name) %>%
             stringr::str_to_sentence())
  
  gg <- data %>%
    filter(!is.na(.data$se_coef)) %>%
    ggplot(aes(y = adjustment_y, color = cdm_name, group = dash)) +
    geom_point(aes(x = hr), size = 1.25) +
    geom_linerange(aes(xmin = lower_hr, xmax = upper_hr, linetype = dash)) +
    scale_y_continuous(breaks = y_breaks, labels = outcome_labels$outcome_name, 
                       limits = c(0, max(y_pos$adjustment_y)+1) ) +
    # scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 2, by = 0.5), oob=scales::rescale_none, , trans = "log10") +
    scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = c(0.1, 0.25, 0.5, 1, 2), trans = "log10", oob=scales::rescale_none) +
    coord_cartesian(xlim = c(0.05, 1.5)) +
    geom_vline(xintercept = 1) +
    ggplot2::theme_test() +
    theme(
      axis.title.y = element_blank()
    ) +
    facet_grid(study ~ window) +
    scale_color_manual(values = c("#32A251FF", "#ACD98DFF", "#FF7F0FFF", "#3CB7CCFF", "#B85A0DFF", "#B85A0DFF")) +
    guides(color = guide_legend(title = "Database"),
           linetype = "none") +
    xlab("Subdistribution Hazard Ratio")
  
  if (length(out_group) == 3) {
    gg <- gg +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 6.5, ymax = 13.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0)
  } else {
    gg <- gg +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = 6.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 13.5, ymax = 19.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0)
  }
  
  ggsave(
    paste0("Figure2_", forest_groups_ve$group[ii], "_", forest_groups_ve$comparison[ii], "_", forest_groups_ve$censoring_method[ii],".png"),
    plot = gg,
    path = here("figures_pacs"),
    scale = 1,
    width = 3000,
    height = 2000,
    units = "px",
    dpi = 300
  )
}


## CVE - Meta Analysis ----
for (ii in 1:nrow(forest_meta_cve)) {
  
  y_pos <- tibble(outcome_name = c(rev(oth), NA, NA, rev(ate), NA, NA, rev(vte))) %>%
    filter(!is.na(outcome_name)) %>%
    inner_join(data_meta %>% 
                 select(outcome_name, nice_outcome_name, group)) %>%
    distinct() %>%
    group_by(group) %>%
    mutate("adjustment_y" = row_number())
  
  y_breaks <- c(1:4, 7:10, 13:15)
  
  y_scale <- list()
  
  breaks_groups <- list("VTE" = 1:3, "ATE" = 1:4, "Others" = 1:4)
  labels_groups <- list("VTE" = y_pos %>% filter(group == "VTE") %>% pull(nice_outcome_name),
                        "ATE" = y_pos %>% filter(group == "ATE") %>% pull(nice_outcome_name),
                        "CD + HS" = y_pos %>% filter(group == "CD + HS") %>% pull(nice_outcome_name))
  
  
  y_scale = list(group == "VTE" ~ scale_y_continuous(breaks = c(1:3), 
                                                     labels = c("Pulmonary embolism","Deep vein thrombosis","Venous thromboembolism"),
                                                     limits = c(0.5,3.5)),
                 group == "ATE" ~ scale_y_continuous(breaks = c(1:4), 
                                                     labels = c("Myocardial infarction","Transient ischemic attack","Ischemic stroke","Arterial thrombosis/thromboembolism"),
                                                     limits = c(0.5,4.5)),
                 group == "CD + HS" ~ scale_y_continuous(breaks = c(1:4), 
                                                         labels = c("Ventricular arrhythmia or cardiac arrest","Myocarditis pericarditis","Haemorrhagic stroke","Heart failure"),
                                                         limits = c(0.5,4.5)))
  
  gg <- data_meta %>%
    select(-nice_outcome_name) %>%
    filter(comparison == forest_meta_cve$exposure_name[ii]) %>%
    filter(censoring_method == forest_meta_cve$censoring_method[ii]) %>%
    inner_join(y_pos) %>%
    mutate(outcome_name = factor(outcome_name, 
                                 levels = c(vte, ate, oth)),
           group = factor(group, levels = c("VTE", "ATE", "Others"), labels = c("VTE", "ATE", "CD + HS"))) %>%
    ggplot(aes(y = adjustment_y, color = outcome_name, group = dash)) +
    geom_point(aes(x = hr), size = 1.5) +
    geom_linerange(aes(xmin = lower_hr, xmax = upper_hr, linetype = dash), linewidth = 0.8) +
    # scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 2, by = 0.5), oob=scales::rescale_none) + 
    geom_vline(xintercept = 1) +
    ggplot2::theme_test() +
    facet_grid(group ~ window, scales = "free_y") +
    guides(color = guide_legend(title = "Database")) +
    facetted_pos_scales(y = y_scale) +
    theme_gray() +
    theme(
      axis.title.y = element_blank(),
      legend.position = "none"
    ) +
    scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = c(0.1, 0.25, 0.5, 1, 2), trans = "log10", oob=scales::rescale_none) +
    coord_cartesian(xlim = c(0.05, 1.5)) +
    scale_color_manual(values = c("#01497c", "#2a6f97", "#2a6f97", 
                                  "#2d6a4f", "#32A251FF", "#32A251FF", "#32A251FF", 
                                  "#e36414", "#e36414", "#e36414", "#e36414")) +
    guides(linetype = "none") +
    xlab("Subdistribution Hazard Ratio")
  
  gt = ggplot_gtable(ggplot_build(gg))
  gt$heights[8] <- gt$heights[8]*0.75
  
  ggsave(
    paste0("Figure3_", forest_meta_cve$exposure_name[ii], "_", forest_meta_cve$censoring_method[ii],".png"),
    plot = gt,
    path = here("figures_pacs"),
    scale = 1,
    width = 3000,
    height = 2000,
    units = "px",
    dpi = 300
  )
}


# CVE - Individual + meta----
for (ii in 1:nrow(forest_groups_cve)) {
  switch (forest_groups_cve$group[ii],
          "VTE" = out_group <- vte,
          "ATE" = out_group <- ate,
          "Others" = out_group <- oth
  )
  
  y_pos <- expand_grid(outcome_name = rev(out_group),
                       cdm_name = c("Meta Analysis", "GOLD", "AURUM"))
  ind <- seq(1, nrow(y_pos), 3)
  
  if (length(out_group) == 3) {
    y_pos <- rbind(y_pos[1:(ind[2]-1),],
                   tibble(outcome_name = c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[2]:(ind[3]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[3]:nrow(y_pos),]) %>%
      mutate("adjustment_y" = row_number())
    y_breaks <- seq(3,nrow(y_pos),nrow(y_pos)/length(out_group))
  } else {
    y_pos <- rbind(y_pos[1:(ind[2]-1),],
                   tibble(outcome_name = c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[2]:(ind[3]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[3]:(ind[4]-1),],
                   tibble(outcome_name =  c("temp", "temp"), cdm_name = NA),
                   y_pos[ind[4]:nrow(y_pos),]) %>%
      mutate("adjustment_y" = row_number())
    y_breaks <- seq(4,nrow(y_pos),nrow(y_pos)/length(out_group))
  }
  y_pos <- y_pos %>%
    filter(outcome_name != "temp")
  
  data <- estimates %>%
    filter(group == forest_groups_cve$group[ii]) %>%
    filter(comparison == forest_groups_cve$comparison[ii]) %>%
    filter(censoring_method == forest_groups_cve$censoring_method[ii])  %>%
    inner_join(y_pos,
               by = c("outcome_name", "cdm_name"))  %>%
    mutate(cdm_name = factor(cdm_name, levels = c("AURUM", "GOLD", "Meta Analysis"))) %>%
    filter(cdm_name != "SIDIAP") %>%
    filter(cdm_name != "CORIVA") %>%
    mutate(dash = ifelse(i2 <= 0.4 | is.na(i2), FALSE, TRUE))
  
  
  xlim <- c(-10,100)
  
  outcome_labels <- y_pos %>%
    group_by(outcome_name) %>%
    filter(adjustment_y == min(adjustment_y)) %>%
    arrange(adjustment_y) %>%
    mutate(outcome_name = gsub("_", " ", .data$outcome_name) %>%
             stringr::str_to_sentence())
  
  gg <- data %>%
    filter(!is.na(.data$se_coef)) %>%
    ggplot(aes(y = adjustment_y, color = cdm_name, group = dash)) +
    geom_point(aes(x = hr), size = 1.25) +
    geom_linerange(aes(xmin = lower_hr, xmax = upper_hr, linetype = dash)) +
    scale_y_continuous(breaks = y_breaks, labels = outcome_labels$outcome_name, 
                       limits = c(0, max(y_pos$adjustment_y)+1) ) +
    # scale_x_continuous(limits = c(0, 1.5), breaks = seq(0, 2, by = 0.5), oob=scales::rescale_none) + 
    scale_x_continuous(breaks = c(0.1, 0.25, 0.5, 1, 2), labels = c(0.1, 0.25, 0.5, 1, 2), trans = "log10", oob=scales::rescale_none) +
    coord_cartesian(xlim = c(0.05, 1.5)) +
    geom_vline(xintercept = 1) +
    ggplot2::theme_test() +
    theme(
      axis.title.y = element_blank()
    ) +
    facet_grid(study ~ window) +
    scale_color_manual(values = c("#32A251FF", "#3CB7CCFF","#B85A0DFF")) +
    guides(color = guide_legend(title = "Database"),
           linetype = "none") +
    xlab("Subdistribution Hazard Ratio")
  
  if (length(out_group) == 3) {
    gg <- gg +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 4.5, ymax = 9.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0)
  } else {
    gg <- gg +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 0.5, ymax = 4.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0) +
      geom_rect(aes(xmin = xlim[1], xmax = xlim[2], ymin = 9.5, ymax = 14.5), 
                fill = "gray", 
                alpha = 0.01, 
                color = "gray",
                linetype = 0)
  }
  
  ggsave(
    paste0("Figure4_", forest_groups_cve$group[ii], "_", forest_groups_cve$comparison[ii], "_", forest_groups_cve$censoring_method[ii],".png"),
    plot = gg,
    path = here("figures_pacs"),
    scale = 1,
    width = 3000,
    height = 2000,
    units = "px",
    dpi = 300
  )
}



