## ----include = FALSE,message=FALSE,warning=FALSE-------------------------------------------------------------------------------------------
library(tidyverse)
library(gtsummary)
library(plotly)
library(knitr)
library(ggpubr)
library(cowplot)
library(kableExtra)
library(car)
library(patchwork)


theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d


## ----message=FALSE,warning=FALSE-----------------------------------------------------------------------------------------------------------
sleep_df <- read_csv("data/cmu-sleep.csv") |> 
  janitor::clean_names() |> 
  mutate(demo_race = case_when(demo_race == 0 ~ "Underrepresented", 
                                     demo_race == 1 ~ "Non-underrepresented"),
         demo_race = fct_relevel(demo_race, "Non-underrepresented"),
         demo_gender = case_when(demo_gender == 0 ~ "Male", 
                                     demo_gender == 1 ~ "Female"),
         demo_gender = fct_relevel(demo_gender, "Male"), 
         demo_firstgen = case_when(demo_firstgen == "0" ~ "Non-first gen", 
                                   demo_firstgen == "1" ~ "First-gen", 
                                   TRUE ~ NA),
         demo_firstgen = fct_relevel(demo_firstgen, "Non-first gen"), 
         time_collection = case_when(cohort == "lac1" ~ "Spring, 2018", 
                            cohort == "lac2" ~ "Spring, 2017",
                            cohort == "nh" ~ "Spring, 2016",
                            cohort == "uw1" ~ "Spring, 2018", 
                            cohort == "uw2" ~ "Spring, 2019"), 
         university = case_when(cohort == "lac1" ~ "CMU", 
                            cohort == "lac2" ~ "CMU",
                            cohort == "nh" ~ "NDU",
                            cohort == "uw1" ~ "UW", 
                            cohort == "uw2" ~ "UW"))



## tr:hover {background-color: coral;}
## th {
##   height: 5px;
## }

## ----fig.width = 5, fig.height = 4, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
sleep_df <- sleep_df |> 
  rename(`Cohort` =`cohort`, 
         `University` =`university`, 
         `Race` =`demo_race`, 
         `Gender` =`demo_gender`, 
         `First-Generation` =`demo_firstgen`, 
         `Relative Course Load`= `zterm_units_zof_z`, 
         `End-of-term GPA` =  `term_gpa`, 
         `Cumulative GPA` = `cum_gpa`)

#write_csv(sleep_df, "data/cleaned_cmu_sleep.csv")

summary_tbl <- sleep_df |> 
  select(`University`, `Race`, `Gender`, `First-Generation`, time_collection,
         `Relative Course Load`, `End-of-term GPA`, `Cumulative GPA`) |> 
  tbl_summary(
    by = `University`,
    statistic = list(
      all_continuous() ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} / {N} ({p}%)"), 
    label = list(
      time_collection ~ "Time")) |> 
  add_overall() |> 
  modify_spanning_header(c("stat_1", "stat_2", "stat_3") ~ "**University**") |> 
  bold_labels() |>
  italicize_labels() |> 
  add_p()

as_kable_extra(summary_tbl) |> 
  kable_minimal()


## ----echo = FALSE, message=FALSE,warning=FALSE---------------------------------------------------------------------------------------------
long_sleep_df <- sleep_df |> 
  pivot_longer(
      cols = c(`Relative Course Load`, `End-of-term GPA`, `Cumulative GPA`), 
      names_to = "academic_info", 
      values_to = "unit"
    ) 

### normality check
shapiro_res <- long_sleep_df |> 
  nest(data = -academic_info) |> 
  mutate(
    model = map(data, \(x) shapiro.test(x$unit)), 
    results = map(model, broom::tidy)
  ) |> 
  select(academic_info, results) |> 
  unnest(results) |> 
  mutate_if(is.numeric, signif, 3) |> 
  mutate(academic_info = fct_relevel(academic_info, "Relative Course Load", "End-of-term GPA", "Cumulative GPA"))

### equality of variance
get_levene_res <- function(demo){
  res <- long_sleep_df |> 
    nest(data = -academic_info) |> 
    mutate(
    model = map(data, \(x) leveneTest(unit ~ get(demo), x)), 
    results = map(model, broom::tidy), 
    group = demo) |> 
    unnest(results) |> 
  mutate_if(is.numeric, signif, 3) |> 
  select(academic_info, group, statistic, `p.value`) 
  return(res)
}

demo_list <- c("University", "Race", "Gender", "First-Generation")
levene_res <- lapply(demo_list, get_levene_res) 


## ------------------------------------------------------------------------------------------------------------------------------------------
plot_courseload <- function(demo, var_equal) {
  sleep_df |> 
    drop_na(rlang::sym(demo), `Relative Course Load`) |> 
    ggplot(aes(x = get(demo), y = `Relative Course Load`)) +
    geom_violin(alpha = 0.4, fill = "#70BBAA") + 
    labs(y = "", x = "", fill = "") +
    scale_y_continuous(expand = c(0,0), limits = c(-3, 5)) +
    stat_compare_means(method = "t.test", paired = FALSE, 
                       method.args = list(var.equal = var_equal), 
                       label.y = 4, 
                       label.x = 1.5) +
    stat_summary(fun = "mean", fun.min = "mean", color = "skyblue", 
                 fun.max= "mean", size= 0.2, geom = "crossbar")
                           
}


## ----fig.width = 5, fig.height = 4, out.width = "60%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Race"
comp <- list(c("Underrepresented", "Non-underrepresented"))
plot_courseload(demo_name, TRUE)


## ----fig.width = 5, fig.height = 4, out.width = "60%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Gender"
comp <- list(c("Male", "Female"))
plot_courseload(demo = demo_name, TRUE)


## ----fig.width = 5, fig.height = 4, out.width = "60%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "First-Generation"
comp <- list(c("Non-first gen", "First-gen"))
plot_courseload(demo = demo_name, TRUE)


## ----fig.width = 5, fig.height = 4, out.width = "60%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "University"
plot_courseload(demo = demo_name, TRUE)


## ------------------------------------------------------------------------------------------------------------------------------------------
plot_gpa <- function(demo, gpa, var_equal, comp) {
  subset_df <- sleep_df |> 
    drop_na(rlang::sym(demo), rlang::sym(gpa)) 
  
  max_y <- max(subset_df[,gpa], na.rm = TRUE)
  
  y_positions <- seq(max_y, by = 0.5, length.out = length(comp))
  
  cols <- c("End-of-term GPA" = "#ffaaaa", 
            "Cumulative GPA" = "#ffddba")
  
  subset_df |> 
    ggplot(aes(x = get(demo), y = get(gpa), fill = gpa)) +
    geom_violin(alpha = 0.2) + 
    labs(y = "", x = "", fill = "") +
    scale_y_continuous(expand = c(0,0), limits = c(min(subset_df[,gpa]), max(y_positions) + 0.5)) +
    #facet_wrap(~academic_info, ncol = 3, scales = "free") +
    theme(panel.spacing = unit(1, "lines")) +
    scale_fill_manual(values = cols) +
    stat_compare_means(aes(label = paste0("p = ", p.format)), method = "t.test", 
                       paired = FALSE, comparisons = comp,
                       method.args = list(var.equal = var_equal), 
                       p.adjust.method = "bonferroni", 
                       label.y = y_positions) +
    stat_summary(fun = "mean", fun.min = "mean", color = "skyblue", 
                 fun.max= "mean", size= 0.2, geom = "crossbar") 
}


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Race"
comp = list(c("Underrepresented", "Non-underrepresented"))
plot_gpa(demo = demo_name, "End-of-term GPA", FALSE, comp) +
  plot_gpa(demo = demo_name, "Cumulative GPA", FALSE, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Gender"
comp <- list(c("Male", "Female"))
plot_gpa(demo = demo_name, "End-of-term GPA", TRUE, comp) +
  plot_gpa(demo = demo_name, "Cumulative GPA", TRUE, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "First-Generation"
comp <- list(c("Non-first gen", "First-gen"))
plot_gpa(demo = demo_name, "End-of-term GPA", TRUE, comp) +
  plot_gpa(demo = demo_name, "Cumulative GPA", TRUE, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "University"
comp = list(c("CMU", "NDU"), c("NDU", "UW"), c("CMU", "UW"))
plot_gpa(demo = demo_name, "End-of-term GPA", FALSE, comp) +
  plot_gpa(demo = demo_name, "Cumulative GPA", FALSE, comp)


## ------------------------------------------------------------------------------------------------------------------------------------------
plot_loadgpa <- function(demo) {
  sleep_df |> 
    drop_na(rlang::sym(demo), `End-of-term GPA`, `Relative Course Load`) |> 
    ggplot(aes(y = `End-of-term GPA`, x = `Relative Course Load`)) +
    geom_hex() +
    labs(y = "End-of-term GPA", x = "Relative Course Load", color = "") +
    #geom_smooth(se = FALSE) +
    facet_wrap(~get(demo), scales = "free") +
    theme_classic2() +
    theme(
    strip.text = element_text(size = 14),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5))
}


## ----fig.width = 7, fig.height = 4, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Race"
plot_loadgpa(demo = demo_name)


## ----fig.width = 7, fig.height = 4, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Gender"
plot_loadgpa(demo = demo_name)


## ----fig.width = 7, fig.height = 4, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "First-Generation"
plot_loadgpa(demo = demo_name)


## ----fig.width = 7, fig.height = 4, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "University"
plot_loadgpa(demo = demo_name)


## ------------------------------------------------------------------------------------------------------------------------------------------
lm(`End-of-term GPA` ~ `Relative Course Load`*Race +
     `Relative Course Load`*`Gender` +
     `Relative Course Load`*`First-Generation` +
     `Relative Course Load`*`University`, 
                data = sleep_df) |> 
  broom::tidy() |> 
  mutate(term= str_replace(term, "RaceUnderrepresented", "Race - Underrepresented"), 
         term= str_replace(term, "GenderFemale", "Gender - Female"), 
         term= str_replace(term, "`First-Generation`First-gen", "First-gen"), 
         term= str_replace(term, "UniversityUW", "University - UW")) |> 
  rename(`SE` = `std.error`) |> 
  mutate(estimate = round(`estimate`, digits = 3), 
         `SE` = round(`SE`, digits = 3), 
         statistic = round(`statistic`, digits = 3)) |> 
  mutate(`p.value` = signif(`p.value`, digits = 3)) |> 
  knitr::kable(digits = 170) |> 
  row_spec(2, background = "darkgrey") |> 
  kable_minimal()


## ------------------------------------------------------------------------------------------------------------------------------------------
cols <- c("End-of-term GPA" = "#ffaaaa", 
            "Cumulative GPA" = "#ffddba")

plot_improv <- function(demo, comp) {
  subset_df <- sleep_df |> 
    drop_na(rlang::sym(demo), `End-of-term GPA`, `Cumulative GPA`) |> 
    pivot_longer(
      cols = c(`End-of-term GPA`, `Cumulative GPA`), 
      names_to = "gpa", 
      values_to = "unit"
    ) |> 
    mutate(demo_gpa = paste(get(demo), gpa, sep = "_"))
  
  max_y <- max(subset_df[,"unit"], na.rm = TRUE)
  
  subset_df |> 
    ggplot(aes(x = get(demo), fill = gpa, y = unit)) +
    geom_boxplot(alpha = 0.5) + 
    labs(y = "", x = "", fill = "") +
    scale_y_continuous(expand = c(0,0), limits = c(min(subset_df[,"unit"]), max_y + 0.5)) +
    scale_fill_manual(values = cols) +
    stat_compare_means(method = "t.test", 
                       paired = TRUE, 
                       p.adjust.method = "bonferroni", 
                       label.y = max_y)
}


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Race"
plot_improv(demo = demo_name, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "Gender"
plot_improv(demo = demo_name, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "First-Generation"
plot_improv(demo = demo_name, comp)


## ----fig.width = 7, fig.height = 4, out.width = "80%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
demo_name = "University"
plot_improv(demo = demo_name, comp)


## ----fig.width = 5, fig.height = 2, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
ggplot(long_sleep_df, aes(sample = `unit`)) +
  stat_qq(size = 0.4, shape = 21) +
  stat_qq_line() +
  labs(x = "Theoritical Quantiles", y = "Sample Quantiles", 
       title = "Normal Q-Q Plot") +
  facet_wrap(~academic_info, ncol = 3)


## caption {
##       color: #000022;
##       font-weight: bold;
##       font-size: 20px;
##     }

## ------------------------------------------------------------------------------------------------------------------------------------------
shapiro_res |> 
  select(-method) |> 
  column_to_rownames("academic_info") |> 
  kable(digits = 25, align = "l", caption = "Shapiro-Wilk Normality Test") |> 
  kable_minimal()


## ----fig.width = 5, fig.height = 2, out.width = "90%", dpi=600,message=FALSE,warning=FALSE-------------------------------------------------
Reduce(rbind, levene_res) |> 
  select(-statistic) |> 
    pivot_wider(
      names_from = group, 
      values_from = `p.value`) |> 
  column_to_rownames("academic_info") |> 
  kable(digits = 25, align = "l", caption = "Levene's Test For Homogeneity of Variance: P-values") |> 
  kable_minimal()


## ----warning = FALSE, message = FALSE------------------------------------------------------------------------------------------------------
summary_tbl_first <- sleep_df |> 
  select(`Race`, `Gender`, `First-Generation`, `University`) |> 
  tbl_summary(
    by = `First-Generation`,
    statistic = list(
      all_categorical() ~ "{n} / {N} ({p}%)")) |> 
  add_overall() |> 
  modify_spanning_header(c("stat_1", "stat_2") ~ "**First-Generation**") |> 
  bold_labels() |>
  italicize_labels() |> 
  add_p()

as_kable_extra(summary_tbl_first) |> 
  kable_minimal()

