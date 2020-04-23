library(tidyverse)
library(patchwork)
library(ggdag)
library(dagitty)
library(MatchIt)
library(broom)

edu_age <- read_csv("data/edu_age.csv") %>% 
  mutate(treated = recode(treatment, Treatment = "Treated", Control = "Untreated"))


# General simple DAGs -----------------------------------------------------

super_simple_dag <- dagify(
  Y ~ X,
  coords = list(x = c(X = 1, Y = 2),
                y = c(X = 1, Y = 1))
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 8) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                size = 3) +
  labs(tag = "A") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

status_example_dag <- dagify(
  Y ~ A + B,
  X ~ A + B,
  exposure = "X",
  outcome = "Y",
  latent = "B",
  coords = list(x = c(X = 1, Y = 4, A = 2, B = 3),
                y = c(X = 1, Y = 1, A = 2, B = 2)),
  labels = c(X = "Treatment",
             Y = "Outcome",
             B = "Latent")
) %>% 
  tidy_dagitty() %>% 
  node_status() %>% 
  mutate(status = str_to_title(status))

status_dag <- ggplot(status_example_dag, 
                     aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status), size = 8) +
  geom_dag_text(data = filter(status_example_dag, name %in% c("A", "B")),
                color = "black", family = "LM Roman 10", fontface = "bold.italic",
                nudge_x = -0.01, size = 3) +
  geom_dag_text(data = filter(status_example_dag, name %in% c("X", "Y")),
                color = "white", family = "LM Roman 10", fontface = "bold.italic",
                nudge_x = -0.01, size = 3) +
  geom_dag_text_repel(aes(label = label), 
                      nudge_x = c(0.5, 0.6, -0.6),
                      nudge_y = c(0, 0, 0),
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, size = 3) +
  scale_color_manual(values = c("grey30", "grey85", "black"), 
                     na.value = "grey60", guide = FALSE) +
  labs(tag = "B") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

time_dag <- dagitty('dag {
"bolditalic(X[t-1])" [pos="1,1"]
"bolditalic(A[t-1])" [pos="1.5,2"]
"bolditalic(Y[t-1])" [pos="2,1"]
"bolditalic(X)" [pos="3,1"]
"bolditalic(A)" [pos="3.5,2"]
"bolditalic(Y)" [pos="4,1"]
"bolditalic(A[t-1])" -> "bolditalic(X[t-1])"
"bolditalic(A[t-1])" -> "bolditalic(Y[t-1])"
"bolditalic(A[t-1])" -> "bolditalic(A)"
"bolditalic(X[t-1])" -> "bolditalic(Y[t-1])"
"bolditalic(Y[t-1])" -> "bolditalic(X)"
"bolditalic(A)" -> "bolditalic(Y)"
"bolditalic(A)" -> "bolditalic(X)"
"bolditalic(X)" -> "bolditalic(Y)"
}') %>% 
  tidy_dagitty() %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 8) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                size = 3, parse = TRUE) +
  labs(tag = "C") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

general_examples <- super_simple_dag + status_dag + time_dag

ggsave("output/general-examples.pdf", general_examples, 
       width = 9, height = 2, units = "in", device = cairo_pdf)


 # Three types of associations ---------------------------------------------

mediator <- dagify(
  Y ~ Z,
  Z ~ X,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 8) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                nudge_x = -0.01, size = 3) +
  labs(tag = "A") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

confounder <- dagify(
  Y ~ Z,
  X ~ Z,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 8) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                nudge_x = -0.01, size = 3) +
  labs(tag = "B") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

collider <- dagify(
  Z ~ Y,
  Z ~ X,
  coords = list(x = c(X = 1, Y = 3, Z = 2),
                y = c(X = 1, Y = 1, Z = 2))
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 8) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                nudge_x = -0.01, size = 3) +
  labs(tag = "C") +
  theme_dag(base_size = 8, base_family = "LM Roman 10")

combined_associations <- mediator + confounder + collider

ggsave("output/association-examples.pdf", combined_associations, 
       width = 9, height = 2, units = "in", device = cairo_pdf)


# Money -> votes simple ---------------------------------------------------

money_votes_simple_dag <- dagify(
  Y ~ X + conf + med,
  med ~ X,
  X ~ conf,
  coll ~ X + Y,
  exposure = "X",
  outcome = "Y",
  coords = list(x = c(X = 1, Y = 4, conf = 2.5, med = 3, coll = 2),
                y = c(X = 2, Y = 2, conf = 3, med = 1, coll = 1)),
  labels = c("Y" = "Total\nvotes",
             "X" = "Money\nraised",
             "med" = "Hired\ncampaign\nmanager",
             "conf" = "Candidate\nquality",
             "coll" = "Won\nelection")
) %>% 
  tidy_dagitty() %>% 
  node_status()


plot_money_votes_simple <- ggplot(money_votes_simple_dag, 
                                  aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_text_repel(aes(label = label), 
                      nudge_y = c(0.3, 0.3, -0.4, 0, 0),
                      nudge_x = c(0, 0, 0, 0.5, -0.5),
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, size = 3) +
  scale_color_manual(values = c("grey50", "grey10"), na.value = "grey80", guide = FALSE) +
  theme_dag(base_family = "LM Roman 10")

ggsave("output/money-votes-simple.pdf", plot_money_votes_simple, 
       width = 6, height = 4, units = "in", device = cairo_pdf)


# Money -> votes complex --------------------------------------------------

money_votes_complex_dag <- dagify(
  votes ~ money + quality + party + district + manager,
  money ~ quality + party + district,
  party ~ history,
  district ~ history,
  won ~ money + votes,
  manager ~ money,
  exposure = "money",
  outcome = "votes",
  latent = "history",
  coords = list(x = c(money = 1, votes = 5, quality = 2, party = 3, 
                      district = 4, history = 4, manager = 4, won = 2),
                y = c(money = 2, votes = 2, quality = 3, party = 4, 
                      district = 3, history = 4, manager = 1, won = 1)),
  labels = c("votes" = "Total\nvotes",
             "money" = "Money\nraised",
             "quality" = "Candidate\nquality",
             "party" = "Party",
             "district" = "District",
             "history" = "History\n(unobserved)",
             "manager" = "Hired\ncampaign\nmanager",
             "won" = "Won\nelection")
)

money_votes_complex_dag_tidy <- money_votes_complex_dag %>% 
  tidy_dagitty() %>% 
  node_status()

# List all paths
# dagitty::paths(money_votes_complex_dag)

plot_money_votes_complex <- ggplot(money_votes_complex_dag_tidy, 
                                   aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_text_repel(aes(label = label),
                      nudge_x = c(0.5, 0.5, 0.5, 0, -0.5, -0.5, 0, -0.5),
                      nudge_y = c(0, 0, 0, -0.5, 0, 0, -0.5, 0),
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, seed = 1234) +
  scale_color_manual(values = c("grey50", "grey80", "grey10"), na.value = "grey80", guide = FALSE) +
  theme_dag(base_family = "LM Roman 10")

ggsave("output/money-votes-complex.pdf", plot_money_votes_complex, 
       width = 6, height = 4, units = "in", device = cairo_pdf)


# Removing arrows in an RCT -----------------------------------------------

observational_dag <- dagify(
  Y ~ X + A,
  X ~ A,
  coords = list(x = c(X = 1, Y = 2, A = 1.5),
                y = c(X = 1, Y = 1, A = 2))
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 11) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                size = 4) +
  labs(title = "A: Observational DAG") +
  theme_dag(base_size = 11, base_family = "LM Roman 10")
observational_dag

rct_dag <- dagify(
  Y ~ X + A,
  coords = list(x = c(X = 1, Y = 2, A = 1.5),
                y = c(X = 1, Y = 1, A = 2)),
  labels = c(X = "X = x")
) %>% 
  ggplot(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(color = "grey80", size = 11) +
  geom_dag_text(color = "black", family = "LM Roman 10", fontface = "bold.italic",
                size = 4) +
  geom_dag_text_repel(aes(label = label),
                      nudge_x = 0, nudge_y = 0.25,
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, seed = 1234) +
  labs(title = "B: Experimental DAG (do(X = x))") +
  theme_dag(base_size = 11, base_family = "LM Roman 10")
rct_dag

observational_dag + rct_dag

ggsave("output/rct-dag.pdf", observational_dag + rct_dag, 
       width = 6, height = 2.9, units = "in", device = cairo_pdf)


# Matching example --------------------------------------------------------

matched <- matchit(treatment_num ~ education + age, data = edu_age,
                   method = "nearest", distance = "mahalanobis")

matched_pairs <- tibble(trt = rownames(matched$match.matrix),
                        ctrl = matched$match.matrix) %>% 
  mutate_all(as.numeric)

edu_age_filtered <- edu_age %>% 
  filter(id %in% c(matched_pairs$trt, matched_pairs$ctrl)) %>% 
  left_join(matched_pairs, by = c("id" = "trt")) %>% 
  left_join(select(edu_age, id, education_ctrl = education, age_ctrl = age),
            by = c("ctrl" = "id"))

edu_age_matched <- edu_age %>% 
  left_join(matched_pairs, by = c("id" = "trt")) %>% 
  left_join(select(edu_age, id, education_ctrl = education, age_ctrl = age),
            by = c("ctrl" = "id")) %>% 
  filter(treatment == "Treatment")

matching1 <- ggplot(edu_age, aes(x = education, y = age, fill = fct_rev(treated))) +
  geom_point(size = 3, pch = 21, color = "white") +
  scale_fill_manual(values = c("grey65", "black"), name = NULL) +
  guides(color = FALSE) +
  labs(x = "Education", y = "Age", tag = "A") +
  cowplot::theme_cowplot(font_size = 11, font_family = "LM Roman 10") +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.key.size =  unit(0.2, "inches"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, 10, -10, -10))

matching2 <- ggplot(edu_age, aes(x = education, y = age, fill = treatment)) +
  geom_point(size = 3, pch = 21, color = "white", alpha = 0.2) +
  geom_point(data = edu_age_filtered, size = 3, pch = 21, color = "white") +
  geom_segment(data = edu_age_matched, 
               aes(x = education, xend = education_ctrl,
                   y = age, yend = age_ctrl),
               linetype = "11", color = "grey50", size = 1) +
  scale_fill_manual(values = c("grey65", "black"), name = NULL) +
  scale_color_manual(values = c("grey65", "black"), name = NULL) +
  guides(color = FALSE) +
  labs(x = "Education", y = "Age", tag = "B") +
  cowplot::theme_cowplot(font_size = 11, font_family = "LM Roman 10") +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.key.size =  unit(0.2, "inches"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, 10, -10, -10))

ggsave("output/matching-example.pdf", matching1 + matching2,
       width = 8, height = 3, units = "in", device = cairo_pdf)


# IPW example -------------------------------------------------------------

model_treatment <- glm(treatment ~ education + age, 
                       data = mutate(edu_age, treatment = factor(treatment)),
                       family = binomial(link = "logit"))

edu_age_ipw <- augment_columns(model_treatment, edu_age, 
                               type.predict = "response") %>% 
  rename(propensity = .fitted) %>% 
  mutate(ipw = (treatment_num / propensity) + 
           (1 - treatment_num) / (1 - propensity))

plot_edu_age_ipw <- ggplot(edu_age_ipw, aes(x = education, y = age, 
                                            fill = fct_rev(treated), size = ipw)) +
  geom_point(pch = 21, color = "white") +
  scale_fill_manual(values = c("grey65", "black"), name = NULL) +
  guides(color = FALSE, size = FALSE,
         fill = guide_legend(override.aes = list(size = 3))) +
  labs(x = "Education", y = "Age") +
  cowplot::theme_cowplot(font_size = 11, font_family = "LM Roman 10") +
  theme(legend.position = "top",
        legend.justification = "right",
        legend.key.size =  unit(0.2, "inches"),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.margin = margin(-5, 10, -10, -10))

ggsave("output/ipw-example.pdf", plot_edu_age_ipw,
       width = 4, height = 3, units = "in", device = cairo_pdf)


# Mosquito net DAGs -------------------------------------------------------

mosquito_dag_tidy <- dagify(
  malaria_risk ~ net + income + health + temperature + resistance,
  net ~ income + health + temperature + eligible + household,
  eligible ~ income + household,
  health ~ income,
  exposure = "net",
  outcome = "malaria_risk",
  coords = list(x = c(malaria_risk = 7, net = 3, income = 4, health = 5,
                      temperature = 6, resistance = 8.5, eligible = 2, household = 1),
                y = c(malaria_risk = 2, net = 2, income = 3, health = 1,
                      temperature = 3, resistance = 2, eligible = 3, household = 2)),
  labels = c(malaria_risk = "Risk of\nmalaria", net = "Mosquito\nnet", income = "Income",
             health = "Health", temperature = "Nighttime \ntemperatures", 
             resistance = "Insecticide\nresistance",
             eligible = "Eligible\nfor program", household = "Number in\nhousehold")
) %>% 
  tidy_dagitty() %>% 
  node_status() %>% 
  adjust_for(var = c("income", "temperature", "health")) %>% 
  mutate(adjusted = str_to_title(adjusted))

mosquito_dag_plain <- ggplot(mosquito_dag_tidy, 
                             aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges() +
  geom_dag_point(aes(color = status)) +
  geom_dag_text_repel(aes(label = label),
                      nudge_x = c(-0.5, 0.5, 0, 0.3, 0, 0, 0.5, 0),
                      nudge_y = c(0, 0, -0.3, 0, -0.3, -0.3, 0, -0.3),
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, size = 3) +
  scale_color_manual(values = c("grey50", "grey10"), na.value = "grey80", guide = FALSE) +
  theme_dag(base_family = "LM Roman 10")

ggsave("output/mosquito-dag-plain.pdf", mosquito_dag_plain,
       width = 6, height = 4, units = "in", device = cairo_pdf)

mosquito_dag_adjusted <- ggplot(mosquito_dag_tidy, 
                                aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_dag_edges(aes(edge_colour = adjusted)) +
  geom_dag_point(aes(color = adjusted, shape = adjusted)) +
  geom_dag_text_repel(aes(label = label),
                      nudge_x = c(-0.5, 0.5, 0, 0.3, 0, 0, 0.5, 0),
                      nudge_y = c(0, 0, -0.3, 0, -0.3, -0.3, 0, -0.3),
                      color = "black", family = "LM Roman 10",
                      fontface = "bold", lineheight = 0.95, size = 3) +
  scale_color_manual(values = c("grey35", "grey70"), name = NULL) +
  ggraph::scale_edge_color_manual(values = c("grey35", "grey80"), name = "") +
  scale_shape_manual(values = c(15, 19), name = NULL) +
  guides(shape = guide_legend(override.aes = list(size = 4))) +
  theme_dag(base_family = "LM Roman 10") +
  theme(legend.position = "top")

ggsave("output/mosquito-dag-adjusted.pdf", mosquito_dag_adjusted,
       width = 6, height = 4.2, units = "in", device = cairo_pdf)
