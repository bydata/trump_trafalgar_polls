library(tidyverse)
library(ggtext)
library(extrafont)
library(colorspace)

# load fonts
loadfonts()

# Load data (from fivethirtyeight.com)
# https://www.nytimes.com/elections/2016/results/president
# https://projects.fivethirtyeight.com/2016-election-forecast/utah/
trafalgar <- read_tsv(file.path("input", "trafalgar_polls.tsv"))
glimpse(trafalgar)


# Calculate error
trafalgar <- trafalgar %>% 
  mutate(error = projected_trump - election_trump,
         error_abs = abs(error),
         error_polls = polling_avg_trump - election_trump,
         error_polls_abs = abs(error_polls),
         error_polls_adj = polling_avg_adj_trump - election_trump,
         error_polls_adj_abs = abs(error_polls_adj),
         error_538proj = proj538_trump  - election_trump,
         error_538proj_polls_abs = abs(error_538proj),
         state = factor(state))


# https://fivethirtyeight.com/features/trump-is-just-a-normal-polling-error-behind-clinton/
error_margin <- 2.5

plot_averages_col <- colorspace::lighten("darkblue", amount = 0.2)
plot_averages_col <- qualitative_hcl(5, palette = "Dark 3")[2]
diverging_colors <- diverging_hcl(3, palette = "Berlin")


plot_title <- "Trafalgar Group mit einigen guten Treffern,\naber sind das mehr als Glückstreffer?"

plot_subtitle <- glue::glue("Die Positionen der <b style='font-family: Open Sans'>Punkte</b>
zeigen die absolute Abweichung<br>
des prognostizierten Vorsprung Trumps
gegenüber Clinton vom Wahlergebnis an.<br>
Die Färbung gibt an, ob Trafalgar Group im Vergleich zum Durchschnitt <br>
der anderen Institute
<b style='color:{diverging_colors[1]};font-family:Open Sans'>besser</b>,
<b style='color:{diverging_colors[3]};font-family:Open Sans'>schlechter</b> oder
<b style='color:{diverging_colors[2]};font-family:Open Sans'>gleich</b>
abgeschnitten hat.
")

plot_caption <- "Dargestellt sind die Umfrageergebnisse von Trafalgar in den Bundesstaaten. 
Gab es mehr als eine Umfrage, wird die Umfrage, die näher am Wahltag liegt, angezeigt.
Trafalgar veröffentlichte nur in den dargestellten Bundesstaaten Umfrageergebnisse.
Als gleich gelten Umfragen, deren Ergebnisse weniger als eine Schwankungsbreite von 
+/-2,5 Prozentpunkten voneinander abweichen.
Quelle: FiveThirtyEight.com"


trafalgar %>% 
  mutate(diff_to_avg = error_abs - error_polls_adj_abs,
         compared_to_benchmark = case_when(
           diff_to_avg > error_margin ~ "Schlechter",
           diff_to_avg >= -error_margin & diff_to_avg <= error_margin ~ "Gleich",
           diff_to_avg < -error_margin ~ "Genauer")
  ) %>% 
  arrange(state) %>% 
  ggplot(aes(state, error_abs)) +
  # geom_polygon(aes(state, y = error_margin, group = 1),
  #              col = NA, fill = "grey70", alpha = 0.1) +
  # avg. error of the polls
  geom_polygon(aes(y = error_polls_adj_abs, group = NA,  col = (error_polls > 0)), 
               col = plot_averages_col, fill = NA, size = 0.5, 
               lty = "dotted", alpha = 0.8) +
  geom_point(aes(fill = compared_to_benchmark), 
             size = 4,
             col = "white",
             shape = 21, alpha = 0.7, 
             show.legend = FALSE
             ) +
  ggrepel::geom_text_repel(aes(label = round(error_abs, 1)),
            col = "grey40", direction = "both",
            size = 2.5,
            nudge_x = 0.2, nudge_y = 0.2,
            family = "Open Sans") + 
  # annotate("text", label = "Die graue Fläche zeigt\nden Fehlerbereich\nder Umfragen",
  #          x = 2.5, y = 5, hjust = 0, color = "grey30", size = 3,
  #          family = "Open Sans Light") + 
  # geom_segment(x = 2.5, xend = 2.5,
  #            y = 4.9, yend = error_margin + 0.1,
  #            col = "grey60", size = 0.1) +
  # Annotations for poll averages
  annotate("text", label = "Durchschnittliche Abweichungen\nder Umfragen vom\nWahlergebnis",
           x = 5.5, y = 5.1, hjust = 0, color = plot_averages_col, 
           size = 3,
           family = "Open Sans Light") + 
  coord_polar(theta = "x", start = 0) +
  # scale_fill_manual(values = c("Schlechter" = "red", "Genauer" = "blue", "Gleich" = "grey50")) +
  scale_fill_discrete_diverging(palette = "Berlin") +
  labs(title = plot_title,
       subtitle = plot_subtitle,
       caption = plot_caption) +
  theme_minimal(base_family = "Open Sans Light") +
  theme(plot.title = element_text(family = "Open Sans SemiBold", size = 16,
                                  lineheight = 1.1),
        plot.subtitle = element_markdown(color = "grey20",
                                         size = 9,
                                         lineheight = 1.2),
        plot.caption = element_text(family = "Open Sans Light", color = "grey20",
                                    hjust = 0, lineheight = 1, size = 8),
        axis.title = element_blank(),
        axis.text.x = element_text(family = "Open Sans", face = "bold", 
                                   size = 12, color = "grey20"),
        axis.text.y = element_blank(),
        legend.position = "top",
        legend.justification = "left")

ggsave(file.path("plots", "trafalgar_polls.png"), type = "cairo", dpi = 320,
       width = 5, height = 5, scale = 1.4)

