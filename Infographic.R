# Import necessary packages and fonts
library(tidyverse)
library(patchwork)
library(opendatatoronto)
library(sf)
library(scico)
library(lubridate)
library(ggsankey)
library(magick)
library(cowplot)
library(ggimage)
library(showtext)
font_add_google("Roboto Condensed")
showtext_auto()
my_font <- "Roboto Condensed"

# Gathering resources from open.toronto.ca 
package <- show_package("ec53f7b2-769b-4914-91fe-a37ee27a90b3")
resources <- list_package_resources("ec53f7b2-769b-4914-91fe-a37ee27a90b3")
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
collisions <- filter(datastore_resources, row_number()==2) %>% get_resource()
collisions_no_geom <- collisions %>% st_drop_geometry()

resources <- list_package_resources("neighbourhoods")
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))
neighbourhoods <- filter(datastore_resources, row_number()==1) %>% get_resource()

# Set theme
custom_theme <- function() {
  theme(
    text=element_text(family=my_font),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 12),
    axis.text = element_text(face = "bold")
  )
}

# Plot 1 - Continuous 2D/3D plot
xy <- data.frame(st_coordinates(collisions$geometry))
data_coords <- cbind(collisions, xy)

# Clean coordinates with non-specified address (NSA)
data_coords <- data_coords %>% 
  filter(Division != "NSA" & Atom != "NSA" & Neighbourhood != "NSA" & X < 0 & Y > 0)

colors <- c("#FFFFFF", scico(4, begin=0.1))
p1 <- ggplot(data_coords) +
  stat_density_2d(aes(x = X, y = Y, fill = after_stat(density)), n=300, alpha = 1, geom = "raster", contour = FALSE) +
  geom_sf(data = neighbourhoods, alpha = 0.01, color = "black") +
  scale_fill_gradientn(colors=colors, values=c(0, 0.1, 0.2, 0.4, 1), name="Collision density") +
  ggtitle("Collisions ") +
  ggtitle("Most dangerous neighbourhoods for car accidents", subtitle = "Number of collisions by region") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  xlab(NULL) +
  ylab(NULL) +
  theme_void() + 
  theme(
    text=element_text(family=my_font),
    plot.title = element_text(face = "bold", size=14),
    plot.subtitle = element_text(size = 12)
  )

# Plot 2 - Wildcard
# Get number of collisions per month
collision_ts <- collisions_no_geom %>% 
  group_by(date=as.Date(paste(format(OccurrenceDate,'%Y-%m'), "-01", sep=""))) %>% 
  summarize(n = n())

# Set pandemic dates
pandemic <- data.frame(x1=as.Date("2020-01-02"), 
                       x2=as.Date("2022-06-01"), 
                       xmid=as.Date("2021-03-01"), 
                       y=1000)

colors <- scico(4, palette = 'lajolla')

p2 <- ggplot(collision_ts, aes(x=date, y=n)) +
  geom_smooth(color = colors[2], method="gam", alpha=0) +
  geom_line(color = colors[4]) + 
  geom_segment(data=pandemic, 
               aes(x = x1, y = y, xend = x2, yend = y),
               color=colors[3], alpha=1, size=0.75) +
  geom_segment(data=pandemic, 
               aes(x = x1, y = y, xend = x1, yend = y+200, colour = "segment"),
               color=colors[3], alpha=1, size=0.75) +
  geom_segment(data=pandemic, 
               aes(x = x2, y = y, xend = x2, yend = y+200, colour = "segment"),
               color=colors[3], alpha=1, size=0.75) + 
  geom_text(data=pandemic,
            aes(x = xmid, y = y-400, label="COVID-19 Pandemic"),
            fontface="bold",
            color=colors[3]) +
  xlab(NULL) +
  ylab(NULL) +
  scale_x_date(breaks = scales::breaks_pretty(10)) +
  ggtitle("Impacts of the COVID-19 Pandemic",
          subtitle = "Collisions per month") +
  theme_minimal() +
  custom_theme()

# Plots 3 and 4 - Categorical
levels = rev(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))

# Get dates and weekday for each date as a factor
weekdays <- collisions_no_geom %>% 
  group_by(date=as.Date(format(OccurrenceDate,'%Y-%m-%d'))) %>% 
  mutate(weekday=factor(Day_of_Week, levels = levels, ordered = TRUE)) %>%
  select(all_of(c("date", "weekday"))) %>%
  unique()

# Get number of collisions per day and join to get weekday
collision_ts_2 <- collisions_no_geom %>% 
  group_by(date=as.Date(format(OccurrenceDate,'%Y-%m-%d'))) %>% 
  summarise(n = n()) %>%
  inner_join(weekdays, by="date")

p3 <- ggplot(collision_ts_2, 
       aes(x=n, y=weekday, fill=weekday)) +
  geom_violin(adjust=2, draw_quantiles = c(0.5), show.legend = FALSE) + 
  xlab("Collisions per day") +
  ylab(NULL) +
  scale_fill_scico_d(palette="lajolla", begin=0.2, end=0.6) +
  theme(legend.position="none") +
  ggtitle('Busier days and times lead to more collisions',
          subtitle = 'Collisions per weekday and hour') +
  theme_minimal() + 
  custom_theme()

# Function to convert hour to AM/PM format
convert_time <- function (hour_num) {
  if (is.na(hour_num))
    return("12 AM")
  else if (hour_num <= 11)
    return(paste(hour_num, "AM"))
  else if (hour_num == 12)
    return("12 PM")
  else
    return(paste(hour_num-12, "PM"))
}

# Factor levels - sorted AM/PM time format
levels = c(paste0("", 1:11, " AM"), "12 PM", paste0("", 1:11, " PM"), "12 AM")

# Get number of collisions for each hour
hour_time <- collisions_no_geom %>% 
  mutate(hour=factor(map(Hour, convert_time), levels = levels, ordered = TRUE)) %>%
  group_by(date=as.Date(format(OccurrenceDate,'%Y-%m-%d')), hour=hour) %>% 
  summarise(n = n())

p4 <- ggplot(hour_time, 
       aes(x=n, y=fct_rev(hour), fill=hour)) +
  #stat_density_ridges(quantile_lines = TRUE, quantiles = 2, show.legend = FALSE, alpha = 0.95) +
  #geom_violin(width=0.9, size=0.2, adjust=0.5, draw_quantiles = c(0.5), show.legend=FALSE) + 
  geom_boxplot(outlier.shape = NA, show.legend = FALSE, ) +
  xlab("Collisions per hour") +
  ylab(NULL) +
  xlim(0, 35) +
  scale_fill_scico_d(palette="lajolla", begin=0.2, end=0.6) +
  theme_minimal() + 
  custom_theme() + 
  theme(axis.text.y = element_text(size=7.5))

p34 <- p3 | p4

# Plot 5 - Homebrew
# Preprocess collision to create Sankey steps
# (each collisions incurs in one of the following: no damage, property damage, personal injury or fatality)
collision_subsets <- collisions_no_geom %>% 
  mutate("Damage" = ifelse(!is.na(Fatalities) | Injury_Collisions == "YES" | PD_Collisions == "YES", "Damage: Yes", "Damage: No")) %>%
  mutate("Type" = ifelse (!is.na(Fatalities), "Fatalities", ifelse(Injury_Collisions == "YES", "Personal Injury", ifelse(PD_Collisions == "YES", "Property Damage", NA)))) %>%
  as_tibble() %>%
  select(all_of(c("Damage", "Type")))

# Create Sankey long format
total <- nrow(collision_subsets)
Collision <- rep("Collision", total)
sankey_data <- cbind(Collision, collision_subsets)
sankey_data_long <- sankey_data %>% 
  make_long(Collision, "Damage", "Type") %>%
  mutate(next_node = ifelse(x == "Damage" & node == "No", NA, next_node)) %>%
  filter(!is.na(node))

# Get percentage of each class
pcts <- sankey_data_long %>%
  group_by(node) %>%
  summarise(n = n()) %>% 
  mutate(pct = n/total)

# Merge to include percentages
sankey_data_long2 <- merge(sankey_data_long, pcts, by.x = 'node', by.y = 'node', all.x = TRUE)

p5_pre <- ggplot() +
  geom_sankey(data=sankey_data_long2, 
              aes(x = x, 
                  next_x = next_x, 
                  node = node, 
                  next_node = next_node,
                  fill = factor(node),
                  label = paste0(node, ' (', round(pct*100,1), '%)' )),
              flow.alpha = 0.8,
              smooth = 5,
              node.color = "gray30") +
  geom_sankey_label(data=sankey_data_long2, 
                   aes(x = x, 
                       next_x = next_x, 
                       node = node, 
                       next_node = next_node,
                       fill = factor(node),
                       label = paste0(node, '\n(', round(pct*100,1), '%)' )),
                   size = 4, color = "white", family=my_font) +
  scale_fill_scico_d(palette="lajolla", begin=0.3, end=0.7) +
  theme_sankey(base_size = 12) +
  labs(x = NULL) +
  labs(y = NULL) +
  ggtitle("How bad is the damage?",
          subtitle="Property Damage: an individual's property has been damaged or the value of damages is less than $2,000\nPersonal Injury: an individual involved in a collision suffered personal injuries\nFatalities: an individual’s injuries from a collision result in a fatality within 30 days") + 
  theme(legend.position = "none") +
  scale_size_identity() +
  coord_cartesian(clip = 'off') + 
  theme(axis.text.x=element_blank()) +
  custom_theme()

# Include image icons
collision_image <- image_read("images/Collision.png")
property_damage_image <- image_read("images/PropertyDamage.png")
personal_injury_image <- image_read("images/PersonalInjury.png")
fatal_image <- image_read("images/Fatal.png")
p5 <- ggdraw() + draw_plot(p5_pre) + 
  draw_image(collision_image, scale=0.07, x=0.25, y=0.4, valign = 0, halign = 0) + 
  draw_image(property_damage_image, scale=0.08,  x=0.89, y=0.55, valign = 0, halign = 0) +
  draw_image(personal_injury_image, scale=0.07,  x=0.88, y=0.2, valign = 0, halign = 0) +
  draw_image(fatal_image, scale=0.07, x=0.87, y=0.02, valign = 0, halign = 0)

# Infographic
info <- p1 / p2 / p34 / p5 + scale_size_identity() +
  plot_layout(heights = c(0.8, 0.5, 0.7, 1)) +
  plot_annotation(title = "Toronto Traffic Collisions Infographic",
                  subtitle = "A visualization of all Motor Vehicle Collision occurrences from 2014 to 2022",
                  caption = "Data: open.toronto.ca\nIcons: flaticon.com", 
                  theme = theme(plot.title = element_text(size = 22, face = "bold", family = my_font),
                                plot.subtitle = element_text(size = 14, family = my_font),
                                plot.caption = element_text(color = "grey50", face = "bold.italic", family = my_font)))

# Export info PDF (portrait mode, 10x20 inches)
info
