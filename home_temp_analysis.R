

## Analysis

url_survey <- 'https://docs.google.com/spreadsheets/d/1tkMVLbG8fxVcSGqr6WyPJwyHY-YOEN0BDbpGfByAiKw/edit?usp=sharing'
therm_survey <- gsheet2tbl(url_survey)
names(therm_survey)[2] <- "Daytime"
names(therm_survey)[3] <- "Nighttime"

therm_survey <- therm_survey %>%
  select(c(2:3))%>%
  mutate(Daytime_Base = as.numeric(str_extract(Daytime, "^\\d{2}")),
         Nighttime_Base = as.numeric(str_extract(Nighttime, "^\\d{2}")),
         Difference = Nighttime_Base - Daytime_Base,
         Change = case_when(
           Difference < 0 ~ "Coolor at Nights",
           Difference > 0 ~ "Warmer at Nights",
           Difference == 0 ~ "Same Temperature"),
         Daytime = paste(Daytime, "", sep = " "),
         Nighttime = paste(Nighttime, "", sep = ""))%>%
  rename("Daytime Temp. Preference"= "Daytime",
         "Nighttime Temp. Preference"= "Nighttime",
         "Daytime-to-Nighttime Change"= "Change")


df <- therm_survey[, c(1, 6, 2)]

# set up data for sankey diagram
TotalCount = nrow(df)

# Step 1
df <- df %>%
  make_long(`Daytime Temp. Preference`, `Daytime-to-Nighttime Change`, `Nighttime Temp. Preference`)


# Step 2
dagg <- df %>%
  dplyr::group_by(node)%>%
  tally()

dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n/TotalCount)


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

labels <- labs(title = "Home Thermostat Temperature Range",
               subtitle = "Daytime and Nighttime Preferences--Spring and Summer Heat",
               caption = "todd sink")

# Create chart
pl <- ggplot(df2, aes(x = x,
                      next_x = next_x,
                      node = node,
                      next_node = next_node,
                      fill = factor(node),
                      label = paste0(node, " [", "n=", n, ' (',  round(pct* 100,1), '%)', ']' )) ) +
  geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)+
  geom_sankey_label(size = 3.0, color = "black", fill= "white", hjust = -0.01)+
  covdata_theme() +
  xlab("") +
  labels +
  scale_fill_manual(values = c('Warmer at Nights'  = "red",
                               'Same Temperature'  ="#878787",
                               'Coolor at Nights'  = "blue", 
                               '75+ '  = "#ff0000",
                               '72-74 '  = "#FB6A4A", 
                               '69-71 ' = "#FED976",
                               '66-68 ' = "#41B6C4",
                               '63-65 ' = "#225EA8",
                               '60-62 ' = "#081D58",
                               '75+'  = "#ff0000",
                               '72-74'  = "#FB6A4A", 
                               '69-71' = "#FED976",
                               '66-68' = "#41B6C4",
                               '63-65' = "#225EA8",
                               '60-62' = "#081D58",
                               '55-59' = "#081D58")) +
  
  theme(legend.position = "none",
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),  
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 11),
        plot.caption = element_text(margin = margin(t=0,b=0, r=-0.7),vjust = 0, hjust = 1, size = 9))


pl

