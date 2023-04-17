## README



``` r
url_survey <- 'https://docs.google.com/spreadsheets/d/1tkMVLbG8fxVcSGqr6WyPJwyHY-YOEN0BDbpGfByAiKw/edit?usp=sharing'
therm_survey <- gsheet2tbl(url_survey)
names(therm_survey)[2] <- "Daytime"
names(therm_survey)[3] <- "Nightime"

therm_survey <- therm_survey %>%
  select(c(2:3))%>%
  mutate(Daytime_Base = as.numeric(str_extract(Daytime, "^\\d{2}")),
         Nightime_Base = as.numeric(str_extract(Nightime, "^\\d{2}")),
         Difference = Nightime_Base - Daytime_Base,
         Change = case_when(
           Difference < 0 ~ "Coolor at Nights",
           Difference > 0 ~ "Warmer at Nights",
           Difference == 0 ~ "Same Temperature"),
         Daytime = paste(Daytime, "daytime", sep = " "),
         Nightime = paste(Nightime, "nightime", sep = " "))%>%
  rename("Daytime Temp. Preference"= "Daytime",
         "Nightime Temp. Preference"= "Nightime")




df <- therm_survey[, c(1, 6, 2)]
TotalCount = nrow(df)

df <- df %>%
  make_long(`Daytime Temp. Preference`, Change, `Nightime Temp. Preference`)



# Step 2
dagg <- df %>%
  dplyr::group_by(node)%>%
  tally()

dagg <- dagg%>%
  dplyr::group_by(node)%>%
  dplyr::mutate(pct = n/TotalCount)


# Step 3
df2 <- merge(df, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

pl <- ggplot(df2, aes(x = x
                      , next_x = next_x
                      , node = node
                      , next_node = next_node
                      , fill = factor(node)
                      
                      , label = paste0(node," [", "n=", n, ' (',  round(pct* 100,1), '%)', ']' ))
)

pl <- pl +geom_sankey(flow.alpha = 0.5,  color = "gray40", show.legend = TRUE)
pl <- pl +geom_sankey_label(size = 3, color = "black", fill= "white", hjust = -0.1)

pl <- pl +  covdata_theme()+
  xlab("")
pl <- pl + theme(legend.position = "none")
pl <- pl +  theme(axis.text.y = element_blank()
                  , axis.ticks = element_blank()  
                  , panel.grid = element_blank())
#pl <- pl + scale_fill_viridis_d(option = "inferno")
pl <- pl + labs(title = "Sankey diagram using ggplot")
#pl <- pl + labs(subtitle = "using  David Sjoberg's ggsankey package")
pl <- pl + labs(caption = "todd sink")
#pl <- pl + labs(fill = 'Nodes')
pl <- pl + scale_fill_manual(values = c('Warmer at Nights'  = "red",
                                        'Same Temperature'  ="#878787",
                                        'Coolor at Nights'  = "blue", 
                                        '75+ daytime'  = "#ff0000",
                                        '72-74 daytime'  = "#FB6A4A", 
                                        '69-71 daytime' = "#FED976",
                                        '66-68 daytime' = "#41B6C4",
                                        '63-65 daytime' = "#225EA8",
                                        '60-62 daytime' = "#081D58",
                                        '75+ nightime'  = "#ff0000",
                                        '72-74 nightime'  = "#FB6A4A", 
                                        '69-71 nightime' = "#FED976",
                                        '66-68 nightime' = "#41B6C4",
                                        '63-65 nightime' = "#225EA8",
                                        '60-62 nightime' = "#081D58",
                                        '55-59 nightime' = "#081D58"
                                        
                                        
) )

#pl <- pl + scale_fill_viridis_d(option = "inferno")
pl
```

![](home_temp_analysis_files/figure-markdown_github/unnamed-chunk-2-1.png)