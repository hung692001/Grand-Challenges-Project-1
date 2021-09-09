#[7]Bar chart race of top 10 states with highest cases per year
#Method is referenced, sorting and variable manipulation is carried out  
library(gganimate)
library(tidyverse)
# Sorting the data
maindata <- read.csv("hate_crime.csv")
stacked1991 <- data.frame(maindata[which(maindata$DATA_YEAR == 1991),])
statename1991 <- data.frame(table(stacked1991$STATE_ABBR)) 
stacked1991 <- data.frame(year = rep(1991),table(stacked1991$STATE_NAME),stateabbr = statename1991$Var1)
statesperyear <- stacked1991
for(i in 1992:2019){
  currentdata <- data.frame(maindata[which(maindata$DATA_YEAR == i),])
  currentstate <- data.frame(table(currentdata$STATE_ABBR)) 
  currentdata <- data.frame(year = rep(i),table(currentdata$STATE_NAME),stateabbr = currentstate$Var1)
  statesperyear <- rbind(statesperyear,currentdata)
}
statesperyear <- statesperyear[-c(200),]
#Sorts data to contain ranks for each year, retains top 10 for each year
formattedstates <- statesperyear %>%
  group_by(year) %>%
  mutate(rank = rank(-Freq),
         Value_rel = Freq/Freq[rank==1],
         Value_lbl = paste0(" ",Freq)) %>%
  group_by(Var1) %>% 
  filter(rank <=10) %>%
  ungroup()
#Due to some similarities in case numbers, we manually alter the positions of ranks
formattedstates[183,"rank"]<- 8
formattedstates[190,"rank"]<- 9
formattedstates[243,"rank"]<- 3
formattedstates[247,"rank"]<- 4
#transition_states() sections data into states by year
#view_follow() then displays data for the specific state
anim <- ggplot(formattedstates, aes(rank, group = Var1, 
                                  fill = as.factor(Var1), color = as.factor(Var1))) +
  geom_tile(aes(y = Freq/2,
                height = Freq,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Var1, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=Freq,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm")) +
  transition_states(year, transition_length = 6, state_length = 4, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Hate Crimes Per State Per Year : {closest_state}',  
       subtitle  =  "Top 10 States with Most Cases Each Year ",
       caption  = "Cases Per Year") 

#Animating the data and producing a gif

animate (anim, 580, fps = 20,  width = 1200, height = 1000, 
        renderer = gifski_renderer("gganim.gif"), end_pause = 15, start_pause =  15) 
