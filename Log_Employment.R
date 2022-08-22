
pkg.list <- c("tidyverse", "data.table", "dplyr", "readxl", "ggrepel", "ggpubr")
library(easypackages)
libraries(pkg.list)

################################# define functions #####################################

###### function to map growth rate vs. log employment size #####
map <- function(data, year, occ, low, high) {
  options(scipen=999)
  ggplot(data, aes(x = Employment, y = Growth, color = Job_Prospects)) + 
    geom_point(alpha = .88, size = 1.5) + 
    scale_color_manual(values=c("#878787", "#DF8E06", "#48A4E3")) +
    scale_x_continuous(trans='log10') +
    geom_label_repel(aes(label= ifelse((Occupation %in% occ) | Growth > high | Growth < low, Occupation,'')),
              size=2.6, max.overlaps=200, label.size=0.12, label.padding=0.15,
              nudge_y=3, direction='both', seed=999,
              min.segment.length=0, show.legend=FALSE, force=50) +
    guides(color = guide_legend("Job Prospects")) +
    labs(x="Employment Size (log10)", y="Projected Employment Change, %", 
         title=year) +
    theme(axis.text.x = element_text(size=10, hjust=0.6), 
          axis.text.y = element_text(size=10),
          plot.title = element_text(size=20, hjust=0.5),
          legend.position="top", legend.box = "horizontal")
}

read <- function(xl, sheet) {
  data <- read_excel(xl, sheet)
  df <- as.data.frame(data)
  # select columns
  df <- select(df, "Input.occupation-title", "Answer.c1", "Answer.e2",
                    "Answer.e2a-1.1", "Answer.e2a-2.2", "Answer.e2a-3.3", 
                    "Answer.e2a-4.4", "Answer.e2a-5.5", "Answer.e2a-6.6",
                    "Answer.e2a-7.7", "Answer.e2a-8.8", "Input.Year")
  df["Answer.c1", "Answer.e2", "Input.Year"] <- lapply(df["Answer.c1", "Answer.e2", "Input.Year"], 
                                               function(x) as.numeric(as.character(x)))
  
  # aggregate boolean columns into one column
  df$Job_Prospect <- case_when(
    df["Answer.e2a-1.1"] == TRUE ~ "Decline",
    df["Answer.e2a-2.2"] == TRUE ~ "Decline",
    df["Answer.e2a-3.3"] == TRUE ~ "Decline",
    df["Answer.e2a-4.4"] == TRUE ~ "Stable",
    df["Answer.e2a-5.5"] == TRUE ~ "Stable",
    df["Answer.e2a-6.6"] == TRUE ~ "Stable",
    df["Answer.e2a-7.7"] == TRUE ~ "Growing",
    df["Answer.e2a-8.8"] == TRUE ~ "Growing"
  ) 
  
  read4 <- function(xl, sheet) {
    data <- read_excel(xl, sheet)
    df <- as.data.frame(data)
    # select columns
    df <- select(df, "Input.occupation-title", "Answer.c1", "Input.Year")
    df["Answer.c1", "Input.Year"] <- lapply(df["Answer.c1",  "Input.Year"], 
                                                         function(x) as.numeric(as.character(x)))
    df <- select(df, "Input.occupation-title", "Answer.c1", "Input.Year")
    colnames(df) <- c("Occupation","Employment", "Year")
    #df <- na.omit(df)
    return(df)
  }
    
  read2 <- function(xl, sheet) {
    data <- read_excel(xl, sheet)
    df <- as.data.frame(data)
    # select columns
    df <- select(df, "Input.occupation-title", "Answer.c1",  "Input.Year")
    df["Answer.c1", "Input.Year"] <- lapply(df["Answer.c1", "Input.Year"], 
                                            function(x) as.numeric(as.character(x)))
    
    df <- select(df, "Input.occupation-title", "Answer.c1", "Input.Year")
    colnames(df) <- c("Occupation","Employment", "Year")
    #df <- na.omit(df)
  return(df)
  }
  
  read3 <- function(xl, sheet) {
    data <- read_excel(xl, sheet)
    df <- as.data.frame(data)
    # select columns
    df <- select(df, "...29", "...40",  "...30")
    df <- df[-c(1),]
    df["...40", "...30"] <- lapply(df["...40", "...30"], 
                                            function(x) as.numeric(as.character(x)))
    
    df <- select(df, "...29", "...40",  "...30")
    colnames(df) <- c("Occupation","Employment", "Year")
    df["Employment"] <- lapply(df["Employment"], 
                                   function(x) as.numeric(as.character(x)))
    df["Year"] <- lapply(df["Year"], 
                               function(x) as.numeric(as.character(x)))
    #df <- na.omit(df)
    #total["Employment"] <- lapply(total["Employment"], function(x) if (is.na(x)) total else x)
    return(df)
  }

  
############################## read data from all years ###############################
## set file directory ##
setwd('/Users/omgitsmonday/Desktop/FinalSpreadsheetRepo-2')

df_1998 = read2("1998_data_Lachanski_Zhao_Lan_final.xlsx", "1998_cleaning")
df_2000 = read3("2000_data_Lan_v2.0.xlsx", "2000_data_Fu")

df_2002 = read2("2002_data_Fu_Lan_0704_final.xlsx", "2002_Cleaning")
df_2004 = read2("2004_data_Zhao_Lan_0704_final.xlsx", "2004_cleaning")
df_2006 = read2("2006_data_Zihan_Lan_0704_final.xlsx", "2006_Cleaning")
df_2008 = read2("2008_data_Zhao_Lan_0704_final.xlsx", "2008_cleaning")

df_2010 = read4("2010_data_Fu.xlsx", "2010_Cleaning")
df_2012 = read4("2012_data_Zhao.xlsx", "2012_Cleaning")
df_2012 = read4("2012_data_Zhao.xlsx", "2012_Cleaning")
df_2014 = read4("2014_data_Fu.xlsx", "2014_Cleaning")
df_2016 = read4("2016_data_Zhao.xlsx", "2016_Cleaning")
df_2016 = read4("2016_data_Zhao.xlsx", "2016_Cleaning")
df_2016 = read4("2016_data_Zhao.xlsx", "2016_Cleaning")
df_2018 = read4("2018_data_Zihan.xlsx", "2018_Cleaning")
df_2020 = read4("2020_data_Zhao.xlsx", "2020_Cleaning")

total <- bind_rows(df_1998, df_2000, df_2002,df_2004,df_2006,df_2008,df_2010,df_2012,df_2014, df_2016, df_2018,df_2020)
total["Employment"] <- lapply(total["Employment"], function(x) as.numeric(as.character(x)))

df <- pivot_wider(total, names_from = Year, values_from = Employment)
df <- as.data.frame(df)
df['2000'] <- lapply(df['2000'], function(x) as.numeric(as.character(x)))
df['2002'] <- lapply(df['2002'], function(x) as.numeric(as.character(x)))
df['2004'] <- lapply(df['2004'], function(x) as.numeric(as.character(x)))
df['2020'] <- lapply(df['2020'], function(x) as.numeric(as.character(x)))
df['2020'] <- lapply(df['2020'], function(x) as.numeric(as.character(x)))
df['2020'] <- lapply(df['2020'], function(x) as.numeric(as.character(x)))
df['2020'] <- lapply(df['2020'], function(x) as.numeric(as.character(x)))

df$diff <- (as.numeric(unlist(df$'2020')) - as.numeric(unlist(df$'2000'))) / as.numeric(unlist(df$'2000'))
top <- df %>% 
  arrange(diff) %>%
  slice(1:10)
total <- filter(total, Occupation %in% top$Occupation)

#total <- group_by(total, Occupation)

#top %>% pivot_longer(c(, y, z), names_to = "key", values_to = "Employment")

#top <- t(top)
#top <- as.data.frame(top)
a <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Computer Programmers' & (Year == '2000'|
                                                                           Year == '2002'|
                                                                           Year == '2004'|
                                                                           Year == '2006'|
                                                                           Year == '2008')), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_line(data=total%>% filter(Occupation == 'Computer Programmers' & (Year == '2012'|
                                                                           Year == '2014'|
                                                                           Year == '2016'|
                                                                           Year == '2018'|
                                                                           Year == '2020')), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_line(data=total%>% filter(Occupation == 'Computer Programmers' & (Year == '2008'|
                                                                           Year == '2012')), 
            linetype = "dashed",
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=1, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Computer Programmers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Computer Programmers (-61%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 


b <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Information Clerks' & Year != '1998'
                                 & Year != '2000'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_path(data=total%>% filter(Occupation == 'Information Clerks' & Year != '1998'
                                 & (Year == '2000' | Year == '2012')), linetype = "dashed",
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.7) +
  geom_point(data = total%>% filter(Occupation == 'Information Clerks' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  #geom_line(data = filter(total%>% filter(Occupation == 'Information Clerks' & Year != '1998' & is.na(Employment)), is.na(Employment)), linetype = "dashed") +
  labs(x="Year", y="Number of Jobs", title='Information Clerks (-22%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

c <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Private Detectives and Investigators' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Private Detectives and Investigators' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1.5), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Private Detectives and Investigators (-46%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

d <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Drafters' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Drafters' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Drafters (-29%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

e <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Dancers and Choreographers' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Dancers and Choreographers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Dancers and Choreographers (-27%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

f <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Podiatrists' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Podiatrists' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Podiatrists (-25%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

g <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Boilermakers' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Boilermakers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Boilermakers (-20%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

h <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Air Traffic Controllers' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Air Traffic Controllers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Air Traffic Controllers (-19%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

i <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Announcers' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Announcers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Announcers (-19%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

j <- ggplot() +
  geom_line(data=total%>% filter(Occupation == 'Line Installers and Repairers' & Year != '1998'), 
            aes(x=Year, y=Employment, group =1), color="#BF0004", size=0.8, alpha=0.9) +
  geom_point(data = total%>% filter(Occupation == 'Line Installers and Repairers' & Year != '1998'), 
             aes(x=Year, y=Employment, group =1), color="#BF0004") +
  labs(x="Year", y="Number of Jobs", title='Line Installers and Repairers (-13%)') +
  theme_light() +
  theme(axis.text.x = element_text(size=10, hjust=0.6), 
        axis.text.y = element_text(size=10),
        plot.title = element_text(size=11, hjust=0.5, face="bold")) 

ggarrange(a, b, c, d, e, f, g, h, i, j,
          labels = c("(1)", "(2)", "(3)", "(4)", "(5)", "(6)", "(7)", "(8)", "(9)", "(10)"),
          ncol = 2, nrow = 5)







total <- total%>% filter(Occupation == 'Information Clerks' & Year != '1998')
x=total$Year
y=total$Employment

plot(y~x , 
     lwd=4 , type="l" , bty="n" , ylab="value of y (decreasing)" , col=rgb(0.2,0.4,0.6,0.8) )

#Add the grey lines
abline(v=seq(0,900,100) , col="grey" , lwd=0.6)

# hardcode long occupations titles
df_2010$Occupation[df_2010$Occupation == 
    "Home Health Aides and Personal and Home Care Aides"] <- 
    "Home Health Aides"

df_2010$Occupation[df_2010$Occupation == 
    "Photographic Process Workers and Processing Machine Operators"] <- 
    "Photographic Process Workers"

df_2010$Occupation[df_2010$Occupation == 
    "Postal Service Mail Sorters, Processes, and Processing Machine Operates"] <- 
    "Mail Sorters and Machine Operators"

df_2010$Occupation[df_2010$Occupation == 
    "Veterinary Technologists and Technicians"] <- 
    "Veterinary Technicians"

df_2010$Occupation[df_2010$Occupation == 
      "Computer Software Engineers and Computer Programmers"] <- 
      "Software Engineers"

df_2010$Occupation[df_2010$Occupation == 
  "Social Scientists, Other"] <- 
  "Social Scientists"

df_2010$Occupation[df_2010$Occupation == 
  "Secretaries and Administrative Assistants"] <- 
  "Secretaries"

df_2012$Occupation[df_2012$Occupation == 
                     "Veterinary Technologists and Technicians"] <- 
  "Veterinary Technicians"

df_2012$Occupation[df_2012$Occupation == 
                     "Home Health Aides and Personal and Home Care Aides"] <- 
  "Home Health Aides"

df_2014$Occupation[df_2014$Occupation == 
                     "Farmers, Ranchers, and Other Agricultural Managers"] <- 
  "Agricultural Workers"

df_2014$Occupation[df_2014$Occupation == 
                     "Reporters, Correspondents, and Broadcast News Analysts"] <- 
  "Reporters and News Analysts"

############################## call functions all years ###############################
#filter(df_2010, (Growth > 0 | Growth < 0))
occ <- c("Computer Operators", "Home Health Aides", "Respiratory Therapy Technicians",
        "Software Engineers", "Social Scientists",  "Sales Worker Supervisors", 
         "Respiratory Therapists", "Software Developers",
        "Medical Scientists", "Respiratory Therapy Technicians", 
        "Agricultural Workers", "Computer Programmers"
        )

map(df_2010, 2010, occ, -27, 38)
map(df_2012, 2012, occ, -10, 45)
#filter(df_2012, (Growth < 0 | Growth > 0))
map(df_2014, 2014, occ, -10, 43)
#filter(df_2014, (Growth < -10 | Growth > 43))
map(df_2016, 2016, occ, -11, 39)
map(df_2018, 2018, occ, -13, 35)
map(df_2020, 2020, occ, -14, 31)
#filter(df_2020, (Growth < -14 | Growth > 31))


topEmployContraction <- function(data) {
  topContraction <- data %>% 
    arrange(asc(data$Growth)) %>%
    slice(1:10)
  ggplot(top, aes(x = Occupation, y = Employment / 1000)) + 
    geom_col() + 
    labs(x="Occupation Name", y="Employment in thousands", title="Top10 Occupations By Largest Employment") +
    theme(axis.text.x = element_text(size=6, angle = 50, hjust=1))
}

################################## issues & questions ################################

