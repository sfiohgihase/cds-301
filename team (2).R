library("rjson")
library(tidyverse)
library(ggplot2)
library(sf)
library(ggmap)
library(readxl)
library(rgdal)
library(maptools)
library(viridis)
library(rayshader)
library(RColorBrewer)
library(tidyverse)
library(gganimate)
library(gifski)
 
# install.packages("devtools")
# install.packages("ggmap")
# install.packages("rgdal")

############################# ##1

# prepare map <- basic map polygon
seoul1 = readOGR("sig.shp", encoding = 'cp949')
seoul2 = st_as_sf(seoul1)
seoul2$SIG_CD<-as.numeric(seoul2$SIG_CD)
seoulf <- subset(seoul2, seoul2$SIG_CD < 12000)

# prepare dataset
df_seoul <- read.csv("C:\\Users\\moon chung Sook\\Documents\\카카오톡 받은 파일\\서울시 부동산 실거래가 정보 2021.csv", header = T, fileEncoding = "euc-kr")
df_seoul_price <- df_seoul %>% mutate(price_m2 = 물건금액.만원. / 건물면적... )
df_gu <- df_seoul_price %>% 
  group_by(자치구명) %>% 
  summarise(average_price = mean(price_m2), 자치구코드) %>%
  arrange(desc(average_price))
df_gu <- unique(df_gu)
df_gu <- df_gu %>% rename(SIG_CD = 자치구코드)

# merge (data+geo)
mergeseoul <- merge(df_gu, seoulf, by = "SIG_CD")
mergeseoulf <- st_as_sf(mergeseoul)



#### choropleth to 3D
map3d = ggplot(mergeseoulf) +
  geom_sf(aes(fill = average_price)) +
  ylab("latitude") + xlab("longtitude") +
  scale_fill_viridis("Average Price") +
  ggtitle("Average Price of real estate per Gu in 2021") +
  theme_bw()+
  geom_sf_text(mapping = aes(label = 자치구명), color = "black", size = 3)

map3d

plot_gg(map3d, multicore = TRUE, width = 6 ,height=6, zoom = 0.6)


###########################################
## create gu list
allgu <- c("강남", "강동", "강북", "강서", "관악","광진", "구로","금천",
           "노원", "도봉", "동대문", "동작", "마포", "서대문", "서초", "성동",
           "성북", "송파", "양천", "영등포", "용산", "은평" ,"종로" , "중구", "중랑")

## create rank data
강남 <- c(01,01,01,01)
강동<- c(08,10,10,10)
강북<-c(24,24,25,25)
강서<- c(16,19,18,18)
관악<- c(19,18,19,19)
광진<- c(11,11,11,14)
구로<-c(21,20,21,21)
금천<-c(23,23,22,23)
노원<- c(18,17,17,12)
도봉<-c(25,25,24,24)
동대문<- c(15,14,12,11)
동작<- c(09,08,08,07)
마포<- c(06,06,07,06)
서대문<- c(14,15,13,16)
서초<- c(02,02,02,02)
성동<- c(05,05,04,04)
성북<- c(17,16,15,15)
송파<- c(04,04,05,05)
양천<- c(13,13,16,17)
영등포<- c(10,09,09,09)
용산<- c(03,03,03,03)
은평<- c(20,21,20,22)
종로<- c(12,12,14,13)
중구<- c(07,07,06,08)
중랑<-c(22,22,23,20)



## Merge rank data in one vector
rankgu <- c(강남, 강동, 강북,강서,관악,광진,구로,금천,노원,도봉,동대문,
            동작,마포,서대문,서초,성동,성북,송파,양천,영등포,용산,은평,종로,중구,중랑)

## create data frame for gg animation
dfgu <- data.frame(gu = rep(allgu, 4), 
                   year = rep(2018:2021, each = 25), 
                   pts = c(rep(NA, 25*3),
                           c(25, 16, 1, 8, 7, 12, 5, 3, 14, 2, 15, 19,
                             20, 10, 24, 22, 11, 21, 9, 17, 23, 4, 13, 18, 6)),
                   stringsAsFactors = FALSE) %>%
  arrange(gu, year) %>%
  mutate(rank = as.integer(rankgu)) %>% 
  arrange(year, rank, gu) 


## visualization by adding the transition function and control it by animate function
gu_animation <- ggplot(data = dfgu, 
                       aes(x = year, y = rank, group = gu, col = gu)) +
  scale_y_continuous(trans = "reverse", breaks = 1:25) + 
  scale_x_continuous(breaks = 1:4, expand = c(0, 2)) +
  xlim(2018,2021.5) +
  labs(x = "year", y = element_blank(),
       title = "rank of average price of real estate 2018-2021",
       subtitle = "year: {frame_along}") + 
  geom_line() +
  geom_segment(aes(xend = 29, yend = rank),
               linetype = 2, col = "gray") +
  geom_point(size = 2) +
  geom_text(aes(x = 2021.1, label = gu), col = 'black') +
  geom_text(aes(x = 2022.1, label = pts), col = 'black') +
  transition_reveal(along = year) + # Main point
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major.y = element_line(color = c("#F0F0F0")),
        panel.grid.major.x = element_line(color = c("#F0F0F0")),
        axis.ticks = element_line(color = c("#F0F0F0")),
        legend.position = "none",
        plot.subtitle = element_text(size = 15))


animate(plot = gu_animation, nframes = 160, end_pause = 20,
        width = 600, height = 600) 

## save and export the gif
anim_save(filename = "gu_animation.gif",
          animation = gu_animation,
          nframes = 160, end_pause = 20, 
          width = 600, height = 600, # control size
          renderer = gifski_renderer(loop = FALSE)) 

###########################################
# data processing
seoul_est <- read.csv("C:\\Users\\moon chung Sook\\Documents\\카카오톡 받은 파일\\team\\seoulest.csv", header = T, fileEncoding = "euc-kr")
seoul_est_price <- seoul_est %>% mutate(price_m2 = 물건금액.만원. / 건물면적... )
seoul_est_cut <- seoul_est_price %>% select(접수연도, 자치구명, price_m2)
seoul_est_filter <- seoul_est_cut %>% 
  group_by(자치구명, 접수연도) %>% 
  summarise(average_price = mean(price_m2)) %>%
  arrange(desc(average_price))
seoul_est_filter <- unique(seoul_est_filter)




# pivot_wider to make dataset for bar plot
df_wide <- pivot_wider(seoul_est_filter,
                       names_from = '접수연도',
                       values_from = average_price)
df_wide <- rename(df_wide, "drop" = "2017")
df_wide <- subset(df_wide, select=-drop)
df_final <- df_wide %>% select(자치구명, "2018", "2019", "2020", "2021", "2022") 
df_top <- df_final %>% filter(자치구명 %in% c("강남구", "서초구", "용산구", "성동구", "송파구"))
df_topf <- subset(df_top, select=-자치구명) 



# prepare for the bar plot
bar_temp <-data.frame(df_topf)
bar_final <- as.matrix(bar_temp)
color <- brewer.pal(5, "Pastel1")
barplot(bar_final, beside = T, col = color, names = c("2018", "2019", "2020", "2021", "2022"),
        main = "Housing Price Fluctuations in Five Gu in Seoul \n over the Past Five Years",
        xlab = "year", ylab = "price (in ten thousand won per m^2)")
legend("topleft", legend = c("강남구", "서초구", "용산구", "성동구", "송파구"), fill = color,
       cex = 1.1)

