library(tidyverse)
library(taylor)
library(ggbump)
library(ggridges)
library(gt)
library(gtExtras)

View(taylor_album_songs)

df <- taylor_album_songs %>%
  group_by(album_name, album_release, track_number, track_name) %>%
  select(where(is.numeric), -where(is.integer))

df_pivot <- df %>%
  pivot_longer(cols = !c(album_name, album_release, track_number, track_name),
               names_to = 'param',
               values_to = 'value') %>%
  ungroup() %>%
  filter(!is.na(value) & param != 'instrumentalness') %>%
  mutate(across(c(album_name, param), as.factor),
         across(value, ~round(., 5))
         )

df_pivot_summary <- df_pivot %>%
  summarise(avg = mean(value),
            .by = c(album_name, album_release, param)
            ) 
# Plots ----
cover_links <- c(
  "Taylor Swift" = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-self-titled-billboard-1240.jpg?w=768",
  "Speak Now" = "https://www.billboard.com/wp-content/uploads/2022/06/taylor-swift-speak-now-billboard-1240.jpg?w=768",
  "1989" = "https://www.billboard.com/wp-content/uploads/2015/06/taylor-swift-1989-album-billboard-1548.jpg?w=768",
  "reputation" = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-reputation-billboard-1240.jpg?w=1024",
  "Lover" = "https://www.billboard.com/wp-content/uploads/media/Taylor-Swift-Lover-album-art-2019-billboard-1240.jpg?w=768",
  "folklore" = "https://www.billboard.com/wp-content/uploads/2020/12/Taylor-swift-folklore-cover-billboard-1240-1607121703.jpg?w=768",
  "evermore" = "https://www.billboard.com/wp-content/uploads/2020/12/taylor-swift-cover-2020-billboard-1240-1607612466.jpg?w=768",
  "Fearless (Taylor's Version)" = "https://www.billboard.com/wp-content/uploads/2021/04/Taylor-Swift-fearless-album-art-cr-Beth-Garrabrant-billboard-1240-1617974663.jpg?w=768",
  "Red (Taylor's Version)" = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-red-taylors-version-billboard-1240.jpg?w=768",
  "Midnights" = "https://www.billboard.com/wp-content/uploads/2022/10/taylor-swift-midnights-album-cover-2022-billboard-1240.jpg?w=768"
)


p <-
  ggplot(df_pivot,
         aes(
           y = fct_reorder(album_name, album_release),
           x = value,
           fill = album_name
         )) +
  geom_density_ridges_gradient(alpha = 0.5,
                               quantile_lines = TRUE,
                               quantiles = 2) +
  scale_fill_albums() +
  theme_minimal() +
  facet_wrap(vars(param),
             ncol = 3,
             scales = 'free_x')

p

