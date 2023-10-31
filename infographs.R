library(tidyverse)
library(taylor)
library(ggridges)
library(spotifyr)
# library(gt)
# library(gtExtras)

# Spotify API credentials -------------------------------------------------
Sys.setenv(SPOTIFY_CLIENT_ID = 'b529464265704c7e98966a15941616ef')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '3b891564daa54b32829db99a97472f2d')

access_token <- get_spotify_access_token()


tswift <- get_artist_audio_features('taylor swift')

# Albums ----
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

covers <- image_read(cover_links)

albums_covers <- taylor_albums %>%
  mutate(art = cover_links[album_name]) %>%
  filter(ep==F & !is.na(art)) %>%
  mutate(art = paste('<img src=',art, 'width="20" />'))
  
alffd <- paste('<img src=',cover_links, 'width="20" />')

# Songs ----
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
             nrow = 1,
             scales = 'free_x') +
  scale_y_discrete(labels = function(.x)
    paste0(as.character(.x), "<br>",
           image_read(alffd[.x]))
  )

p
