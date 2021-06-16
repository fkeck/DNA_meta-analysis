library(tidyverse)
library(tidytext)
library(ggwordcloud)

zot <- read_csv("/home/ecoadmin/Documents/temp/full_text_html.csv")

# saved the full text in the Notes column
ft <- zot$Notes

# Full text is in html so I need to remove the tags
ft <- htm2txt::htm2txt(ft)


# Extract 10 words before and after the target expression
res <- str_extract_all(ft, "(\\w+ ){0,10}((R|r)eference databas(e|es))|((R|r)eference sequenc(e|es))( \\w+){0,10}")

res <- unlist(res)
res <- str_remove_all(res, "((R|r)eference databas(e|es))|((R|r)eference sequenc(e|es))")
res <- paste(res, collapse = " ")


tibble(txt = res) %>%
  unnest_tokens(word, txt) %>%
  anti_join(stop_words) %>% # Remove stop words
  inner_join(get_sentiments("bing")) %>% # Inner join with a reference list of sentiment-associated words
  group_by(word) %>% 
  mutate(n = length(word)) %>%
  distinct()  %>% 
  ggplot() +
  geom_text_wordcloud_area(aes(label = word,
                               size = n,
                               color = sentiment)) +
  scale_size_area(max_size = 24) +
  theme_minimal()




