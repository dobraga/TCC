library(tidyverse)
library(recipes)
library(magrittr)

# Bairros ----
bairros = readRDS('../output/bairros.rds')

# Testando quantidade de clusters ====
wss = map_dbl(
  1:10,
  function(x){
    kmeans(bairros %>% select(-c(zona, subprefeitura, bairro)), x)$tot.withinss
  }
) %>%
  tibble(Withinss=.) %>%
  ggplot() +
  geom_point(aes(x=1:10, y=Withinss)) +
  geom_line(aes(x=1:10, y=Withinss))+
  scale_x_continuous(breaks = 1:10)+
  labs(x='Quantidade de Clusters', y='Variação Total Dentro do Cluster')

km = kmeans(bairros %>% select(-c(zona, subprefeitura, bairro)), 4)

km$centers %<>%
  as_tibble() %>%
  mutate(cluster_origin = row_number()) %>%
  arrange(desc(idh)) %>%
  mutate(cluster = row_number())

bairros$cluster = tibble(cluster_origin = km$cluster) %>%
  left_join(
    km$centers %>% 
    select(contains('cluster')), by = "cluster_origin"
  ) %>% .$cluster

# saveRDS(bairros, '../output/bairros_fim.rds')

# Lendo e Transformando  ----
df = read_csv("../input/listings.csv", col_types = cols())
df %<>% inner_join(bairros, by = c("neighbourhood_cleansed"="bairro"))

ajusta_numero = function(string){
  string %>% str_remove_all('\\$|\\,')  %>% as.double()
}

# df %>% 
#   select(amenities) %>% 
#   tidyr::separate_rows(amenities, sep = ",") %>% 
#   mutate(
#     amenities = stringr::str_replace_all(amenities,'\\"|\\{|\\}|\\\\', "")
#   ) %>% 
#   distinct() %>% View()
# 
# df %>% transmute(
#   qtd_verificacoes = str_count(host_verifications, ",") + 1
# ) %>% 
#   ggplot(aes(x=qtd_verificacoes)) + geom_histogram()
# 
# df %>% transmute(
#   qtd_facilidade = str_count(amenities, ",") + 1,
# ) %>% 
#   ggplot(aes(x=qtd_facilidade)) + geom_histogram()


df = recipe(price~ ., data = df) %>% 
  step_zv(all_predictors()) %>% 
  step_rm(
    last_scraped, name, summary, space, neighborhood_overview, notes, 
    access, interaction, house_rules, picture_url, host_name, host_since, 
    host_location, host_about, host_response_rate, host_acceptance_rate, 
    host_thumbnail_url, host_picture_url, host_listings_count, 
    host_total_listings_count, neighbourhood, city, state, zipcode, 
    market, smart_location, bed_type, square_feet, weekly_price, 
    monthly_price, security_deposit, cleaning_fee, 
    minimum_minimum_nights, maximum_minimum_nights, 
    minimum_maximum_nights, maximum_maximum_nights, 
    minimum_nights_avg_ntm, maximum_nights_avg_ntm, calendar_updated, 
    availability_30, availability_60, availability_90, availability_365, 
    calendar_last_scraped, number_of_reviews, number_of_reviews_ltm, 
    first_review, last_review, review_scores_rating, 
    review_scores_accuracy, review_scores_cleanliness, 
    review_scores_checkin, review_scores_communication, 
    review_scores_location, review_scores_value, 
    require_guest_profile_picture, require_guest_phone_verification, 
    calculated_host_listings_count, 
    calculated_host_listings_count_entire_homes, 
    calculated_host_listings_count_private_rooms, is_location_exact,
    calculated_host_listings_count_shared_rooms, reviews_per_month,
    instant_bookable, street, host_response_time, host_neighbourhood
  ) %>% 
  step_mutate(
    bathrooms = coalesce(bathrooms, 0), 
    bedrooms = coalesce(bedrooms, 0), 
    beds = coalesce(beds, 0),
    price = ajusta_numero(price),
    extra_people = ajusta_numero(extra_people),
    guests_included = if_else(
      guests_included>=accommodates, accommodates, guests_included
    ),
    extra_people = if_else(guests_included>=accommodates, 0, extra_people),
    fl_extra_people = if_else(extra_people>0, 1, 0),
    host_is_superhost = coalesce(host_is_superhost, FALSE),
    host_has_profile_pic = coalesce(host_has_profile_pic, FALSE),
    host_identity_verified = coalesce(host_identity_verified, FALSE),
    cluster_4 = if_else(cluster == 4, 1, 0),
    cancellation_policy = as.character(cancellation_policy),
    cancellation_policy = case_when(
      grepl("strict", cancellation_policy) ~ "strict",
      TRUE ~ cancellation_policy
    ),
    
    # fl_tv = as.numeric(as.numeric(grepl("TV", amenities))),
    # fl_wifi = as.numeric(as.numeric(grepl("Wifi", amenities))),
    # fl_porteiro = as.numeric(as.numeric(grepl("Doorman", amenities))),
    # fl_elevador = as.numeric(as.numeric(grepl("Elevator", amenities))),
    # fl_jacuzzi = as.numeric(as.numeric(grepl("Hot tub", amenities))),
    # fl_permitido_fumar = as.numeric(as.numeric(grepl("Smoking allowed", amenities))),
    # fl_ar_condicionado = as.numeric(as.numeric(grepl("Air conditioning", amenities))),
    # fl_piscina = as.numeric(grepl("(Pool)|(Private pool)|(Shared pool)", amenities)),
    # fl_piscina_privada = as.numeric(grepl("Private pool", amenities)),
    # fl_vista_praia = as.numeric(grepl("Beach view", amenities)),
    # fl_aceita_pets = as.numeric(grepl("Pets allowed", amenities)),
    # fl_academia = as.numeric(grepl("Gym|gym", amenities)),
    # qtd_facilidade = str_count(amenities, ",") + 1,
    # qtd_verificacoes = str_count(host_verifications, ",") + 1
    
  ) %>% 
  step_rm(
    extra_people
  ) %>% 
  add_role(
    id, new_role = 'ID'
  ) %>% 
  step_filter(
    price>0
  ) %>% 
  step_rm(
    listing_url, description, transit, host_id, host_url, 
    host_verifications, amenities
  ) %>% 
  step_mutate_at(
    host_is_superhost, host_has_profile_pic, host_identity_verified,
    fn = ~if_else(.,1,0)
  ) %>% 
  step_other(
    property_type, other = "Other"
  ) %>% 
  prep() %>% juice() %>% 
  filter(
    price <= 1384
  )



# Tipo de propriedade ----

map_dbl(
  1:10,
  function(x){
    kmeans(df %>% select(bedrooms, bathrooms, accommodates), x)$tot.withinss
  }
) %>%
  tibble(Withinss=.) %>%
  ggplot() +
  geom_point(aes(x=1:10, y=Withinss)) +
  geom_line(aes(x=1:10, y=Withinss))+
  scale_x_continuous(breaks = 1:10)+
  labs(x='Quantidade de Clusters', y='Variação Total Dentro do Cluster')


km = kmeans(df %>% select(bedrooms, bathrooms, accommodates), 6)

km$centers %<>%
  as_tibble() %>%
  mutate(cluster_origin = row_number()) %>%
  arrange(desc(accommodates)) %>%
  mutate(cluster = row_number())

df$cluster_property = tibble(cluster_origin = km$cluster) %>%
  left_join(
    km$centers %>% 
      select(contains('cluster')), by = "cluster_origin"
  ) %>% .$cluster


sum_property = df %>% 
  group_by(cluster_property) %>% 
  summarise(median_property = median(price))


# df %<>% left_join(sum_property) não serviu muito


# Vizinhos ----
dist = function(x1,x2,y1,y2){
  sqrt((x1-x2)^2+(y1-y2)^2)
}

df_latlong = df %>% 
  select(
    id, cluster, neighbourhood_cleansed, latitude, 
    longitude, price, bedrooms, beds, bathrooms
  )
# saveRDS(df_latlong, './output/df_latlong.rds')

take_near = function(id_, qtd){
  aux = df_latlong %>% filter(id==id_)
  
  df_latlong %>% 
    rename_all(~paste0(.,'.y')) %>% 
    filter(id.y!=aux$id, cluster.y==aux$cluster) %>% 
    select(-cluster.y, -neighbourhood_cleansed.y) %>% 
    mutate(
      id=aux$id, latitude=aux$latitude, longitude=aux$longitude, 
      dist = dist(latitude,latitude.y,longitude,longitude.y)
    ) %>% 
    top_n(qtd, wt = desc(dist)) %>% 
    select(-dist) %>% 
    group_by(id, latitude, longitude) %>% 
    nest(
      data = c(id.y, latitude.y, longitude.y, 
        price.y, bedrooms.y, beds.y, bathrooms.y)
      )
}

# library(furrr)
# future::plan(multisession)
# df_near = future_map_dfr(
#   .x=df_latlong$id, 
#   .f=function(x){take_near(x,15)}, 
#   .progress = TRUE
#   ) %>% ungroup()
# saveRDS(df_near, '../output/near2.rds')

# Testando Vizinhos ====

df_near = readRDS('../output/near2.rds')

df %>% filter(neighbourhood_cleansed=='Vaz Lobo') %>% .$id
df %>% filter(neighbourhood_cleansed=='Campo Grande') %>% .$id

(a = df_near %>% filter(id==87202) %>% unnest(data))

map = sf::read_sf("../output/neighbourhoods.geojson") %>% 
  select(-neighbourhood_group)

ggplot() +
  geom_sf(data=map) +
  geom_point(data=a, aes(x=longitude,y=latitude), color='red') +
  geom_point(data=a, aes(x=longitude.y,y=latitude.y), color='blue')

# Finalizando transformação dos vizinhos ====

(df_near_mean = df_near %>% 
  select(-latitude, -longitude) %>% 
  unnest(data) %>% 
  select(-id.y, -latitude.y, -longitude.y) %>% 
  group_by(id) %>% 
  summarise_all(median) %>% 
  rename_all(~str_replace(., '\\.y', '_near'))
)

df %<>% left_join(df_near_mean, by='id')

df %<>% mutate(
  fl_mais_banheiros = if_else(bathrooms >= bathrooms_near, 1, 0),
  fl_mais_quartos = if_else(bedrooms >= bedrooms_near, 1, 0),
  fl_mais_camas = if_else(beds >= beds_near, 1, 0)
)

# saveRDS(df, '../output/df.rds')
