read_csv("data/Hayes_2018/Hayes_et_al._2018_site.data.csv") %>%
  select(`site.id`,`date`,`location detail`,`GPS coords`) %>%
  group_by(`site.id`) %>%
  summarise_all(.funs = mean) -> sites_char_info


read_csv("data/Hayes_2018/Hayes_et_al._2018_site.data.csv") %>%
select(-`sample #`,-`sample detail`,-`species sampled`,-`replicate #`,-`country`,-`laboratory`) %>%
group_by(`site.id`) %>%
  left_join(sites_char_info,by="sites_char_info") %>%
  summarise_all(.funs = mean) -> site_data 




View(site_data)

group_by()
-> site_data


111