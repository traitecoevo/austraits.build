read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_biomass_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  mutate(root_shoot_ratio = `rootDW (g)`/(`leafDW (g)`+ `stemDW (g)`),
         leaf_area_ratio_cm2_g = `LA (cm-2)`/(`rootDW (g)`+`leafDW (g)`+ `stemDW (g)`),
         specific_leaf_area_cm2_g = `LA (cm-2)`/`leafDW (g)`) %>%
  rename(date_biomass = `Date`) %>%
  mutate(Potnum = as.numeric(Potnum)) -> biomass

read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  subset(Organ == "leaf") %>%
  rename(leaf_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, leaf_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`, 
         date_carbo = `Date`) %>%
  select(-Organ) -> leaf_carbo
  
read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  subset(Organ == "stem") %>%
  rename(stem_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, stem_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`) %>%
  select(-Organ, -Date) -> stem_carbo

read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_carbohydrates_20120327-20121016_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  subset(Organ == "root") %>%
  rename(root_soluble_sugar_mg_per_g = `SolSugW (mg g-1 DW)`, root_soluble_starch_mg_per_g = `StarchW (mg g-1 DW)`) %>%
  select(-Organ, -Date) -> root_carbo

leaf_carbo %>%
  full_join(stem_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  full_join(root_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  mutate(Potnum = as.numeric(Potnum)) -> all_carbo
  
read_csv("data/Duan_2015/raw/GHS30_Pines-TxCxW_GasExchange_20120326-20120808_L1.csv") %>%
  subset(Species == "C. rhomboidea") %>%
  mutate(Potnum = as.numeric(Potnum)) -> gas_exchange

gas_exchange %>%
  full_join(all_carbo,by=c("Species","Temp","CO2","Water","Potnum")) %>%
  full_join(biomass, by=c("Species","Temp","CO2","Water","Potnum")) %>%
  group_by(Potnum) %>%
  arrange(rev(Day)) %>%
  mutate_at(vars(15:31),funs(replace(.,duplicated(.),NA))) %>%
  ungroup() %>%
  mutate(Species = "Callitris rhomboidea") %>%
  mutate_at(c("Photo (umol m-2 s-1)","Rdark (umol m-2 s-1)" ), ~na_if(.,0)) %>%
  mutate(sample_day = if_else(is.na(Day),"",paste("measured_at_day_",Day,sep=""))) %>%
  mutate(context = paste("grown_at_",Temp,"_temp_and_",CO2,"_ppm_CO2_with_",Water,"_treatment",sep="")) %>%
  mutate(context = if_else(sample_day=="",context,paste(context,"_and_",sample_day,sep=""))) %>%
  write_csv("data/Duan_2015/data.csv") -> test

unique(test$context)
[1] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment"                        
[2] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment"                        
[3] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment"                        
[4] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment"                        
[5] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment"                        
[6] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment"                        
[7] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment"                        
[8] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment"                        
[9] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_106"
[10] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_106"
[11] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_106"
[12] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_106"
[13] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_122"
[14] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_122"
[15] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_136"
[16] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_136"
[17] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_86" 
[18] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_93" 
[19] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_93" 
[20] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_93" 
[21] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_93" 
[22] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_93" 
[23] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_93" 
[24] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_93" 
[25] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_93" 
[26] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_106"
[27] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_106"
[28] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_79" 
[29] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_79" 
[30] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_86" 
[31] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_86" 
[32] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_86" 
[33] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_86" 
[34] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_86" 
[35] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_86" 
[36] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_86" 
[37] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_72" 
[38] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_72" 
[39] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_72" 
[40] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_79" 
[41] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_79" 
[42] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_79" 
[43] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_79" 
[44] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_79" 
[45] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_79" 
[46] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_65" 
[47] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_65" 
[48] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_65" 
[49] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_65" 
[50] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_72" 
[51] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_72" 
[52] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_72" 
[53] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_72" 
[54] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_72" 
[55] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_57" 
[56] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_57" 
[57] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_57" 
[58] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_57" 
[59] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_65" 
[60] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_65" 
[61] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_65" 
[62] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_65" 
[63] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_51" 
[64] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_51" 
[65] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_51" 
[66] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_51" 
[67] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_57" 
[68] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_57" 
[69] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_57" 
[70] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_57" 
[71] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_44" 
[72] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_44" 
[73] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_44" 
[74] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_44" 
[75] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_51" 
[76] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_51" 
[77] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_51" 
[78] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_51" 
[79] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_37" 
[80] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_37" 
[81] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_37" 
[82] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_37" 
[83] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_44" 
[84] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_44" 
[85] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_44" 
[86] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_44" 
[87] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_30" 
[88] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_30" 
[89] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_30" 
[90] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_37" 
[91] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_37" 
[92] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_37" 
[93] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_37" 
[94] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_23" 
[95] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_23" 
[96] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_30" 
[97] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_30" 
[98] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_30" 
[99] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_30" 
[100] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_30" 
[101] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_16" 
[102] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_23" 
[103] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_23" 
[104] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_23" 
[105] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_23" 
[106] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_23" 
[107] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_23" 
[108] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_16" 
[109] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_16" 
[110] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_16" 
[111] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_16" 
[112] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_16" 
[113] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_9"  
[114] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_9"  
[115] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_9"  
[116] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_16" 
[117] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_16" 
[118] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_9"  
[119] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_9"  
[120] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_9"  
[121] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_1"  
[122] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_1"  
[123] "grown_at_Elv_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_1"  
[124] "grown_at_Elv_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_1"  
[125] "grown_at_Amb_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_1"  
[126] "grown_at_Amb_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_1"  
[127] "grown_at_Elv_temp_and_640_ppm_CO2_with_Wet_treatment_and_measured_at_day_1"  
[128] "grown_at_Elv_temp_and_400_ppm_CO2_with_Wet_treatment_and_measured_at_day_1"  
[129] "grown_at_Amb_temp_and_400_ppm_CO2_with_Dry_treatment_and_measured_at_day_9"  
[130] "grown_at_Amb_temp_and_640_ppm_CO2_with_Dry_treatment_and_measured_at_day_9"  




#questions
#In the carbohydrates data file, there are two entries for leaf values for Potnum 356 - should one of these possibly be a different pot number?
  