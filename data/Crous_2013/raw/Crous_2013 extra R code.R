read_csv("data/Crous_2013/raw/Globulus_Austraits.csv") %>%
  mutate(context = (paste(`Temp-trt`,"_temp_and_",Treatment,"_CO2_treatment_and_measured_in_",month,sep=""))) %>%
  write_csv("data/Crous_2013/data.csv")

read_csv("data/Crous_2013/data.csv") %>%
  distinct(.,context, .keep_all = TRUE) %>%
  select(context,`Temp-trt`,Treatment,month) %>%
  write_csv("data/Crous_2013/raw/context_to_edit.csv")

read_csv("data/Crous_2013/raw/context_to_edit.csv") -> context_table

