#### Summary tables
df_reg <- df_nw%>%
  select(-id_row)%>%
  group_by(Treatment)%>%
  summarise(across(c("PLT":"HCT"),
                   list(~mean(.x, na.rm = T),
                        ~plotrix::std.error(.x, na.rm = T))))%>%
  mutate(across(where(is.numeric), ~round(.x, 2)))%>%
  unite("PLT", starts_with("PLT"), sep = "±")%>%
  unite("HGB", starts_with("HGB"), sep = "±")%>%
  unite("MCH", c("MCH_1", "MCH_2"), sep = "±")%>%
  unite("MCV", starts_with("MCV"), sep = "±")%>%
  unite("PCT", starts_with("PCT"), sep = "±")%>%
  unite("RBC", starts_with("RBC"), sep = "±")%>%
  unite("RDW-CV (%)", starts_with("RDW-CV (%)"), sep = "±")%>%
  unite("RDW-SD", starts_with("RDW-SD"), sep = "±")%>%
  unite("WBC", starts_with("WBC"), sep = "±")%>%
  unite("MCHC", starts_with("MCHC"), sep = "±")%>%
  unite("HCT", starts_with("HCT"), sep = "±")


#### ANOVA Results
df <- df%>%
  select(-c(sex, `egg_stage`))

formulae <- lapply(colnames(df)[-1],
                   function(x) as.formula(paste0(x, "~ month")))
res <- lapply(formulae, function(x) summary.aov(aov(x, data = df)))
names(res) <- format(formulae)
res

#### ANOVA Tables
# df%>%
#   rstatix::anova_test(tl~month*sex)
library(agricolae)  

bw_c <- plot(HSD.test(aov(bw~month, data = df),"month"))

df_reg$wg <- c("bc","b","a","bc","bc","c")


gw_c <- plot(HSD.test(aov(gw~month, data = df),"month"))
df_reg$g <- c("ab","ab","a","bc","b","c")

sl_c <- plot(HSD.test(aov(sl~month, data = df),"month",alpha = 0.05))
df_reg$sg <- c("c","b","a","c","c","c")

tl_c <- plot(HSD.test(aov(tl~month, data = df),"month",alpha = 0.05))
df_reg$tg <- c("bc","b","a","c","bc","c")

#### Final Analysis Table
df_reg%>%
  unite("PLT", starts_with("PLT"), sep = "")%>%
  unite("HGB", starts_with("HGB", ""), sep = "")%>%
  unite("MCH", c("MCH_1", "MCH_2"), sep = "")%>%
  unite("MCV", starts_with("MCV"), sep = "")%>%
  unite("PCT", starts_with("PCT"), sep = "")%>%
  unite("RBC", starts_with("RBC"), sep = "")%>%
  unite("RDW-CV (%)", starts_with("RDW-CV (%)"), sep = "")%>%
  unite("RDW-SD", starts_with("RDW-SD"), sep = "")%>%
  unite("WBC", starts_with("WBC"), sep = "")%>%
  unite("MCHC", starts_with("MCHC"), sep = "")%>%
  unite("HCT", starts_with("HCT"), sep = "")%>%
  write.csv(., "Final table for parameters.csv", row.names = F)

