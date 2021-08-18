library(readr)
library(dplyr)
library(ggplot2)
library(zoo)
library(tidyr)
library(tibble)
library(here)

here()

df = read_csv('polls/polls.csv') %>%
  rename(left=`DIE LINKE`, afd=`AfD`, cdu=`CDU/CSU`, fdp=FDP, spd=SPD, greens=`GRÜNE`, other=Sonstige) %>%
  mutate(id_=paste(published_date, source_name, sep="_")) %>%
  mutate(start_date=as.Date(start_date),
         end_date=as.Date(end_date),
         published_date=as.Date(published_date))

calc_var = function(p, n) {
  return((p * (1 - p)) / n)
}

df = df %>%
  mutate(cdu = cdu / 100,
         spd = spd / 100,
         fdp = fdp / 100,
         greens = greens / 100,
         left = left / 100,
         afd = afd / 100,
         other = other / 100) %>%
  mutate(cdu_var=calc_var(cdu, sample_size),
         spd_var=calc_var(spd, sample_size),
         fdp_var=calc_var(fdp, sample_size),
         greens_var=calc_var(greens, sample_size),
         left_var=calc_var(left, sample_size),
         afd_var=calc_var(afd, sample_size),
         other_var=calc_var(other, sample_size))

ivw_mean = function(y, var, weight=1) {
  num = sum((y * weight) / var)
  den = sum(weight / var)
  return(num / den)
}

ivw_var = function(var, weight=1) {
  num = 1
  den = sum(weight / var)
  return(num / den)
}

n_days = 7
first_date = min(df$published_date) + n_days
current_date = first_date
end_date = Sys.Date()

df_ivw = tibble(
  date = as.Date(NA),
  party = as.character(NA),
  mean = numeric(),
  var = numeric()
)

while(current_date <= end_date) {
  
  df_current_date = df %>%
    filter(published_date < current_date & published_date > current_date - n_days) %>%
    mutate(weight = as.double((1 - (current_date - published_date) / n_days))) %>%
    mutate(weight= weight / sum(weight))
  
  if (nrow(df_current_date) == 0) {
    df_ivw = df_ivw %>%
      bind_rows(df_ivw %>% filter(date==current_date - 1) %>% mutate(date=current_date))
  } else {
    
    cdu_mean_ivw = ivw_mean(df_current_date$cdu, df_current_date$cdu_var, df_current_date$weight)
    cdu_var_ivw = ivw_var(df_current_date$cdu_var, df_current_date$weight)
    
    spd_mean_ivw = ivw_mean(df_current_date$spd, df_current_date$spd_var, df_current_date$weight)
    spd_var_ivw = ivw_var(df_current_date$spd_var, df_current_date$weight)
    
    fdp_mean_ivw = ivw_mean(df_current_date$fdp, df_current_date$fdp_var, df_current_date$weight)
    fdp_var_ivw = ivw_var(df_current_date$fdp_var, df_current_date$weight)
    
    greens_mean_ivw = ivw_mean(df_current_date$greens, df_current_date$greens_var, df_current_date$weight)
    greens_var_ivw = ivw_var(df_current_date$greens_var, df_current_date$weight)
    
    left_mean_ivw = ivw_mean(df_current_date$left, df_current_date$left_var, df_current_date$weight)
    left_var_ivw = ivw_var(df_current_date$left_var, df_current_date$weight)
    
    afd_mean_ivw = ivw_mean(df_current_date$afd, df_current_date$afd_var, df_current_date$weight)
    afd_var_ivw = ivw_var(df_current_date$afd_var, df_current_date$weight)
    
    other_mean_ivw = ivw_mean(df_current_date$other, df_current_date$other_var, df_current_date$weight)
    other_var_ivw = ivw_var(df_current_date$other_var, df_current_date$weight)
    
    df_ivw = df_ivw %>% 
      add_row(date=current_date, party='cdu', mean=cdu_mean_ivw, var=cdu_var_ivw) %>%
      add_row(date=current_date, party='spd', mean=spd_mean_ivw, var=spd_var_ivw) %>%
      add_row(date=current_date, party='fdp', mean=fdp_mean_ivw, var=fdp_var_ivw) %>%
      add_row(date=current_date, party='greens', mean=greens_mean_ivw, var=greens_var_ivw) %>%
      add_row(date=current_date, party='left', mean=left_mean_ivw, var=left_var_ivw) %>%
      add_row(date=current_date, party='afd', mean=afd_mean_ivw, var=afd_var_ivw) %>%
      add_row(date=current_date, party='other', mean=other_mean_ivw, var=other_var_ivw)
    
  }

   current_date = as.Date(current_date + 1)
}

df_ivw_final = df_ivw %>% 
  mutate(
    error = qnorm(0.975) * sqrt(var),
    low = mean - error,
    high = mean + error) %>%
  mutate(party = factor(party, levels=c("cdu", "spd", "afd", "fdp", "left", "greens", "other"))) %>%
  filter(party != 'other') %>%
  group_by(party) %>% 
  summarize(mean=smooth.spline(mean, spar=0.6)$y, 
            low=smooth.spline(low, spar=0.6)$y, 
            high=smooth.spline(high, spar=0.6)$y, 
            date=date,
            .groups='drop')

df_polls_raw = df %>%
  mutate(date=published_date) %>%
  select(date, cdu, spd, afd, fdp, left, greens) %>%
  pivot_longer(!date, names_to='party', values_to='mean') %>%
  mutate(party = factor(party, levels=c("cdu", "spd", "afd", "fdp", "left", "greens", "other"))) %>%
  filter(date > first_date) %>%
  filter(party != "other")
  
party_colors = c(afd='#00ADEF', left='#C72763', fdp='#FAD71D', greens='#0A8A00', cdu='#333333', spd='#F50000', other='grey')
party_labels = c("CDU/CSU", "SPD", "AfD", "FDP", "The Left", "The Greens")
p = ggplot(data=df_ivw_final, aes(x=date, y=mean, group=party, color=party)) +
  geom_line()+
  geom_ribbon(aes(ymin=low, ymax=high, fill=party), alpha=0.2, linetype="blank") +
  geom_point(data=df_polls_raw, alpha=0.4) +
  scale_colour_manual(values=party_colors, labels=party_labels) +
  scale_fill_manual(values=party_colors, labels=party_labels) +
  theme_minimal() +
  labs(title=sprintf("Polling Average Germany - %s", end_date)) +
  xlab(label="date") +
  ylab(label="percent")
p

ggsave(sprintf('graphics/polling_avg_ivw_%s.png', end_date), p, width=1200, height=900, units='px', dpi=72, bg='white')
