library(tidyverse)

file_number = 1
data_all = data.frame()

for (ratio in c(.8, 1.4, 1.6)) {
  for (cohens_d in c(.15, .7, 1.5)) {
    subject_count_A = 90 + runif(1, -10, 10)
    subject_count_B = 90 + runif(1, -10, 10)
    
    base_RT = 50
    delta = base_RT * (ratio - 1)
    noise = abs(delta) / cohens_d
    
    data_A = 
      tibble(
        subject = 1:subject_count_A,
        tutorial = "A",
        completion_time = 500 + rnorm(subject_count_A, base_RT, noise*0.8)
      )
    
    data_B = 
      tibble(
        subject = 1:subject_count_B,
        tutorial = "B",
        completion_time = 500 + rnorm(subject_count_B, base_RT + delta, noise*1.1)
      )
    
    # merge the datasets
    data = rbind(data_A, data_B)
    
    # shuffle, add subject ID, round completion time
    data = data %>% 
      sample_n(nrow(data)) %>% 
      mutate(subject = 1:n()) %>% 
      mutate(completion_time = round(completion_time)) %>% 
      mutate(experiment = file_number)
    
    if (nrow(data_all) == 0)
      data_all = data
    else
      data_all = rbind(data_all, data)
    
    effsize::cohen.d(completion_time ~ tutorial, data)$conf.int %>% print()
    
    write_csv(data, paste0("simple effect ", file_number, ".csv"))
    file_number = file_number + 1
  }
}

write_csv(data_all, "simple effect all.csv")

ggplot(data_all) +
  aes(x=completion_time, color=tutorial) +
  geom_density() +
  facet_wrap(~experiment, ncol = 3) +
  theme_bw()


read_csv('simple effect 9.csv') %>% effsize::cohen.d(completion_time ~ tutorial, .)