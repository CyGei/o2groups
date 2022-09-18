library(tidyverse)
library(purrr)


#data
df <- tibble(dates = sample(seq(as.Date('2020/01/01'), as.Date('2020/06/01'), by="day"), 100),
             group = rep(LETTERS[1:10], each = 10 ),
             individual_id = rep(1:10, length.out = 100))

#list
list_of_df <- split(df, f = df$group)
list_of_df

#vectorise columns within list:
test <- df %>% 
  split(., f = .$group) %>% 
  map(.x = .,
      .f = ~c(.x))
test


#outbreaker_data() for each list element
o2_groups_data <- function(input, 
                           dates,
                           dna, 
                           ctd,
                           w_dens,
                           f_dens,
                           workers){
  
  #future settings
  require(future.apply)
  oplan <- plan(multisession, workers = workers)
  on.exit(plan(oplan))
  
  #loop
  future_lapply(input, 
         function(x){
           outbreaker_data(dates = x[[dates]],
                           w_dens = w_dens, 
                           f_dens = f_dens,
                           dna = dna,
                           ctd = ctd)
         })
}

dat <- o2_groups_data(input = test,
                dates = "dates",
                dna = NULL, 
                ctd = NULL,
                w_dens = fake_outbreak$w,
                f_dens = fake_outbreak$f,
                workers = 3)
dat 



#outbreaker() for each list element;
o2_groups <- function( input,
                       data = outbreaker_data(),
                       config = create_config(),
                       priors = custom_priors(),
                       likelihoods = custom_likelihoods(),
                       moves = custom_moves(),
                       workers){
  
  #future settings
  require(future.apply)
  oplan <- plan(multisession, workers = workers)
  on.exit(plan(oplan))
  
  #loop
  future_lapply(input, 
                function(x){
                  outbreaker(data = x,
                             config = create_config(),
                             priors = custom_priors(),
                             likelihoods = custom_likelihoods(),
                             moves = custom_moves())
                },
                future.seed = TRUE)
}

availableCores()
res <- o2_groups(input = dat,
                 workers = 10)
