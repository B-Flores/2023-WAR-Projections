


chadwick_player_lu_updated <- function() {
  df <- tibble()
  urls <- c("https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-0.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-1.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-2.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-3.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-4.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-5.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-6.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-7.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-8.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-9.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-a.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-b.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-c.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-d.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-e.csv",
            "https://raw.githubusercontent.com/chadwickbureau/register/master/data/people-f.csv")
  
  for(url in urls){
  suppressWarnings(
    temp <- read_csv(url) %>%
      select(-key_npb)
    )
    df <- bind_rows(df, temp)
  }
  
 
  return(df)
}

