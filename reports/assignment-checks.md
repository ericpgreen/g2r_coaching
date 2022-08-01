Cohort 1 Coach Assignments
================

# Introduction

In Cohort 1, IPA randomly assigned coaches to format (individual/group)
and then to groups assigned to the same format (individual: arms 1 and
3; group: arm 2). These assignments are spread across two files from Jan
10, 2019:

``` r
  path <- "/Users/epg4/Box Sync/AVSI Coaching/AVSI Coaching (Duke IPA)/Design/Assignments/"
  
  ipa_host <- readxl::read_excel(paste0(path, "coach_gr25_long_host_10 Jan 2019_sortCoach.xls")) %>%
    mutate(setting = "Host")
  
  ipa_ref <- readxl::read_excel(paste0(path, "coach_gr25_long_ref_10 Jan 2019_sortCoach.xls")) %>%
    mutate(setting = "Refugee")
  
  ipa <- bind_rows(ipa_host, ipa_ref) %>%
    mutate(in_ipa = "yes") 
```

After randomization, AVSI made some changes to coach assignments. This
file came from AVSI’s M&E team:

``` r
  avsi <- readxl::read_excel(paste0(path, "Cohort one coach assignment.xlsx")) %>%
      rename("Setting" = "Participant Type") %>%
    mutate(in_avsi = "yes") %>%
    select(-`Coach Name`, -`Group Name`)
```

# Number of coaches

``` r
  ipa %>%
    distinct(coachid, .keep_all = TRUE) %>%
    group_by(setting) %>% 
    count() %>% ungroup %>%
    mutate(N = sum(n))
```

    ## # A tibble: 2 × 3
    ##   setting     n     N
    ##   <chr>   <int> <int>
    ## 1 Host       97   193
    ## 2 Refugee    96   193

``` r
  avsi %>%
    distinct(`Coach ID`, .keep_all = TRUE) %>%
    group_by(Setting) %>% 
    count() %>% ungroup %>%
    mutate(N = sum(n))
```

    ## # A tibble: 2 × 3
    ##   Setting     n     N
    ##   <chr>   <int> <int>
    ## 1 Host      103   204
    ## 2 Refugee   101   204

# Matching coach IDs

``` r
  ipa %>%
    distinct(coachid, .keep_all = TRUE) %>%
    rename("Village Cluster" = "vil_clus") %>%
    select(coachid, in_ipa, `Village Cluster`) %>%
    full_join(select(distinct(avsi, `Coach ID`, .keep_all=TRUE),
                     `Coach ID`, in_avsi, `Village Cluster`), 
              by = c("coachid" = "Coach ID")) %>%
    mutate(in_ipa = case_when(
      is.na(in_ipa) ~ "no",
      TRUE ~ in_ipa
    )) %>%
    mutate(in_avsi = case_when(
      is.na(in_avsi) ~ "no",
      TRUE ~ in_avsi
    )) %>%
   {. ->> combined} %>%
    group_by(in_ipa, in_avsi) %>% count()
```

    ## # A tibble: 3 × 3
    ## # Groups:   in_ipa, in_avsi [3]
    ##   in_ipa in_avsi     n
    ##   <chr>  <chr>   <int>
    ## 1 no     yes        49
    ## 2 yes    no         38
    ## 3 yes    yes       155

# Number of groups per coach

IPA randomized group coaches to 5 groups.

``` r
  ipa %>%
    group_by(grouptype, coachid) %>% count(name = "groups") %>% 
    group_by(grouptype, groups) %>% count(name = "coach count")
```

    ## # A tibble: 4 × 3
    ## # Groups:   grouptype, groups [4]
    ##   grouptype groups `coach count`
    ##   <chr>      <int>         <int>
    ## 1 group          4             1
    ## 2 group          5            17
    ## 3 ind            1           174
    ## 4 ind            2             1

AVSI found that 5 groups was too much and added more coaches to reduce
the group load to 3.

``` r
  avsi %>%
    mutate(grouptype = case_when(
      `HH Arm` == "Arm 2" ~ "group",
      TRUE ~ "ind")) %>%
    group_by(grouptype, `Coach ID`) %>% count(name = "groups") %>% 
    group_by(grouptype, groups) %>% count(name = "coach count")
```

    ## # A tibble: 3 × 3
    ## # Groups:   grouptype, groups [3]
    ##   grouptype groups `coach count`
    ##   <chr>      <int>         <int>
    ## 1 group          3            30
    ## 2 ind            1           172
    ## 3 ind            2             2

# Matching village cluster IDs

``` r
  ipa %>%
    distinct(vil_clus, .keep_all = TRUE) %>%
    select(vil_clus, in_ipa) %>%
    full_join(select(distinct(avsi, `Village Cluster`, .keep_all=TRUE),
                     `Village Cluster`, in_avsi), 
              by = c("vil_clus" = "Village Cluster")) %>%
    mutate(in_ipa = case_when(
      is.na(in_ipa) ~ "no",
      TRUE ~ in_ipa
    )) %>%
    mutate(in_avsi = case_when(
      is.na(in_avsi) ~ "no",
      TRUE ~ in_avsi
    )) %>%
    group_by(in_ipa, in_avsi) %>% count()
```

    ## # A tibble: 1 × 3
    ## # Groups:   in_ipa, in_avsi [1]
    ##   in_ipa in_avsi     n
    ##   <chr>  <chr>   <int>
    ## 1 yes    yes        58

# Matching coach assignments for coach IDs in both

``` r
  combined %>%
    filter(in_ipa == "yes" & in_avsi == "yes") %>%
    mutate(same_vc = case_when(
      `Village Cluster.x` == `Village Cluster.y` ~ "yes",
      TRUE ~ "no"
    )) %>%
    group_by(same_vc) %>% count() %>%
    ungroup() %>% mutate(N = sum(n))
```

    ## # A tibble: 2 × 3
    ##   same_vc     n     N
    ##   <chr>   <int> <int>
    ## 1 no         28   155
    ## 2 yes       127   155
