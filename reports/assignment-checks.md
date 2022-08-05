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

# Coach IDs in IPA but not AVSI

``` r
  ipa_not_avsi <- combined %>%
    filter(in_ipa == "yes" & in_avsi == "no") %>%
    distinct(coachid) 

  avsi_not_ipa <- combined %>%
    filter(in_ipa == "no" & in_avsi == "yes") %>%
    distinct(coachid) 
  
  writexl::write_xlsx(list(ipa_not_avsi = ipa_not_avsi, 
                           avsi_not_ipa = avsi_not_ipa), 
                      path = "assignment checks.xlsx")
```

# Multiple coaches per arm per village

Antoine notices that some clusters have multiple coaches in a given arm:

``` r
  avsi %>%
    group_by(`Village Cluster`, `HH Arm`) %>%
    count(name = "number coaches") %>%
    filter(`number coaches`>1) %>%
    left_join(select(avsi, `Village Cluster`, `HH Arm`)) %>%
    distinct(`Village Cluster`, `HH Arm`, .keep_all = TRUE)
```

    ## # A tibble: 75 × 3
    ## # Groups:   Village Cluster, HH Arm [75]
    ##    `Village Cluster`                      `HH Arm` `number coaches`
    ##    <chr>                                  <chr>               <int>
    ##  1 BASE CAMP 1                            Arm 1                   2
    ##  2 BASE CAMP 1                            Arm 2                   2
    ##  3 BENGA B (1) + LYAKATAMA (27)           Arm 2                   2
    ##  4 BENGA CENTRAL (2) + KANYEGARAMIRE (23) Arm 3                   2
    ##  5 BIGOLO (103)                           Arm 2                   2
    ##  6 BIGOLO (103)                           Arm 3                   2
    ##  7 BITOJO (5) + BUBAARE (6)               Arm 1                   2
    ##  8 BITOJO (5) + BUBAARE (6)               Arm 2                   3
    ##  9 BITOJO (5) + BUBAARE (6)               Arm 3                   2
    ## 10 BUGUTA B                               Arm 2                   2
    ## # … with 65 more rows

I think the issue here is that some village clusters have more than one
group in arm 1, more than 1 group in arm 2, etc.

``` r
  ipa %>% 
    mutate(treat = tolower(treat)) %>%
    group_by(vil_clus, treat) %>% 
    count(name = "number of groups") %>% 
    filter(`number of groups`>1)
```

    ## # A tibble: 75 × 3
    ## # Groups:   vil_clus, treat [75]
    ##    vil_clus                               treat `number of groups`
    ##    <chr>                                  <chr>              <int>
    ##  1 BASE CAMP 1                            arm 1                  2
    ##  2 BASE CAMP 1                            arm 2                  2
    ##  3 BENGA B (1) + LYAKATAMA (27)           arm 2                  2
    ##  4 BENGA CENTRAL (2) + KANYEGARAMIRE (23) arm 3                  2
    ##  5 BIGOLO (103)                           arm 2                  2
    ##  6 BIGOLO (103)                           arm 3                  2
    ##  7 BITOJO (5) + BUBAARE (6)               arm 1                  2
    ##  8 BITOJO (5) + BUBAARE (6)               arm 2                  2
    ##  9 BITOJO (5) + BUBAARE (6)               arm 3                  2
    ## 10 BUGUTA B                               arm 2                  2
    ## # … with 65 more rows

Confirming that group IDs do not repeat in clusters:

``` r
  avsi %>%
    group_by(`Village Cluster`, `Group ID`) %>%
    count() %>%
    filter(n>1) %>%
    left_join(select(avsi, `Village Cluster`, `HH Arm`, `Coach ID`, `Group ID`))
```

    ## # A tibble: 0 × 5
    ## # Groups:   Village Cluster, Group ID [0]
    ## # … with 5 variables: Village Cluster <chr>, Group ID <chr>, n <int>,
    ## #   HH Arm <chr>, Coach ID <chr>
