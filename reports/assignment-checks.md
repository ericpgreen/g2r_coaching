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
  
  ipa
```

    ## # A tibble: 265 × 7
    ##    coachid           vil_clus    list_hh_gr25_ABC grouptype treat setting in_ipa
    ##    <chr>             <chr>       <chr>            <chr>     <chr> <chr>   <chr> 
    ##  1 HOST_POS_95_ADD_1 NTONWA (87) C                ind       Arm 1 Host    yes   
    ##  2 HOST_POS_96_ADD_2 RUBONA (98) B                ind       Arm 1 Host    yes   
    ##  3 HOST_POS_97_ADD_3 BISAYUMBE … C                ind       Arm 1 Host    yes   
    ##  4 KM011             NTONWA (87) A                ind       Arm 3 Host    yes   
    ##  5 KM012             KANYONZA I… B                group     Arm 2 Host    yes   
    ##  6 KM012             RWENSIKIZA… D                group     Arm 2 Host    yes   
    ##  7 KM012             KAKINGA (4… E                group     Arm 2 Host    yes   
    ##  8 KM012             BIGOLO (10… F                group     Arm 2 Host    yes   
    ##  9 KM012             BIGOLO (10… G                group     Arm 2 Host    yes   
    ## 10 KM013             KAYEMBE (7… C                group     Arm 2 Host    yes   
    ## # … with 255 more rows

After randomization, AVSI made some changes to coach assignments. This
file came from AVSI’s M&E team:

``` r
  avsi <- readxl::read_excel(paste0(path, "Cohort one coach assignment.xlsx")) %>%
      rename("Setting" = "Participant Type") %>%
    mutate(in_avsi = "yes") %>%
    select(-`Coach Name`, -`Group Name`)
  
  avsi
```

    ## # A tibble: 266 × 8
    ##    Subcounty     `Village Clust…` Village Setting `HH Arm` `Group ID` `Coach ID`
    ##    <chr>         <chr>            <chr>   <chr>   <chr>    <chr>      <chr>     
    ##  1 NKOMA-KATALY… BULEGEYA II (13… BULEGE… Host    Arm 3    G2R-033    KM067     
    ##  2 NKOMA-KATALY… BULEGEYA II (13… BULEGE… Host    Arm 1    G2R-031    KM317     
    ##  3 NKOMA-KATALY… BULEGEYA II (13… BULEGE… Host    Arm 2    G2R-032    KM125     
    ##  4 NKOMA-KATALY… DAMASIKO (138) … DAMASI… Host    Arm 1    G2R-026    KM136     
    ##  5 NKOMA-KATALY… DAMASIKO (138) … DAMASI… Host    Arm 3    G2R-027    KM073     
    ##  6 NKOMA-KATALY… DAMASIKO (138) … DAMASI… Host    Arm 2    G2R-028    KM200     
    ##  7 NKOMA-KATALY… KATALYEBA (139)… KATALY… Host    Arm 2    G2R-029    KM200     
    ##  8 NKOMA-KATALY… KATALYEBA (139)… KATALY… Host    Arm 1    G2R-030    KM156     
    ##  9 NKOMA-KATALY… KATALYEBA (139)… KATALY… Host    Arm 3    G2R-014    KM133     
    ## 10 NKOMA-KATALY… MAHANI I (141) … RWEMBU… Host    Arm 2    G2R-013    KM341     
    ## # … with 256 more rows, and 1 more variable: in_avsi <chr>

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
