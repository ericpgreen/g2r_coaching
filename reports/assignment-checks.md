Cohort 1 Coach Assignments
================

# Introduction

In Cohort 1, IPA randomly assigned coaches to program format (idividual
vs group coaching) and then to one or more groups of participants
assigned to the same format. The group coaches were randomly assigned to
5 groups of \~25 households each in Arm 2 (group coaching plus asset
transfer), while the individual coaches were randomly assigned 1:1 to
one ‘group’ of \~25 households in Arm 1 (individual coaching plus asset
transfer) or Arm 3 (individual coaching, no asset transfer).

We are interested in using data from Cohort 1 to estimate the ‘coach
effect’ on participant outcomes, but it’s unclear to what extent the
original coach randomization was maintained. At some point after IPA
randomized coaches, AVSI modified some assignments for logistical
reasons. The purpose of this analysis is to document deviations from the
original randomization.

# How many coaches were randomly assigned?

The first step is to determine how many coaches were randomly assigned
in Cohort 1 and to document their original assignments. For this we will
bind together two IPA datasets created on January 10, 2019:

``` r
  ipa_host <- readxl::read_excel(paste0(path, "coach_gr25_long_host_10 Jan 2019_sortCoach.xls")) %>%
    mutate(setting = "Host")
  
  ipa_ref <- readxl::read_excel(paste0(path, "coach_gr25_long_ref_10 Jan 2019_sortCoach.xls")) %>%
    mutate(setting = "Refugee")
  
  ipa <- bind_rows(ipa_host, ipa_ref) %>%
    mutate(in_ipa = "yes") 
  
  ipa %>%
    group_by(grouptype, coachid) %>% count(name = "groups") %>% 
    group_by(grouptype, groups) %>% count(name = "coach count") %>%
    ungroup() %>%
    mutate(total = sum(`coach count`))
```

    ## # A tibble: 4 × 4
    ##   grouptype groups `coach count` total
    ##   <chr>      <int>         <int> <int>
    ## 1 group          4             1   193
    ## 2 group          5            17   193
    ## 3 ind            1           174   193
    ## 4 ind            2             1   193

These files list 193 coaches: 175 assigned to individual coaching and 18
assigned to group coaching. According to our notes, we expected this
number to be 194 coaches (18 group / 176 individual). **IPA: Is this
file missing a coach?**

# Actual coach assignments

After randomization, AVSI made some changes to coach assignments. This
file came from AVSI’s M&E team:

``` r
  avsi <- readxl::read_excel(paste0(path, "Cohort one coach assignment.xlsx")) %>%
      rename("Setting" = "Participant Type") %>%
    mutate(in_avsi = "yes") %>%
    select(-`Coach Name`, -`Group Name`) %>%
    rename(coachid = "Coach ID")
```

The AVSI file identifies 204 coaches, so 11 more than the IPA records.
No groups were assigned more than one coach in this file, so replacement
coaches are not the obvious answer to this discrepancy.

``` r
  avsi %>%
    group_by(`Group ID`) %>%
    count(name = "number of coaches per group") %>% 
    group_by(`number of coaches per group`) %>%
    count(name = "groups")
```

    ## # A tibble: 1 × 2
    ## # Groups:   number of coaches per group [1]
    ##   `number of coaches per group` groups
    ##                           <int>  <int>
    ## 1                             1    266

It’s worth checking to make sure that the files include the same number
of groups. The IPA file does not include the same `Group ID` variable so
we can’t compare on it, but we can concatenate IPA’s village cluster
variable and the randomization group letter variable and then count the
number of distinct values to get the number of groups represented in the
IPA file:

``` r
  ipa %>%
    mutate(group = paste(vil_clus, list_hh_gr25_ABC)) %>%
    distinct(group) %>%
    count()
```

    ## # A tibble: 1 × 1
    ##       n
    ##   <int>
    ## 1   265

It appears that the files are off by 1 group (266 AVSI vs 265 IPA). **Is
it possible that AVSI added a group after randomization?**

# Why different numbers of coaches?

The AVSI file identifies 204 coaches, and the IPA file identifies 193
coaches. Why the discrepancy?

To investigate further we can attempt to match coaches across files.

``` r
  ipa %>%
    distinct(coachid, .keep_all = TRUE) %>%
    rename("Village Cluster" = "vil_clus") %>%
    select(coachid, in_ipa, `Village Cluster`) %>%
    full_join(select(distinct(avsi, coachid, .keep_all=TRUE),
                     coachid, in_avsi, `Village Cluster`), 
              by = "coachid") %>%
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

This shows that 155 coach IDs are present in both files. Of the 87
single source coach IDs, 38 are only present in the IPA file and 49 are
only present in the AVSI file.

## Coaches only in the AVSI file

Let’s look at the 49 coaches only present in the AVSI file and join with
AVSI-hiring date data:

``` r
  avsi_hiring <- readxl::read_excel(paste0(path, "hiring.xlsx"),
                                    sheet = "reformatted") %>%
    mutate(hired_after_2018 = case_when(
      is.na(start) ~ NA_character_,
      start > lubridate::ymd("2018-12-31") ~ "Yes",
      TRUE ~ "No"
    ))

  combined %>%
    filter(in_ipa == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    group_by(hired_after_2018, dropped) %>%
    count()
```

    ## # A tibble: 4 × 3
    ## # Groups:   hired_after_2018, dropped [4]
    ##   hired_after_2018 dropped     n
    ##   <chr>            <chr>   <int>
    ## 1 No               No          9
    ## 2 No               Yes         1
    ## 3 Yes              No         27
    ## 4 <NA>             <NA>       12

According to AVSI’s hiring data, 10/49 coaches only in the AVSI file
were hired in 2018. 9/10 remained employed through the end of Cohort 1;
1/10 terminated before 2019. **Why are these 9 coaches not in the IPA
file?**

``` r
  combined %>%
    filter(in_ipa == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(hired_after_2018 == "No", dropped=="No") %>%
    select(coachid, `Village Cluster.y`, end)
```

    ## # A tibble: 9 × 3
    ##   coachid `Village Cluster.y`                                end                
    ##   <chr>   <chr>                                              <dttm>             
    ## 1 KM317   BULEGEYA II (137) + NTUNTU II (146)                2021-06-30 00:00:00
    ## 2 KM196   MAHEGA B2+B3                                       2021-06-30 00:00:00
    ## 3 KM346   MABAALE II (125) + KABUGA I (110) + KIDUNDUMA (12… 2021-06-30 00:00:00
    ## 4 KM301   KYEMPANGO C1                                       2021-06-30 00:00:00
    ## 5 KM293   NTENUNGI C                                         2021-06-30 00:00:00
    ## 6 KM273   RUBONA (98)                                        2021-06-30 00:00:00
    ## 7 KM052   NTONWA (87)                                        2021-06-30 00:00:00
    ## 8 KM315   KAYEMBE (75) + RUTABO I (100) + RUTABO II (101) +… 2021-06-30 00:00:00
    ## 9 KM311   BITOJO (5) + BUBAARE (6)                           2021-06-30 00:00:00

27/49 coaches only in the AVSI file were hired after 2018 and remained
in the program until the end of Cohort 1. **Were these replacements
coaches??**

``` r
  combined %>%
    filter(in_ipa == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(hired_after_2018 == "Yes" & dropped == "No") %>%
    select(coachid, `Village Cluster.y`) %>%
    print(n=Inf)
```

    ## # A tibble: 27 × 2
    ##    coachid `Village Cluster.y`                                              
    ##    <chr>   <chr>                                                            
    ##  1 KM296   KYEMPANGO A3                                                     
    ##  2 KM294   KYEMPANGO A3                                                     
    ##  3 KM345   BUGUTA B                                                         
    ##  4 KM295   KYEMPANGO A3                                                     
    ##  5 KM285   KYEMPANGO B2                                                     
    ##  6 KM344   KYEMPANGO A4                                                     
    ##  7 KM312   RWEMBIRIZI (134) + BUSINGYE MABAALE (105) + KAHARA (112)         
    ##  8 KM291   KYEMPANGO A3                                                     
    ##  9 KM347   KYEMPANGO A5                                                     
    ## 10 KM304   KYEMPANGO C2                                                     
    ## 11 KM286   KYEMPANGO C1                                                     
    ## 12 KM290   KYEMPANGO A3                                                     
    ## 13 KM281   KYEMPANGO A4                                                     
    ## 14 KM284   KYEMPANGO C1                                                     
    ## 15 KM305   KYEMPANGO A3                                                     
    ## 16 KM283   MAHEGA B4+C1                                                     
    ## 17 KM288   KAYEMBE (75) + RUTABO I (100) + RUTABO II (101) + KAGOYIGOYI (69)
    ## 18 KM343   KIBOOTA (76) + RUBAZI (97)                                       
    ## 19 KM289   RUBONA (98)                                                      
    ## 20 KM287   KAKOONA (70) + MASANGI (82)                                      
    ## 21 KM318   NYUNDO (58) + RWENSIKIZA I (61)                                  
    ## 22 KM282   RWENSIKIZA II (62) + RUSHANGO (59)                               
    ## 23 KM292   KAROKARUNGI (52) + KABINGO (46)                                  
    ## 24 KM302   BISAYUMBE (4) + MUKUKURU (29)                                    
    ## 25 KM325   BENGA B (1) + LYAKATAMA (27)                                     
    ## 26 KM310   KIRINDA (26)                                                     
    ## 27 KM324   KAISHUNGA (20) + KABAARE (15) + BUSINGYE (8)

12/49 coaches only in the AVSI file have no hiring data. **Why??**

``` r
  combined %>%
    filter(in_ipa == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(is.na(hired_after_2018)) %>%
    select(coachid, `Village Cluster.y`)
```

    ## # A tibble: 12 × 2
    ##    coachid `Village Cluster.y`                                     
    ##    <chr>   <chr>                                                   
    ##  1 KM313   MAHANI I (141) + RWEMBURARA (148)                       
    ##  2 KM338   KYEMPANGO A3                                            
    ##  3 KM339   KYEMPANGO C1                                            
    ##  4 KM340   NKOMA A B split_2                                       
    ##  5 KM214   KAROKARUNGI (117) + MABAALE I (124)                     
    ##  6 KM333   MAHEGA B4+C1                                            
    ##  7 KM388   NKOMA A B split_2                                       
    ##  8 KM316   RWEMBIRIZI (134) + BUSINGYE MABAALE (105) + KAHARA (112)
    ##  9 KM393   NTENUNGI C                                              
    ## 10 KM257   KAIHORA D                                               
    ## 11 KM342   KABARUNGI (68)                                          
    ## 12 KM330   BENGA CENTRAL (2) + KANYEGARAMIRE (23)

## Coaches only in the IPA file

The following 38 coaches are only present in the IPA file:

``` r
  combined %>%
    filter(in_avsi == "no") %>%
    select(coachid, `Village Cluster.x`) %>%
    print(n=Inf)
```

    ## # A tibble: 38 × 2
    ##    coachid           `Village Cluster.x`                                        
    ##    <chr>             <chr>                                                      
    ##  1 HOST_POS_95_ADD_1 NTONWA (87)                                                
    ##  2 HOST_POS_96_ADD_2 RUBONA (98)                                                
    ##  3 HOST_POS_97_ADD_3 BISAYUMBE (4) + MUKUKURU (29)                              
    ##  4 KM016             NYUNDO (58) + RWENSIKIZA I (61)                            
    ##  5 KM017             KIBOOTA (76) + RUBAZI (97)                                 
    ##  6 KM023             RWEMBIRIZI (134) + BUSINGYE MABAALE (105) + KAHARA (112)   
    ##  7 KM024             KYAKAITABA (80)                                            
    ##  8 KM047             KAISHUNGA (20) + KABAARE (15) + BUSINGYE (8)               
    ##  9 KM077             IBUGA (13) + KAKINDO (21) + RWAKASIRABO (38)               
    ## 10 KM078             BITOJO (5) + BUBAARE (6)                                   
    ## 11 KM104             BENGA CENTRAL (2) + KANYEGARAMIRE (23)                     
    ## 12 KM108             KAYEMBE (75) + RUTABO I (100) + RUTABO II (101) + KAGOYIGO…
    ## 13 KM117             RWENSIKIZA II (62) + RUSHANGO (59)                         
    ## 14 KM127             KAROKARUNGI (117) + MABAALE I (124)                        
    ## 15 KM152             KATEBE (54) + BUTERANIRO (44)                              
    ## 16 KM186             KAROKARUNGI (52) + KABINGO (46)                            
    ## 17 NEW_1             RUBONA (98)                                                
    ## 18 NEW_2             KAKOONA (70) + MASANGI (82)                                
    ## 19 KM038             KAIHORA D                                                  
    ## 20 KM043             NKOMA A B split_2                                          
    ## 21 KM094             KAIHORA A                                                  
    ## 22 KM100             NKOMA C                                                    
    ## 23 KM107             NTENUNGI C                                                 
    ## 24 KM123             MAHEGA B4+C1                                               
    ## 25 KM149             MAHEGA B2+B3                                               
    ## 26 KM269             MAHEGA B2+B3                                               
    ## 27 POS_86_ADD_1      KYEMPANGO C2                                               
    ## 28 POS_87_ADD_2      KYEMPANGO C1                                               
    ## 29 POS_88_ADD_3      KYEMPANGO C1                                               
    ## 30 POS_89_ADD_4      KYEMPANGO A3                                               
    ## 31 POS_90_ADD_5      KYEMPANGO A3                                               
    ## 32 POS_91_ADD_6      KYEMPANGO A4                                               
    ## 33 POS_92_ADD_7      KYEMPANGO A3                                               
    ## 34 POS_93_ADD_8      MAHEGA B4+C1                                               
    ## 35 POS_94_ADD_9      NTENUNGI C                                                 
    ## 36 POS_95_ADD_10     KYEMPANGO A3                                               
    ## 37 POS_96_ADD_11     KYEMPANGO A3                                               
    ## 38 POS_97_ADD_12     KYEMPANGO C1

20/38 coaches have hiring information:

``` r
  combined %>%
    filter(in_avsi == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(!is.na(hired_after_2018)) %>%
    select(coachid, `Village Cluster.x`, start, end, dropped)
```

    ## # A tibble: 20 × 5
    ##    coachid `Village Cluster.x`   start               end                 dropped
    ##    <chr>   <chr>                 <dttm>              <dttm>              <chr>  
    ##  1 KM016   NYUNDO (58) + RWENSI… 2018-04-01 00:00:00 2019-06-14 00:00:00 Yes    
    ##  2 KM017   KIBOOTA (76) + RUBAZ… 2018-04-01 00:00:00 2019-07-31 00:00:00 Yes    
    ##  3 KM023   RWEMBIRIZI (134) + B… 2018-04-01 00:00:00 2019-06-07 00:00:00 Yes    
    ##  4 KM024   KYAKAITABA (80)       2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ##  5 KM047   KAISHUNGA (20) + KAB… 2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ##  6 KM077   IBUGA (13) + KAKINDO… 2018-04-01 00:00:00 2019-03-01 00:00:00 Yes    
    ##  7 KM078   BITOJO (5) + BUBAARE… 2018-04-01 00:00:00 2019-07-01 00:00:00 Yes    
    ##  8 KM104   BENGA CENTRAL (2) + … 2018-04-01 00:00:00 2019-07-31 00:00:00 Yes    
    ##  9 KM108   KAYEMBE (75) + RUTAB… 2018-04-01 00:00:00 2019-03-06 00:00:00 Yes    
    ## 10 KM117   RWENSIKIZA II (62) +… 2018-04-01 00:00:00 2018-12-31 00:00:00 Yes    
    ## 11 KM127   KAROKARUNGI (117) + … 2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ## 12 KM152   KATEBE (54) + BUTERA… 2018-04-01 00:00:00 2018-12-13 00:00:00 Yes    
    ## 13 KM186   KAROKARUNGI (52) + K… 2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ## 14 KM038   KAIHORA D             2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ## 15 KM043   NKOMA A B split_2     2018-04-01 00:00:00 2019-12-09 00:00:00 Yes    
    ## 16 KM094   KAIHORA A             2018-04-01 00:00:00 2021-06-30 00:00:00 No     
    ## 17 KM100   NKOMA C               2018-04-01 00:00:00 2019-06-04 00:00:00 Yes    
    ## 18 KM107   NTENUNGI C            2018-04-01 00:00:00 2019-11-25 00:00:00 Yes    
    ## 19 KM123   MAHEGA B4+C1          2018-04-01 00:00:00 2019-09-24 00:00:00 Yes    
    ## 20 KM149   MAHEGA B2+B3          2018-04-01 00:00:00 2021-06-30 00:00:00 No

13/20 with hiring data did not remain in the coaching role through the
end of Cohort 1.

``` r
  combined %>%
    filter(in_avsi == "no") %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(!is.na(hired_after_2018)) %>%
    group_by(dropped) %>%
    count()
```

    ## # A tibble: 2 × 2
    ## # Groups:   dropped [2]
    ##   dropped     n
    ##   <chr>   <int>
    ## 1 No          7
    ## 2 Yes        13

Here is the full disposition of coaches across both IPA and AVSI’s
files:

``` r
  combined %>%
    left_join(avsi_hiring, by = "coachid") %>%
    group_by(in_avsi, in_ipa, hired_after_2018, dropped) %>%
    count()
```

    ## # A tibble: 11 × 5
    ## # Groups:   in_avsi, in_ipa, hired_after_2018, dropped [11]
    ##    in_avsi in_ipa hired_after_2018 dropped     n
    ##    <chr>   <chr>  <chr>            <chr>   <int>
    ##  1 no      yes    No               No          7
    ##  2 no      yes    No               Yes        13
    ##  3 no      yes    <NA>             <NA>       18
    ##  4 yes     no     No               No          9
    ##  5 yes     no     No               Yes         1
    ##  6 yes     no     Yes              No         27
    ##  7 yes     no     <NA>             <NA>       12
    ##  8 yes     yes    No               No        145
    ##  9 yes     yes    No               Yes         1
    ## 10 yes     yes    Yes              No          7
    ## 11 yes     yes    <NA>             <NA>        2

# How was compliance among the matches?

145 coaches present in both files were hired in 2018 and remained in the
coaching role through the end of Cohort 1.

``` r
  combined %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(in_avsi=="yes", in_ipa=="yes", 
           hired_after_2018=="No", dropped=="No") %>%
    mutate(match_cluster = case_when(
      `Village Cluster.x` == `Village Cluster.y` ~ "Yes",
      TRUE ~ "No"
    )) %>%
    group_by(match_cluster) %>%
    count()
```

    ## # A tibble: 2 × 2
    ## # Groups:   match_cluster [2]
    ##   match_cluster     n
    ##   <chr>         <int>
    ## 1 No               28
    ## 2 Yes             117

117/145 (80%) of these matching coaches appear to have served in their
originally assigned village clusters. Here are the non-matching cluster
coaches:

``` r
  combined %>%
    left_join(avsi_hiring, by = "coachid") %>%
    filter(in_avsi=="yes", in_ipa=="yes", 
           hired_after_2018=="No", dropped=="No") %>%
    mutate(match_cluster = case_when(
      `Village Cluster.x` == `Village Cluster.y` ~ "Yes",
      TRUE ~ "No"
    )) %>%
    filter(match_cluster=="No") %>%
    select(coachid, `Village Cluster.x`, `Village Cluster.y`) %>%
    print(n=Inf)
```

    ## # A tibble: 28 × 3
    ##    coachid `Village Cluster.x`                                           Villa…¹
    ##    <chr>   <chr>                                                         <chr>  
    ##  1 KM012   KANYONZA I (50) + RWOMURIRO (63)                              RWENSI…
    ##  2 KM013   KAYEMBE (75) + RUTABO I (100) + RUTABO II (101) + KAGOYIGOYI… NTONWA…
    ##  3 KM014   LYAMUGENYI (123) + MIKOLE I (127)                             RUTOOM…
    ##  4 KM036   MAHANI I (141) + RWEMBURARA (148)                             KAISHU…
    ##  5 KM039   MABAALE II (125) + KABUGA I (110) + KIDUNDUMA (120)           BIGOLO…
    ##  6 KM040   IBUGA (13) + KAKINDO (21) + RWAKASIRABO (38)                  KAKUMB…
    ##  7 KM075   DAMASIKO (138) + NYAMUCHWA (147)                              KYAKAI…
    ##  8 KM091   RUBONA (98)                                                   KANYON…
    ##  9 KM124   LYAMUGENYI (123) + MIKOLE I (127)                             KATEMB…
    ## 10 KM125   KAKUMBU (115) + KYAMUHAMIRA (122)                             BULEGE…
    ## 11 KM128   BITOJO (5) + BUBAARE (6)                                      MAHANI…
    ## 12 KM130   MABAALE II (125) + KABUGA I (110) + KIDUNDUMA (120)           KAKUMB…
    ## 13 KM131   MAHANI I (141) + RWEMBURARA (148)                             LYAMUG…
    ## 14 KM136   KYAKAITABA (80)                                               DAMASI…
    ## 15 KM137   KAYEMBE (75) + RUTABO I (100) + RUTABO II (101) + KAGOYIGOYI… IBUGA …
    ## 16 KM139   KAKUMBU (115) + KYAMUHAMIRA (122)                             MABAAL…
    ## 17 KM148   MAHANI I (141) + RWEMBURARA (148)                             BITOJO…
    ## 18 KM154   KATEMBWE II (118) + KANTEMBWE I (116) + KAJWAMUSHANA (113)    LYAMUG…
    ## 19 KM155   KANYONZA I (50) + RWOMURIRO (63)                              RUBONA…
    ## 20 KM180   NYAKATOOMA (37) + SOWETO (40) + KAROKARUNGI (25)              KATEMB…
    ## 21 KM187   KATEMBWE II (118) + KANTEMBWE I (116) + KAJWAMUSHANA (113)    NYAKAT…
    ## 22 KM195   BULEGEYA II (137) + NTUNTU II (146)                           KATEBE…
    ## 23 KM200   KATALYEBA (139) + NKOMA (144)                                 DAMASI…
    ## 24 KM202   KAKUMBU (115) + KYAMUHAMIRA (122)                             IBUGA …
    ## 25 KM044   MAHANI 1 split_2                                              BASE C…
    ## 26 KM045   KYEMPANGO A4                                                  NKOMA …
    ## 27 KM141   KYEMPANGO A3                                                  NKOMA C
    ## 28 KM142   NKOMA A B split_2                                             MIKOLE…
    ## # … with abbreviated variable name ¹​`Village Cluster.y`

# Outstanding Questions

1.  We thought IPA randomized 194 coaches. The Jan 2019 files list 193
    coaches by our count. Is a coach missing?

2.  We count 266 groups in the AVSI file vs 265 groups in the IPA file.
    The IPA file does not contain group numbers, so we can’t identify
    which group is potentially missing.

3.  49 coach IDs only appear in the AVSI file:

-   10/49 coaches were hired in 2018. 9/10 remained employed through the
    end of Cohort 1; 1/10 terminated before 2019. Why are these 9
    coaches not in the IPA file?

-   27/49 coaches only in the AVSI file were hired after 2018 and
    remained in the program until the end of Cohort 1. Were these
    replacements coaches?

-   12/49 coaches only in the AVSI file have no hiring data. Why?

4.  38 coaches IDs only appear in the IPA file:

-   20/38 coaches have hiring information

    -   13/20 with hiring data did not remain in the coaching role
        through the end of Cohort 1.
