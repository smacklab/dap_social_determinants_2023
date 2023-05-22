# dap_social_determinants_2023
Code for McCoy, Brassington, et al, 2023, Social determinants of health and disease in companion dogs: A cohort study from the Dog Aging Project . doi: 10.1093/emph/eoad011 


# apply for access here https://dogagingproject.org/open_data_access/ and download these files:
"HLES_dog_owner", "HLES_health_conditions", "HLES_cancer_conditions", "ENVIRONMENT", and "DogOverview"

## packages required 
library(dplyr)
library(readr)
library(labelled)
library(magrittr)
library(haven)

## filter data to include these variables 
dog_id,dd_sex,dd_breed_pure,dd_age_years,pa_activity_level,
                                         pa_avg_activity_intensity,pa_avg_daily_active_hours,pa_avg_daily_active_minutes,
                                         pa_on_leash_off_leash_walk,pa_on_leash_walk_average_pace_pct,pa_on_leash_walk_avg_hours,
                                         pa_on_leash_walk_avg_minutes,pa_on_leash_walk_brisk_pace_pct,pa_on_leash_walk_frequency,
                                         pa_on_leash_walk_jog_pace_pct,pa_on_leash_walk_reasons_activity_and_enjoyment,
                                         pa_on_leash_walk_reasons_dog_relieve_itself,pa_on_leash_walk_reasons_exercise_for_dog,
                                         pa_on_leash_walk_reasons_exercise_for_owner,pa_on_leash_walk_reasons_training_obedience,
                                         pa_on_leash_walk_run_pace_pct,pa_on_leash_walk_slow_pace_pct,
                                         pa_other_aerobic_activity_frequency,pa_physical_games_frequency,pa_swim,
                                         de_lifetime_residence_count,de_past_residence_zip_count,de_past_residence_country_count,
                                         de_neighborhood_has_sidewalks,de_dogpark,de_dogpark,de_routine_hours_per_day_roaming_house
                                         ,de_work,de_routine_hours_per_day_roaming_outside,de_routine_hours_per_day_in_yard,
                                         de_routine_hours_per_day_with_other_animals,de_routine_hours_per_day_chained_outside,
                                         de_routine_hours_per_day_away_from_home,de_sitter_or_daycare,
                                         de_routine_hours_per_day_with_adults,de_routine_consistency,de_daytime_sleep_avg_hours,
                                         de_routine_hours_per_day_in_garage,de_room_or_window_air_conditioning_present,
                                         de_routine_hours_per_day_with_children,de_routine_hours_per_day_with_teens,
                                         de_routine_hours_per_day_in_outdoor_kennel,de_routine_hours_per_day_in_crate,
                                         oc_household_person_count,oc_household_adult_count,oc_household_child_count,
                                         oc_primary_residence_ownership,oc_secondary_residence, dd_activities_companion_animal,
                                         dd_alternate_recent_residence_count,dd_breed_mixed_primary,dd_breed_mixed_secondary,
                                         dd_weight_lbs,dd_spay_or_neuter_age,dd_spayed_or_neutered,ss_household_dog_count,
                                         od_max_education,od_age_range_years,od_annual_income_range_usd,hs_general_health,
                                         de_recreational_spaces, cv_disadvantage_index, cv_median_income, dog_id, cv_stability_index, 
                                         wv_walkscore_descrip, cv_pct_less_than_100k,cv_pct_below_125povline,cv_pct_jobless16to64mf, 
                                         cv_population_density, cv_pct_same_house_1yrago, cv_pct_owner_occupied, cv_pct_us_born, Breed_Status


## creating the dataframe for factor analysis 
new_df <- df %>% select(cv_median_income,od_annual_income_range_usd,cv_pct_less_than_100k,
                               cv_pct_below_125povline,cv_pct_jobless16to64mf, de_dogpark,de_sitter_or_daycare,de_routine_consistency,
                               cv_stability_index, ss_household_dog_count,od_age_range_years,
                               od_max_education, oc_household_adult_count,oc_household_child_count,
                               de_work ,dd_activities_companion_animal,de_routine_hours_per_day_with_other_animals,
                               de_routine_hours_per_day_with_teens, de_routine_hours_per_day_with_children, 
                               de_routine_hours_per_day_with_adults, de_lifetime_residence_count, de_neighborhood_has_sidewalks,
                               de_routine_hours_per_day_roaming_house,de_routine_hours_per_day_roaming_outside,
                               de_recreational_spaces,wv_walkscore_descrip,cv_population_density, 
                               de_routine_hours_per_day_in_garage, oc_secondary_residence, cv_pct_same_house_1yrago, cv_pct_owner_occupied, cv_pct_us_born)

##change rownames to dog_id

rownames(new_df)=mega_block$dog_id

##remove dog_id for factor analysis  

new_df <- new_df[,1:32]

##run factor analysis and specify 5 factors

factor_model = fa(new_df, 5)

##pulling out loadings for each factor

factor_model_loadings <- as.data.frame.table(factor_model$loadings)

##extract scores 

factor_model_scores <- as.data.frame(factor_model_loadings$scores)




## creating mobility metric 
##pulling out z-scores of desired variables 
mobility_df <- df %>% select(pa_activity_level, pa_avg_activity_intensity, pa_on_leash_walk_frequency, 
                                       pa_other_aerobic_activity_frequency, pa_physical_games_frequency, 
                                       pa_avg_daily_active_total, pa_on_leash_walk_avg_total) %>% mutate_all(.,scale)
mobility_df$mobility <- rowMeans(mobility_df)


## combinine disease, mobility, and fa results dfs
final_df <- factor_model_final
final_df$mobility = mb_final_mobility$mobility
final_df <- left_join(final_df, disease_count, by="dog_id")


## add in cancer incidence
final_df <- left_join(final_df, cancer, by="dog_id")
final_df$disease_count <- final_df$disease_count + final_df$cancer



## modeling

##health 

lm(health ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + scale(dd_weight_lbs), data = final_df)

library(MASS)
m <- polr(as.factor(health) ~ scale(dd_age_years) + scale(dd_weight_lbs) + MR1 + MR2 + MR3 + MR4 + MR5 , data = final_df, Hess=TRUE)
ctable <- coef(summary(m))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)

##disease

glm(disease_count ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + scale(dd_weight_lbs), data = final_df, family= poisson)

##mobility

lm(mobility ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + scale(dd_weight_lbs), data = final_df)



###interactive models

##health

lm(health ~ MR1*scale(dd_age_years) + MR2*scale(dd_age_years)+ MR3*scale(dd_age_years) + MR4*scale(dd_age_years) + MR5*scale(dd_age_years) +
                             MR1*scale(dd_weight_lbs) + MR2*scale(dd_weight_lbs)+ MR3*scale(dd_weight_lbs) + MR4*scale(dd_weight_lbs) + MR5*scale(dd_weight_lbs), data = final_df)


##disease

glm(disease_count ~ MR1*scale(dd_age_years) + MR2*scale(dd_age_years)+ MR3*scale(dd_age_years) + MR4*scale(dd_age_years) + MR5*scale(dd_age_years) +
                              MR1*scale(dd_weight_lbs) + MR2*scale(dd_weight_lbs)+ MR3*scale(dd_weight_lbs) + MR4*scale(dd_weight_lbs) + MR5*scale(dd_weight_lbs), data = final_df, family= poisson)


##mobility

lm(mobility ~ MR1*scale(dd_age_years) + MR2*scale(dd_age_years)+ MR3*scale(dd_age_years) + MR4*scale(dd_age_years) + MR5*scale(dd_age_years) +
                             MR1*scale(dd_weight_lbs) + MR2*scale(dd_weight_lbs)+ MR3*scale(dd_weight_lbs) + MR4*scale(dd_weight_lbs) + MR5*scale(dd_weight_lbs), data = final_df)




###mixed vs pure models 

##health

lm(health ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
                           scale(dd_weight_lbs), data = subset(final_df ,breed == "mixed"))
lm(health ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
              scale(dd_weight_lbs), data = subset(final_df ,breed == "pure"))

##disease 

glm(disease_count ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
              scale(dd_weight_lbs), family = poisson, data = subset(final_df,breed == "mixed"))
glm(disease_count ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
              scale(dd_weight_lbs), family = poisson, data = subset(final_df,breed == "pure"))

##mobility 

lm(mobility ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
              scale(dd_weight_lbs),data = subset(final_df,breed == "mixed"))
lm(mobility ~ MR1 + MR2 + MR3 + MR4 + MR5 + scale(dd_age_years) + 
             scale(dd_weight_lbs),data = subset(final_df,breed == "pure"))






##Subset the data and run models with purebreed dogs only and control for breed as a random effect

##Linear mixed effects models controlling for breed as a random effect (use data= purebred_only)

library(lmerTest)

#health

summary(lmer(hs_general_health ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + 
                             scale(dd_weight_lbs) + (1|dd_breed_pure), data = purebred_only))


#disease

summary(glmer(disease_count ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + 
                             scale(dd_weight_lbs) + (1|dd_breed_pure), data = purebred_only, family= poisson))


#mobility

summary(lmer(mobility ~ MR1 + MR2+ MR3 + MR4 + MR5 + scale(dd_age_years) + 
                            scale(dd_weight_lbs) + (1|dd_breed_pure), data = purebred_only))
