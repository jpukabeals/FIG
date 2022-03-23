# Global model for FIG experiment
# all response variables were not fit with same global model, limiting application of the function
# when the data is melted/pivoted longer, this works better
# for example, data in original format, yield.3cut breaks global model because there are no levels of follow.cut


# import data
# library(googlesheets4)
# driveurl<-"https://docs.google.com/spreadsheets/d/16qh8tlNDfsdnwmDvass7WY5EFLY_U0AzQZVDhPQMhjQ/edit#gid=0"
# dat<-read_sheet(driveurl,gs4_deauth())
# library(lme4)
# library(car)


# function to create model summary and anova 
# we use sprintf argument to insert text into the formula argument of lme4 without problems
# without sprintf, I could not figure out how to accomplish this task


global_model_1response <- function(y){
  mod_summary <- lmer(sprintf("%s ~ timing.1cut*follow.cut +
               (1|site/block/timing.1cut) + (1|env)",y),
       dat)
  
  mod_anova <- dat %>%
    lmer(sprintf("%s ~ timing.1cut*follow.cut +
               (1|site/block/timing.1cut) + (1|env)",y),.) %>%
    car::Anova()
  
  mylist <- list("summary" = mod_summary,
                 "anova" = mod_anova)
  return(mylist)
}
# global_model_1response("yield.1cut")
# global_model_1response("yield.1cut")$anova
# global_model_1response("yield.2cut")$anova
# global_model_1response("yield.3cut")$anova
# function works great for detecting anova in global model!
# for yield.3cut, there is only one level of follow.cut so the function breaks


# second function for conducting pairwise compariso
global_model_2input <- function(y,x1){
  mod_emmeans_x1 <-   dat %>%
    lmer(sprintf("%s ~ %s * follow.cut +
                 (1|site/block/timing.1cut) + (1|env)",y,x1),
         data=.) %>%
    emmeans::emmeans(sprintf("%s",x1),
                     data=dat) %>%
    multcomp::cld(Letters=letters,
                  reverse=T) %>%
    mutate(group=str_trim(.group))
  output_list <- list("emmeans_x1" = mod_emmeans_x1)
  return(output_list)
}
# global_model_1response("yield.2cut") #effect of timing.1cut
# global_model_2input("yield.2cut",
#                     "timing.1cut") #comparison of estimated means

# we can export dataframe output for graphing
# global_model_2input("yield.2cut",
#                     "timing.1cut")$emmeans_x1 %>%
#   left_join(dat,.) %>%
#   ggplot(aes(timing.1cut,yield.2cut)) +
#   stat_summary(geom="bar") +
#   geom_text(aes(label=group,
#                 vjust=-1),
#             stat = "summary")
