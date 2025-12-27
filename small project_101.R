library(tidyverse)

starwars


view(starwars)
# what is the avg height of male ?
male_height <- starwars %>% 
  filter( sex == "male") %>% 
summarise( avg_malehight = mean(height, na.rm = TRUE))
           
   
    view(male_height)
    
 # what is the average maale eight from this data?
    male_height <- starwars %>% 
      filter( sex == "male") %>% 
      select(sex , height) %>% 
      summarise( avg_malehight = mean(height, na.rm = TRUE))
    print(male_height)
                 
     view(male_height)
        
       
     # what the height for  male and female in thsi data?
     
     male_height1 <- starwars %>% 
       filter(sex %in% c("male", "female")) %>% 
        select(sex , height) 
     view(male_height1)
     print(male_height1)
       
     
     # who is few centimeters taller between male and female ?
     male_height2 <- starwars %>% 
       filter(sex %in% c("male", "female")) %>% 
       select(sex , height) %>% 
       group_by(sex) %>% 
       summarise( avg_male = mean(height, na.rm = TRUE,
                avg_female = mean(height, na.rm = TRUE)))
     view(male_height2)
     print(male_height2)
     
     ## DATA VISUALIZATION BY PRACTICE 
     library(ggplot2)
     
     ggplot(male_height2, aes(x = "", y = avg_male, fill = sex)) +
       geom_bar(stat = "identity", width = 1) +
       coord_polar("y") +
       labs(title = "Average Height by Sex", fill = "Sex") +
       theme_void()
     
     # show pie chart the percent of male and female 
     male_height2 <- male_height2 %>%
       mutate(
         percent = avg_male / sum(avg_male) * 100,
         label = paste0(sex, "\n", round(percent, 1), "%")
       )
     
     #######################################
     
     ggplot(male_height2, aes(x = "", y = avg_male, fill = sex)) +
       geom_bar(stat = "identity", width = 1, color = "white") +
       coord_polar("y") +
       geom_text(aes(label = label),
                 position = position_stack(vjust = 0.5),
                 size = 5) +
       labs(title = "Average Height by Sex", fill = "Sex") +
       theme_void()
     
 #######################################################
     
     male_height2 <- male_height2 %>%
       mutate(
         percent = avg_male / sum(avg_male) * 100,
         label = paste0(round(percent, 1), "%")
       )
     ########################################
     
     ggplot(male_height2, aes(x = "", y = avg_male, fill = sex)) +
       geom_bar(stat = "identity", width = 1, color = "white") +
       coord_polar("y") +
       geom_text(aes(label = label),
                 position = position_stack(vjust = 0.5),
                 size = 5) +
       labs(title = "Average Height by Sex", fill = "Sex") +
       theme_void()
     
     ########################################
     
     ggplot(starwars, aes(x = height))+
       geom_histogram(bins = 20)
     ########################################
    
     ggplot(data = starwars) +
       geom_point(mapping = aes(x = height, 
                                y = mass,
                                color = ))
     ########################################
     # draw scatter plot 
     
     iris
     ggplot(iris, aes(x = Sepal.Length,
                      y = Sepal.Width))+
       geom_point()
     
     ## graph histogram 
     
     ggplot(iris, aes(x = Sepal.Length))+
       geom_histogram(bins = 20)
     
     ## graph bar 
     
     ggplot (starwars, aes(x = sex))+
       geom_bar(color ="pink",
                fill = "lightblue")
     #######################################
     ggplot(starwars, aes(x = sex)) +
       geom_bar(color = "pink", fill = "lightblue") +
       geom_text( stat = "frequency distribution",
         aes(label = paste0(..count..)),
         vjust = -0.3,
         size = 5) +
       labs(title = "Star Wars Characters by Sex") +
       theme_minimal()
     
     ###############################
     starwars %>%
       count(sex) %>%
       mutate(percent = round(100 * n / sum(n), 1)) %>%
       ggplot(aes(x = sex, y = n)) +
       geom_col(color = "pink", fill = "lightblue") +
       geom_text(aes(label = paste0(n, " (", percent, "%)")),
                 vjust = -0.3,
                 size = 5) +
       labs(
         title = "Star Wars Characters by Sex",
         x = "Sex",
         y = "Count"
       ) +
       theme_minimal()
     
     #############
     starwars %>%
       filter(!is.na(sex)) %>%        # drop NA
       count(sex) %>%
       mutate(percent = round(100 * n / sum(n), 1)) %>%
       ggplot(aes(x = sex, y = n)) +
       geom_col(color = "pink", fill = "lightblue") +
       geom_text(
         aes(label = paste0(n, " (", percent, "%)")),
         vjust = -0.3,
         size = 5
       ) +
       labs(
         title = "Star Wars Characters by Sex",
         x = "Sex",
         y = "Count"
       ) +
       theme_minimal()
     
     #####
     starwars %>%
       count(sex, name = "n", .drop = TRUE) %>%
       filter(!is.na(sex)) %>%
       mutate(percent = round(100 * n / sum(n), 1)) %>%
       ggplot(aes(x = sex, y = n)) +
       geom_col(color = "pink", fill = "lightblue") +
       geom_text(aes(label = paste0(n, " (", percent, "%)")),
                 vjust = -0.3,
                 size = 5) +
       theme_minimal()
     
     
     
     # bar color 
     ggplot(starwars, aes( x = eye_color,
                       fill = eye_color))+
       geom_bar(show.legend = FALSE)+
       scale_color_brewer(palette = "dark2")
     view(starwars)
     
     
     ## box plot 
     
     ggplot(starwars, aes ( x = mass,
                        y = birth_year,
                        color = sex))+
       geom_boxplot(show.legend = FALSE)+
       scale_fill_brewer(palette = "dark2")
     
     #add titles labels 
     ggplot(data = starwars)+
       geom_point(mapping = aes(x = mass, y = height,
                                color = eye_color))+
       labs(x = " height of people",
            y = " sex of people ",
            title = " height distribution of people",
            caption = "Source : height  and eye color distribution of people 2024")+
       scale_color_brewer(palette = "dark2")
                      

