

#When is the safest time to go out? When should people walk their dogs and go bike ride?
#What sources of pollution are near you?
#5a1644

Pm.25 info should be at the top, write in red in other words- if you are explaining it to a 5 year oakland
in list of options
write the name of it 
add in picture, with different particulates
write that you can click on the map to view- it is interactive

maybe add in a panel like on weather app
that has different activites
going outside
wear a mask


~/Weekly_AQ/app_editing.R
~/Weekly_AQ/apps/app.R
~/Weekly_AQ/app_edited/app.R


#input under div-class:
style = "display:flex; align-items:center; gap:10px;",
tags$img(src = "patch1-1.svg", height = "50px", style = "display:block;"), 

rsconnect::deployApp(
  appDir = "/Users/jackyverduzco/Weekly_AQ/apps",
  appName = "apps",
  account = "roseairquality"
)
