#install.packages("devtools")
library(devtools)
#install_github("Rfacebook", "pablobarbera", subdir="Rfacebook")
require("Rfacebook")
#671755522900353
#1803067243268561
fb_oauth <- fbOAuth(app_id="1803067243268561", app_secret="2c0945e50ee5a4936c87cc3f09b4918f",extended_permissions = TRUE)
save(fb_oauth, file="fb_oauth")
load("fb_oauth")
me <- getUsers("me",token=fb_oauth)
my_likes <- getLikes(user="me", token=fb_oauth)
dude <- getFriends(token=fb_oauth, simplify = FALSE)
#updateStatus("This is me Posting from my Analytics App",token=fb_oauth)
# View(searchPagesGay)
# searchDapl <-searchGroup(string="cubs",token=fb_oauth, n = 1000)
# #getPage(pageid , token, n = n)
# pages <- searchPages( string="cubs", token=fb_oauth, n=1000)
# map('usa') # create a map 
# with(filter(pages), points(longitude, latitude))
