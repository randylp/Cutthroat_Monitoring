library(rdrop2)


#Authenticates so that you can access dropbox from Shiny Servers
token <- drop_auth()
#Update to the filepath on the user's computer
saveRDS(token, "C:\\Users\\dappdrd\\Desktop\\Cutthroat\\droptoken.rds")

