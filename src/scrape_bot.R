# To scrape twitter for the prop 11 tweets at certain intervals

#### Setup ####
source("src/twitter_scrape.R")
counter = 0

#### Scrape ####

while(counter < 15){

print("Charlie Away!")
print("Scraping Twitter for #Prop11, #YESon11, and #NoOn11!")

# Scrape
prop11_tweets()
counter = counter + 1
print("All Done!")

# Waits
print("Scraping again in 5 hours ...")
Sys.sleep(3600)
print("Scraping again in 4 hours ...")
Sys.sleep(3600)
print("Scraping again in 3 hours ...")
Sys.sleep(3600)
print("Scraping again in 2 hours ...")
Sys.sleep(3600)
print("Scraping again in 1 hour ...")
Sys.sleep(3600)

}
