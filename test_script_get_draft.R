#get_draft Test Script

#Initialize Test Information
setwd("/Users/maxkwan/Documents/Everything/College/Senior/IE332/Project/") 
source('get_draft.R')
case_name <- c('mock1','mock1_clean')
case_input <- data.frame('mock1.txt','mock1_clean.txt'), "league_id" = c(1,1))

#The correct output has either been calculated (draft position, team, id) or it has been exhaustively analyzed through our database to pick up player name
draft_position <- 1:192
team <- rep(c(paste('Team',1:12),paste('Team', 12:1)),8)
id <- rep(c(paste('l',1:12,sep ="_"),paste('l', 12:1,sep ="_")),8)
player <- c("Connor McDavid",        "Leon Draisaitl",        "Nathan MacKinnon",      "Nikita Kucherov",       "Andrei Vasilevskiy"   , "Mikko Rantanen"       
            ,"David Pastrnak"       , "Artemi Panarin"      ,  "Auston Matthews"      , "Brad Marchand"       ,  "Alex Ovechkin"       ,  "Cale Makar"           
            ,"Patrick Kane"          ,"Mitch Marner"       ,   "Jonathan Huberdeau",    "Sidney Crosby"      ,   "Sebastian Aho"       ,  "Brayden Point"        
            ,"Mika Zibanejad"        ,"Robin Lehner"       ,   "John Carlson"      ,    "Adam Fox"           ,   "Brady Tkachuk"       ,  "Aleksander Barkov"    
            ,"Andrei Svechnikov"     ,"Darcy Kuemper"      ,   "Kirill Kaprizov"   ,    "Jake Guentzel"      ,   "Victor Hedman"       ,  "Steven Stamkos"       
            ,"Connor Hellebuyck"     ,"Max Pacioretty"     ,   "Matthew Tkachuk"   ,    "Patrice Bergeron"   ,   "Alex DeBrincat"      ,  "Mark Scheifele"       
            ,"John Tavares"          ,"Igor Shesterkin"    ,   "Elias Pettersson"  ,    "Mark Stone"         ,   "Frederik Andersen"   ,  "Quinn Hughes"         
            ,"Gabriel Landeskog"     ,"Aaron Ekblad"       ,   "Alex Pietrangelo"  ,    "Kyle Connor"        ,   "Johnny Gaudreau"     ,  "J.T. Miller"          
            ,"Semyon Varlamov"       ,"Marc-Andre Fleury"  ,   "Ilya Sorokin"      ,    "Brock Boeser"       ,   "Morgan Rielly"       ,  "Charlie McAvoy"       
            ,"Evgeny Kuznetsov"      ,"Mathew Barzal"      ,   "Nikolaj Ehlers"    ,    "Carey Price"        ,   "Jack Eichel"         ,  "Roope Hintz"          
            ,"Dougie Hamilton"       ,"Nicklas Backstrom"  ,   "Cam Talbot"        ,    "Tyson Barrie"       ,   "Ilya Samsonov"       ,  "Ondrej Palat"         
            ,"Roman Josi"            ,"Ryan O'Reilly"      ,   "Evgeni Malkin"     ,    "Anze Kopitar"       ,   "Ryan Nugent-Hopkins" ,  "Philipp Grubauer"     
            ,"Seth Jones"            ,"Miro Heiskanen"     ,   "Jack Campbell"     ,    "Juuse Saros"        ,   "Alexander Radulov"   ,  "Petr Mrazek"          
            ,"Spencer Knight"        ,"Sam Reinhart"       ,   "Jason Robertson"   ,    "T.J. Oshie"         ,   "Kevin Lankinen"      ,  "Elias Lindholm"       
            ,"Shea Theodore"         ,"Kris Letang"        ,   "Nick Suzuki"       ,    "Mackenzie Blackwood",   "Martin Necas"        ,  "Jakob Chychrun"       
            ,"Jordan Binnington"     ,"Brayden Schenn"     ,   "Thatcher Demko"    ,    "Sam Bennett"        ,   "Cole Caufield"       ,  "Vladimir Tarasenko"   
            ,"Torey Krug"            ,"Darnell Nurse"      ,   "Tyler Seguin"      ,    "Andre Burakovsky"   ,   "Blake Wheeler"       ,  "Drew Doughty"         
            ,"Bryan Rust"            ,"John Klingberg"     ,   "Linus Ullmark"     ,    "Zach Werenski"      ,   "Filip Forsberg"      ,  "Sergei Bobrovsky"     
            ,"Vitek Vanecek"         ,"MacKenzie Weegar"   ,   "Jeff Petry"        ,    "Ryan Ellis"         ,   "Thomas Chabot"       ,  "William Nylander"     
            ,"David Perron"          ,"Tomas Hertl"        ,   "Sean Couturier"    ,    "Mikhail Sergachev"  ,   "Jonathan Marchessault", "Jamie Benn"           
            ,"Jared Spurgeon"        ,"Ivan Provorov"      ,   "Neal Pionk"        ,    "Ty Smith"           ,   "Jack Hughes"         ,  "Taylor Hall"          
            ,"Samuel Girard"         ,"Devon Toews"        ,   "Mathew Dumba"      ,    "Oliver Ekman-Larsson",  "Vince Dunn"          ,  "Jakub Voracek"        
            ,"Alec Martinez"         ,"Anthony DeAngelo"   ,   "Mark Giordano"     ,    "Mattias Ekholm"     ,   "Brent Burns"       ,    "Rasmus Dahlin"        
            ,"Matt Grzelcyk"         ,"Erik Karlsson"      ,   "Ryan Suter"        ,    "Justin Schultz"     ,   "Jamie Drysdale"    ,    "Jaccob Slavin"        
            ,"Jeremy Swayman"        ,"Alex Nedeljkovic"   ,   "Bo Horvat"         ,    "Kaapo Kahkonen"     ,   "Jake Oettinger"      ,  "Tom Wilson"           
            ,"Carter Hart"           ,"Ryan Strome"        ,   "Joe Pavelski"      ,    "Chris Kreider"      ,   "Brendan Gallagher"  ,   "Kevin Fiala"          
            ,"Vincent Trocheck"      ,"Dominik Kubalik"    ,   "Anthony Mantha"    ,    "Alexis Lafreniere"  ,   "Jakub Vrana"          , "Teuvo Teravainen"     
            ,"Jacob Markstrom"       ,"Patric Hornqvist"   ,   "Jean-Gabriel Pageau",   "Chris Driedger"     ,   "Carter Verhaeghe"    ,  "Viktor Arvidsson"     
            ,"Evander Kane"          ,"Pavel Buchnevich"   ,   "Oliver Bjorkstrand",    "Patrik Laine"       ,   "William Karlsson"    ,  "Tyler Toffoli"        
            ,"Zach Hyman"            ,"Cam Atkinson"       ,   "Calvin Petersen"   ,    "Elvis Merzlikins"   ,   "John Gibson"         ,  "Claude Giroux"        
            ,"Tristan Jarry"         ,"Dylan Larkin"       ,   "Sean Monahan"      ,    "Conor Garland"      ,   "Jaroslav Halak"      ,  "Joshua Norris"        
            ,"Joel Eriksson Ek"      ,"Tim Stutzle"        ,   "Jaden Schwartz"    ,    "Anders Lee"         ,   "Trevor Zegras"       ,  "Jeff Carter"  )
pos <- c("F","F","F","F","G","F","F","F","F","F","F","D","F","F","F","F","F","F","F","G","D","D","F","F","F","G",
         "F","F","D","F","G","F","F","F","F","F","F","G","F","F","G","D","F","D","D","F","F","F","G","G","G","F",
         "D","D","F","F","F","G","F","F","D","F","G","D","G","F","D","F","F","F","F","G","D","D","G","G","F","G",
         "G","F","F","F","G","F","D","D","F","G","F","D","G","F","G","F","F","F","D","D","F","F","F","D","F","D",
         "G","D","F","G","G","D","D","D","D","F","F","F","F","D","F","F","D","D","D","D","F","F","D","D","D","D",
         "D","F","D","D","D","D","D","D","D","D","D","D","D","D","G","G","F","G","G","F","G","F","F","F","F","F",
         "F","F","F","F","F","F","G","F","F","G","F","F","F","F","F","F","F","F","F","F","G","G","G","F","G","F",
         "F","F","G","F","F","F","F","F","F","F")
mock1_output <- data.frame(draft_position,team, id, player,pos)

case_output <- (mock1_output)
test_cases <- data.frame(case_name,case_input,case_output)
test_results <- paste("",length(case_name))

#Conduct Broad Test
for (i in i:length(case_name)) {
  test_output <- get_draft(paste(case_input[i,]))[,c(5:7)]
  if (case_output == test_output) {
    test_results[i] = TRUE
  } else {
    test_results[i] = FALSE
  }
}

#Results
results <- cbind(case_name,test_results)