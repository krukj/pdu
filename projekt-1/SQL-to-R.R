### Przetwarzanie Danych Ustrukturyzowanych 2023L
### Praca domowa nr 3
###
### UWAGA:
### nazwy funkcji oraz ich parametrow powinny pozostac niezmienione.
###  
### Wskazane fragmenty kodu przed wyslaniem rozwiazania powinny zostac 
### zakomentowane
###

# -----------------------------------------------------------------------------#
# Wczytanie danych oraz pakietow.
# !!! Przed wyslaniem zakomentuj ten fragment
# Posts <- read.csv("/Users/julia/Desktop/R/travel_stackexchange_com/Posts.csv")
# Comments <- read.csv("/Users/julia/Desktop/R/travel_stackexchange_com/Comments.csv")
# Users <- read.csv("/Users/julia/Desktop/R/travel_stackexchange_com/Users.csv")
# install.packages(c("sqldf", "dplyr", 'data.table'))
# head(Users)
# head(Posts)
# head(Comments)
# library(sqldf)
# library(dplyr)
# library(data.table)
# -----------------------------------------------------------------------------#




# -----------------------------------------------------------------------------#
# Zadanie 1
# -----------------------------------------------------------------------------#

# 10 lokalizacji z najwieksza liczba UpVotes

sql_1 <- function(Users){
  
  sqldf("SELECT Location, SUM(UpVotes) as TotalUpVotes FROM Users
  WHERE Location != ''
  GROUP BY Location
  ORDER BY TotalUpVotes DESC LIMIT 10")
  
}


base_1 <- function(Users){

    # wybranie niepustych wynikow z kolumny Location
    x <- Users[Users$Location != "", ] 
    
    # usuniecie wynikow, w ktorych UpVotes jest NA
    x <- x[!is.na(x$UpVotes), ]

    # zsumowanie UpVotes po lokalizacji
    x <- aggregate(x$UpVotes,
                   by = x["Location"],
                   FUN = sum)

    # zmiana nazwy kolumny
    colnames(x)[2] <- "TotalUpVotes" 
    
    # posegregowanie wg TotalUpVotes
    x <- x[order(x$TotalUpVotes, decreasing = TRUE), ]
    
    # zmiana nazw wierszy aby numeracja byla od 1
    rownames(x) <- NULL
    
    # wybranie pierwszych 10 wynikow
    x <- x[1:10, ] 
    
}


dplyr_1 <- function(Users){

  x <- Users %>%
    # usuniecie wynikow z pusta lokalizacja
    filter(Location != "") %>%  

    # pogrupowanie wg lokalizacji
    group_by(Location) %>% 
    
    # zsumowanie UpVotes, nazwanie kolumny TotalUpVotes
    summarise(TotalUpVotes = sum(UpVotes)) %>% 

    # posegregowanie malejaco
    arrange(desc(TotalUpVotes)) %>% 

    # wybranie 10 pierwszych wynikow
    slice_head(n = 10)

    # zwrocenie jako ramka danych
    as.data.frame(x)

}

table_1 <- function(Users){

    # zapisanie Users jako data table
    users_dt <- as.data.table(Users)
    
    # usuniecie wynikow z pusta lokalizacja
    x <- users_dt[Location != '']
    
    # zsumowanie UpVotes, nazwanie kolumny TotalUpVotes,
    # pogrupowanie wg lokalizacji
    x <- x[, .(TotalUpVotes = sum(UpVotes)),
           by = Location]
    
    # posegregowanie malejaco
    x <- x[order(TotalUpVotes, decreasing = TRUE)]

    # wybranie 10 pierwszych wynikow
    x <- x[1:10]
    
    # zwrocenie jako ramka danych
    as.data.frame(x)
}
# # Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# 
# all_equal(sql_1(Users), base_1(Users))
# all_equal(sql_1(Users), dplyr_1(Users))
# all_equal(sql_1(Users), table_1(Users))
# 
# # Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
# microbenchmark::microbenchmark(
#   sqldf = sql_1(Users),
#   base = base_1(Users),
#   dplyr = dplyr_1(Users),
#   data.table = table_1(Users) )

# Unit: milliseconds
# expr        min        lq      mean    median        uq       max neval
# sqldf 179.716653 181.28037 185.36041 181.82696 182.93335 422.54231   100
# base  98.563467  99.04835 104.94115  99.48853 101.23277 145.85520   100
# dplyr  23.803903  24.22159  26.22966  24.40500  24.77060  65.56909   100
# data.table   9.909864  10.10330  11.15586  10.26816  10.52175  50.08257   100
  
# -----------------------------------------------------------------------------#
# Zadanie 2
# -----------------------------------------------------------------------------#

# liczba postow o PostId 1 lub 2 i maksymalny wynik w danym miesiacu, 
# w ktorym bylo wiecej niz 1000 postow

sql_2 <- function(Posts){
  
  sqldf("SELECT 
    STRFTIME('%Y', CreationDate) AS Year,
    STRFTIME('%m', CreationDate) AS Month,
    COUNT(*) AS PostsNumber,
    MAX(Score) AS MaxScore
  FROM Posts
  WHERE PostTypeId IN (1, 2)
  GROUP BY Year, Month HAVING PostsNumber > 1000 ")
  
}

base_2 <- function(Posts){

    # wybranie wynikow, gdzie PostTypeId = 1 lub 2
    x <- subset(Posts, PostTypeId %in% c(1, 2))

    # wybranie oddzielnie roku i miesiaca i dodanie do x
    x["Year"] <- substring(x$CreationDate, 1, 4)
    x["Month"] <- substring(x$CreationDate, 6, 7)
    
    # posegregowanie x wg liczby wystapien i wybranie tylko kolumn Year i Month
    x1 <- aggregate(x$Score, 
                    by = x[, c("Year", "Month")],
                    FUN = length)
    
    # zmiana nazwy kolumny
    colnames(x1)[3] <- "PostsNumber"
    
    # wybranie wynikow, w ktorych PostsNumber > 1000
    x1 <- x1[x1$PostsNumber > 1000, ]
    
    # usuniecie nazw wierszy aby byly numerowane od 1
    rownames(x1) <- NULL
    
    # wybranie max ze Score
    x2 <- aggregate(x$Score,
                    by = x[, c("Year", "Month")],
                    FUN = max)
    
    # zmiana nazwy kolumny
    colnames(x2)[3] <- "MaxScore"
    
    # polaczenie dwoch ramek
    x <- merge(x1, x2,
               by = c("Year", "Month"))
    
    
}
  
    

dplyr_2 <- function(Posts){

  x <- Posts %>%
    # wybranie wynikow z PostTypeId = 1 lub 2
    filter(PostTypeId %in% c(1, 2)) %>%
    
    # dodanie kolumn Year i Month
    mutate(
      Year = substring(CreationDate, 1, 4),
      Month = substring(CreationDate, 6, 7)) %>%
    
    # wybranie kolumn Year, Month, Score
    select(Year, Month, Score) %>%
    
    # pogrupowanie wg Year i Month
    group_by(Year, Month) %>%
    
    # zsumowanie PostsNumber, wybranie max x MaxScore bez wartosci NA
    reframe(
      PostsNumber = length(Score),
      MaxScore = max(Score, na.rm = TRUE)) %>%
    
    # wybranie tych wynikow, gdzie PostsNumber > 1000
    filter(PostsNumber > 1000) 
    
    # zwrocenie jako ramka danych
    as.data.frame(x)

}

table_2 <- function(Posts){

    # zapisanie Posts jako data table
    posts_dt <- as.data.table(Posts)
    
    # wybranie wynikow dla ktorych PostTypeId = 1 lub 2 i kolumn CreationDate
    # i Score
    x <- posts_dt[PostTypeId %in% c(1, 2), .(CreationDate, Score)]
    
    # rozdzielenie roku i miesiaca
    year <- substring(x$CreationDate, 1, 4)
    month <- substring(x$CreationDate, 6, 7)
    
    # stworzenie kolumn Year i Month
    x <- x[, c("Year", "Month") := .(year, month)]
    
    # zliczenie liczby postow i max Score dla poszczegolnych miesiecy
    x <- x[, .(PostsNumber = length(Score), MaxScore = max(Score)), 
           by = .(Year, Month)]
    
    # wybranie wynikow, gdzie PostsNumber > 1000
    x <- x[PostsNumber > 1000]
    
    # zwrocenie jako ramka danych
    as.data.frame(x)
}

# # Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem
# 
# all_equal(sql_2(Posts), base_2(Posts))
# all_equal(sql_2(Posts), dplyr_2(Posts))
# all_equal(sql_2(Posts), table_2(Posts))
# 
# # Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
# microbenchmark::microbenchmark(
#   sqldf = sql_2(Posts),
#   base = base_2(Posts),
#   dplyr = dplyr_2(Posts),
#   data.table = table_2(Posts) )

# Unit: milliseconds
# expr       min        lq      mean    median        uq       max neval
# sqldf 723.65660 736.22306 743.20976 738.86574 741.01116 792.70884   100
# base 137.60363 139.06189 171.03384 179.41588 183.14171 371.42716   100
# dplyr  31.50362  32.04187  42.54039  32.28293  32.59881 269.15319   100
# data.table  19.99070  20.21140  26.69108  20.42790  21.01605  77.37393   100

# -----------------------------------------------------------------------------#
# Zadanie 3
# -----------------------------------------------------------------------------#

# 10 uzytkownikow (ich Id i DisplayName) majacych najwiecej Views dla
# postow o PostTypeId 1

sql_3 <- function(Posts, Users){

  # zwraca ViewCount dla postow o PostTypeId = 1 dla uzytkownika o danym Id 
  Questions <- sqldf( 'SELECT OwnerUserId, SUM(ViewCount) as TotalViews
                                      FROM Posts
                                      WHERE PostTypeId = 1
                                      GROUP BY OwnerUserId' )
  
  # zwraca Id, DisplayName i TotalViews dla 10 uzytkownikow o najwiekszej
  # wartosci TotalViews
  sqldf( "SELECT Id, DisplayName, TotalViews
                FROM Questions
                JOIN Users
                ON Users.Id = Questions.OwnerUserId
                ORDER BY TotalViews DESC
                LIMIT 10")
}

base_3 <- function(Posts, Users){

    # wybranie wynikow, w ktorych PostTypeId = 1
    questions <- subset(Posts, Posts$PostTypeId == 1)
    
    # zsumowanie ViewCount dla danego OwnerUserId
    questions <- aggregate(questions$ViewCount,
                           by = questions["OwnerUserId"],
                           FUN = sum)
    
    # zmiana nazwy kolumny
    colnames(questions)[2] <- "TotalViews"
    
    # zlaczenie questions i Users tam, gdzie OwnerUserId = Id
    x <- merge(Users, questions, 
               by.x = "Id", by.y = "OwnerUserId") 
    
    # wybranie niektorych kolumn
    x <- x[c("Id", "DisplayName", "TotalViews")]
    
    # posegregowanie malejaco wg TotalViews
    x <- x[order(x$TotalViews, decreasing = TRUE), ]
    
    # zmiana numeracji wierszy aby byly od 1
    rownames(x) <- NULL
    
    # wybranie 10 pierwszych wynikow
    x <- x[1:10, ]
  
}

dplyr_3 <- function(Posts, Users){
    
    x <- Posts %>%
      
      # wybranie wynikow, w ktorych PostTypeId = 1
      filter(PostTypeId == 1) %>%
      
      # pogrupowanie wg OwnerUserId
      group_by(OwnerUserId) %>%
      
      # zsumowanie ViewsCount dla danego OwnerUserId
      summarize(TotalViews = sum(ViewCount)) %>%
      
      # zlaczenie z Users tam, gdzie OwnerUserId = Id
      inner_join(Users, by = c("OwnerUserId" = "Id")) %>%
      
      # wybranie niektorych kolumn
      select(Id = OwnerUserId, DisplayName, TotalViews) %>%
      
      # posegregowanie malejaco po TotalViews
      arrange(desc(TotalViews)) %>%
      
      # wybranie 10 pierwszych wynikow
      slice_head(n = 10)
    
    # zwrocenie jako ramka danych
    as.data.frame(x)
}

table_3 <- function(Posts, Users){

    # zapisanie Posts jako data table
    posts_dt <- as.data.table(Posts)
    
    # wybranie wynikow, dla ktorych PostTypeId = 1
    questions <- posts_dt[PostTypeId == 1]
    
    # zsumowanie ViewCount dla danego OwnerUserId
    questions <- questions[, sum(ViewCount), by = OwnerUserId]
    
    # zmiana nazwy kolumny
    colnames(questions)[2] <- "TotalViews"
    
    # zapisanie Users jako data table
    users_dt <- as.data.table(Users)
    
    # zlaczenie users i q tam, gdzie Id = OwnerUserId
    x <- merge(users_dt[, .(Id, DisplayName)], 
               questions,
               by.x = "Id", 
               by.y = "OwnerUserId")
    
    # posegregowanie malejaco po TotalViews
    x <- x[order(TotalViews, decreasing = TRUE)]
    
    # wybranie 10 pierwszych wynikow
    x <- x[1:10]
    
    # zwrocenie jako ramka danych
    as.data.frame(x)

}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# all_equal(sql_3(Posts, Users), base_3(Posts, Users))
# all_equal(sql_3(Posts, Users), dplyr_3(Posts, Users))
# all_equal(sql_3(Posts, Users), table_3(Posts, Users))
# 
# # Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
# microbenchmark::microbenchmark(
#   sqldf = sql_3(Posts, Users),
#   base = base_3(Posts, Users),
#   dplyr = dplyr_3(Posts, Users),
#   data.table = table_3(Posts, Users) )

# Unit: milliseconds
# expr       min        lq      mean    median        uq        max neval
# sqldf 831.14884 844.24074 858.17413 849.23312 856.44133 1093.07525   100
# base 247.76759 249.44127 271.23133 250.85413 291.92955  484.75034   100
# dplyr  67.89223  69.03457  75.03330  69.33168  69.98600  122.22227   100
# data.table  23.67119  23.87506  30.60776  24.02047  25.07759   77.76585   100

# -----------------------------------------------------------------------------#
# Zadanie  4
# -----------------------------------------------------------------------------#


# 5 uzytkownikow (ich DisplayName, QuestionsNumber, AnswersNumber, Location
# Reputation, UpVotes, DownVotes) o najwiekszej liczbie odpowiedzi

sql_4 <- function(Posts, Users){

    # 
    # zwraca liczbe odpowiedzi na posty o PostTypeId = 2 dla danego uzytkownika
    Answers <- sqldf("
      SELECT COUNT(*) as AnswersNumber, OwnerUserId
      FROM Posts
      WHERE PostTypeId = 2
      GROUP BY OwnerUserId")
    
    
    # zwraca liczbe pytan do postow o PostTypeId = 2 dla danego uzytkownika
    Questions <- sqldf("
      SELECT COUNT(*) as QuestionsNumber, OwnerUserId
      FROM Posts
      WHERE PostTypeId = 1
      GROUP BY OwnerUserId")
    
    
    # zlaczenie kolumn z liczba odpowiedzi i liczba pytan dla danego uzytkownika
    # tam, gdzie odpowiedzi jest wiecej niz pytan,
    # sortowanie malejaco po AnswersNumber
    # 5 pierwszych wynikow
    PostsCounts <- sqldf("
      SELECT Answers.AnswersNumber, Answers.OwnerUserId, 
      Questions.QuestionsNumber
      FROM Answers
      JOIN Questions
      ON Answers.OwnerUserId = Questions.OwnerUserId
      WHERE AnswersNumber > QuestionsNumber
      ORDER BY AnswersNumber DESC
      LIMIT 5")
    
    # zlaczenie PostsCounts i Users dla danego Id uzytkownika
    # wybranie niektorych kolumn 
    sqldf("
    SELECT DisplayName, QuestionsNumber, AnswersNumber,
    Location, Reputation, UpVotes, DownVotes 
    FROM PostsCounts
    JOIN Users
    ON PostsCounts.OwnerUserId = Users.Id")

}

base_4 <- function(Posts, Users){

    # wybranie wynikow, w ktorych PostTypeId = 2
    answers <- subset(Posts, Posts$PostTypeId == 2)
    
    # zliczenie wynikow dla danego OwnerUserId
    answers <- aggregate(answers$OwnerUserId, by = answers["OwnerUserId"],
                         FUN = length)
    
    # zamiana kolejnosci kolumn, zmiana nazwy
    answers <- answers[c(2, 1)]
    colnames(answers)[1] <- "AnswersNumber"
    
    # wybranie wynikow, w ktorych PostTypeId = 1
    questions <- subset(Posts, Posts$PostTypeId == 1)
    
    # zliczenie wynikow dla danego OwnerUserId
    questions <- aggregate(questions$OwnerUserId,
                           by = questions["OwnerUserId"],
                           FUN = length)
    
    questions <- questions[c(2, 1)]
    colnames(questions)[1] <- "QuestionsNumber"
    
    
    # zlaczenie answers i questions wg OwnerUserId
    postscounts <- merge(answers, questions, by = "OwnerUserId")
    
    # wybranie wynikow w ktorych AnswersNumber > QuestionsNumber
    postscounts <- subset(postscounts,
                          postscounts$AnswersNumber > postscounts$QuestionsNumber)
    
    # posortowanie malejaco po kolumnie AnswersNumber
    postscounts <- postscounts[order(postscounts$AnswersNumber,
                                     decreasing = TRUE), ]
    
    # wybranie 5 wynikow
    postscounts <- postscounts[1:5, ]
    
    # zmiana numeracji wierszy
    rownames(postscounts) <- NULL
    
    # zlaczenie postscounts i Users tam, gdzie OwnerUserId = Id
    x <- merge(postscounts, Users,
               by.x = "OwnerUserId", by.y = "Id")
    
    # wybranie niektorych kolumn
    x <- x[c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location",
             "Reputation", "UpVotes", "DownVotes")]
    
    # posortowanie malejaco wg AnswersNumber
    x <- x[order(x$AnswersNumber, decreasing = TRUE), ]

    
}

dplyr_4 <- function(Posts, Users){
 
  answers <- Posts %>%
    # wybranie wynikow z PostTypeId = 2
    filter(PostTypeId == 2) %>%
    
    # pogrupowanie wg OwnerUserId
    group_by(OwnerUserId) %>%
    
    # zliczenie liczby odpowiedzi dla danego OwnerUserId
    summarise(AnswersNumber = length(OwnerUserId))
    
    
  questions <- Posts %>%
    # wybranie wynikow z PostTypeId = 1
    filter(PostTypeId == 1) %>%
    
    # pogrupowanie wg OwnerUserId
    group_by(OwnerUserId) %>%
    
    # zliczenie liczby pytan dla danego OwnerUserId
    summarise(QuestionsNumber = length(OwnerUserId))
  

  posts <- 
      # zlaczenie a i q po OwnerUserId
      inner_join(answers, questions,
                 by = "OwnerUserId", na_matches = "never") %>%
    
      # wybranie tych wynikow, gdzie AnswersNumber > QuestionsNumber
      filter(AnswersNumber > QuestionsNumber) %>%
    
      # posortowanie malejaco po AnswersNumber
      arrange(desc(AnswersNumber)) %>%
    
      # wybranie pierwszych 5 wynikow
      slice_head(n = 5)
  
  x <-
    # zlaczenie p i Users tam, gdzie OwnerUserId = Id
    inner_join(posts, Users, by = c("OwnerUserId" = "Id")) %>%
    
    # wybranie niektorych kolumn
    select(DisplayName, QuestionsNumber, AnswersNumber, 
           Location, Reputation, UpVotes, DownVotes) %>%
    
    # posortowanie malejaco po AnswersNumber
    arrange(desc(AnswersNumber))
  
    # zwrocenie jako ramka danych
    as.data.frame(x)
}

table_4 <- function(Posts, Users){

  # zapisanie Posts jako data table
  posts_dt <- as.data.table(Posts)
  
  # wybranie tych wynikow, gdzie PostTypeId = 2
  answers <- posts_dt[PostTypeId == 2]
  
  # zliczenie liczby odpowiedzi dla danego OwnerUserId
  answers <- answers[, .(AnswersNumber = length(PostTypeId)),
                     by = OwnerUserId]
  
  # wybranie tych wynikow, gdzie PostTypeId = 1
  questions <- posts_dt[PostTypeId == 1]
  
  # zliczenie liczby pytan dla danego OwnerUserId
  questions <- questions[, .(QuestionsNumber = length(PostTypeId)),
                         by = OwnerUserId]
  
  
  # zlaczenie a i q wg OwnerUserId, uminiecie wartosci NA
  posts <- na.omit(merge(answers, questions, by = "OwnerUserId"))
  
  # wybranie tych wynikow, gdzie AnswersNumber > QuestionsNumber
  posts <- posts[AnswersNumber > QuestionsNumber]
  
  # posortowanie malejaco wg AnswersNumber
  posts <- posts[order(AnswersNumber, decreasing = TRUE)]
  
  # wybranie pierwszych 5 wynikow
  posts <- posts[1:5]
  
  # zapisanie Users jako data table
  users_dt <- as.data.table(Users)
  
  # zlaczenie p i users tam, gdzie OwnerUserId = Id
  x <- merge(posts, users_dt[, .(Id, DisplayName, Location, Reputation, 
                          UpVotes, DownVotes)], 
             by.x = "OwnerUserId", by.y = "Id")
  
  # zamiana kolejnosci kolumn
  x <- x[, c("DisplayName", "QuestionsNumber", "AnswersNumber", "Location",
           "Reputation", "UpVotes", "DownVotes")]
  
  # zwrocenie jako ramka danych
  as.data.frame(x)
  
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# all_equal(sql_4(Posts, Users), base_4(Posts, Users))
# all_equal(sql_4(Posts, Users), dplyr_4(Posts, Users))
# all_equal(sql_4(Posts, Users), table_4(Posts, Users))
# 
# # Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
# microbenchmark::microbenchmark(
#   sqldf = sql_4(Posts, Users),
#   base = base_4(Posts, Users),
#   dplyr = dplyr_4(Posts, Users),
#   data.table = table_4(Posts, Users) )
# 
# # Unit: milliseconds
# # expr        min         lq       mean     median         uq       max neval
# sqldf 1515.17111 1530.93863 1556.17024 1542.33941 1577.26145 1796.9968   100
# base  329.11077  331.77657  356.79688  367.18128  373.79038  591.8213   100
# dplyr   95.35423   96.07411  111.00585   97.04649  137.33161  164.7730   100
# data.table   56.11313   57.54528   72.64252   59.44211   99.55958  132.1918   100

# -----------------------------------------------------------------------------#
# Zadanie 5
# -----------------------------------------------------------------------------#

# 10 postow z najwiekszym CommentsTotalScore

sql_5 <- function(Posts, Comments, Users){

  # zsumowanie Score dla danego PostId
  CmtTotScr <- sqldf('SELECT PostId, SUM(Score) AS CommentsTotalScore
                        FROM Comments
                        GROUP BY PostId')
  
  # zlaczenie CmtTotScr i Posts tam, gdzie Id = PostId i tam,
  # gdzie PostTypeId = 1, wybranie niektorych kolumn
  
  PostsBestComments <- sqldf('
        SELECT Posts.OwnerUserId, Posts.Title, Posts.CommentCount,
                Posts.ViewCount, CmtTotScr.CommentsTotalScore
        FROM CmtTotScr
        JOIN Posts ON Posts.Id = CmtTotScr.PostId
        WHERE Posts.PostTypeId=1')
  
  
  # zlaczenie PostsBestComments i Users tam, gdzie OwnerUserId = Id,
  # posegregowanie malejaco po CommentsTotalScore i wybranie 10 
  # pierwszych wynikow
  
  sqldf('SELECT Title, CommentCount, ViewCount, CommentsTotalScore,
    DisplayName, Reputation, Location
            FROM PostsBestComments
            JOIN Users ON PostsBestComments.OwnerUserId = Users.Id
            ORDER BY CommentsTotalScore DESC
            LIMIT 10')
}

base_5 <- function(Posts, Comments, Users){

    # zsumowanie Score dla danego PostId
    CmtTotScr <- aggregate(Comments$Score, 
                           by = Comments["PostId"],
                           FUN = sum)
    # zmiana nazwy kolumny
    colnames(CmtTotScr)[2] <- "CommentsTotalScore"
    
    # wybranie wynikow, gdzie PostTypeId == 1
    PostsBestComments <- subset(Posts,
                                Posts$PostTypeId == 1)
    
    # zlaczenie CommentsTotalScore i PostsBestComments tam, gdzie
    # PostId == Id
    PostsBestComments <- merge(CmtTotScr, PostsBestComments,
                               by.x = "PostId", by.y = "Id")
    
    # wybranie niektorych kolumn
    PostsBestComments <- PostsBestComments[c("OwnerUserId", "Title",
                                             "CommentCount", "ViewCount", 
                                             "CommentsTotalScore")]
    
    # zlaczenie PostsBestComments i Users tam, gdzie OwnerUserId == Id
    x <- merge(PostsBestComments, Users,
               by.x = "OwnerUserId", by.y = "Id")
    
    # wybranie niektorych kolumn
    x <- x[c("Title", "CommentCount", "ViewCount", "CommentsTotalScore",
             "DisplayName", "Reputation", "Location")]
    
    # posortowanie malejaco wg CommentsTotalScore
    x <- x[order(x$CommentsTotalScore, decreasing = TRUE), ]
    
    # wybranie 10 pierwszych wynikow
    x <- x[1:10, ]
    
}

dplyr_5 <- function(Posts, Comments, Users){
    # 
    comments <- Comments %>%
      # pogrupowanie wg PostId
      group_by(PostId) %>%
      
      # zsumowanie Score dla danego PostId 
      summarise(CommentsTotalScore = sum(Score))
    
      
    posts <- Posts %>%
      
      # wybranie tych wynikow, dla ktorych PostTypeId = 1
      filter(PostTypeId == 1) %>%
      
      # zlaczenie z c tam, gdzie Id = PostId
      inner_join(comments, by = c("Id"="PostId")) %>%
      
      # wybranie niektorych kolumn
      select(OwnerUserId, Title,
             CommentCount, ViewCount, 
             CommentsTotalScore)
  

    # zlaczenie p i Users tam, gdzie OwnerUserId = Id
    x <- inner_join(posts, Users,
                    by = c("OwnerUserId" = "Id")) %>%
      
      # wybranie niektorych kolumn
      select(Title, CommentCount, ViewCount, CommentsTotalScore,
             DisplayName, Reputation, Location) %>%
      
      # posegregowanie malejaco po CommentsTotalScore
      arrange(desc(CommentsTotalScore)) %>%
      
      # wybranie 10 pierwszych wynikow
      slice_head(n = 10)
    
    # zwrocenie jako ramka danych
    as.data.frame(x)
    
    
}

table_5 <- function(Posts, Comments, Users){
    
    # zapisanie Comments jako data table
    comments_dt <- as.data.table(Comments)
    
    # zsumowanie Score dla kazdego PostId
    comments <- comments_dt[, sum(Score), by = PostId]
    
    # zmiana nazwy kolumny
    colnames(comments)[2] <- "CommentsTotalScore"
    
    # zapisanie Posts jako data table
    posts_dt <- as.data.table(Posts)
    
    # wybranie tych wynikow, gdzie PostTypeId = 1
    posts <- posts_dt[PostTypeId == 1]
    
    # zlaczenie p i c tam, gdzie Id = PostId
    posts <- merge(posts[, .(Id, OwnerUserId, Title, CommentCount, ViewCount)],
                   comments,
               by.x = "Id", by.y = "PostId")
    
    # zapisanie Users jako data table
    users_dt <- as.data.table(Users)
    
    # zlaczenie p i users tam, gdzie Id = OwnerUserId
    x <- merge(users_dt[, .(Id, DisplayName, Reputation, Location)],
               posts,
               by.x = "Id", by.y = "OwnerUserId")
    
    # posegregowanie malejaco po CommentsTotalScore
    x <- x[order(CommentsTotalScore, decreasing = TRUE)]
    
    # wybranie 10 pierwszych wynikow
    x <- x[1:10]
    
    # zamiana kolejnosci kolumn
    x <- x[, c("Title", "CommentCount", "ViewCount", "CommentsTotalScore", 
               "DisplayName", "Reputation", "Location")]

    # zamiana na ramke danych
    as.data.frame(x)
}

# Sprawdzenie rownowaznosci wynikow - zakomentuj te czesc przed wyslaniem

# all_equal(sql_5(Posts, Comments, Users), base_5(Posts, Comments, Users))
# all_equal(sql_5(Posts, Comments, Users), dplyr_5(Posts, Comments, Users))
# all_equal(sql_5(Posts, Comments, Users), table_5(Posts, Comments, Users))
# 
# # Porowanie czasow wykonania - zakomentuj te czesc przed wyslaniem
# 
# microbenchmark::microbenchmark(
#   sqldf = sql_5(Posts, Comments, Users),
#   base = base_5(Posts, Comments, Users),
#   dplyr = dplyr_5(Posts, Comments, Users),
#   data.table = table_5(Posts, Comments, Users) )

# Unit: milliseconds
# expr        min         lq       mean     median         uq       max neval
# sqldf 1286.82497 1305.13342 1318.79715 1311.21559 1318.93335 1462.4607   100
# base  707.53462  748.75904  760.41739  754.10125  760.23225 1024.5756   100
# dplyr  169.87788  179.40909  206.97707  215.99958  222.82951  264.1477   100
# data.table   52.49907   53.32517   74.27067   54.89025   96.72449  332.8137   100

