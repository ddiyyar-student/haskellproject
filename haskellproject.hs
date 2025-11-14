data Genre = Action | Comedy | Drama | Horror | SciFi deriving (Show, Eq) 

data Movie = Movie { 
movieId :: Int,        -- unique movie ID 
title :: String,       
genre :: Genre,        
isRented :: Bool       -- movie title -- movie genre -- True if currently rented 
} deriving (Show, Eq) 

data Customer = Customer { 
customerId :: Int,     -- unique customer ID 
name :: String,        -- customer name 
rentedMovies :: [Int]  -- list of rented movie IDs 
} deriving (Show, Eq) 

data RentalSystem = RentalSystem { 
movies :: [Movie],         -- all movies in the system 
customers :: [Customer]    -- all registered customers 
} deriving (Show)

-- "Ctrl + /" for uncommit

-- Adds a new movie to the system 
addMovie :: RentalSystem -> Movie -> RentalSystem
addMovie system newMovie = system { movies = movies system ++ [newMovie] }

-- Adds a new customer to the system 
addCustomer :: RentalSystem -> Customer -> RentalSystem
addCustomer system newCustomer = system { customers = customers system ++ [newCustomer] }

-- -- Rents a movie to a customer (returns error if movie is unavailable) 
-- rentMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem 

-- -- Returns a movie from a customer (returns error if movie is not rented) 
-- returnMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem 

-- Lists all available movies 
listAvailableMovies :: RentalSystem -> [Movie]
listAvailableMovies system = filter (\m -> not (isRented m)) (movies system)

-- Finds movies by genre using a predicate (custom higher-order function) 
findMoviesBy :: (Movie -> Bool) -> RentalSystem -> [Movie]
findMoviesBy predicate system = filter predicate (movies system)

filterByGenre :: Genre -> Movie -> Bool
filterByGenre g movie = genre movie == g

searchingByTitle :: String -> Movie -> Bool
searchingByTitle t movie = title movie == t

-- Calculates total rented movies count using functors and monads 
totalRentedMovies :: RentalSystem -> Maybe Int
totalRentedMovies system = do
    let list = filter isRented (movies system)
    return $ length list
    
main :: IO ()
main = do
    let alice = Customer { customerId = 1, name = "Alice", rentedMovies = [] }
    let bob = Customer { customerId = 2, name = "Bob", rentedMovies = [2] }
    let charlie = Customer { customerId = 3, name = "Charlie", rentedMovies = [] }
    let diana = Customer { customerId = 4, name = "Diana", rentedMovies = [7, 11] }
    let eric = Customer { customerId = 5, name = "Eric", rentedMovies = [] }
    let fiona = Customer { customerId = 6, name = "Fiona", rentedMovies = [3] }
    let george = Customer { customerId = 7, name = "George", rentedMovies = [] }
    let helen = Customer { customerId = 8, name = "Helen", rentedMovies = [15] }
    let customers = [alice, bob, charlie, diana, eric, fiona, george, helen]

    let movie1 = Movie { movieId = 1, title = "Inception", genre = SciFi, isRented = False }
    let movie2 = Movie { movieId = 2, title = "The Dark Knight", genre = Action, isRented = True }
    let movie3 = Movie { movieId = 3, title = "Oppenhaimer", genre = Drama, isRented = True }
    let movie4 = Movie { movieId = 4, title = "Spider Man: Homecoming", genre = SciFi, isRented = False }
    let movie5 = Movie { movieId = 5, title = "Rocky", genre = Action, isRented = False }
    let movie6 = Movie { movieId = 6, title = "Drammatic Film", genre = Drama, isRented = False }
    let movie7  = Movie { movieId = 7,  title = "Interstellar", genre = SciFi, isRented = True }
    let movie8  = Movie { movieId = 8,  title = "Gladiator", genre = Drama, isRented = False }
    let movie9  = Movie { movieId = 9,  title = "Mad Max: Fury Road", genre = Action, isRented = False }
    let movie10 = Movie { movieId = 10, title = "The Matrix", genre = SciFi, isRented = False }
    let movie11 = Movie { movieId = 11, title = "John Wick", genre = Action, isRented = True }
    let movie12 = Movie { movieId = 12, title = "Forrest Gump", genre = Drama, isRented = False }
    let movie13 = Movie { movieId = 13, title = "The Martian", genre = SciFi, isRented = False }
    let movie14 = Movie { movieId = 14, title = "Whiplash", genre = Drama, isRented = False }
    let movie15 = Movie { movieId = 15, title = "Avengers: Endgame", genre = Action, isRented = True }
    let movie16 = Movie { movieId = 16, title = "Tenet", genre = SciFi, isRented = False }
    let movies = [movie1, movie2, movie3, movie4, movie5, movie6, movie7, movie8, movie9, movie10, movie11, movie12, movie13, movie14, movie15, movie16]

    let system = RentalSystem { movies = movies, customers = customers }

    putStrLn "Welcome to the Diyar and Archin Functional Programming project"
    putStrLn "Menu: 1. "