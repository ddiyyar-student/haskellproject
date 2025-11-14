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

-- -- Calculates total rented movies count using functors and monads 
-- totalRentedMovies :: RentalSystem -> Maybe Int

main :: IO ()
main = do
    let alice = Customer { customerId = 1, name = "Alice", rentedMovies = [] }
    let bob = Customer { customerId = 2, name = "Bob", rentedMovies = [2] }
    let charlie = Customer { customerId = 3, name = "Charlie", rentedMovies = [] }
    let customers = [alice, bob, charlie]

    let movie1 = Movie { movieId = 1, title = "Inception", genre = SciFi, isRented = False }
    let movie2 = Movie { movieId = 2, title = "The Dark Knight", genre = Action, isRented = True }
    let movies = [movie1, movie2]

    let system = RentalSystem { movies = movies, customers = customers }

    print $ listAvailableMovies (addMovie system (Movie { movieId = 3, title = "Interstellar", genre = SciFi, isRented = False }))
    