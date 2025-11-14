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

-- -- Lists all available movies 
-- listAvailableMovies :: RentalSystem -> [Movie] 

-- -- Finds movies by genre using a predicate (custom higher-order function) 
-- findMoviesBy :: (Movie -> Bool) -> RentalSystem -> [Movie] 

-- -- Calculates total rented movies count using functors and monads 
-- totalRentedMovies :: RentalSystem -> Maybe Int

main :: IO ()
main = do
    putStrLn "Success?"