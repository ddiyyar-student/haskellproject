data Genre = Action | Comedy | Drama | Horror | SciFi
    deriving (Show, Eq, Read)

data Movie = Movie
    { movieId  :: Int        -- unique movie ID
    , title    :: String     -- movie title
    , genre    :: Genre      -- movie genre
    , isRented :: Bool       -- True if currently rented
    } deriving (Show, Eq)

data Customer = Customer
    { customerId   :: Int     -- unique customer ID
    , name         :: String  -- customer name
    , rentedMovies :: [Int]   -- list of rented movie IDs
    } deriving (Show, Eq)

data RentalSystem = RentalSystem
    { movies    :: [Movie]     -- all movies in the system
    , customers :: [Customer]  -- all registered customers
    } deriving (Show)


-- Adds a new movie to the system
addMovie :: RentalSystem -> Movie -> RentalSystem
addMovie system newMovie =
    system { movies = movies system ++ [newMovie] }

-- Adds a new customer to the system
addCustomer :: RentalSystem -> Customer -> RentalSystem
addCustomer system newCustomer =
    system { customers = customers system ++ [newCustomer] }


-- Rents a movie to a customer (returns error if movie is unavailable)
-- rentMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem

-- Returns a movie from a customer (returns error if movie is not rented)
-- returnMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem


-- Lists all available movies
listAvailableMovies :: RentalSystem -> [Movie]
listAvailableMovies system =
    filter (\m -> not (isRented m)) (movies system)

-- Finds movies by genre using a predicate
findMoviesBy :: (Movie -> Bool) -> RentalSystem -> [Movie]
findMoviesBy predicate system =
    filter predicate (movies system)

filterByGenre :: Genre -> Movie -> Bool
filterByGenre g movie = genre movie == g

searchingByTitle :: String -> Movie -> Bool
searchingByTitle t movie = title movie == t


-- Calculates total rented movies count
totalRentedMovies :: RentalSystem -> Maybe Int
totalRentedMovies system = do
    let list = filter isRented (movies system)
    return $ length list


main :: IO ()
main = do
    let alice   = Customer 1 "Alice"   []
    let bob     = Customer 2 "Bob"     [2]
    let charlie = Customer 3 "Charlie" []
    let diana   = Customer 4 "Diana"   [7, 11]
    let eric    = Customer 5 "Eric"    []
    let fiona   = Customer 6 "Fiona"   [3]
    let george  = Cust
