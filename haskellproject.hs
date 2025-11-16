import Data.List (find)
data Genre = Action | Comedy | Drama | Horror | SciFi
    deriving (Show, Eq, Read)

data Movie = Movie
    { movieId  :: Int        -- unique movie ID
    , title    :: String     -- movie title
    , genre    :: Genre      -- movie genre
    , isRented :: Bool       -- True if currently rented
    , rating :: Maybe [Int]      -- Movie rating
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
rentMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem
rentMovie system cId mId = do
    -- Find customer
    customer <- maybe (Left "Error: Customer not found.") Right $
        find (\c -> customerId c == cId) (customers system)

    -- Find movie
    movie <- maybe (Left "Error: Movie not found.") Right $
        find (\m -> movieId m == mId) (movies system)

    -- Check if movie already rented
    if isRented movie
        then Left "Error: Movie is already rented."
        else do
            -- Updated movie: mark as rented
            let updatedMovie = movie { isRented = True }
            let updatedMovies = map (\m -> if movieId m == mId then updatedMovie else m) (movies system)

            -- Updated customer: add movie ID to their list
            let updatedCustomer = customer { rentedMovies = rentedMovies customer ++ [mId] }
            let updatedCustomers = map
                    (\c -> if customerId c == cId then updatedCustomer else c)
                    (customers system)

            Right $ system { movies = updatedMovies, customers = updatedCustomers }

-- Returns a movie from a customer (returns error if movie is not rented)
returnMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem
returnMovie system cId mId = do
    -- Find customer
    customer <- maybe (Left "Error: Customer not found.") Right $
        find (\c -> customerId c == cId) (customers system)

    -- Check if customer has rented this movie
    if mId `notElem` rentedMovies customer
        then Left "Error: This customer did not rent that movie."
        else do
            -- Find movie
            movie <- maybe (Left "Error: Movie not found.") Right $
                find (\m -> movieId m == mId) (movies system)

            -- Updated movie: mark as returned
            let updatedMovie = movie { isRented = False }
            let updatedMovies = map (\m -> if movieId m == mId then updatedMovie else m) (movies system)

            -- Updated customer: remove movie ID
            let updatedCustomer = customer { rentedMovies = filter (/= mId) (rentedMovies customer) }
            let updatedCustomers = map
                    (\c -> if customerId c == cId then updatedCustomer else c)
                    (customers system)

            Right $ system { movies = updatedMovies, customers = updatedCustomers }


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
    let george  = Customer 7 "George"  []
    let helen   = Customer 8 "Helen"   [15]

    let customers =
            [ alice, bob, charlie, diana, eric, fiona, george, helen ]

    let movie1  = Movie 1  "Inception"              SciFi  False Nothing
    let movie2  = Movie 2  "The Dark Knight"        Action True Nothing
    let movie3  = Movie 3  "Oppenhaimer"            Drama  True Nothing
    let movie4  = Movie 4  "Spider Man: Homecoming" SciFi  False Nothing
    let movie5  = Movie 5  "Rocky"                  Action False Nothing
    let movie6  = Movie 6  "Drammatic Film"         Drama  False Nothing
    let movie7  = Movie 7  "Interstellar"           SciFi  True Nothing
    let movie8  = Movie 8  "Gladiator"              Drama  False Nothing
    let movie9  = Movie 9  "Mad Max: Fury Road"     Action False Nothing
    let movie10 = Movie 10 "The Matrix"             SciFi  False Nothing
    let movie11 = Movie 11 "John Wick"              Action True Nothing
    let movie12 = Movie 12 "Forrest Gump"           Drama  False Nothing
    let movie13 = Movie 13 "The Martian"            SciFi  False Nothing
    let movie14 = Movie 14 "Whiplash"               Drama  False Nothing
    let movie15 = Movie 15 "Avengers: Endgame"      Action True Nothing
    let movie16 = Movie 16 "Tenet"                  SciFi  False Nothing

    let movies =
            [ movie1, movie2, movie3, movie4, movie5, movie6, movie7
            , movie8, movie9, movie10, movie11, movie12, movie13
            , movie14, movie15, movie16
            ]

    let system = RentalSystem movies customers

    putStrLn "Welcome to the Diyar and Archin Functional Programming project :D"
    menu system


menu :: RentalSystem -> IO ()
menu system = do
    putStrLn "Menu: || 1. Add Movie || 2. Add Customer || 3. Rent Movie (in process) ||"
    putStrLn "|| 4. Return Movie (in process) || 5. List of Available Movies || 6. Filter/Find ||"
    putStrLn "|| 7. Total Rented Films || 8. Rating of the films (in progress) || 9. Exit ||"

    input <- getLine

    case input of

        "1" -> do
            putStrLn "Enter new movie ID:"
            movID <- getLine
            let intID = read movID :: Int

            putStrLn "Enter title:"
            title <- getLine

            putStrLn "Enter genre (Action || Comedy || Drama || Horror || SciFi):"
            genre <- getLine
            let gen = read genre :: Genre

            let newMovie = Movie intID title gen False Nothing
            let newSystem = addMovie system newMovie

            putStrLn "Movie added!"
            menu newSystem

        "2" -> do
            putStrLn "Enter new customer ID:"
            cID <- getLine
            let intcID = read cID :: Int

            putStrLn "Enter customer name:"
            cname <- getLine

            let newCustomer = Customer intcID cname []
            let newSystem = addCustomer system newCustomer

            putStrLn "Customer added!"
            menu newSystem
        "3" -> do
            putStrLn "Enter customer ID:"
            cStr <- getLine
            putStrLn "Enter movie ID:"
            mStr <- getLine

            let cId = read cStr :: Int
            let mId = read mStr :: Int

            case rentMovie system cId mId of
                Left err -> putStrLn err >> menu system
                Right newSystem -> do
                    putStrLn "Movie successfully rented!"
                    menu newSystem

        "4" -> do
            putStrLn "Enter customer ID:"
            cStr <- getLine
            putStrLn "Enter movie ID to return:"
            mStr <- getLine

            let cId = read cStr :: Int
            let mId = read mStr :: Int

            case returnMovie system cId mId of
                Left err -> putStrLn err >> menu system
                Right newSystem -> do
                    putStrLn "Movie successfully returned!"
                    menu newSystem

         

        "5" -> do
            putStrLn "Available movies:"
            print (listAvailableMovies system)
            menu system

        "6" -> do
            putStrLn "Choose filter:"
            putStrLn "1. By Genre"
            putStrLn "2. By Title"
            choice <- getLine

            case choice of
                "1" -> do
                    putStrLn "Enter genre (Action | Comedy | Drama | Horror | SciFi):"
                    gStr <- getLine
                    let g = read gStr :: Genre
                    print $ findMoviesBy (filterByGenre g) system
                    menu system

                "2" -> do
                    putStrLn "Enter title:"
                    t <- getLine
                    print $ findMoviesBy (searchingByTitle t) system
                    menu system

        "7" -> do
            putStrLn "Total rented movies:"
            print (totalRentedMovies system)
            menu system

        -- "8" -> do

        "9" -> do
            putStrLn "Good bye!"

        _ -> do
            putStrLn "Exception: Wrong key. Try again."
            menu system
