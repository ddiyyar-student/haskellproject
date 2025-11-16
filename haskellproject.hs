import Data.List (find)

-- Data Types --------------------------------------------------------------

data Genre
    = Action
    | Comedy
    | Drama
    | Horror
    | SciFi
    deriving (Show, Eq, Read)

data Movie = Movie
    { movieId   :: Int
    , title     :: String
    , genre     :: Genre
    , isRented  :: Bool
    } deriving (Show, Eq)

data Customer = Customer
    { customerId   :: Int
    , name         :: String
    , rentedMovies :: [Int]
    } deriving (Show, Eq)

data RentalSystem = RentalSystem
    { movies    :: [Movie]
    , customers :: [Customer]
    } deriving (Show)


-- Add Functions -----------------------------------------------------------

addMovie :: RentalSystem -> Movie -> RentalSystem
addMovie system newMovie =
    system { movies = movies system ++ [newMovie] }

addCustomer :: RentalSystem -> Customer -> RentalSystem
addCustomer system newCustomer =
    system { customers = customers system ++ [newCustomer] }


-- Rent Movie --------------------------------------------------------------

rentMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem
rentMovie system cId mId = do
    customer <- maybe (Left "Error: Customer not found.") Right $
        find (\c -> customerId c == cId) (customers system)

    movie <- maybe (Left "Error: Movie not found.") Right $
        find (\m -> movieId m == mId) (movies system)

    if isRented movie
        then Left "Error: Movie is already rented."
        else do
            let updatedMovie     = movie { isRented = True }
                updatedMovies    = map (\m -> if movieId m == mId then updatedMovie else m)
                                       (movies system)

                updatedCustomer  = customer { rentedMovies = rentedMovies customer ++ [mId] }
                updatedCustomers = map (\c -> if customerId c == cId then updatedCustomer else c)
                                       (customers system)

            return system { movies = updatedMovies, customers = updatedCustomers }


-- Return Movie ------------------------------------------------------------

returnMovie :: RentalSystem -> Int -> Int -> Either String RentalSystem
returnMovie system cId mId = do
    customer <- maybe (Left "Error: Customer not found.") Right $
        find (\c -> customerId c == cId) (customers system)

    if mId `notElem` rentedMovies customer
        then Left "Error: Customer did not rent this movie."
        else do
            movie <- maybe (Left "Error: Movie not found.") Right $
                find (\m -> movieId m == mId) (movies system)

            let updatedMovie     = movie { isRented = False }
                updatedMovies    = map (\m -> if movieId m == mId then updatedMovie else m)
                                       (movies system)

                updatedCustomer  = customer { rentedMovies = filter (/= mId) (rentedMovies customer) }
                updatedCustomers = map (\c -> if customerId c == cId then updatedCustomer else c)
                                       (customers system)

            return system { movies = updatedMovies, customers = updatedCustomers }


-- Utility Functions ------------------------------------------------------

listAvailableMovies :: RentalSystem -> [Movie]
listAvailableMovies system =
    filter (not . isRented) (movies system)

findMoviesBy :: (Movie -> Bool) -> RentalSystem -> [Movie]
findMoviesBy predicate system =
    filter predicate (movies system)

filterByGenre :: Genre -> Movie -> Bool
filterByGenre g movie =
    genre movie == g

searchingByTitle :: String -> Movie -> Bool
searchingByTitle t movie =
    title movie == t

totalRentedMovies :: RentalSystem -> Maybe Int
totalRentedMovies system =
    Just (length (filter isRented (movies system)))


-- Main -------------------------------------------------------------------

main :: IO ()
main = do
    let customers =
            [ Customer 1 "Alice"   []
            , Customer 2 "Bob"     [2]
            , Customer 3 "Charlie" []
            , Customer 4 "Diana"   [7, 11]
            , Customer 5 "Eric"    []
            , Customer 6 "Fiona"   [3]
            , Customer 7 "George"  []
            , Customer 8 "Helen"   [15]
            ]

    let movies =
            [ Movie 1  "Inception"              SciFi  False
            , Movie 2  "The Dark Knight"        Action True
            , Movie 3  "Oppenhaimer"            Drama  True
            , Movie 4  "Spider Man: Homecoming" SciFi  False
            , Movie 5  "Rocky"                  Action False
            , Movie 6  "Drammatic Film"         Drama  False
            , Movie 7  "Interstellar"           SciFi  True
            , Movie 8  "Gladiator"              Drama  False
            , Movie 9  "Mad Max: Fury Road"     Action False
            , Movie 10 "The Matrix"             SciFi  False
            , Movie 11 "John Wick"              Action True
            , Movie 12 "Forrest Gump"           Drama  False
            , Movie 13 "The Martian"            SciFi  False
            , Movie 14 "Whiplash"               Drama  False
            , Movie 15 "Avengers: Endgame"      Action True
            , Movie 16 "Tenet"                  SciFi  False
            ]

    let system = RentalSystem movies customers

    putStrLn "Welcome to the Diyar and Archin Functional Programming Project!"
    menu system


-- Menu -------------------------------------------------------------------

menu :: RentalSystem -> IO ()
menu system = do
    putStrLn ""
    putStrLn "Menu:"
    putStrLn " 1. Add Movie"
    putStrLn " 2. Add Customer"
    putStrLn " 3. Rent Movie"
    putStrLn " 4. Return Movie"
    putStrLn " 5. List Available Movies"
    putStrLn " 6. Filter/Find Movies"
    putStrLn " 7. Total Rented Films"
    putStrLn " 9. Exit"
    putStr "Enter choice: "

    input <- getLine
    putStrLn ""

    case input of
        "1" -> do
            putStrLn "Enter new movie ID:"
            movID <- getLine

            putStrLn "Enter title:"
            mTitle <- getLine

            putStrLn "Enter genre (Action | Comedy | Drama | Horror | SciFi):"
            gStr <- getLine

            let newMovie   = Movie (read movID) mTitle (read gStr) False
                newSystem  = addMovie system newMovie

            putStrLn "Movie added!"
            menu newSystem

        "2" -> do
            putStrLn "Enter new customer ID:"
            cID <- getLine

            putStrLn "Enter customer name:"
            cName <- getLine

            let newCustomer = Customer (read cID) cName []
                newSystem   = addCustomer system newCustomer

            putStrLn "Customer added!"
            menu newSystem

        "3" -> do
            putStrLn "Enter customer ID:"
            cStr <- getLine

            putStrLn "Enter movie ID:"
            mStr <- getLine

            case rentMovie system (read cStr) (read mStr) of
                Left err      -> putStrLn err >> menu system
                Right newSys  -> putStrLn "Movie rented!" >> menu newSys

        "4" -> do
            putStrLn "Enter customer ID:"
            cStr <- getLine

            putStrLn "Enter movie ID to return:"
            mStr <- getLine

            case returnMovie system (read cStr) (read mStr) of
                Left err      -> putStrLn err >> menu system
                Right newSys  -> putStrLn "Movie returned!" >> menu newSys

        "5" -> do
            print (listAvailableMovies system)
            menu system

        "6" -> do
            putStrLn "Filter Options:"
            putStrLn " 1. By Genre"
            putStrLn " 2. By Title"
            choice <- getLine

            case choice of
                "1" -> do
                    putStrLn "Enter genre:"
                    g <- getLine
                    print (findMoviesBy (filterByGenre (read g)) system)
                    menu system

                "2" -> do
                    putStrLn "Enter title:"
                    t <- getLine
                    print (findMoviesBy (searchingByTitle t) system)
                    menu system

                _ -> menu system

        "7" -> do
            print (totalRentedMovies system)
            menu system

        "9" ->
            putStrLn "Goodbye!"

        _ -> do
            putStrLn "Invalid input. Try again."
            menu system
