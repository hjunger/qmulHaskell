-- Coursework: Functional Programming
-- Group: Victor Benning and Henrique Junger

-- To generate the XML, just run the follow command:
-- convertJsonToXml "userprofile.json" "teste.xml"

--import Entites


import Data.Char
import Data.List
import Control.Monad

--j = readFile "userprofile.json"

-- Data Types
data Name = Name {
                firstName::String,
                lastName::String
            }deriving(Show)

data Friend = Friend {
                idFriend::Int,
                nameFriend::String
            }deriving(Show)

data User = User {
                    _id::String,
                    index::Int, 
                    isActive::Bool,
                    balance::String,
                    picture::String, 
                    age::Int,
                    eyeColor::String,
                    name::Name,
                    company::String,
                    email::String,
                    phone::String,
                    address::String,
                    about::String,
                    registered::String,
                    latitude::String,
                    longitude::String,
                    friends::[Friend],
                    greeting::String,
                    favoriteFruit::String
                }deriving(Show)

-- Common Functions
trim::String->String
trim xs = dropSpaceTail "" $ dropWhile isSpace xs

dropSpaceTail::String->String->String
dropSpaceTail maybeStuff "" = ""
dropSpaceTail maybeStuff (x:xs)
        | isSpace x = dropSpaceTail (x:maybeStuff) xs
        | null maybeStuff = x : dropSpaceTail "" xs
        | otherwise       = reverse maybeStuff ++ x : dropSpaceTail "" xs

capitalFst::String->String
capitalFst s = toUpper (head s):tail s

myConcat :: [[a]] -> [a]
myConcat (a:as) = foldl (++) a as

myIndex :: (Eq a) => [a] -> a -> Int
myIndex [] _ = 0
myIndex (x:xs) a = if a == x then 0 else 1 + myIndex xs a

trimJson::[String]->[String]
trimJson [] = []
trimJson (x:xs) = trim x : trimJson xs

removeChar::String->Char->String
removeChar "" _ = ""
removeChar (x:xs) c = if x == c 
                        then removeChar xs c
                        else x : (removeChar xs c)

removeCharList::[String]->Char->[String]
removeCharList [] _ = []
removeCharList (x:xs) c = (removeChar x c) : removeCharList xs c

-- Step by step on tests
-- js <- j
-- let jsonList = concatClasses (removeCharList (trimJson (lines js)) '\"')
-- let jsonListBreak = listBreak isBegin jsonList

-- Functions to get the String from the JSON
strip::String->String->String
strip "" _ = ""
strip stOri "" = stOri
strip stOri stDr = if (take sizeStDr stOri) == stDr then drop sizeStDr stOri else strip (drop 1 stOri) stDr
                    where sizeStDr = length stDr

getBeginWith::[String]->String->String
getBeginWith [] _ = []
getBeginWith (a:as) x = if (take (length x) a) == x then a else getBeginWith as x

getSomeInfo::String->String->Char->String
getSomeInfo "" _ _ = ""
getSomeInfo sth stComp chFin = let st = strip sth stComp in if (last st) == chFin then init st else st

getFirstName::Name->String
getFirstName (Name firstName _) = firstName

getName::Name->String
getName (Name firstName _) = firstName

getLastName::Name->String
getLastName (Name _ lastName) = lastName

getFriends::[Friend]->String
getFriends [] = ""
getFriends (x:xs) = "\t\t<friend>\n\t\t\t<id>" ++ show (idFriend x) ++ 
                    "</id>\n"++ "\t\t\t<name>"++ nameFriend x ++ "</name>\n\r\t\t</friend>\n" ++ getFriends(xs)

listBreak :: (a -> Bool) -> [a] -> [[a]]
listBreak pred [] = []
listBreak pred [x] = [[x]] 
listBreak pred (x0:x1:xs) = 
  if pred x1 then [x0]:(listBreak pred (x1:xs))
  else let (l1:ls) = listBreak pred (x1:xs) in ((x0:l1):ls)

isBegin::String->Bool
isBegin x = (take 1 x)=="{" && (last x)/=','

concatClasses::[String]->[String]
concatClasses [] = []
concatClasses (x:xs) = if (isInfixOf "[" x)&&((length x) > 1) 
                        then 
                            let idx = (myIndex (x:xs) "],") + 1
                            in ((myConcat (take idx (x:xs))) : concatClasses (drop idx (x:xs)))
                        else x : concatClasses xs

-- Functions to create the Data types
-- Example: "{id: 1,name: Rafael Nadal},{id: 2,name: Novak Djokovic},{id: 3,name: Roger Federer}"
createFriends::String->[Friend]
createFriends "" = []
createFriends "]," = []
createFriends s = 
            do
                let idx = myIndex s '}' + 1
                let friend = take idx s
                let friendWithoutBra = getSomeInfo friend "id: " ','
                let idFriend = read (take (myIndex friendWithoutBra ',') friendWithoutBra) :: Int
                let nameFriend = getSomeInfo friend "name: " '}'
                createFriend idFriend nameFriend : createFriends (drop idx s)


createFriend::Int->String->Friend
createFriend ident name =
    Friend {
            idFriend = ident,
            nameFriend = name
        }

createName::String->String->Name
createName firstName lastName = 
    Name {
            firstName = firstName,
            lastName = lastName
        }

createUser::String->Int->Bool->String->String->Int->String->Name->String->String->String->String->String->String->String->String->[Friend]->String->String->User
createUser i idx act bal pic ag ec n com em ph add abt reg lat lon friends gree fav = 
    User {
            _id = i,
            index = idx,
            isActive = act,
            balance = bal,
            picture = pic,
            age = ag,
            eyeColor = ec,
            name = n,
            company = com,
            email = em,
            phone = ph,
            address = add,
            about = abt,
            registered = reg,
            latitude = lat,
            longitude = lon,
            friends = friends,
            greeting = gree,
            favoriteFruit = fav
        }



-- Functions to process the JSON
convertJsonToXml :: FilePath -> FilePath -> IO ()
convertJsonToXml infile outfile = 
  do 
    inf <- readFile infile
    writeFile outfile (processJson inf)

calculateAverageFriends::FilePath->IO()
calculateAverageFriends infile = 
                    do
                        inf <- readFile infile
                        processJsonForAverageFriends inf
                        

processJsonForAverageFriends::String->IO()
processJsonForAverageFriends json = 
                    do
                        let jsonList = concatClasses (removeCharList (trimJson (lines json)) '\"')
                        let jsonListBreak = listBreak isBegin jsonList
                        let users = processJsonList jsonListBreak
                        let result = getAvaregeOfFriends users
                        putStrLn (show result)

getAvaregeOfFriends:: (Fractional a) => [User]-> a
getAvaregeOfFriends [] = 0
getAvaregeOfFriends users = (realToFrac (totalOfFriends users)) / (realToFrac (length users))

totalOfFriends::[User]->Int
totalOfFriends [] = 0
totalOfFriends (u:us) = length (friends u) + totalOfFriends us

processJson::String->String
processJson json = 
                    do
                        let jsonList = concatClasses (removeCharList (trimJson (lines json)) '\"')
                        let jsonListBreak = listBreak isBegin jsonList
                        let users = processJsonList jsonListBreak
                        haskellToXml users

calculateAgeAverage :: FilePath -> IO()
calculateAgeAverage infile = 
                          do 
                            inf <- readFile infile
                            processJsonForAverageAge inf

processJsonForAverageAge:: String ->IO()
processJsonForAverageAge json = 
        do
            let jsonList = concatClasses (removeCharList (trimJson (lines json)) '\"')
            let jsonListBreak = listBreak isBegin jsonList
            let users = processJsonList jsonListBreak
            let total = getTotalAge users
            putStrLn (show (average total))

main =
    do
        putStrLn "0 - EXIT" 
        putStrLn "1 - Parse json to Xml"
        putStrLn "2 - Average age for user on json"
        putStrLn "3 - Average of friends on json"
        choice <- getLine
        let userChoice = read choice :: Int
        when (userChoice > 0) $ do
                callFunction userChoice

callFunction::Int->IO()
callFunction idx
        | idx == 1 = convertJsonToXml jsonFile "convertedJson.xml"
        | idx == 2 = calculateAgeAverage jsonFile
        | idx == 3 = calculateAverageFriends jsonFile
        where jsonFile = "userprofile.json"


processJsonList::[[String]]->[User]
processJsonList [] = []
processJsonList (x:xs) = if (head x)=="{" 
                        then (jsonToUser x) : processJsonList xs
                        else processJsonList xs

jsonToUser::[String]->User
jsonToUser json = 
    do
        let id = getSomeInfo (getBeginWith json "_id:") "_id: " ','
        let index = read (getSomeInfo (getBeginWith json "index: ") "index: " ',') :: Int
        let isActive = read (capitalFst (getSomeInfo (getBeginWith json "isActive: ") "isActive: " ',')) :: Bool
        let balance = getSomeInfo (getBeginWith json "balance: ") "balance: " ','
        let picture = getSomeInfo (getBeginWith json "picture: ") "picture: " ','
        let age = read (getSomeInfo (getBeginWith json "age: ") "age: " ',') :: Int
        let eyeColor = getSomeInfo (getBeginWith json "eyeColor: ") "eyeColor: " ','
        let name = createName (getSomeInfo (getBeginWith json "first: ") "first: " ',') (getSomeInfo (getBeginWith json "last: ") "last: " ',')
        let company = getSomeInfo (getBeginWith json "company: ") "company: " ','
        let email = getSomeInfo (getBeginWith json "email: ") "email: " ','
        let phone = getSomeInfo (getBeginWith json "phone: ") "phone: " ','
        let address = getSomeInfo (getBeginWith json "address: ") "address: " ','
        let about = getSomeInfo (getBeginWith json "about: ") "about: " ','
        let registered = getSomeInfo (getBeginWith json "registered: ") "registered: " ','
        let latitude = getSomeInfo (getBeginWith json "latitude: ") "latitude: " ','
        let longitude = getSomeInfo (getBeginWith json "longitude: ") "longitude: " ','
        let friends = createFriends (getSomeInfo (getBeginWith json "friends: [") "friends: [" ']')
        let greeting = getSomeInfo (getBeginWith json "greeting: ") "greeting: " ','
        let favoriteFruit = getSomeInfo (getBeginWith json "favoriteFruit: ") "favoriteFruit: " ','
        createUser id index isActive balance picture age eyeColor name company email phone address about registered latitude longitude friends greeting favoriteFruit   

haskellToXml::[User]->String
haskellToXml [] = ""
haskellToXml (x:xs) = "<User>\n"++
                        "\t<id>" ++ _id x ++ "</id>\n"++
                        "\t<name> \n\t\t<first>" ++ getFirstName (name x) ++ "</first>\n"++"\t\t<last>" ++ getLastName (name x)++ "</last>\n\r\t</name>\n"++
                        "\t<index>"++ show (index x) ++ "</index>\n"++
                        "\t<isActive>"++ show (isActive x) ++ "</isActive>\n"++
                        "\t<balance>"++ balance x ++ "</balance>\n"++
                        "\t<picture>"++ picture x ++ "</picture>\n"++
                        "\t<age>"++ show (age x) ++ "</age>\n"++
                        "\t<eyeColor>"++ eyeColor x ++ "</eyeColor>\n"++
                        "\t<company>"++ company x ++ "</company>\n"++
                        "\t<email>"++ email x ++ "</email>\n"++
                        "\t<phone>"++ phone x ++ "</phone>\n"++
                        "\t<address>"++ address x ++ "</address>\n"++
                        "\t<registered>"++ registered x ++ "</registered>\n"++
                        "\t<latitude>"++ latitude x ++ "</latitude>\n"++
                        "\t<longitude>"++ longitude x ++ "</longitude>\n"++
                        "\t<friends>\n"++ getFriends (friends x) ++ "\t</friends>\n"++
                        "\t<greeting>"++ greeting x ++ "</greeting>\n"++
                        "\t<favoriteFruit>"++ favoriteFruit x ++ "</favoriteFruit>\n"++
                      "\r</User>\n" ++ haskellToXml xs



getTotalAge::[User] -> [Int]
getTotalAge [] = []
getTotalAge (x:xs) = age x : getTotalAge xs 

average :: (Real a, Fractional b) => [a] -> b
average [] = 0
average xs = realToFrac (sum xs) / genericLength xs

