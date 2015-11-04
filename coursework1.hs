-- Coursework: Functional Programming
-- Group: Victor Benning and Henrique Junger

-- To generate the XML, just run the follow command:
-- processFile processJson "userprofile.json" "teste.xml"

--import Entites
import Data.Char
import Data.List

j = readFile "userprofile.json"

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
processFile :: (String -> String) -> FilePath -> FilePath -> IO ()
processFile process infile outfile = 
  do 
    inf <- readFile infile
    writeFile outfile (process inf)

processJson::String->String
processJson json = 
		do
			let jsonList = concatClasses (removeCharList (trimJson (lines json)) '\"')
			let jsonListBreak = listBreak isBegin jsonList
			let users = processJsonList jsonListBreak
			haskellToXml users

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
						"<id>" ++ _id x ++ "</id>\n"++
						--"<name> <first>"++first x++"</first> <last>"++last x++"</last></name>\n"
						"<index>"++ show (index x) ++ "</index>\n"++
						"<isActive>"++ show (isActive x) ++ "</isActive>\n"++
						"<balance>"++ balance x ++ "</balance>\n"++
						"<picture>"++ picture x ++ "</picture>\n"++
						"<age>"++ show (age x) ++ "</age>\n"++
						"<eyeColor>"++ eyeColor x ++ "</eyeColor>\n"++
						"<company>"++ company x ++ "</company>\n"++
						"<email>"++ email x ++ "</email>\n"++
						"<phone>"++ phone x ++ "</phone>\n"++
						"<address>"++ address x ++ "</address>\n"++
						"<registered>"++ registered x ++ "</registered>\n"++
						"<latitude>"++ latitude x ++ "</latitude>\n"++
						"<longitude>"++ longitude x ++ "</longitude>\n"++
						"<greeting>"++ greeting x ++ "</greeting>\n"++
						"<favoriteFruit>"++ favoriteFruit x ++ "</favoriteFruit>\n"++
					  "</User>" ++ haskellToXml xs


