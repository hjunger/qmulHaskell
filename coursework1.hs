--import Entites
import Data.Char

j = readFile "userprofile.json"

data User = User {
					_id::String,
					index::Int, 
					isActive::Bool,
					balance::String,
					picture::String, 
					age::Int,
					eyeColor::String,
					--name::Name,
					company::String,
					email::String
					--phone::String,
					--address::String,
					--registered::String,
					--latitude::String,
					--longitude::String,
					--friends::[Friend],
					--greeting::String,
					--favoriteFruit::String
				}deriving(Show)


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

-- js <- j
-- let json = removeCharList (trimJson (lines js)) '\"'
-- let id = getSomeInfo (getBeginWith json "_id:") "_id: " ','
-- let indexStr = getSomeInfo (getBeginWith json "index: ") "index: " ','
-- let index = read indexStr :: Int
-- let isActiveStr = capitalFst (getSomeInfo (getBeginWith json "isActive: ") "isActive: " ',')
-- let isActive = read isActiveStr :: Bool
-- let balance = getSomeInfo (getBeginWith json "picture: ") "picture: " ','
-- let picture = getSomeInfo (getBeginWith json "age: ") "age: " ','
-- let ageStr = getSomeInfo (getBeginWith json "age: ") "age: " ','
-- let age = read ageStr :: Int
-- let eyeColor = getSomeInfo (getBeginWith json "eyeColor: ") "eyeColor: " ','
-- let name
-- let company = getSomeInfo (getBeginWith json "eyeColor: ") "eyeColor: " ','
-- let email = getSomeInfo (getBeginWith json "email: ") "email: " ','
-- createUser id index isActive balance picture age eyeColor company email


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

strip::String->String->String
strip "" _ = ""
strip stOri "" = stOri
strip stOri stDr = if (take sizeStDr stOri) == stDr then drop sizeStDr stOri else strip (drop 1 stOri) stDr
                    where sizeStDr = length stDr

getBeginWith::[String]->String->String
getBeginWith [] _ = []
getBeginWith (a:as) x = if (take (length x) a) == x then a else getBeginWith as x

myIndex :: (Eq a) => [a] -> a -> Int
myIndex [] _ = 0
myIndex (x:xs) a = if a == x then 0 else 1 + myIndex xs a

getSomeInfo::String->String->Char->String
getSomeInfo "" _ _ = ""
getSomeInfo sth stComp chFin = let st = strip sth stComp in take (myIndex st chFin) st

createUser::String->Int->Bool->String->String->Int->String->String->String->User
createUser i idx act bal pic ag ec com em = User {
													_id=i,
													index=idx,
													isActive=act,
													balance=bal,
													picture=pic,
													age=ag,
													eyeColor=ec,
													company=com,
													email=em
												}
user1 = createUser "5638b7d3b72286f9bee44e15" 0 True "$1,358.10" "Pricture" 25 "Brown" "Qmul" "victor@qmul.ac.uk"
user2 = createUser "5638b7d30b94173ecb742338" 0 True "$2,000.10" "Pricture" 29 "Black" "Qmul" "henrique@qmul.ac.uk"


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
						--"<phone>"++ phone x ++ "</phone>\n"++
						--"<address>"++ address x ++ "</address>\n"++
						--"<registered>"++ registered x ++ "</registered>\n"++
						--"<latitude>"++ latitude x ++ "</latitude>\n"++
						--"<longitude>"++ longitude x ++ "</longitude>\n"++
						--"<greeting>"++ greeting x ++ "</greeting>\n"++
						--"<favoriteFruit>"++ favoriteFruit x ++ "</favoriteFruit>\n"++
					  "</User>" ++ haskellToXml xs









