--import Entites

j = readFile "userprofile.json"


data User = User {
					_Id::String
				}deriving(Show)


-- js <- j
-- let json = lines js
-- let begin = "    \"_id\": "
-- let test = getSomeInfo (getBeginWith json begin) begin ','
isID::String->Bool
isID s = (take 3 s)=="_id"

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

createUser::String->User
createUser s = User s