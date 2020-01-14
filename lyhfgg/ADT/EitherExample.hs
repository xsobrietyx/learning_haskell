import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String
type Exception = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either Exception Code
lockerLookup lockerNumber map = case Map.lookup lockerNumber map of
                                Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " not exists."
                                Just (state, code) -> if state == Free then Right code else Left $ "Locker " ++
                                  show lockerNumber ++ " has already taken."

-- mocked map of data to test the function
lockers :: LockerMap
lockers = Map.fromList
                 [(100,(Taken, "ZD39I"))
                 ,(101,(Free, "JAH3I"))
                 ,(103,(Free, "IQSA9"))
                 ,(105,(Free, "QOTSA"))
                 ,(109,(Taken, "893JJ"))
                 ,(110,(Taken, "99292"))
                 ]
