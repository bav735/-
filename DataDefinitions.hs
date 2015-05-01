module DataDefinitions where --DataDefinitions.hs

-- Problem1 ProblemA
type OatmealTemp = Int

perfectT, lowestT, highestT :: OatmealTemp
perfectT = 10
lowestT = 0
highestT = 20

toOTfromInt :: Int -> OatmealTemp
toOTfromInt t | t >= 0 && t <= 20 = t                
-- Problem1 Problem A

-- Problem1 Problem B
data Adjustment = TurnLeft|TurnRight|LeaveAsIs deriving (Show, Eq, Ord)
data KnobState = TooHot|TooCold|Perfect deriving (Show, Eq, Ord)

toAdjFromKS :: KnobState -> Adjustment
toAdjFromKS ks = case ks of
	TooHot -> TurnLeft
	TooCold -> TurnRight
	Perfect -> LeaveAsIs
toKSfromTO :: OatmealTemp -> KnobState
toKSfromTO t = if t > perfectT
	then TooHot
	else if t < perfectT
		then TooCold
		else Perfect
-- Problem1 Problem B

-- Problem1 Problem C
oatmeakTempToAdjustment :: OatmealTemp -> Adjustment
oatmeakTempToAdjustment t = toAdjFromKS(toKSfromTO(t))
-- Problem1 Problem C

-- Problem2 Problem A
data DinnerOrder = Chicken | Pasta | NoOrder
-- Problem2 Problem A

-- Problem2 Problem B
dinnerOrderToMsg :: DinnerOrder -> String
dinnerOrderToMsg d = case d of
	Chicken -> "The passenger ordered chicken."
	Pasta -> "The passenger ordered pasta."
	NoOrder -> "The passenger ordered nothing."
-- Problem2 Problem B