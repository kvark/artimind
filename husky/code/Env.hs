module Env
( World(World), Body(Body)
, TestMind(TestMind)
) where

import qualified AI.Core as Ai


data World = World Int	deriving (Show)
instance Ai.World World

data Body = Body	deriving (Eq,Show)
instance Ai.Body Body


actEat	:: (World,Body) -> (World,Body,Ai.Energy)
actEat (World n,b)
	| n<=0		= (World 0, b, -1)
	| otherwise	= (World (n-1), b, 3)

data TestMind = TestMind
instance Ai.Think World Body TestMind where
	decide _ s = (actEat,"eat")
	learn t _ = t
