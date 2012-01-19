module Env
( World(World)
, actEat
) where

import qualified AI.Core as Ai


data World = World Int	deriving (Show)
instance Ai.World World

actEat	:: World -> (World,Ai.Heat)
actEat (World n)
	| n<=0		= (World 0, -1)
	| otherwise	= (World (n-1), 3)

