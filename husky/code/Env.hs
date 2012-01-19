module Env
( World (World)
, Cat
, makeCat
, actEat
) where

import qualified AI.Core	as Ai
import qualified AI.Person	as Ai
import qualified AI.Net		as Ai


data World = World Int	deriving (Show)
instance Ai.World World

actEat	:: World -> (World,Ai.Heat)
actEat (World n)
	| n<=0		= (World 0, -1)
	| otherwise	= (World (n-1), 3)

type Cat = Ai.Person World Ai.Net Ai.Neuron --Ai.ZeroMind
makeCat	:: Cat
makeCat = let
		brain = Ai.Net {Ai.nodes=[], Ai.links=[]}
		sensors = [("const",Ai.feelConst 0)]
		actors = [("eat",actEat)]
	in	Ai.makePerson brain Ai.chooseMax sensors actors
