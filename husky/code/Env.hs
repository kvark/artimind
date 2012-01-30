module Env
( World (World)
, Cat
, getBrain
, makeCat
) where

import qualified AI.Core	as Ai
import qualified AI.Person	as Ai
import qualified AI.Net		as Ai
import qualified System.Random	as R


data World = World Int	deriving (Show)
instance Ai.World World where
	advance _ = World 1

fillMeal	:: World -> Int
fillMeal	(World n)
	| n > 0		= 1
	| otherwise	= 0

actEat	:: World -> (World,Ai.Heat)
actEat (World n)
	| n<=0		= (World 0, -10)
	| otherwise	= (World (n-1), 100)

type Cat = Ai.Person World Ai.Net Ai.Neuron R.StdGen
getBrain :: Cat -> Ai.Net
getBrain = Ai.extractBrain
makeCat	:: Cat
makeCat = let
	brain = Ai.Net {Ai.nodes=[], Ai.links=[], Ai.nextId=0}
	gen = R.mkStdGen 1
	chooser = Ai.chooseRandom 1
	sensors = [( "meal",fillMeal ),( "const",Ai.feelConst 0 )]
	actors = [( "eat",actEat ),( "idle",Ai.actIdle )]
	in Ai.makePerson brain gen chooser sensors actors
