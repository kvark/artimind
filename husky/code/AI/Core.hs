module AI.Core
( World (advance), HeatIn, HeatOut, Sensor, Actor, Ignot, Choice
, Think (alloc,decide,learn)
, Body (addSensors,addActors,stepUp,stepDown,cycle)
, ZeroMind (ZeroMind), zipMap, feelConst
, chooseFirst, chooseLast, chooseMax, chooseRandom
) where

import Data.List (maximumBy)
import System.Random


--- Core types ---

class (Show w) => World w where
	advance	:: w -> w

type HeatIn		= Int					-- input energy
type HeatOut	= Float					-- output energy
type Response	= Int
type Sensor w	= w -> HeatIn			-- input signal
type Actor w	= w -> (w,Response)		-- output signal, changes the world
type Ignot x	= (x,HeatIn)			-- charged input handle
type ChargedOut x	= (x,HeatOut)		-- charged output handle 
type Choice g x	= g -> [ChargedOut x] -> ChargedOut x	-- choose one of charged outputs


class (Eq x, Show t) => Think t x | t->x where
	-- obtain a new handle for input/output --
	alloc	:: t -> Int -> (t,[x])
	-- given a list of charged handles and --
	-- an output handle, produce a charge on it --
	decide	:: t -> [Ignot x] -> x -> HeatOut
	-- adapt to chosen handle with response --
	-- given input charges and output neurons --
	learn	:: t -> [Ignot x] -> [x] -> (x,HeatOut,Response) -> t


--- Helper methods ---

zipMap	:: [a] -> (a->b) -> [(a,b)]
zipMap list fun = zip list (map fun list)

--- Trivial implementations ---

feelConst	:: (World w) => HeatIn -> w -> HeatIn
feelConst val _ = val


data ZeroMind = ZeroMind deriving (Show)
instance Think ZeroMind () where
	alloc t n = (t,replicate n ())
	decide _ _ _ = 1
	learn t _ _ _ = t


--- Decision choosers ---

compareHeat	:: (Ord a) => (b,a) -> (b,a) -> Ordering
compareHeat (_,y1) (_,y2) = compare y1 y2

chooseFirst,chooseLast,chooseMax	:: (RandomGen g) => Choice g x
chooseFirst _	= head
chooseLast _	= last
chooseMax _		= maximumBy compareHeat

chooseRandom	:: (RandomGen g, Show x) => Int -> HeatOut -> Choice g x
chooseRandom power offset gen endPoints = let
	wFun (x,h) = (x,h^power)
	weighted = map wFun endPoints
	total = sum (map ((+offset) . snd) weighted)
	(point,_) = randomR (0,total) gen
	folder (w,rez) (cur,h)
		| w<=0			= (0,rez)
		| w<=h+offset	= (0,(cur,h))
		| otherwise		= (w-h-offset,rez)
	accum = (point, head endPoints)
	(_,result) = foldl folder accum weighted
	in result


--- Body class ---

class (World w) => Body b w | b->w where
	addSensors	:: b -> [(String,Sensor w)] -> b
	addActors	:: b -> [(String,Actor w)] -> b
	stepUp		:: w -> b -> (String,HeatOut,Actor w)
	stepDown	:: w -> b -> (String,HeatOut,Response) -> b
	cycle		:: w -> b -> (w,b,String,Response)
	cycle world body = let
		(name,heat,actor) = stepUp world body
		(newWorld,response) = actor world
		newBody = stepDown world body (name,heat,response)
		in (newWorld,newBody,name,response)
