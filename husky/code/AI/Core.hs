module AI.Core
( World (advance), Heat, Sensor, Actor, Ignot, Choice
, Think (alloc,decide,learn)
, Body (addSensors,addActors,stepUp,stepDown,cycle)
, ZeroMind (ZeroMind), feelConst, actIdle
, chooseFirst, chooseMax, chooseRandom
) where

import Data.List (maximumBy)
import System.Random


--- Core types ---

class (Show w) => World w where
	advance	:: w -> w

type Heat		= Int					-- input/output energy
type Sensor w	= w -> Heat				-- input signal
type Actor w	= w -> (w,Heat)			-- output signal, changes the world
type Ignot x	= (x,Heat)				-- charged internal handle
type Choice g x	= g -> [Ignot x] -> x	-- choose a handle among charged ones


class (Eq x, Show t) => Think t x | t->x where
	-- obtain a new handle for input/output --
	alloc	:: t -> Int -> (t,[x])
	-- given a list of charged handles and --
	-- an output handle, produce a charge on it --
	decide	:: t -> [Ignot x] -> x -> Heat
	-- adapt to chosen handle with response --
	learn	:: t -> [Ignot x] -> Ignot x -> t


--- Trivial implementations ---

feelConst	:: (World w) => Heat -> w -> Heat
feelConst val _ = val

actIdle	:: (World w) => Actor w
actIdle w = (w,-1)


data ZeroMind = ZeroMind deriving (Show)
instance Think ZeroMind () where
	alloc t n = (t,replicate n ())
	decide _ _ _ = 1
	learn t _ _ = t


--- Decision choosers ---

compareHeat	:: (Ord a) => (b,a) -> (b,a) -> Ordering
compareHeat (_,y1) (_,y2) = compare y1 y2

chooseFirst	:: (RandomGen g) => Choice g x
chooseFirst _ = fst . head

chooseMax 	:: (RandomGen g) => Choice g x
chooseMax _ = fst . maximumBy compareHeat

chooseRandom	:: (RandomGen g) => Int -> Choice g x
chooseRandom power gen ignots = let
	wFun (x,h) = (x,h^power)
	weighted = map wFun ignots
	total = sum (map snd weighted)
	(point,_) = randomR (1,total) gen
	folder (rez,w) (cur,h)
		| w<=0		= (rez,0)
		| w<=h		= (cur,0)
		| otherwise	= (rez,w-h)
	accum = (fst (head ignots),point)
	rig = foldl folder accum weighted
	in fst rig


--- Body class ---

class (World w) => Body b w | b->w where
	addSensors	:: b -> [(String,Sensor w)] -> b
	addActors	:: b -> [(String,Actor w)] -> b
	stepUp		:: w -> b -> (String,Actor w)
	stepDown	:: w -> b -> (String,Heat) -> b
	cycle		:: w -> b -> (w,b,String,Heat)
	cycle world body = let
		(name,actor) = stepUp world body
		(newWorld,response) = actor world
		newBody = stepDown world body (name,response)
		in (newWorld,newBody,name,response)
