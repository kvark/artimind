module AI.Person
( Person
, extractBrain
, makePerson
)where

import Data.Maybe (fromJust)
import Data.List (find,intercalate)
import Debug.Trace
import System.Random (RandomGen,next)
import AI.Core


data (World w, Think t x, RandomGen g) => Person w t x g =
	Person t g (Choice g x) [(x,String,Sensor w)] [(x,String,Actor w)]

extractBrain	:: (World w, Think t x, RandomGen g) => Person w t x g -> t
extractBrain (Person brain _ _ _ _) = brain
attachHand	:: x -> (String,y) -> (x,String,y)
attachHand hand (name,q) = (hand,name,q)
getHand		:: (x,y,z) -> x
getHand (hand,_,_) = hand
getName		:: (x,String,z) -> String
getName (_,name,_) = name
getString	:: (Show x) => (x,String,z)	-> String
getString (hand,name,_) = show (name,hand)


instance (World w, Show x, Think t x, RandomGen g) => Show (Person w t x g) where
	show (Person brain _ _ sensors actors) = let
		sens = "\n\tSensors: "++ intercalate "," (map getString sensors)
		acts = "\n\tActors: " ++ intercalate "," (map getString actors)
		in	show brain ++ sens ++ acts


instance (World w, Show x, Think t x, RandomGen g) => Body (Person w t x g) w where
	addSensors (Person brain gen chooser sensors actors) input = let
		(newBrain,handles) = alloc brain (length input)
		ns = zipWith attachHand handles input
		in	Person newBrain gen chooser (ns++sensors) actors
	addActors (Person brain gen chooser sensors actors) act = let
		(newBrain,handles) = alloc brain (length act)
		na = zipWith attachHand handles act
		in	Person newBrain gen chooser sensors (na++actors)
	stepUp world (Person brain gen chooser sensors actors) = let
		getSignal (hand,_,sense) = (hand,sense world)
		inputs = map getSignal sensors
		decision = decide brain inputs
		outHands = map getHand actors
		outIgnots = zipMap outHands decision
		(choice,heat) = trace (show outIgnots) (chooser gen outIgnots)
		target = find ((==choice) . getHand) actors
		(_,name,act) = trace (show (choice,heat)) (fromJust target)
		in (name,heat,act)
	stepDown world (Person brain gen chooser sensors actors) (name,heat,response) = let
		getSignal (h,_,sense) = (h,sense world)
		inputs = map getSignal sensors
		target = find ((==name) . getName) actors
		(hand,_,_) = fromJust target
		newBrain = learn brain inputs (map getHand actors) (hand,heat,response)
		g2 = snd (next gen)
		in	Person newBrain g2 chooser sensors actors


makePerson	:: (World w, Show x, Think t x, RandomGen g) =>
	t -> g -> Choice g x -> [(String,Sensor w)] -> [(String,Actor w)] -> Person w t x g
makePerson brain gen chooser sensors actors = let
	p0 = Person brain gen chooser [] []
	p1 = addSensors p0 sensors
	p2 = addActors p1 actors
	in	p2
