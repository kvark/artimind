module AI.Net
( Mind
) where

import AI.Core


data Neuron = Neuron	deriving (Show,Eq)


data Link =	Link	{
	source	:: Neuron,
	target	:: Neuron
}deriving (Show,Eq)


data Mind = Mind	{
	nodes	:: [Neuron],
	links	:: [Link]
}deriving ()

data AnyBody
instance Body AnyBody

data AnyWorld
instance World AnyWorld

transmitCost = 0.1

---	calculate the propagated neuron charge	---
propagate	:: [Link] -> Neuron -> Real
propagate li n = let
		incidents = filter ((n==) . target) li
		inputs = map (propagate li) incidents
		total = sum inputs -transmitCost
	in	max 0 total


instance Think AnyWorld AnyBody Mind where
	decide t sensors =
		let
		in
	learn t _ = t
