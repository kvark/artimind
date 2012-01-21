module AI.Net
( Neuron
, Net (Net,nodes,links)
) where

import AI.Core
import Data.List (find)

data Neuron = Neuron	deriving (Show,Eq)


data Link =	Link	{
	source	:: Neuron,
	target	:: Neuron
}deriving (Show,Eq)


data Net = Net	{
	nodes	:: [Neuron],
	links	:: [Link]
}deriving ()


transmitCost = 0.1

---	calculate the propagated neuron charge	---
--- TODO: cache results in a map ---
type Pair = Ignot Neuron
propagate	:: [Link] -> [Pair] -> Neuron -> Float
propagate li charged_inputs n = let
		oper :: (Maybe Pair) -> Float
		oper Nothing = let
				incidents = map source $ filter ((n==) . target) li
				inputs = map (propagate li charged_inputs) incidents
				total = sum inputs -transmitCost
			in	max 0 total
		oper (Just (nr,heat)) = fromIntegral heat
		ignot = find ((n==) . fst) charged_inputs
	in oper ignot


instance Think Net Neuron where
	alloc t num = let
			nr = take num $ repeat Neuron
			m = Net {nodes = nr ++ nodes t, links = links t}
		in	(m,nr)
	decide t charged_inputs outputs = let
			mapper = round . (propagate (links t) charged_inputs)
			charges = map mapper outputs
		in	zip outputs charges
	learn t (n,response) = t
