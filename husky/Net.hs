module AI.Net
( Mind
) where

import AI.Core


data Neuron = Neuron	{
}deriving (Show,Eq)


data Link =	Link	{
	source	:: Neuron,
	target	:: Neuron
}deriving (Show,Eq)


data Mind = Mind	{
	nodes	:: [Neuron],
	links	:: [Link]
}deriving ()


instance Think Mind where
	decide _ _ = (id,"default")
	learn t _ = t

