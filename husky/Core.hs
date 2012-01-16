module Core
(	Body, World, Sensor, Actor,
	Think, Neuron, Link, Mind
) where


class Body b
class World w

type Sensor	st	= st -> Int
type Actor st	= st -> st


class Think t where
	decide	:: (World w, Body b) => t -> [Sensor (w,b)] -> Actor (w,b)
	learn	:: (World w, Body b) => t -> [Actor (w,b)] -> t


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


data MindZero = MindZero	{}

instance Think MindZero where
--	decide _ _ = id
--	learn t _ = t


instance Think Mind where
--	decide _ _ = id
--	learn t _ = t
