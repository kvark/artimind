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


transmitCost :: Float
transmitCost = 0.1

---	calculate the propagated neuron charge	---
--- TODO: cache results in a map ---
type Pair = Ignot Neuron
propagate	:: [Link] -> [Pair] -> Neuron -> Float
propagate lin charged_inputs n = let
		oper :: (Maybe Pair) -> Float
		oper Nothing = let
				gather fun m = filter ((m==) . fun) lin
				incidents = map source	$ gather target n
				inCounter = fromIntegral . length . (gather source)
				magnitudes = map inCounter incidents
				inputs = map (propagate lin charged_inputs) incidents
				total = sum (zipWith (/) inputs magnitudes)
			in	max 0 (total-transmitCost)
		oper (Just (_,heat)) = fromIntegral heat
		ignot = find ((n==) . fst) charged_inputs
	in oper ignot

--- calculate the output signal per link on a neuron ---
signalOut	:: [Link] -> [Pair] -> Neuron -> Float
signalOut lin charged_inputs n = let
		oper :: (Maybe Pair) -> Float
		oper Nothing = 0.0
		oper (Just (_,heat)) = fromIntegral heat
		gather fun m = filter ((m==) . fun) lin
		incidents = map source	$ gather target n
		sReceived = sum $ map (signalOut lin charged_inputs) incidents
		nOut = fromIntegral $ length $ gather source n
		sInput = oper $ find ((n==) . fst) charged_inputs
	in (sReceived + sInput) / nOut

--- calculate the input signal per link on a neuron ---
signalIn	:: [Link] -> Pair -> Neuron -> Float
signalIn lin charged_out n = let
		sOutput
			| n == (fst charged_out)	= fromIntegral (snd charged_out)
			| otherwise					= 0
		gather fun m = filter ((m==) . fun) lin
		outcidents = map target $ gather source n
		sResponse = sum $ map (signalIn lin charged_out) outcidents
		nIn = fromIntegral $ length $ gather target n
	in (sResponse + sOutput) / nIn


--- instantiating Think class with out neural network ---
instance Think Net Neuron where
	alloc t num = let
			nr = take num $ repeat Neuron
			m = Net {nodes = nr ++ nodes t, links = links t}
		in	(m,nr)
	decide t charged_inputs outputs = let
			mapper = round . (propagate (links t) charged_inputs)
			charges = map mapper outputs
		in	zip outputs charges
	learn t (_,response)
		| response>0	= t
		| response<0	= t
		| otherwise		= t
