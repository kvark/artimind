module AI.Net
( Neuron
, Net (Net,nodes,links)
) where

import AI.Core
import Data.List (find,(\\))


data Neuron = Neuron	deriving (Show,Eq)
type Link =	(Neuron,Neuron)


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
				incidents = map fst	$ gather snd n
				inCounter = fromIntegral . length . (gather fst)
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
		incidents = map fst	$ gather snd n
		sReceived = sum $ map (signalOut lin charged_inputs) incidents
		nOut = fromIntegral $ length $ gather fst n
		sInput = oper $ find ((n==) . fst) charged_inputs
	in (sReceived + sInput) / nOut

--- calculate the input signal per link on a neuron ---
signalIn	:: [Link] -> Pair -> Neuron -> Float
signalIn lin chargedOut n = let
		sOutput
			| n == (fst chargedOut)	= fromIntegral (snd chargedOut)
			| otherwise					= 0
		gather fun m = filter ((m==) . fun) lin
		outcidents = map snd $ gather fst n
		sResponse = sum $ map (signalIn lin chargedOut) outcidents
		nIn = fromIntegral $ length $ gather snd n
	in (sResponse + sOutput) / nIn


getEffect	:: [Link] -> ([Pair],Pair) -> Link -> Float
getEffect lins (chInputs,chOut) (src,dst) = let
		sOut = signalOut lins chInputs src
		sIn = signalIn lins chOut dst
	in sOut / sIn

type ExtremeFunc a = (a->a->Ordering) ->[a] ->a

findExist	:: [Link] -> ([Pair],Pair) -> ExtremeFunc (Float,Link) -> Link
findExist lins charged exFun = let
		effects = map (getEffect lins charged) lins
		combined = zip effects lins
		cmpFun (a,_) (b,_) = compare a b
		rez = exFun cmpFun combined
	in snd rez

findAbsent	:: Net -> ([Pair],Pair) -> ExtremeFunc (Float,Link) -> Link
findAbsent net charged exFun = let
		newLins = [(a,b) | a<-(nodes net), b<-(nodes net), b /= a] \\ (links net)
		effects = map (getEffect (links net) charged) newLins
		combined = zip effects newLins
		cmpFun (a,_) (b,_) = compare a b
		rez = exFun cmpFun combined
	in snd rez

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
		| response>0	= let
			in t
		| response<0	= let
			in t
		| otherwise		= t
