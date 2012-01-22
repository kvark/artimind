module AI.Net
( Neuron
, Net (Net,nodes,links)
) where

import AI.Core
import Data.List


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
type FLink = (Float,Link)

findExist	:: [Link] -> ([Pair],Pair) -> ExtremeFunc FLink -> FLink
findExist lins charged exFun = let
		effects = map (getEffect lins charged) lins
		combined = zip effects lins
		cmpFun (a,_) (b,_) = compare a b
	in exFun cmpFun combined

findAbsent	:: Net -> ([Pair],Pair) -> ExtremeFunc FLink -> FLink
findAbsent net charged exFun = let
		newLins = [(a,b) | a<-(nodes net), b<-(nodes net), b /= a] \\ (links net)
		effects = map (getEffect (links net) charged) newLins
		combined = zip effects newLins
		cmpFun (a,_) (b,_) = compare a b
	in exFun cmpFun combined

subLearn	:: Net -> (FLink,FLink) -> (Float->Bool,Float->Bool) -> Net
subLearn t0 (best,worst) (conA,conB) = let
		t1
			| conA (fst best) = let
				tx = Net { nodes=nodes t0, links=(snd best):(links t0) }
			in tx
			| otherwise = t0
		t2
			| conB (fst worst) = let
				tx = Net { nodes=nodes t1, links=(links t1) \\ [snd worst] }
			in tx
			| otherwise = t1
	in	t2


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
	learn t charged@(_,(_,response))
		| response>0	= let
				best = findAbsent t charged maximumBy
				worst = findExist (links t) charged minimumBy
			in subLearn t (best,worst) (( >0.1 ),( <(-0.1) ))
		| response<0	= let
				best = findAbsent t charged minimumBy
				worst = findExist (links t) charged maximumBy
			in subLearn t (best,worst) (( <0.0 ),( >0.1 ))
		| otherwise		= t
