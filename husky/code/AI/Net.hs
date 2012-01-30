module AI.Net
( Neuron (Neuron)
, Net (Net,nodes,links,nextId)
, propagate, signalOut, signalIn, getEffect
, getCompliment, findLink, findBoth, subLearn
) where

import AI.Core
import Data.List
import Data.Maybe (isNothing,fromJust)

data Neuron = Neuron Int	deriving (Show,Eq)
type Link =	(Neuron,Neuron)


data Net = Net	{
	nodes	:: [Neuron],
	links	:: [Link],
	nextId	:: Int
}deriving (Show)


transmitCost :: Float
transmitCost = 0.1

cached	:: (Int->a) -> Int->a
cached fun = (map fun [0..] !!)


---	calculate the propagated neuron charge	---
--- TODO: cache results in a map ---
type Pair = Ignot Neuron
propagateDir	:: [Link] -> [Pair] -> Neuron -> Float
propagateDir lin charge n = let
	oper :: Maybe Pair -> Float
	oper Nothing = let
		gather fun m = filter ((m==) . fun) lin
		incidents = map fst	$ gather snd n
		inCounter = fromIntegral . length . gather fst
		magnitudes = map inCounter incidents
		inputs = map (propagate lin charge) incidents
		total = sum (zipWith (/) inputs magnitudes)
		in	max 0 (total-transmitCost)
	oper (Just (_,heat)) = fromIntegral heat
	ignot = find ((n==) . fst) charge
	in oper ignot

propagate	:: [Link] -> [Pair] -> Neuron -> Float
propagate lins charge (Neuron n) = cached (propagateDir lins charge . Neuron) n


--- calculate the output signal per link on a neuron ---
signalOutDir	:: [Link] -> [Pair] -> Neuron -> Float
signalOutDir lin charged_inputs n = let
	oper :: Maybe Pair -> Float
	oper Nothing = 0.0
	oper (Just (_,heat)) = fromIntegral heat
	gather fun m = filter ((m==) . fun) lin
	incidents = map fst	$ gather snd n
	sReceived = sum $ map (signalOut lin charged_inputs) incidents
	nOut = fromIntegral $ length $ gather fst n
	sInput = oper $ find ((n==) . fst) charged_inputs
	in (sReceived + sInput) / (nOut+1)

signalOut	:: [Link] -> [Pair] -> Neuron -> Float
signalOut lins chargedIn (Neuron n) = cached (signalOutDir lins chargedIn . Neuron) n

--- calculate the input signal per link on a neuron ---
signalInDir	:: [Link] -> Pair -> Neuron -> Float
signalInDir lin chargedOut n = let
	sOutput
		| n == fst chargedOut	= fromIntegral (snd chargedOut)
		| otherwise					= 0
	gather fun m = filter ((m==) . fun) lin
	outcidents = map snd $ gather fst n
	sResponse = sum $ map (signalIn lin chargedOut) outcidents
	nIn = fromIntegral $ length $ gather snd n
	in (sResponse + sOutput) / (nIn+1)

signalIn	:: [Link] -> Pair -> Neuron -> Float
signalIn lins chargedOut (Neuron n) = cached (signalInDir lins chargedOut . Neuron) n


getEffect	:: [Link] -> ([Pair],Pair) -> Link -> Float
getEffect lins (chInputs,chOut) (src,dst) = let
	sOut = signalOut lins chInputs src
	sIn = signalIn lins chOut dst
	in sOut * sIn

getCompliment	:: Net -> [Link]
getCompliment net = [(a,b) | a<-nodes net, b<-nodes net, b /= a] \\ links net

type ExtremeFunc a = (a->a->Ordering) ->[a] ->a
type FLink = (Float,Link)
type MayLink = Maybe FLink
type FunLink = ExtremeFunc FLink

findLink	:: [Link] -> (Link->Float) -> FunLink -> MayLink
findLink [] _ _ = Nothing
findLink lins fun exFun = let
	effects = map fun lins
	combined = zip effects lins
	cmpFun (a,_) (b,_) = compare a b
	in Just (exFun cmpFun combined)

findBoth	:: Net -> ([Pair],Pair) -> (FunLink,FunLink) -> (MayLink,MayLink)
findBoth net charge (funOut,funIn) = let
	fe = getEffect (links net) charge
	ma = findLink (getCompliment net) fe funOut
	mb = findLink (links net) fe funIn
	in (ma,mb)


--- modify the net based on choosen best/worst links candidates and conditions ---
subLearn	:: Net -> (MayLink,MayLink) -> (Float->Bool,Float->Bool) -> Net
subLearn t0 (mbest,mworst) (conA,conB) = let
	t1
		| isNothing mbest = t0
		| (conA . fst . fromJust) mbest = let
			goodLink = snd (fromJust mbest)
			tx = Net { nodes=nodes t0, nextId=nextId t0, links=goodLink : links t0 }
			in tx
		| otherwise = t0
	t2
		| isNothing mworst = t1
		| (conB . fst . fromJust) mworst = let
			badLinks = [snd (fromJust mworst)]
			tx = Net { nodes=nodes t1, nextId=nextId t1, links=links t1 \\ badLinks }
			in tx
		| otherwise = t1
	in	t2


--- instantiating Think class with out neural network ---
instance Think Net Neuron where
	alloc t num = let
		base = nextId t
		nr = map Neuron [base..(base+num-1)]
		m = Net {nodes = nr ++ nodes t, links = links t, nextId = base+num}
		in	(m,nr)
	decide t chargeIn outputs = let
		mapper = round . propagate (links t) chargeIn
		outCharges = map mapper outputs
		in	zip outputs outCharges
	learn t charge@(_,(_,response))
		| response>0	= let
			bw = findBoth t charge (maximumBy,minimumBy)
			in subLearn t bw (( >0.1 ),( <(-0.1) ))
		| response<0	= let
			bw = findBoth t charge (minimumBy,maximumBy)
			in subLearn t bw (( <0.0 ),( >0.1 ))
		| otherwise		= t
