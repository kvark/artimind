module AI.Net
( Neuron (Neuron)
, Net (Net,nodes,links,nextId)
, propagate--, signalOut, signalIn, transitLink, getEffect
--, getCompliment, findLink, findBoth, subLearn
) where

import AI.Core
import Data.List
import Data.Maybe ()
import Debug.Trace


type NeuroFun = [Float] -> Float
data Neuron = Neuron NeuroFun Int
type Link =	(Neuron,Neuron)
type Pair = Ignot Neuron

instance Show Neuron where
	show (Neuron _ num) = "Neuron " ++ show num
instance Eq Neuron where
	(Neuron _ a) == (Neuron _ b) = a == b

data Net = Net	{
	nodes	:: [Neuron],
	links	:: [Link],
	nextId	:: Int
}deriving (Show)

standardFun :: NeuroFun
standardFun list = (sum list) - 0.1

cached	:: (Int->a) -> Int->a
cached fun = (map fun [0..] !!)


---	calculate the propagated neuron charge	---

type LinkAccess		= Link -> Neuron
type LinkFunction	= [Link] -> Neuron -> [Neuron]
getLinksAny :: LinkAccess -> LinkAccess -> LinkFunction
getLinksAny accessor extractor linSet n = let
	connections = filter ((n==) . accessor) linSet
	in map extractor connections
getLinksIn,getLinksOut :: LinkFunction
getLinksIn	= getLinksAny snd fst
getLinksOut	= getLinksAny fst snd


propagateDir,propagate	:: [Link] -> [Pair] -> Neuron -> HeatOut
propagateDir linSet charge n@(Neuron functor _) = let
	oper :: Maybe Pair -> Float
	oper Nothing = let
		incidents = getLinksIn linSet n
		inCounter :: Neuron -> HeatOut	-- number of outputs of a neuron
		inCounter = fromIntegral . length . getLinksOut linSet
		magnitudes,inputs :: [Float]
		magnitudes = map inCounter incidents
		inputs = map (propagate linSet charge) incidents
		total = functor (zipWith (/) inputs magnitudes)
		in max 0 total
	oper (Just (_,heat)) = fromIntegral heat
	ignot = find ((n==) . fst) charge
	in oper ignot

propagate lins charge (Neuron fun n) = cached (propagateDir lins charge . Neuron fun) n

{-
computeFlowDir	:: [Link] -> Neuron -> Neuron -> HeatOut
computeFlowDir linSet dest source
	| dest == source	= 0.0
	| otherwise 		= let	
	in 0.0-}

getCompliment	:: Net -> [Neuron] -> [Neuron] -> [Link]
getCompliment net exIn exTo = let
	nodesIn = nodes net \\ exIn
	nodesTo = nodes net \\ exTo
	linkSet = [(a,b) | a<-nodesIn, b<-nodesTo, b /= a]
	in linkSet \\ links net


--- instantiating Think class with our neural network ---
instance Think Net Neuron where
	alloc t num = let
		base = nextId t
		nr = map (Neuron standardFun) [base..(base+num-1)]
		m = Net {nodes = nr ++ nodes t, links = links t, nextId = base+num}
		in	(m,nr)

	decide = propagate . links

	learn t charges outputs (nOut,heat,response)
		| response>0	= let
			simulate :: Link -> HeatOut
			simulate ln = propagate (ln:links t) charges nOut
			preLinks = getCompliment t outputs (map fst charges)
			candidates = zipMap preLinks simulate
			folder (l1,r1) (l2,r2)
				| r2>r1		= (l2,r2)
				| otherwise	= (l1,r1)
			(best,rv) = foldl folder ((nOut,nOut),0.0) candidates
			in if trace (show candidates) (rv > heat+0.5)
				then Net { nodes=nodes t, nextId=nextId t, links = best:links t }
				else t
		| response<0	= let
			simulate :: Link -> HeatOut
			simulate ln = propagate (links t \\ [ln]) charges nOut
			candidates = zipMap (links t) simulate
			folder (l1,r1) (l2,r2)
				| r2<r1		= (l2,r2)
				| otherwise	= (l1,r1)
			(best,rv) = foldl folder ((nOut,nOut),0.0) candidates
			in if rv < heat-0.5
				then Net { nodes=nodes t, nextId=nextId t, links = links t \\ [best] }
				else t
		| otherwise	= t

	{-
	learn t charge (_,response)
		| response>0	= let
			bw = findBoth t charge (maximumBy,minimumBy)
			in subLearn t bw (( >0.5 ),( <0.1 ))
		| response<0	= let
			bw = findBoth t charge (minimumBy,maximumBy)
			in subLearn t bw (( <0.1 ),( >0.5 ))
		| otherwise		= t
	-}
