module AI.Net
( Neuron (Neuron)
, Net (Net,nodes,links,nextId)
, propagate--, signalOut, signalIn, transitLink, getEffect
--, getCompliment, findLink, findBoth, subLearn
) where

import AI.Core
import Data.List
import Data.Maybe ()

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
getLinksIn	= getLinksAny fst snd
getLinksOut	= getLinksAny snd fst


propagateDir,propagate	:: [Link] -> [Pair] -> Neuron -> Float
propagateDir linSet charge n@(Neuron functor _) = let
	oper :: Maybe Pair -> Float
	oper Nothing = let
		incidents = getLinksIn linSet n
		inCounter :: Neuron -> Float	-- number of outputs of a neuron
		inCounter = fromIntegral . length . getLinksOut linSet
		magnitudes,inputs :: [Float]
		magnitudes = map inCounter incidents
		inputs = map (propagate linSet charge) incidents
		total = functor (zipWith (/) inputs magnitudes)
		in	max 0 total
	oper (Just (_,heat)) = fromIntegral heat
	ignot = find ((n==) . fst) charge
	in oper ignot

propagate lins charge (Neuron fun n) = cached (propagateDir lins charge . Neuron fun) n

computeFlowDir	:: [Link] -> Neuron -> Neuron -> Float
computeFlowDir linSet dest source
	| dest == source	= 0.0
	| otherwise 		= let	
	in 0.0

getCompliment	:: Net -> [Link]
getCompliment net = [(a,b) | a<-nodes net, b<-nodes net, b /= a] \\ links net

{-
--- calculate the output signal per link on a neuron ---
signalOutDir,signalOut	:: [Link] -> [Pair] -> Neuron -> Float
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

signalOut lin chargedIn (Neuron n) = cached (signalOutDir lin chargedIn . Neuron) n

--- calculate the input signal per link on a neuron ---
signalInDir,signalIn	:: [Link] -> Pair -> Neuron -> Float
signalInDir lin chargedOut n = let
	sOutput
		| n == fst chargedOut	= fromIntegral (snd chargedOut)
		| otherwise					= 0
	gather fun m = filter ((m==) . fun) lin
	outcidents = map snd $ gather fst n
	sResponse = sum $ map (signalIn lin chargedOut) outcidents
	nIn = fromIntegral $ length $ gather snd n
	in (sResponse + sOutput) / (nIn+1)

signalIn lins chargedOut (Neuron n) = cached (signalInDir lins chargedOut . Neuron) n

--- calculate the transit connectivity between nodes ---
transitLinkDir,transitLink2,transitLink	:: [Link] -> Neuron -> Neuron -> Float
transitLinkDir all_links to from = let
	wave = map snd $ filter ((from==) . fst) all_links
	summa = sum $ map (transitLink all_links to) wave
	result
		| to==from	= 1.0
		| null wave	= 0.0
		| otherwise = summa / fromIntegral (length wave)
	in result

transitLink2 all_links to (Neuron n) = cached (transitLinkDir all_links to . Neuron) n
transitLink all_links (Neuron n) from = let
	xxx num = transitLink2 all_links (Neuron num) from
	in cached xxx n

--- evaluate a potential/old link profit ---
getEffect	:: [Link] -> ([Pair],Pair) -> Link -> Float
getEffect lins (chInputs,chOut) (src,dst) = let
	sOut = signalOut lins chInputs src
	sIn = signalIn lins chOut dst
	in sOut * sIn


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
	transit = transitLink (links net)
	feIn = getEffect (links net) charge
	feOut (a,b) = (feIn (a,b)) * (1.0 - transit a b)
	ma = findLink (getCompliment net) feOut funOut
	mb = findLink (links net) feIn funIn
	in (ma,mb)


--- modify the net based on chosen best/worst links candidates and conditions ---
subLearn	:: Net -> (MayLink,MayLink) -> (Float->Bool,Float->Bool) -> Net
subLearn t (mbest,mworst) (conA,conB) = let
	getLinks	:: MayLink -> (Float->Bool) -> [Link]
	getLinks Nothing _  = []
	getLinks (Just (heat,lin)) con	= [lin | con heat]
	goodLinks = getLinks mbest conA
	badLinks = getLinks mworst conB
	newLinks = (links t \\ badLinks) ++ goodLinks
	in Net { nodes=nodes t, nextId=nextId t, links=newLinks }
-}

--- instantiating Think class with our neural network ---
instance Think Net Neuron where
	alloc t num = let
		base = nextId t
		nr = map (Neuron standardFun) [base..(base+num-1)]
		m = Net {nodes = nr ++ nodes t, links = links t, nextId = base+num}
		in	(m,nr)

	decide t charges = round . propagate (links t) charges

	learn t charges (nOut,response)
		| response>0	= let
			simulate :: Link -> Float
			simulate ln = propagate (ln:links t) charges nOut
			candidates = zipMap (getCompliment t) simulate
			folder (l1,r1) (l2,r2)
				| r2<r1		= (l2,r2)
				| otherwise	= (l1,r1)
			(best,rv) = foldl1 folder candidates
			in if rv > (fromIntegral response) + 0.5
				then Net { nodes=nodes t, nextId=nextId t, links = best:links t }
				else t
		| response<0	= let
			simulate :: Link -> Float
			simulate ln = propagate (links t \\ [ln]) charges nOut
			candidates = zipMap (links t) simulate
			folder (l1,r1) (l2,r2)
				| r2<r1		= (l2,r2)
				| otherwise	= (l1,r1)
			(best,rv) = foldl1 folder candidates
			in if rv < (fromIntegral response) - 0.5
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
