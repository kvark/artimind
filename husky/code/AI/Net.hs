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

propagateDir,propagate	:: [Link] -> [Pair] -> Neuron -> Float
propagateDir lin charge n@(Neuron functor _) = let
	oper :: Maybe Pair -> Float
	oper Nothing = let
		gather accessor m = filter ((m==) . accessor) lin
		incidents :: [Neuron]
		incidents = map fst	$ gather snd n
		inCounter :: Neuron -> Float	-- number of outputs of a neuron
		inCounter = fromIntegral . length . gather fst
		magnitudes,inputs :: [Float]
		magnitudes = map inCounter incidents
		inputs = map (propagate lin charge) incidents
		total = functor (zipWith (/) inputs magnitudes)
		in	max 0 total
	oper (Just (_,heat)) = fromIntegral heat
	ignot = find ((n==) . fst) charge
	in oper ignot

propagate lins charge (Neuron fun n) = cached (propagateDir lins charge . Neuron fun) n

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
	-- todo: cache propagate result --
	decide t charges = round . (propagate (links t) charges)
	learn t _ _ = t
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
