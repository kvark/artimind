namespace artimind

import System
import System.Collections.Generic

public struct Number:
	private static final Decay	= 0.9f
	private static final MaxVal	= 1f / (1f-Decay)
	private static final KT		= Math.Log(Decay)

	private timeBase	as single
	private val			as single
	
	private def constructor(init as single):
		timeBase,val = 0f,init
	
	public static final	Zero	= Number(0f)
	public static final	One		= Number(1f)
	
	public Value[time as single] as single:
		# normalizing the result
		get: return (1f-Decay) * val * Math.Exp( KT*(time-timeBase) )
		set: # value is RELATIVE
			multi = Math.Exp( KT*(timeBase-time) )
			if multi > 1.0e5:
				timeBase = time
				val /= multi
				multi = 1f
			val += multi * value



public class Axon:
	public dest		as Neuron	= null
	public power	as single	= 0f
	public response	= Number.One
	public profit	= Number.One
	public def getCovariation(n as Neuron, t as single) as single:
		return profit.Value[t] - response.Value[t] * n.load.Value[t]


public class Neuron:
	public final arms	= List[of Axon]()
	public charge	as single	= 0f
	public response	as single	= 0f
	public sum		as single	= 0f
	public load		= Number.Zero
	
	public def linkTo(n as Neuron) as void:
		arms.Add( Axon(dest:n,power:1f) )
		sum += 1f
	public def cutOff() as void:
		arms.Clear()
		sum = 0f
	public def clean() as void:
		charge = response = 0f
		return	if sum < 1.0e10
		for ax in arms:
			ax.power *= 0.01f
		sync()
	public def sync() as void:
		sum = 0f
		for ax in arms:
			sum += ax.power
