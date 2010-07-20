namespace artimind

import System
import System.Collections.Generic

public struct Number:
	private final static KT	= 1.5f
	private timeBase	as single
	private val			as single
	
	public def constructor(init as single):
		timeBase,val = 0f,init
	
	public Value[time as single] as single:
		get: return val * Math.Exp( KT*(timeBase-time) )
		set: # value is RELATIVE
			multi = Math.Exp( KT*(time-timeBase) )
			if multi > 1.0e12:
				timeBase = time
				val /= multi
				multi = 1f
			val += multi * value



public class Axon:
	public dest		as Neuron	= null
	public power	as single	= 0f
	public response	 = Number(0f)


public class Neuron:
	public final arms	= List[of Axon]()
	public charge	as single	= 0f
	public sum		as single	= 0f
	public load		= Number(0f)
	
	public def linkTo(n as Neuron) as void:
		arms.Add( Axon(dest:n,power:1f) )
		sum += 1f
	public def cutOff() as void:
		arms.Clear()
		sum = 0f
	public def clean() as void:
		charge = 0f
		return	if sum < 1.0e10
		sum = 0f
		for ax in arms:
			ax.power *= 0.01f
			sum += ax.power
