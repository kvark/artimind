namespace artimind

import System

public class World:
	public food	as bool	= false

env = World()
print "Hello, World!"

ct = Cat('cell',env)
while true:
	print "(food:${env.food})"
	key = Console.ReadKey(true)
	if key.Key == ConsoleKey.Escape:
		break
	elif key.KeyChar == char('f'):
		env.food = true
	elif key.Key == ConsoleKey.Spacebar:
		met = ct.step()
		if met:	print "'${ct.name}' does '${met.Name}', result ${met.result}"
		else:	print "'${ct.name}' cant make a choice"
