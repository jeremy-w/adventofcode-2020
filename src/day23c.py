# Let's try treating a list as a map from cup => next cup!

from pprint import pprint
from collections import deque

print('===== day 23 =====')

def to_cups(i: str):
	ns = [int(x) for x in i]
	cups = (len(ns) + 1) * [0]
	for cup, next in zip(ns, ns[1:]):
		cups[cup] = next
	cups[ns[-1]] = ns[0]
	cups[0] = ns[0] # point to start
	return cups

def step(current, cups): 
	picked_up = []
	
	p = current
	for i in range(3):
		picked_up.append(cups[p])
		p = cups[p]
	#print(picked_up)
	
	dest = find_dest(current - 1, picked_up)

	# next after current is next after end up pick up range
	cups[current] = cups[picked_up[-1]]

	# next after pu is the one after dest
	cups[picked_up[-1]] = cups[dest]
	# after dest is the start of pick up range
	cups[dest] = picked_up[0]
	return cups

MAX = 9
def find_dest(n, nope):
	#assert ns != []
	# assert len([x for x in ns if x > 9 or x < 1]) == 0
	n = wrap(n)
	while n in nope:
		n = wrap(n - 1)
	return n

def wrap(n):
	if n < 1:
		n = MAX
	return n

def run(cups, steps):
	#seen = dict()
	curr = cups[0]
	for i in range(steps):
		if (i % 1_000_000) == 0:
			print(i)
		#h = str(cups)
		#seen[h] = i
		
		cups = step(curr, cups)
		curr = cups[curr]
	return cups

def solution(cups):
	result = []
	c = 1
	for i in range(len(cups) - 2):
		result.append(cups[c])
		c = cups[c]
	return ''.join((str(x) for x in result))

def mk_ex():
	return to_cups("389125467")

example = to_cups("389125467")
real_input = to_cups("716892543")

p1_answer = solution(run(real_input, 100))
print('part 1 answer:', p1_answer)
assert p1_answer == '49725386', 'wrong p1 answer'

ONE_MIL = 1_000_000
MAX = ONE_MIL

def desc(cups, i=0, n=20):
	if n > len(cups):
		n = len(cups)
	c = i
	ans = []
	for _ in range(n):
		ans.append(cups[c])
		c = cups[c]
	return ans

def grow_input(cups):
	print('before growth', desc(cups))
	s = [i for i, c in enumerate(cups) if c == cups[0]]
	end = s[-1]
	cups[end] = 10
	rest = list(range(11, ONE_MIL+2))
	rest[-1] = cups[0]
	result = cups + rest
	assert max(result) == 1_000_000, 'max was %s' % max(result)
	assert len(result) == 1_000_000 + 1, 'len was %s - cups len %s - rest len %d' % (len(result), len(cups), len(rest))
	return result

def solution2(cups):
	a = cups[1]
	b = cups[a]
	result = a*b
	print('a', a, '- b', b, ' - part 2 solution', result)
	return result

example = to_cups("389125467")
real_input = to_cups("716892543")
e2 = grow_input(example)
print(desc(e2), desc(e2, i=len(e2) - 10))
a2 = grow_input(real_input)

ten_mil = 10_000_000

def p2(cups):
	print('solving part 2')
	after = run(cups, ten_mil)
	result = solution2(after)
	print('done', result)
	return result

expected = 149245887792
assert p2(e2) == expected, "failed p2 test"

print("ok - p2 test")

p2(a2)
# answer: 538935646702
