

# Used to derive the pubkeys by running a secret number of rounds called the loop size.
PRESHARED_KEY = 7

def transform(key, loop_size):
	value = 1
	for round in range(loop_size):
		value = run_round(value, key)
	return value

# Used in running rounds.
# It's a prime number.
MOD_NUMBER = 20_201_227

def run_round(value, key):
	mult = value * key
	result = mult % MOD_NUMBER
	return result

# Can we skip intermediate mod? Yes.
def transform_pow(key, loop_size):
	return key**loop_size % MOD_NUMBER

public_keys = [15_733_400, 6_408_062]

example_keys = [5_764_801, 17_807_724]
example_loops = [8, 11]
example_encryption_key = 14897079

# Verify we can find the encryption key once we find a loops value.
assert transform(example_keys[1], example_loops[0]) == example_encryption_key
assert transform(example_keys[0], example_loops[1]) == example_encryption_key

assert transform(example_keys[1], example_loops[0]) == transform_pow(example_keys[1], example_loops[0])

# Now goal is to find the loop value (secret input) for one of the
# pubkeys (output) and the preshared key (known input) to derive
# the encryption key (puzzle answer for part 1).

# set up so we can resume later
def find_rounds(pubkey_output, value = 1, after_rounds = 0):
	while value != pubkey_output:
		#ivalue = value
		# zomg inlining run_round made it finish instantly
		value = (value * PRESHARED_KEY) % MOD_NUMBER
		#value = run_round(value, PRESHARED_KEY)
		after_rounds += 1
		# print('find_rounds(%s, value=%s, after_rounds=%s) = %s' % (pubkey_output, ivalue, after_rounds, value))
	return after_rounds

def find_rounds_pow(pubkey_output):
	p = 0
	while transform_pow(PRESHARED_KEY, p) != pubkey_output:
		p += 1
		if p % 10_000 == 0:
			print('tried', p)
	return p

t0 = find_rounds(example_keys[0])
assert t0 == example_loops[0], "bad rounds output of %s, expected %s" % (t0, example_loops[0])
print('ok - correctly found rounds for example')

N = 1
real_rounds = find_rounds(public_keys[N])
print('rounds for pubkey[%s]: %s' % (N, real_rounds))

# rounds for pubkey[1]: 10459425

p1 = transform(public_keys[1-N], real_rounds)
print('p1 answer:', p1)

# answer: 16457981

# p2 is blocked till you have 49 stars - every other puzzle must have been solved!
# As of Christmas, I still need:
# day 20 both (puzzle piece rotations)
# day 22 p2 (recursive war - probably nbd using my existing Mercury code, just was fatigued then)
# day 23 p2 (10mil rounds of cups)
