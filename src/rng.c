/*
 * rng.c -- xoshiro256+ uniform generator seeded via splitmix64.
 *
 * Pure C, no R dependency; safe to call from OpenMP worker threads (each thread
 * uses its own rngt).  See ibgs.h for the public interface.
 *
 * Why a custom RNG?  The parallel block screening (glm.c) needs a generator
 * that (a) can be advanced independently on each thread with no shared state or
 * locking, and (b) produces results that do not depend on how the work is split
 * across threads.  R's own RNG is global mutable state and is unsafe to call
 * from worker threads, so each block instead carries a private xoshiro256+
 * state seeded deterministically (the seeds themselves are drawn serially from
 * R's RNG in drwblks(); see glm.c).
 *
 * The algorithms here are by Blackman & Vigna (https://prng.di.unimi.it/):
 *   - splitmix64  : a fast mixing function used to expand one 64-bit seed into
 *                   the 256-bit xoshiro state.
 *   - xoshiro256+ : the actual stream generator (xor / shift / rotate, hence
 *                   "xoshiro").
 */
#include "ibgs.h"

/*
 * splitmix64: a SplitMix-style mixer.  Each call advances the input counter by
 * the odd constant 0x9E3779B97F4A7C15 (the 64-bit fixed point of the golden
 * ratio, phi*2^64) and then runs the integer through two multiply-xorshift
 * "avalanche" rounds so that incrementing the counter by 1 scrambles all 64
 * output bits.  We use it only to turn a single seed into four well-mixed
 * 64-bit words; this matters because xoshiro behaves poorly if seeded with a
 * state that is mostly zero, and splitmix64 guarantees a high-entropy fill.
 */
static uint64_t splitmix64(uint64_t *x)
{
    uint64_t z = (*x += 0x9E3779B97F4A7C15ULL);
    z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9ULL;
    z = (z ^ (z >> 27)) * 0x94D049BB133111EBULL;
    return z ^ (z >> 31);
}

/* Expand the 64-bit seed into the 256-bit xoshiro state with four splitmix64
 * draws.  Any seed (including 0) yields a non-degenerate state this way. */
void rngseed(rngt *r, uint64_t seed)
{
    uint64_t sm = seed;
    for (int i = 0; i < 4; i++) r->s[i] = splitmix64(&sm);
}

/* 64-bit left rotation (bits shifted off the top re-enter at the bottom). */
static inline uint64_t rotl(uint64_t x, int kk) { return (x << kk) | (x >> (64 - kk)); }

/*
 * xoshiro256+ : period 2^256 - 1, state = four 64-bit words.
 *
 * The output is simply s[0] + s[3] (the "+" variant).  This is the form the
 * authors recommend specifically for producing floating-point numbers: it is
 * the fastest variant (no output scrambler), and although its lowest few bits
 * have low linear complexity, rngunif() below keeps only the top 53 bits, so
 * those weak low bits are never seen.  (The xoshiro256** / ++ variants spend
 * extra work hardening the low bits and are only needed when the full 64-bit
 * integer output is consumed.)
 *
 * The body is the standard xoshiro state transition: one xor/shift term `t`,
 * four xors that mix the words, and a final rotate of s[3].
 */
static uint64_t xonext(rngt *r)
{
    uint64_t *s = r->s;
    uint64_t result = s[0] + s[3];
    uint64_t t = s[1] << 17;
    s[2] ^= s[0]; s[3] ^= s[1]; s[1] ^= s[2]; s[0] ^= s[3];
    s[2] ^= t;    s[3] = rotl(s[3], 45);
    return result;
}

/*
 * Uniform double in [0, 1) with full 53-bit (IEEE double mantissa) resolution.
 * Take the top 53 bits of the 64-bit output (>> 11 discards the weak low 11
 * bits) to get an integer in {0, ..., 2^53 - 1}, then divide by 2^53 =
 * 9007199254740992.  Every representable multiple of 2^-53 in [0,1) is hit with
 * equal probability, and 1.0 is never returned.
 */
double rngunif(rngt *r)
{
    return (xonext(r) >> 11) * (1.0 / 9007199254740992.0);
}
