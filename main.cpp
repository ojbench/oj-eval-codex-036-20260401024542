#include <vector>
#include <cstring>
#include <cstdint>
#include <iostream>
#include <algorithm>
#include <string>

struct dynamic_bitset {
    static constexpr std::size_t B = 64;
    std::vector<unsigned long long> a;
    std::size_t n = 0; // number of bits

    dynamic_bitset() = default;
    ~dynamic_bitset() = default;
    dynamic_bitset(const dynamic_bitset&) = default;
    dynamic_bitset& operator=(const dynamic_bitset&) = default;

    dynamic_bitset(std::size_t n_) : a((n_ + B - 1) / B, 0ULL), n(n_) {}

    dynamic_bitset(const std::string &str) {
        n = str.size();
        a.assign((n + B - 1) / B, 0ULL);
        for (std::size_t i = 0; i < n; ++i) {
            char c = str[i];
            if (c == '1') set(i, true);
            else set(i, false);
        }
    }

    static inline std::size_t idx(std::size_t i) { return i / B; }
    static inline std::size_t off(std::size_t i) { return i % B; }

    bool operator[](std::size_t i) const {
        if (i >= n) return false;
        return (a[idx(i)] >> off(i)) & 1ULL;
    }

    dynamic_bitset &set(std::size_t i, bool val = true) {
        if (i >= n) return *this; // ignore out-of-range
        unsigned long long mask = 1ULL << off(i);
        if (val) a[idx(i)] |= mask;
        else a[idx(i)] &= ~mask;
        return *this;
    }

    dynamic_bitset &push_back(bool val) {
        std::size_t oldn = n;
        ++n;
        if (idx(oldn) >= a.size()) a.push_back(0ULL);
        set(oldn, val);
        return *this;
    }

    bool none() const {
        if (n == 0) return true;
        std::size_t full = n / B;
        for (std::size_t i = 0; i < full; ++i) if (a[i]) return false;
        std::size_t rem = n % B;
        if (rem == 0) return true;
        unsigned long long mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1ULL));
        return (a[full] & mask) == 0ULL;
    }
    bool all() const {
        if (n == 0) return true; // convention: empty -> all true
        std::size_t full = n / B;
        for (std::size_t i = 0; i < full; ++i) {
            if (~a[i]) return false;
        }
        // handle last partial block
        std::size_t rem = n % B;
        if (rem == 0) return true;
        unsigned long long mask = (rem == 64 ? ~0ULL : ((1ULL << rem) - 1ULL));
        return (a[full] & mask) == mask;
    }

    std::size_t size() const { return n; }

    dynamic_bitset &operator|=(const dynamic_bitset &other) {
        std::size_t m = std::min(n, other.n);
        if (m == 0) return *this;
        std::size_t full = m / B;
        std::size_t rem = m % B;
        for (std::size_t i = 0; i < full; ++i) {
            a[i] |= other.a[i];
        }
        if (rem) {
            std::size_t last = full;
            unsigned long long mask = (1ULL << rem) - 1ULL;
            unsigned long long part = ((a[last] | other.a[last]) & mask);
            a[last] = (a[last] & ~mask) | part;
        }
        return *this;
    }

    dynamic_bitset &operator&=(const dynamic_bitset &other) {
        std::size_t m = std::min(n, other.n);
        if (m == 0) return *this;
        std::size_t full = m / B;
        std::size_t rem = m % B;
        for (std::size_t i = 0; i < full; ++i) {
            a[i] &= other.a[i];
        }
        if (rem) {
            std::size_t last = full;
            unsigned long long mask = (1ULL << rem) - 1ULL;
            unsigned long long part = ((a[last] & other.a[last]) & mask);
            a[last] = (a[last] & ~mask) | part;
        }
        return *this;
    }

    dynamic_bitset &operator^=(const dynamic_bitset &other) {
        std::size_t m = std::min(n, other.n);
        if (m == 0) return *this;
        std::size_t full = m / B;
        std::size_t rem = m % B;
        for (std::size_t i = 0; i < full; ++i) {
            a[i] ^= other.a[i];
        }
        if (rem) {
            std::size_t last = full;
            unsigned long long mask = (1ULL << rem) - 1ULL;
            unsigned long long part = ((a[last] ^ other.a[last]) & mask);
            a[last] = (a[last] & ~mask) | part;
        }
        return *this;
    }

    dynamic_bitset &operator<<=(std::size_t k) {
        if (k == 0 || n == 0) return *this;
        std::size_t newn = n + k;
        std::size_t blocks_old = a.size();
        std::size_t blocks_new = (newn + B - 1) / B;
        a.resize(blocks_new, 0ULL);
        std::size_t sh_blocks = k / B;
        std::size_t sh_bits = k % B;
        if (sh_bits == 0) {
            for (std::size_t i = blocks_old; i-- > 0;) {
                a[i + sh_blocks] = a[i];
            }
        } else {
            for (std::size_t i = blocks_old; i-- > 0;) {
                unsigned long long cur = a[i];
                unsigned long long hi = cur << sh_bits;
                unsigned long long lo = (i ? (a[i - 1] >> (B - sh_bits)) : 0ULL);
                a[i + sh_blocks] = hi | lo;
            }
        }
        std::fill(a.begin(), a.begin() + sh_blocks, 0ULL);
        n = newn;
        return *this;
    }

    dynamic_bitset &operator>>=(std::size_t k) {
        if (k == 0 || n == 0) return *this;
        if (k >= n) {
            n = 0;
            a.clear();
            return *this;
        }
        std::size_t sh_blocks = k / B;
        std::size_t sh_bits = k % B;
        std::size_t blocks_old = a.size();
        if (sh_bits == 0) {
            for (std::size_t i = 0; i + sh_blocks < blocks_old; ++i) {
                a[i] = a[i + sh_blocks];
            }
        } else {
            for (std::size_t i = 0; i + sh_blocks < blocks_old; ++i) {
                unsigned long long cur = a[i + sh_blocks];
                unsigned long long hi = cur >> sh_bits;
                unsigned long long lo = 0ULL;
                if (i + sh_blocks + 1 < blocks_old) lo = a[i + sh_blocks + 1] << (B - sh_bits);
                a[i] = hi | lo;
            }
        }
        // Update length and trim extra blocks
        n -= k;
        a.resize((n + B - 1) / B);
        if (!a.empty() && (n % B)) {
            unsigned long long mask = (1ULL << (n % B)) - 1ULL;
            a.back() &= mask;
        }
        return *this;
    }

    dynamic_bitset &set() {
        if (n == 0) return *this;
        std::size_t blocks = a.size();
        std::fill(a.begin(), a.end(), ~0ULL);
        if (n % B) {
            unsigned long long mask = (1ULL << (n % B)) - 1ULL;
            a.back() = mask;
        }
        return *this;
    }

    dynamic_bitset &flip() {
        for (auto &v : a) v = ~v;
        if (!a.empty() && (n % B)) {
            unsigned long long mask = (1ULL << (n % B)) - 1ULL;
            a.back() &= mask;
        }
        return *this;
    }

    dynamic_bitset &reset() {
        std::fill(a.begin(), a.end(), 0ULL);
        return *this;
    }
};

// A simple command-based interface to exercise the bitset.
// Format:
// First line: integer Q operations
// Then Q lines of operations among:
//   init n            -> reinit bitset with length n (all 0)
//   init_str s        -> init from string s (lowest bit first)
//   get i             -> print 0/1
//   set i v           -> set bit i to v (0/1)
//   push v            -> push_back v (0/1)
//   none              -> print 1 if none else 0
//   all               -> print 1 if all else 0
//   size              -> print size
//   or s|i j          -> bitwise op with another bitset: provide string or integer length then string
//   and ...           -> same as or
//   xor ...           -> same as or
//   shl k             -> <<= k
//   shr k             -> >>= k
// For binary ops we accept: provide a string argument following command.

int main() {
    std::ios::sync_with_stdio(false);
    std::cin.tie(nullptr);

    dynamic_bitset bs;
    int Q;
    if (!(std::cin >> Q)) return 0;
    for (int qi = 0; qi < Q; ++qi) {
        std::string cmd; std::cin >> cmd;
        if (cmd == "init") {
            std::size_t n; std::cin >> n; bs = dynamic_bitset(n);
        } else if (cmd == "init_str") {
            std::string s; std::cin >> s; bs = dynamic_bitset(s);
        } else if (cmd == "get") {
            std::size_t i; std::cin >> i; std::cout << (bs[i] ? 1 : 0) << '\n';
        } else if (cmd == "set") {
            std::size_t i; int v; std::cin >> i >> v; bs.set(i, v != 0);
        } else if (cmd == "push" || cmd == "push_back") {
            int v; std::cin >> v; bs.push_back(v != 0);
        } else if (cmd == "none") {
            std::cout << (bs.none() ? 1 : 0) << '\n';
        } else if (cmd == "all") {
            std::cout << (bs.all() ? 1 : 0) << '\n';
        } else if (cmd == "size") {
            std::cout << bs.size() << '\n';
        } else if (cmd == "or" || cmd == "and" || cmd == "xor" || cmd == "|" || cmd == "&" || cmd == "^") {
            std::string s; std::cin >> s; dynamic_bitset other(s);
            if (cmd == "or" || cmd == "|") bs |= other;
            else if (cmd == "and" || cmd == "&") bs &= other;
            else bs ^= other;
        } else if (cmd == "setall" || cmd == "set_all") {
            bs.set();
        } else if (cmd == "resetall" || cmd == "reset_all" || cmd == "reset") {
            bs.reset();
        } else if (cmd == "flipall" || cmd == "flip_all" || cmd == "flip") {
            bs.flip();
        } else if (cmd == "shl" || cmd == "<<" || cmd == "left" || cmd == "lshift") {
            std::size_t k; std::cin >> k; bs <<= k;
        } else if (cmd == "shr" || cmd == ">>" || cmd == "right" || cmd == "rshift") {
            std::size_t k; std::cin >> k; bs >>= k;
        } else {
            // unknown command: ignore line
        }
    }
    return 0;
}
