#include <cmath>
#include <iostream>
#include <utility>

template <typename T> T my_min(T a, T b)
{
    return a < b ? a : b;
}

template <unsigned N, unsigned D> struct helper {
    static const bool is_prime =
        N % D == 0 ? false : helper<N, D - 1>::is_prime;
    const static unsigned least_divisor = helper<N, D - 1>::least_divisor != 0
                                              ? helper<N, D - 1>::least_divisor
                                          : N % D == 0 ? D
                                                       : 0;
};

template <unsigned N> struct helper<N, 1u> {
    static const bool is_prime = true;
    const static unsigned least_divisor = 0;
};

template <unsigned N, unsigned D>
const bool helper_is_prime_v = helper<N, D>::is_prime;

template <unsigned N, unsigned D>
const unsigned helper_least_divisor_v = helper<N, D>::least_divisor;

template <unsigned N> struct is_prime {
    static const bool value =
        helper_is_prime_v<N, static_cast<unsigned>(sqrt(N))>;
};

template <> struct is_prime<1u> {
    static const bool value = false;
};

template <unsigned N> const bool is_prime_v = is_prime<N>::value;

template <unsigned N> struct least_divisor {
    const static unsigned value =
        helper_least_divisor_v<N, static_cast<unsigned>(sqrt(N))> == 0
            ? N
            : helper_least_divisor_v<N, static_cast<unsigned>(sqrt(N))>;
};

template <> struct least_divisor<1u> {
    const static unsigned value = 1u;
};

template <unsigned N> const unsigned least_divisor_v = least_divisor<N>::value;

template <unsigned N, unsigned D> struct is_pow {
    static const bool value = N % D != 0 ? false : is_pow<N / D, D>::value;
};

template <unsigned D> struct is_pow<D, D> {
    static const bool value = true;
};

template <unsigned D> struct is_pow<0u, D> {
    static const bool value = false;
};

template <unsigned N, unsigned D> const bool is_pow_v = is_pow<N, D>::value;

template <unsigned N> struct has_primitive_root {
    static const bool value =
        N % 4 == 0
            ? false
            : is_pow_v<N / (2 - (N % 2)), least_divisor_v<N / (2 - (N % 2))>>;
};

template <> struct has_primitive_root<1u> {
    static const bool value = false;
};

template <> struct has_primitive_root<2u> {
    static const bool value = true;
};

template <> struct has_primitive_root<4u> {
    static const bool value = true;
};

template <unsigned N>
const bool has_primitive_root_v = has_primitive_root<N>::value;

unsigned phi(unsigned n)
{
    unsigned result = n;
    for (unsigned i = 2; i * i <= n; ++i) {
        if (n % i == 0) {
            while (n % i == 0) {
                n /= i;
            }
            result -= result / i;
        }
    }
    if (n > 1) {
        result -= result / n;
    }
    return result;
}

template <typename T> T my_gcd(T a, T b)
{
    while (b) {
        a %= b;
        std::swap(a, b);
    }
    return a;
}

template <unsigned N> class Residue
{
  private:
    unsigned value = 0;

    static void N_is_prime_checker()
    {
        int a[is_prime_v<N> ? 1 : -1];
        a[0] = 0;
        std::cerr << a[0];
    }

  public:
    Residue()
    {
    }

    explicit Residue(int x)
    {
        if (x < 0) {
            value = N - ((-x) % N);
        } else {
            value = x % N;
        }
    }

    explicit operator int() const
    {
        return static_cast<int>(value);
    }

    Residue& operator+=(Residue other)
    {
        value += other.value;
        value %= N;
        return *this;
    }

    Residue& operator-=(Residue other)
    {
        value += N - other.value;
        value %= N;
        return *this;
    }

    Residue& operator*=(Residue other)
    {
        value = (static_cast<unsigned long long>(value) *
                 static_cast<unsigned long long>(other.value)) %
                N;
        return *this;
    }

    Residue operator+(Residue other) const
    {
        Residue copy = Residue(value);
        copy += other;
        return copy;
    }

    Residue operator-(Residue other) const
    {
        Residue copy = Residue(value);
        copy -= other;
        return copy;
    }

    Residue operator*(Residue other) const
    {
        Residue copy = Residue(value);
        copy *= other;
        return copy;
    }

    bool operator==(Residue other) const
    {
        return value == other.value;
    }

    bool operator!=(Residue other) const
    {
        return !(*this == other);
    }

    Residue pow(unsigned k) const
    {
        Residue result = Residue(1);
        Residue a = *this;
        while (k) {
            if (k & 1) {
                result *= a;
            }
            a *= a;
            k /= 2;
        }
        return result;
    }

    Residue pow(signed k) const = delete;

    Residue getInverse() const
    {
        N_is_prime_checker();
        Residue result = *this;
        result = result.pow(N - 2);
        return result;
    }

    Residue& operator/=(Residue other)
    {
        N_is_prime_checker();
        *this *= other.getInverse();
        return *this;
    }

    Residue operator/(Residue other) const
    {
        N_is_prime_checker();
        Residue copy = Residue(value);
        copy /= other;
        return copy;
    }

    unsigned order() const
    {
        if (my_gcd(value, N) != 1) {
            return 0;
        }
        unsigned phi_n = phi(N);
        if (value == 1) {
            return 1;
        }
        unsigned result = phi_n;
        for (unsigned i = 2; i * i <= N; ++i) {
            if (phi_n % i == 0) {
                if (pow(i) == Residue(1)) {
                    result = i;
                    break;
                }
                if (pow(N / i) == Residue(1)) {
                    result = N / i;
                }
            }
        }
        return result;
    }

    static Residue getPrimitiveRoot()
    {
        if (N == 2) {
            return Residue(1);
        }
        unsigned phi_n = phi(N);
        for (unsigned i = 2; i < N; ++i) {
            if (my_gcd(i, N) != 1)
                continue;
            if (Residue(i).order() == phi_n) {
                return Residue(i);
            }
        }
        return Residue(0);
    }
};
