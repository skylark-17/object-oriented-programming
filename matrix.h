#pragma GCC optimize("02")
#include <algorithm>
#include <cmath>
#include <complex>
#include <cstdio>
#include <cstring>
#include <initializer_list>
#include <iostream>
#include <string>
#include <utility>
#include <vector>

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
    explicit Residue<N>(int x)
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
    Residue<N>& operator+=(Residue<N> other)
    {
        value += other.value;
        value %= N;
        return *this;
    }
    Residue<N> operator+(Residue<N> other) const
    {
        Residue<N> copy = Residue<N>(value);
        copy += other;
        return copy;
    }
    Residue<N>& operator-=(Residue<N> other)
    {
        value += N - other.value;
        value %= N;
        return *this;
    }
    Residue<N> operator-(Residue<N> other) const
    {
        Residue<N> copy = Residue<N>(value);
        copy -= other;
        return copy;
    }
    Residue<N>& operator*=(Residue<N> other)
    {
        value = (static_cast<unsigned long long>(value) *
                 static_cast<unsigned long long>(other.value)) %
                N;
        return *this;
    }
    Residue<N> operator*(Residue<N> other) const
    {
        Residue<N> copy = Residue<N>(value);
        copy *= other;
        return copy;
    }
    bool operator==(Residue<N> other) const
    {
        return value == other.value;
    }
    bool operator!=(Residue<N> other) const
    {
        return !(*this == other);
    }
    Residue<N> pow(unsigned k) const
    {
        Residue<N> result = Residue<N>(1);
        Residue<N> a = *this;
        while (k) {
            if (k & 1) {
                result *= a;
            }
            a *= a;
            k /= 2;
        }
        return result;
    }
    Residue<N> pow(signed k) const = delete;
    Residue<N> getInverse() const
    {
        N_is_prime_checker();
        Residue<N> result = *this;
        result = result.pow(N - 2);
        return result;
    }
    Residue<N>& operator/=(Residue<N> other)
    {
        N_is_prime_checker();
        *this *= other.getInverse();
        return *this;
    }
    Residue<N> operator/(Residue<N> other) const
    {
        N_is_prime_checker();
        Residue<N> copy = Residue<N>(value);
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
                if (pow(i) == Residue<N>(1)) {
                    result = i;
                    break;
                }
                if (pow(N / i) == Residue<N>(1)) {
                    result = N / i;
                }
            }
        }
        return result;
    }
    static Residue<N> getPrimitiveRoot()
    {
        if (N == 2) {
            return Residue<N>(1);
        }
        unsigned phi_n = phi(N);
        for (unsigned i = 2; i < N; ++i) {
            if (my_gcd(i, N) != 1)
                continue;
            if (Residue<N>(i).order() == phi_n) {
                return Residue<N>(i);
            }
        }
        return Residue<N>(0);
    }
};

template <unsigned N>
std::ostream& operator<<(std::ostream& out, const Residue<N>& residue)
{
    out << int(residue);
    return out;
}

template <unsigned N>
std::istream& operator>>(std::istream& in, Residue<N>& residue)
{
    int x;
    in >> x;
    residue = Residue<N>(x);
    return in;
}

// Namespace Fast Fourier Transformation

template <typename T> T my_pow(T x, T y, T mod)
{ // x ^ y
    T result = 1;
    while (y) {
        if (y & 1) {
            result *= x;
            result %= mod;
        }
        x *= x;
        x %= mod;
        y /= 2;
    }
    return result;
}

using ll = long long;
using ld = double;
using cld = std::complex<ld>;
using double_polynom = std::vector<ld>;
using complex_polynom = std::vector<cld>;
using integer_polynom = std::vector<ll>;

ld pi = 3.141592653589793238462643383279502884197169;

integer_polynom rev[30];
complex_polynom w[30];
bool caluculated[30];

namespace NFFT
{
void precalc(int N)
{
    int logn = 0;
    int p = 1;
    while (p < N) {
        p *= 2;
        logn++;
    }
    if (caluculated[logn])
        return;
    caluculated[logn] = true;
    rev[logn].resize(N);
    rev[logn][0] = 0;
    for (int i = 1; i < N; ++i) {
        rev[logn][i] = rev[logn][i / 2] / 2;
        if ((i & 1) == 1)
            rev[logn][i] += N / 2;
    }
    w[logn].resize(N, 0);
    w[logn][1] = cld(1);
    for (int L = 1; L < N / 2; L *= 2) {
        for (int i = 0; i < L; i++) {
            w[logn][L * 2 + i * 2] = w[logn][L + i];
            w[logn][L * 2 + i * 2 + 1] =
                w[logn][L + i] * std::polar(ld(1), ld(pi) / ld(L) / 2.0);
        }
    }
}

void fft(cld* P, int n)
{
    if (n == 1) {
        return;
    }
    if (n == 2) {
        int logn = 1;
        for (int i = 0; i < n; ++i) {
            if (i < rev[logn][i])
                std::swap(P[i], P[rev[logn][i]]);
        }
        cld u = P[0];
        cld v = P[1] * w[logn][1];
        P[0] = u + v;
        P[1] = u - v;
        return;
    }
    int logn = 0;
    int p = 1;
    while (p < n) {
        p *= 2;
        logn++;
    }
    for (int i = 0; i < n; ++i) {
        if (i < rev[logn][i])
            std::swap(P[i], P[rev[logn][i]]);
    }
    for (int L = 1; L <= n / 2; L *= 2) {
        for (int i = 0; i < n; i += 2 * L) {
            cld* it1 = P + i;
            cld* it2 = P + i + L;
            std::vector<cld>::iterator it3 = w[logn].begin() + L;
            for (int j = 0; j < L; ++j, ++it1, ++it2, ++it3) {
                cld u = *it1;
                cld v = *it2 * *it3;
                *it1 = u + v;
                *it2 = u - v;
            }
        }
    }
}

integer_polynom multiply(const integer_polynom s1, const integer_polynom& s2)
{
    std::cout << "mult - ";
    int M = std::max(s1.size(), s2.size());
    int N = 1;
    while (N < M) {
        N *= 2;
    }
    N *= 2;
    precalc(N);
    cld* c1 = new cld[N];
    for (size_t i = 0; i < s1.size(); ++i) {
        c1[i] = s1[i];
    }
    cld* c2 = new cld[N];
    for (size_t i = 0; i < s2.size(); ++i) {
        c2[i] = s2[i];
    }
    fft(c1, N);
    fft(c2, N);
    for (int i = 0; i < N; ++i) {
        c1[i] = c1[i] * c2[i];
    }
    fft(c1, N);
    reverse(c1 + 1, c1 + N);
    integer_polynom ans(N);
    for (int i = 0; i < N; ++i) {
        ans[i] = (round(real(c1[i] / cld(N))));
    }
    delete[] c1;
    delete[] c2;
    std::cout << "correct\n";
    return ans;
}

} // end of namespace NFFT

class BigInteger
{ // a[0] - smallest
  public:
    integer_polynom a;
    static const int base = 10;
    bool is_positive = true;
    void shrink_to_fit()
    {
        if (a.empty()) {
            a.push_back(0);
            is_positive = true;
        }
        while (a.size() > 1 && a.back() == 0) {
            a.pop_back();
        }
        if (a.size() == 1 && a[0] == 0) {
            is_positive = true;
        }
        a.shrink_to_fit();
    }

    void update()
    {
        for (size_t i = 0; i < a.size(); i++) {
            if (a[i] < 0) {
                if (i + 1 == a.size())
                    a.push_back(0);
                a[i + 1] -= (-a[i] + base - 1) / base;
                a[i] += ((-a[i] + base - 1) / base) * base;
                continue;
            }
            if (a[i] < base) {
                break;
            }
            if (a[i] >= base) {
                if (i + 1 == a.size())
                    a.push_back(0);
                a[i + 1] += a[i] / base;
                a[i] %= base;
            }
        }
        shrink_to_fit();
        if (a.back() < 0) {
            a.back() *= -1;
            is_positive ^= 1;
        }
    }
    int last_not_zero() const
    {
        int last_not_zero = -1;
        for (size_t i = 0; i < a.size(); i++) {
            if (a[i] != 0)
                last_not_zero = i;
        }
        return last_not_zero;
    }

  public:
    bool is_zero() const
    {
        return last_not_zero() == -1;
    }

  public:
    friend BigInteger abs(const BigInteger& A);
    friend bool operator<(const BigInteger& A, const BigInteger& B);
    friend bool operator>(const BigInteger& A, const BigInteger& B);
    friend bool operator<=(const BigInteger& A, const BigInteger& B);
    friend bool operator>=(const BigInteger& A, const BigInteger& B);
    friend bool operator==(const BigInteger& A, const BigInteger& B);
    friend bool operator!=(const BigInteger& A, const BigInteger& B);
    friend BigInteger operator*(const BigInteger& A, const BigInteger& B);
    friend BigInteger operator/(const BigInteger& A, const BigInteger& B);
    friend BigInteger operator+(const BigInteger& A, const BigInteger& B);
    friend BigInteger operator-(const BigInteger& A, const BigInteger& B);
    friend BigInteger operator%(const BigInteger& A, const BigInteger& B);
    BigInteger() : a(1, 0), is_positive(true)
    {
    }

    BigInteger(int x) : a(1, std::abs(x))
    {
        while (a.back() >= base) {
            int y = a.back() / base;
            a.back() %= base;
            a.push_back(0);
            a.back() += y;
        }
        is_positive = x >= 0;
    }
    BigInteger(const std::string& S) : is_positive(S[0] != '-')
    {
        if (S == "-0") {
            a = {0};
            return;
        }
        a.reserve(S.size());
        for (int i = S.size() - 1; i >= 0; --i) {
            if (i == 0 && !is_positive)
                break;
            a.push_back(S[i] - '0');
        }
    }
    BigInteger(const char* S) : is_positive(S[0] != '-')
    {
        int sz = std::strlen(S);
        if (sz == 2 && S[0] == '-' && S[0] == '0') {
            a = {0};
            return;
        }
        a.reserve(sz);
        for (int i = sz - 1; i >= 0; --i) {
            if (i == 0 && !is_positive)
                break;
            a.push_back(S[i] - '0');
        }
    }
    BigInteger(const BigInteger& A) : a(A.a), is_positive(A.is_positive)
    {
    }
    void swap(BigInteger& A)
    {
        std::swap(A.a, a);
        std::swap(A.is_positive, is_positive);
    }

    BigInteger& operator=(const BigInteger& A)
    {
        BigInteger new_int = A;
        swap(new_int);
        return *this;
    }
    BigInteger operator-() const
    {
        BigInteger res = *this;
        if (last_not_zero() == -1)
            return res;
        else
            res.is_positive ^= 1;
        return res;
    }

    BigInteger& operator+=(const BigInteger& B)
    {
        if (this == &B) {
            BigInteger copy = B;
            *this += copy;
            return *this;
        }
        if (*this == 0) {
            *this = B;
            return *this;
        }
        if (is_positive == B.is_positive) {
            a.resize(std::max(B.a.size(), a.size()), 0);
            for (size_t i = 0; i < a.size(); ++i) {
                if (i < B.a.size()) {
                    a[i] += B.a[i];
                }
                while (a[i] >= base) {
                    if (i + 1 == a.size()) {
                        a.push_back(0);
                    }
                    a[i] -= base;
                    a[i + 1]++;
                }
            }
        } else {
            if (abs(*this) >= abs(B)) {
                for (size_t i = 0; i < a.size(); ++i) {
                    if (i < B.a.size()) {
                        a[i] -= B.a[i];
                    }
                    while (a[i] < 0) {
                        a[i] += base;
                        a[i + 1]--;
                    }
                }
            } else {
                BigInteger C = B;
                swap(C);
                for (size_t i = 0; i < a.size(); ++i) {
                    if (i < C.a.size()) {
                        a[i] -= C.a[i];
                    }
                    while (a[i] < 0) {
                        a[i] += base;
                        a[i + 1]--;
                    }
                }
            }
        }
        shrink_to_fit();
        return *this;
    }
    BigInteger& operator-=(const BigInteger& B)
    {
        if (*this == B) {
            *this = 0;
            return *this;
        }
        *this += -B;
        return *this;
    }
    BigInteger& operator*=(const BigInteger& B)
    {
        if (*this == 0 || B == 0) {
            *this = 0;
            return *this;
        }
        if (this == &B) {
            BigInteger copy = B;
            *this *= copy;
            return *this;
        }
        if (true) {
            a = NFFT::multiply(a, B.a);
            is_positive = is_positive == B.is_positive;
            shrink_to_fit();
            return *this;
        }
        BigInteger C;
        C.a.resize(a.size() + B.a.size(), 0);
        for (size_t i = 0; i < B.a.size(); ++i) {
            for (size_t j = 0; j < a.size(); j++) {
                C.a[i + j] += a[j] * B.a[i];
            }
        }
        for (size_t i = 0; i < C.a.size(); ++i) {
            int x = C.a[i] / base;
            if (x) {
                if (i + 1 == C.a.size()) {
                    C.a.push_back(0);
                }
                C.a[i] -= base * x;
                C.a[i + 1] += x;
            }
        }
        C.is_positive = is_positive == B.is_positive;
        shrink_to_fit();
        *this = C;
        return *this;
    }
    void sdvig()
    {
        if (!a.empty())
            a.erase(a.begin());
    }
    BigInteger& operator/=(const BigInteger& B)
    {
        if (*this == B) {
            *this = 1;
            return *this;
        }
        if (*this == 0)
            return *this;
        bool f = is_positive == B.is_positive;
        BigInteger res = 0;
        BigInteger copy = *this;
        copy.is_positive = true;
        BigInteger cur = copy;
        BigInteger b = B;
        b.is_positive = true;
        while (b * base <= copy) {
            b *= base;
        }
        //std::cerr << b << " " << cur << '\n';

        while (b.last_not_zero() >= B.last_not_zero()) {
            //std::cerr << b << " " << cur << '\n';
            int cnt = 0;
            while (b <= cur) {
                cur -= b;
                ++cnt;
            }
            //std::cerr << b << " " << cur << '\n';
            res *= base;
            res += cnt;
            b.sdvig();
        }
        shrink_to_fit();
        res.is_positive = f;
        if (res == 0)
            res.is_positive = true;
        *this = res;
        if (*this == 0) {
            is_positive = true;
        }
        return *this;
    }
    BigInteger& operator%=(const BigInteger& B)
    {
        *this = *this - (*this / B) * B;
        return *this;
    }

    BigInteger& operator++()
    {
        *this += 1;
        return *this;
    }
    BigInteger operator++(int)
    {
        BigInteger copy = *this;
        ++*this;
        return copy;
    }
    BigInteger& operator--()
    {
        *this -= 1;
        return *this;
    }
    BigInteger operator--(int)
    {
        BigInteger copy = *this;
        --*this;
        return copy;
    }
    std::string toString() const
    {
        std::string S = "";
        if (!is_positive)
            S.push_back('-');
        for (int i = last_not_zero(); i >= 0; --i) {
            S.push_back(a[i] + '0');
        }
        if (last_not_zero() == -1) {
            S = "0";
        }
        return S;
    }
    explicit operator bool() const
    {
        return !is_zero();
    }
    explicit operator int() const
    {
        int x = 0;
        for (int i = last_not_zero(); i >= 0; --i) {
            x *= base;
            x += a[i];
        }
        if (!is_positive)
            x *= -1;
        return x;
    }
    friend std::ostream& operator<<(std::ostream& out, const BigInteger& A);
    friend std::istream& operator>>(std::istream& in, BigInteger& A);
};

BigInteger abs(const BigInteger& A)
{
    BigInteger B = A;
    B.is_positive = true;
    return B;
}
bool operator==(const BigInteger& A, const BigInteger& B)
{
    if (A.last_not_zero() == -1 && B.last_not_zero() == -1) {
        return true;
    }
    if (B.is_positive != A.is_positive)
        return false;
    int last_A = A.last_not_zero();
    int last_B = B.last_not_zero();
    if (last_A != last_B)
        return false;
    for (int i = 0; i <= last_A; i++) {
        if (A.a[i] != B.a[i])
            return false;
    }
    return true;
}
bool operator!=(const BigInteger& A, const BigInteger& B)
{
    return !(A == B);
}
bool operator<(const BigInteger& A, const BigInteger& B)
{
    if (B.is_positive != A.is_positive) {
        return B.is_positive;
    }
    if (A == B)
        return false;
    if (A.last_not_zero() != B.last_not_zero()) {
        return A.last_not_zero() < B.last_not_zero();
    }
    for (int i = std::min(A.a.size(), B.a.size()) - 1; i >= 0; i--) {
        if (A.a[i] != B.a[i]) {
            return (A.a[i] < B.a[i]) ^ !A.is_positive;
        }
    }
    return false;
}
bool operator<=(const BigInteger& A, const BigInteger& B)
{
    return !(A > B);
}
bool operator>=(const BigInteger& A, const BigInteger& B)
{
    return !(A < B);
}
bool operator>(const BigInteger& A, const BigInteger& B)
{
    return B < A;
}
BigInteger operator+(const BigInteger& A, const BigInteger& B)
{
    BigInteger C = A;
    C += B;
    return C;
}
BigInteger operator-(const BigInteger& A, const BigInteger& B)
{
    BigInteger C = A;
    C -= B;
    return C;
}
BigInteger operator*(const BigInteger& A, const BigInteger& B)
{
    BigInteger C = A;
    C *= B;
    return C;
}
BigInteger operator/(const BigInteger& A, const BigInteger& B)
{
    BigInteger C = A;
    C /= B;
    return C;
}
BigInteger operator%(const BigInteger& A, const BigInteger& B)
{
    BigInteger C = A;
    C %= B;
    return C;
}

BigInteger gcd(BigInteger A, BigInteger B)
{
    A = abs(A);
    B = abs(B);
    while (B > BigInteger(0)) {
        A %= B;
        A.swap(B);
    }
    return A;
    BigInteger result = BigInteger(1);
    A = abs(A);
    B = abs(B);
    //std::cerr << "gcd() " << A << " " << B << '\n';
    while (true) {
        if (A == BigInteger(0)) {
            //std::cerr << B * result << '\n';
            return B * result;
        }
        if (B == BigInteger(0)) {
            //std::cerr << A * result << '\n';
            return A * result;
        }
        if (A == B) {
            //std::cerr << A * result << '\n';
            return A * result;
        }
        if (A == BigInteger(1) || B == BigInteger(1)) {
            //std::cerr << result << '\n';
            return result;
        }
        bool A_even = A.a[0] % 2 == 0, B_even = B.a[0] % 2 == 0;
        if (A_even && B_even) {
            result *= 2;
            A /= 2;
            B /= 2;
            continue;
        }
        if (A_even) {
            A /= 2;
            continue;
        }
        if (B_even) {
            B /= 2;
            continue;
        }
        if (A > B) {
            A = (A - B) / 2;
            continue;
        } else {
            B = (B - A) / 2;
            continue;
        }
    }
    return result;
}

std::ostream& operator<<(std::ostream& out, const BigInteger& A)
{
    out << A.toString();
    return out;
}

std::istream& operator>>(std::istream& in, BigInteger& A)
{
    std::string ss;
    in >> ss;
    A = ss;
    return in;
}
class Rational
{
  private:
    BigInteger p = 1, q = 1;
    void update_rational()
    {
        BigInteger g = gcd(p, q);
        p /= g;
        q /= g;
        if (q < 0) {
            q *= -1;
            p *= -1;
        }
    }
    bool is_zero() const
    {
        return p.is_zero();
    }
    void swap(Rational& A)
    {
        p.swap(A.p);
        q.swap(A.q);
    }

  public:
    friend bool operator<(const Rational& A, const Rational& B);
    friend bool operator>(const Rational& A, const Rational& B);
    friend bool operator<=(const Rational& A, const Rational& B);
    friend bool operator>=(const Rational& A, const Rational& B);
    friend bool operator==(const Rational& A, const Rational& B);
    friend bool operator!=(const Rational& A, const Rational& B);
    friend Rational operator*(const Rational& A, const Rational& B);
    friend Rational operator/(const Rational& A, const Rational& B);
    friend Rational operator+(const Rational& A, const Rational& B);
    friend Rational operator-(const Rational& A, const Rational& B);

    Rational() : p(1), q(1)
    {
    }
    Rational(const BigInteger& X) : p(X), q(1)
    {
    }
    Rational(int x) : p(x), q(1)
    {
    }
    Rational(const Rational& A) : p(A.p), q(A.q)
    {
    }

    Rational(const BigInteger& _p, const BigInteger& _q) : p(_p), q(_q)
    {
        if (q < 0) {
            q *= -1;
            p *= -1;
        }
    }

    Rational getInverse() const
    {
        Rational res;
        res.p = q;
        res.q = p;
        if (res.q < 0) {
            res.q *= -1;
            res.p *= -1;
        }
        return res;
    }
    Rational& operator=(const Rational& A)
    {
        Rational new_rational = A;
        swap(new_rational);
        return *this;
    }

    Rational abs_rational(const Rational& A) const
    {
        Rational B = A;
        B.p = abs(B.p);
        B.q = abs(B.q);
        // std::cout << p << " " << q << '\n';
        return B;
    }
    Rational& operator+=(const Rational& A)
    {
        if (this == &A) {
            Rational copy = A;
            *this += copy;
            update_rational();
            return *this;
        }
        Rational result;
        //std::cout << p << " " << q << " " << A.p << " " << A.q << '\n';
        result.p = p * A.q + q * A.p;
        result.q = q * A.q;
        //std::cout << result.p << " " << result.q << '\n';
        *this = result;
        update_rational();
        return *this;
    }
    Rational& operator-=(const Rational& A)
    {
        if (*this == A) {
            *this = 0;
            update_rational();
            return *this;
        }
        Rational result;
        result.p = p * A.q - q * A.p;
        result.q = q * A.q;
        *this = result;
        update_rational();
        return *this;
    }
    Rational& operator*=(const Rational& A)
    {
        if (this == &A) {
            Rational copy = A;
            *this *= copy;
            update_rational();
            return *this;
        }
        p *= A.p;
        q *= A.q;
        update_rational();
        return *this;
    }
    Rational& operator/=(const Rational& A)
    {
        if (*this == A) {
            *this = 1;
            update_rational();
            return *this;
        }
        //        std::cout << p << " " << q << " " << A.p << " " << A.q << '\n';
        p *= A.q;
        q *= A.p;
        update_rational();
        return *this;
    }
    Rational operator-() const
    {
        Rational copy = *this;
        copy.p *= -1;
        return copy;
    }
    std::string toString() const
    {
        std::string S = "";
        S += p.toString();
        if (q == 1) {
            return S;
        }
        S += '/';
        S += q.toString();
        return S;
    }
    std::string asDecimal(size_t precision = 0) const
    {
        std::string S = "";
        if (p / q == 0 && p < 0) {
            S += '-';
        }
        BigInteger a = p / q;
        S += a.toString();
        if (precision == 0)
            return S;
        S += '.';
        BigInteger b = abs(p) % abs(q);
        int pr = static_cast<int>(precision) * 2 + 7;
        for (int i = 0; i < pr; i++) {
            b *= 10;
        }
        std::string SS = "";
        b /= q;
        std::string T = b.toString();
        for (int i = 0; i < pr - static_cast<int>(T.size()); i++) {
            SS += '0';
        }
        SS += T;
        while (SS.size() > precision) {
            SS.pop_back();
        }
        S += SS;
        return S;
    }
    explicit operator double() const
    {
        std::string S = asDecimal(10);
        double x = 0;
        bool f = S[0] != '-';
        size_t ind = 0;
        for (size_t i = 0; i < S.size(); ++i) {
            if (S[i] == '.') {
                ind = i + 1;
                break;
            }
            if (S[i] == '-') {
                continue;
            }
            x *= 10.0;
            x += S[i] - '0';
        }
        double y = 0;
        int cnt = 0;
        for (size_t i = ind; i < S.size(); ++i) {
            y *= 10.0;
            y += S[i] - '0';
            cnt++;
        }
        while (cnt--) {
            y /= 10.0;
        }
        if (!f) {
            x *= -1.0;
            y *= -1.0;
        }
        return x + y;
    }
};

Rational abs(const Rational& A)
{
    if (A < 0)
        return -A;
    return A;
}

bool operator==(const Rational& A, const Rational& B)
{
    return A.p == B.p && A.q == B.q;
}
bool operator!=(const Rational& A, const Rational& B)
{
    return !(A == B);
}
bool operator<(const Rational& A, const Rational& B)
{
    BigInteger a = A.p, b = A.q, c = B.p, d = B.q;
    BigInteger ad = a * b;
    BigInteger bc = c * d;
    return ad < bc;
    return false;
}
bool operator<=(const Rational& A, const Rational& B)
{
    return !(B < A);
}
bool operator>=(const Rational& A, const Rational& B)
{
    return !(A < B);
}
bool operator>(const Rational& A, const Rational& B)
{
    return B < A;
}
Rational operator+(const Rational& A, const Rational& B)
{
    Rational C = A;
    C += B;
    return C;
}
Rational operator-(const Rational& A, const Rational& B)
{
    Rational C = A;
    C -= B;
    return C;
}
Rational operator*(const Rational& A, const Rational& B)
{
    Rational C = A;
    C *= B;
    return C;
}
Rational operator/(const Rational& A, const Rational& B)
{
    Rational C = A;
    C /= B;
    return C;
}

std::istream& operator>>(std::istream& in, Rational& rational)
{
    std::string s;
    in >> s;
    std::string p, q;
    bool f = true;
    for (size_t i = 0; i < s.size(); ++i) {
        if (f) {
            if (s[i] == '/') {
                f = false;
                continue;
            }
            p.push_back(s[i]);
        } else {
            q.push_back(s[i]);
        }
    }
    if (q.empty()) {
        q = "1";
    }
    rational = Rational(BigInteger(p), BigInteger(q));
    return in;
}

std::ostream& operator<<(std::ostream& out, const Rational& rational)
{
    out << rational.toString();
    return out;
}

template <unsigned M, unsigned N, typename Field = Rational> class Matrix
{
  public:
    std::vector<std::vector<Field>> table;

  public:
    void print() const
    {
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                std::cerr << table[i][j] << " ";
            }
            std::cerr << '\n';
        }
        std::cerr << '\n';
    }

  public:
    Matrix<M, N, Field>(std::initializer_list<std::initializer_list<int>> lst)
    {
        for (auto v : lst) {
            table.push_back({});
            for (auto i : v) {
                table.back().push_back(Field(i));
            }
        }
    }

    Matrix<M, N, Field>() : table(M, std::vector<Field>(N))
    {
    }

    Matrix<M, N, Field>(const Matrix<M, N, Field>& other)
    {
        table = other.table;
    }

    Matrix<M, N, Field>& operator=(const Matrix<M, N, Field>& other)
    {
        table = other.table;
        return *this;
    }
    bool operator==(const Matrix<M, N, Field>& other) const
    {
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                if (table[i][j] != other.table[i][j]) {
                    return false;
                }
            }
        }
        return true;
    }

    bool operator!=(const Matrix<M, N, Field>& other) const
    {
        return !(*this == other);
    }

    Matrix<M, N, Field>& operator+=(const Matrix<M, N, Field>& other)
    {
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                table[i][j] += other.table[i][j];
            }
        }
        return *this;
    }
    Matrix<M, N, Field> operator+(const Matrix<M, N, Field>& other) const
    {
        Matrix<M, N, Field> copy = *this;
        copy += other;
        return copy;
    }
    Matrix<M, N, Field>& operator-=(const Matrix<M, N, Field>& other)
    {
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                table[i][j] -= other.table[i][j];
            }
        }
        return *this;
    }
    Matrix<M, N, Field> operator-(const Matrix<M, N, Field>& other) const
    {
        Matrix<M, N, Field> copy = *this;
        copy -= other;
        return copy;
    }
    Matrix<M, N, Field>& operator*=(const Field& other)
    {
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                table[i][j] *= other;
            }
        }
        return *this;
    }
    Matrix<M, N, Field> operator*(const Field& other) const
    {
        Matrix<M, N, Field> copy = *this;
        copy *= other;
        return copy;
    }
    template <unsigned K>
    Matrix<M, K, Field> operator*(const Matrix<N, K, Field>& other) const
    {
        Matrix<M, K, Field> result_matrix;
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < K; ++j) {
                result_matrix.table[i][j] = Field(0);
            }
        }
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned k = 0; k < K; ++k) {
                for (unsigned j = 0; j < N; ++j) {
                    result_matrix.table[i][k] +=
                        table[i][j] * other.table[j][k];
                }
            }
        }
        return result_matrix;
    }
    Matrix<M, N, Field>& operator*=(const Matrix<N, N, Field>& other)
    {
        *this = *this * other;
        return *this;
    }

  public:
    void do_gauss_algorithm()
    {
        std::cerr << "gauss\n";
        for (unsigned it = 0; it < M; ++it) {
            unsigned ind = 0, ind_leader = N;
            for (unsigned i = it; i < M; ++i) {
                unsigned leader = N;
                for (unsigned j = 0; j < N; ++j) {
                    if (table[i][j] != Field(0)) {
                        leader = j;
                        break;
                    }
                }
                if (leader < ind_leader) {
                    ind = i;
                    ind_leader = leader;
                }
            }
            if (ind_leader == N) {
                return;
            }
            std::cerr << "leader is in " << ind << " row, and in " << ind_leader
                      << " column\n";
            std::swap(table[it], table[ind]);
            for (unsigned i = it + 1; i < M; ++i) {
                if (table[i][ind_leader] == Field(0)) {
                    continue;
                }
                Field coeff =
                    Field(-1) * table[i][ind_leader] / table[it][ind_leader];
                for (unsigned j = ind_leader; j < N; ++j) {
                    table[i][j] += coeff * table[it][j];
                }
            }
        }
    }

  public:
    unsigned rank() const
    {
        Matrix<M, N, Field> copy = *this;
        copy.do_gauss_algorithm();
        for (unsigned i = 0; i < M; ++i) {
            bool f = false;
            for (unsigned j = 0; j < N; ++j) {
                if (copy.table[i][j] != Field(0)) {
                    f = true;
                    break;
                }
            }
            if (!f) {
                return i;
            }
        }
        return M;
    }
    std::vector<Field>& operator[](int i)
    {
        return table[i];
    }
    const std::vector<Field> operator[](int i) const
    {
        return const_cast<const std::vector<Field>>(table[i]);
    }
    Field trace() const
    {
        static_assert(N == M);
        Field result = Field(0);
        for (unsigned i = 0; i < N; ++i) {
            result += table[i][i];
        }
        return result;
    }
    Field det() const
    {
        static_assert(N == M);
        Matrix<M, N, Field> copy = *this;
        copy.do_gauss_algorithm();
        Field result = Field(1);
        for (unsigned i = 0; i < N; ++i) {
            result *= copy.table[i][i];
        }
        return result;
    }
    Matrix<N, M, Field> transposed() const
    {
        Matrix<N, M, Field> result;
        for (unsigned i = 0; i < M; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                result.table[j][i] = table[i][j];
            }
        }
        return result;
    }
    void make_idenity()
    {
        for (unsigned i = 0; i < M; ++i) {
            Field coeff = table[i][i].getInverse();
            for (unsigned j = i; j < N; ++j) {
                table[i][j] *= coeff;
            }
        }
        for (unsigned i = 0; i < M; ++i) {
            std::cerr << "i = " << i << '\n';
            for (unsigned j = i + 1; j < M; ++j) {
                if (table[i][j] == Field(0)) {
                    continue;
                }
                Field coeff = Field(-1) * table[i][j];
                for (unsigned J = j; J < N; ++J) {
                    table[i][J] += table[j][J] * coeff;
                }
            }
        }
    }
    void invert()
    {
        std::cout << std::fixed;
        double start = clock();
        static_assert(N == M);
        Matrix<M, N + N, Field> AE;
        for (unsigned i = 0; i < N; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                AE.table[i][j] = table[i][j];
            }
        }
        for (unsigned i = 0; i < N; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                if (i == j) {
                    AE.table[i][j + N] = Field(1);
                    continue;
                }
                AE.table[i][j + N] = Field(0);
            }
        }
        AE.do_gauss_algorithm();
        std::cerr << "gauss done\n";
        AE.make_idenity();
        for (unsigned i = 0; i < N; ++i) {
            for (unsigned j = 0; j < N; ++j) {
                table[i][j] = AE.table[i][j + N];
            }
        }
        std::cerr << (clock() - start) / CLOCKS_PER_SEC << " sec\n";
    }
    Matrix<N, M, Field> inverted() const
    {
        static_assert(N == M);
        Matrix<N, M, Field> copy = *this;
        copy.invert();
        return copy;
    }
    std::vector<Field> getColumn(unsigned c) const
    {
        std::vector<Field> column;
        for (unsigned i = 0; i < M; ++i) {
            column.push_back(table[i][c]);
        }
        return column;
    }
    std::vector<Field> getRow(unsigned r) const
    {
        return table[r];
    }
};

template <unsigned M, unsigned N, typename Field = Rational>
Matrix<M, N, Field> operator*(const Field& k, const Matrix<M, N, Field>& Matr)
{
    Matrix<M, N, Field> copy = Matr;
    copy *= k;
    return copy;
}

template <unsigned N, typename Field = Rational>
using SquareMatrix = Matrix<N, N, Field>;
