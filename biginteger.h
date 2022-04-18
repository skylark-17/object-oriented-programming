#include <algorithm>
#include <cmath>
#include <complex>
#include <cstdio>
#include <cstring>
#include <iostream>
#include <string>
#include <vector>

class BigInteger;

bool operator<(const BigInteger& first, const BigInteger& second);

bool operator>(const BigInteger& first, const BigInteger& second);

bool operator>=(const BigInteger& first, const BigInteger& second);

bool operator<=(const BigInteger& first, const BigInteger& second);

bool operator==(const BigInteger& first, const BigInteger& second);

bool operator!=(const BigInteger& first, const BigInteger& second);

BigInteger operator+(const BigInteger& first, const BigInteger& second);

BigInteger operator-(const BigInteger& first, const BigInteger& second);

BigInteger operator*(const BigInteger& first, const BigInteger& second);

BigInteger operator/(const BigInteger& first, const BigInteger& second);

BigInteger operator%(const BigInteger& first, const BigInteger& second);

BigInteger abs(const BigInteger& biginteger);

class BigInteger
{

    friend bool operator<(const BigInteger& first, const BigInteger& second);

    friend bool operator==(const BigInteger& first, const BigInteger& second);

  private:
    std::vector<long long> buffer; // buffer[0] - smallest

    static const long long base = 10;

    bool is_positive = true;

    void shrink_to_fit()
    {
        if (buffer.empty()) {
            buffer.push_back(0);
            is_positive = true;
        }
        while (buffer.size() > 1 && buffer.back() == 0) {
            buffer.pop_back();
        }
        if (buffer.size() == 1 && buffer[0] == 0) {
            is_positive = true;
        }
        buffer.shrink_to_fit();
    }

    void update()
    {
        for (size_t i = 0; i < buffer.size(); ++i) {
            if (buffer[i] < 0) {
                if (i + 1 == buffer.size()) {
                    buffer.push_back(0);
                }
                buffer[i + 1] -= (-buffer[i] + base - 1) / base;
                buffer[i] += ((-buffer[i] + base - 1) / base) * base;
                continue;
            }
            if (buffer[i] < base) {
                break;
            }
            if (buffer[i] >= base) {
                if (i + 1 == buffer.size()) {
                    buffer.push_back(0);
                }
                buffer[i + 1] += buffer[i] / base;
                buffer[i] %= base;
            }
        }
        shrink_to_fit();
        if (buffer.back() < 0) {
            buffer.back() *= -1;
            is_positive ^= 1;
        }
    }

    int last_not_zero() const
    {
        int last_not_zero = -1;
        for (size_t i = 0; i < buffer.size(); ++i) {
            if (buffer[i] != 0) {
                last_not_zero = i;
            }
        }
        return last_not_zero;
    }

  public:
    BigInteger() : buffer(1, 0), is_positive(true)
    {
    }

    BigInteger(int x) : buffer(1, std::abs(x))
    {
        while (buffer.back() >= base) {
            int y = buffer.back() / base;
            buffer.back() %= base;
            buffer.push_back(0);
            buffer.back() += y;
        }
        is_positive = x >= 0;
    }

    BigInteger(const std::string& str) : is_positive(str[0] != '-')
    {
        if (str == "-0") {
            buffer = {0};
            return;
        }
        buffer.reserve(str.size());
        for (int i = str.size() - 1; i >= 0; --i) {
            if (i == 0 && !is_positive) {
                break;
            }
            buffer.push_back(str[i] - '0');
        }
    }

    BigInteger(const BigInteger& other)
        : buffer(other.buffer), is_positive(other.is_positive)
    {
    }

  private:
    void swap(BigInteger& other)
    {
        std::swap(other.buffer, buffer);
        std::swap(other.is_positive, is_positive);
    }

  public:
    BigInteger& operator=(const BigInteger& other)
    {
        BigInteger new_bigint = other;
        swap(new_bigint);
        return *this;
    }

    BigInteger operator-() const
    {
        BigInteger res = *this;
        if (last_not_zero() == -1) {
            return res;
        } else {
            res.is_positive ^= 1;
        }
        return res;
    }

    BigInteger& operator+=(const BigInteger& other)
    {
        if (this == &other) {
            BigInteger copy = other;
            *this += copy;
            return *this;
        }
        if (*this == 0) {
            *this = other;
            return *this;
        }
        if (is_positive == other.is_positive) {
            buffer.resize(std::max(other.buffer.size(), buffer.size()), 0);
            for (size_t i = 0; i < buffer.size(); ++i) {
                if (i < other.buffer.size()) {
                    buffer[i] += other.buffer[i];
                }
                while (buffer[i] >= base) {
                    if (i + 1 == buffer.size()) {
                        buffer.push_back(0);
                    }
                    buffer[i] -= base;
                    buffer[i + 1]++;
                }
            }
        } else {
            BigInteger copy_other = other;
            if (abs(*this) < abs(copy_other)) {
                swap(copy_other);
            }
            for (size_t i = 0; i < buffer.size(); ++i) {
                if (i < copy_other.buffer.size()) {
                    buffer[i] -= copy_other.buffer[i];
                }
                while (buffer[i] < 0) {
                    buffer[i] += base;
                    buffer[i + 1]--;
                }
            }
        }
        shrink_to_fit();
        return *this;
    }

    BigInteger& operator-=(const BigInteger& other)
    {
        *this += -other;
        return *this;
    }

    BigInteger& operator*=(const BigInteger& other)
    {
        if (*this == 0 || other == 0) {
            *this = 0;
            return *this;
        }
        if (this == &other) {
            BigInteger copy = other;
            *this *= copy;
            return *this;
        }
        BigInteger result;
        result.buffer.resize(buffer.size() + other.buffer.size(), 0);
        for (size_t i = 0; i < other.buffer.size(); ++i) {
            for (size_t j = 0; j < buffer.size(); ++j) {
                result.buffer[i + j] += buffer[j] * other.buffer[i];
            }
        }
        for (size_t i = 0; i < result.buffer.size(); ++i) {
            int x = result.buffer[i] / base;
            if (x) {
                if (i + 1 == result.buffer.size()) {
                    result.buffer.push_back(0);
                }
                result.buffer[i] -= base * x;
                result.buffer[i + 1] += x;
            }
        }
        result.is_positive = is_positive == other.is_positive;
        *this = result;
        return *this;
    }

  private:
    void divide_base()
    {
        if (!buffer.empty()) {
            buffer.erase(buffer.begin());
        }
    }

  public:
    BigInteger& operator/=(const BigInteger& other)
    {
        if (*this == other) {
            *this = 1;
            return *this;
        }
        if (*this == 0) {
            return *this;
        }
        bool f = is_positive == other.is_positive;
        BigInteger res = 0;
        BigInteger copy = *this;
        copy.is_positive = true;
        BigInteger cur = copy;
        BigInteger other_copy = other;
        other_copy.is_positive = true;
        while (other_copy * base <= copy) {
            other_copy *= base;
        }
        while (other_copy.last_not_zero() >= other.last_not_zero()) {
            int cnt = 0;
            while (other_copy <= cur) {
                cur -= other_copy;
                ++cnt;
            }
            res *= base;
            res += cnt;
            other_copy.divide_base();
        }
        res.is_positive = f;
        if (res == 0) {
            res.is_positive = true;
        }
        *this = res;
        if (*this == 0) {
            is_positive = true;
        }
        return *this;
    }

    BigInteger& operator%=(const BigInteger& other)
    {
        *this = *this - (*this / other) * other;
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
        std::string str = "";
        if (!is_positive) {
            str.push_back('-');
        }
        for (int i = last_not_zero(); i >= 0; --i) {
            str.push_back(buffer[i] + '0');
        }
        if (last_not_zero() == -1) {
            str = "0";
        }
        return str;
    }

    explicit operator bool() const
    {
        return *this != 0;
    }

    explicit operator int() const
    {
        int x = 0;
        for (int i = last_not_zero(); i >= 0; --i) {
            x *= base;
            x += buffer[i];
        }
        if (!is_positive) {
            x *= -1;
        }
        return x;
    }
};

void swap(BigInteger& first, BigInteger& second)
{
    BigInteger copy = first;
    first = second;
    second = copy;
}

bool operator<(const BigInteger& first, const BigInteger& second)
{
    if (first.is_positive != second.is_positive) {
        return second.is_positive;
    }
    if (first.last_not_zero() != second.last_not_zero()) {
        return first.last_not_zero() < second.last_not_zero();
    }
    for (int i = std::min(first.buffer.size(), second.buffer.size()) - 1;
         i >= 0;
         i--) {
        if (first.buffer[i] != second.buffer[i]) {
            return (first.buffer[i] < second.buffer[i]) ^ !first.is_positive;
        }
    }
    return false;
}

bool operator>(const BigInteger& first, const BigInteger& second)
{
    return second < first;
}

bool operator>=(const BigInteger& first, const BigInteger& second)
{
    return !(first < second);
}

bool operator<=(const BigInteger& first, const BigInteger& second)
{
    return !(second < first);
}

bool operator==(const BigInteger& first, const BigInteger& second)
{
    if (first.last_not_zero() == -1 && second.last_not_zero() == -1) {
        return true;
    }
    if (first.is_positive != second.is_positive) {
        return false;
    }
    int last_first = first.last_not_zero();
    int last_second = second.last_not_zero();
    if (last_first != last_second) {
        return false;
    }
    for (int i = 0; i <= last_first; i++) {
        if (first.buffer[i] != second.buffer[i]) {
            return false;
        }
    }
    return true;
}

bool operator!=(const BigInteger& first, const BigInteger& second)
{
    return !(first == second);
}

BigInteger operator+(const BigInteger& first, const BigInteger& second)
{
    BigInteger copy = first;
    copy += second;
    return copy;
}

BigInteger operator-(const BigInteger& first, const BigInteger& second)
{
    BigInteger copy = first;
    copy -= second;
    return copy;
}

BigInteger operator*(const BigInteger& first, const BigInteger& second)
{
    BigInteger copy = first;
    copy *= second;
    return copy;
}

BigInteger operator/(const BigInteger& first, const BigInteger& second)
{
    BigInteger copy = first;
    copy /= second;
    return copy;
}

BigInteger operator%(const BigInteger& first, const BigInteger& second)
{
    BigInteger copy = first;
    copy %= second;
    return copy;
}

BigInteger abs(const BigInteger& biginteger)
{
    BigInteger copy = biginteger;
    if (copy < 0) {
        copy = -copy;
    }
    return copy;
}

BigInteger gcd(BigInteger first, BigInteger second)
{
    first = abs(first);
    second = abs(second);
    while (second > 0) {
        first %= second;
        swap(first, second);
    }
    return first;
}

std::ostream& operator<<(std::ostream& out, const BigInteger& biginteger)
{
    out << biginteger.toString();
    return out;
}

std::istream& operator>>(std::istream& in, BigInteger& biginteger)
{
    std::string str;
    in >> str;
    biginteger = str;
    return in;
}

class Rational;

bool operator<(const Rational& first, const Rational& second);

bool operator>(const Rational& first, const Rational& second);

bool operator>=(const Rational& first, const Rational& second);

bool operator<=(const Rational& first, const Rational& second);

bool operator==(const Rational& first, const Rational& second);

bool operator!=(const Rational& first, const Rational& second);

Rational operator+(const Rational& first, const Rational& second);

Rational operator-(const Rational& first, const Rational& second);

Rational operator*(const Rational& first, const Rational& second);

Rational operator/(const Rational& first, const Rational& second);

class Rational
{

    friend bool operator<(const Rational& first, const Rational& second);

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

    void swap_rational(Rational& other)
    {
        swap(p, other.p);
        swap(q, other.q);
    }

    Rational abs_rational(const Rational& other) const
    {
        Rational copy = other;
        copy.p = abs(copy.p);
        copy.q = abs(copy.q);
        return copy;
    }

  public:
    Rational() : p(1), q(1)
    {
    }

    Rational(const BigInteger& biginteger) : p(biginteger), q(1)
    {
    }

    Rational(int x) : p(x), q(1)
    {
    }

    Rational(const Rational& other) : p(other.p), q(other.q)
    {
    }

    Rational& operator=(const Rational& other)
    {
        Rational new_rational = other;
        swap_rational(new_rational);
        return *this;
    }

    Rational& operator+=(const Rational& other)
    {
        if (this == &other) {
            Rational copy = other;
            *this += copy;
            update_rational();
            return *this;
        }
        Rational result;
        result.p = p * other.q + q * other.p;
        result.q = q * other.q;
        *this = result;
        update_rational();
        return *this;
    }

    Rational& operator-=(const Rational& other)
    {
        *this += -other;
        return *this;
    }

    Rational& operator*=(const Rational& other)
    {
        if (this == &other) {
            Rational copy = other;
            *this *= copy;
            update_rational();
            return *this;
        }
        p *= other.p;
        q *= other.q;
        update_rational();
        return *this;
    }

  private:
    Rational inverted() const
    {
        Rational copy = *this;
        std::swap(copy.p, copy.q);
        return copy;
    }

  public:
    Rational& operator/=(const Rational& other)
    {
        *this *= other.inverted();
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
        std::string str = "";
        str += p.toString();
        if (q == 1) {
            return str;
        }
        str += '/';
        str += q.toString();
        return str;
    }

    std::string asDecimal(size_t precision = 0) const
    {
        std::string result = "";
        if (p / q == 0 && p < 0) {
            result += '-';
        }
        BigInteger first = p / q;
        result += first.toString();
        if (precision == 0)
            return result;
        result += '.';
        BigInteger second = abs(p) % abs(q);
        size_t pr = precision * 2 + 7;
        for (size_t i = 0; i < pr; ++i) {
            second *= 10;
        }
        second /= q;
        std::string s = "";
        std::string t = second.toString();
        for (int i = 0; i < static_cast<int>(pr) - static_cast<int>(t.size());
             ++i) {
            s += '0';
        }
        s += t;
        while (s.size() > precision) {
            s.pop_back();
        }
        result += s;
        return result;
    }

    explicit operator double() const
    {
        std::string str = asDecimal(10);
        double x = 0;
        size_t ind = 0;
        for (size_t i = 0; i < str.size(); ++i) {
            if (str[i] == '.') {
                ind = i + 1;
                break;
            }
            if (str[i] == '-') {
                continue;
            }
            x *= 10.0;
            x += str[i] - '0';
        }
        double y = 0;
        int cnt = 0;
        for (size_t i = ind; i < str.size(); ++i) {
            y *= 10.0;
            y += str[i] - '0';
            cnt++;
        }
        while (cnt--) {
            y /= 10.0;
        }
        if (str[0] == '-') {
            x *= -1.0;
            y *= -1.0;
        }
        return x + y;
    }
};

Rational abs(const Rational& first)
{
    if (first < 0) {
        return -first;
    }
    return first;
}

bool operator<(const Rational& first, const Rational& second)
{
    BigInteger a = first.p, b = first.q, c = second.p, d = second.q;
    BigInteger ad = a * b;
    BigInteger bc = c * d;
    return ad < bc;
}

bool operator>(const Rational& first, const Rational& second)
{
    return second < first;
}

bool operator>=(const Rational& first, const Rational& second)
{
    return !(first < second);
}

bool operator<=(const Rational& first, const Rational& second)
{
    return !(second < first);
}

bool operator==(const Rational& first, const Rational& second)
{
    return !(first < second) && !(second < first);
}

bool operator!=(const Rational& first, const Rational& second)
{
    return !(first == second);
}

Rational operator+(const Rational& first, const Rational& second)
{
    Rational copy = first;
    copy += second;
    return copy;
}

Rational operator-(const Rational& first, const Rational& second)
{
    Rational copy = first;
    copy -= second;
    return copy;
}

Rational operator*(const Rational& first, const Rational& second)
{
    Rational copy = first;
    copy *= second;
    return copy;
}

Rational operator/(const Rational& first, const Rational& second)
{
    Rational copy = first;
    copy /= second;
    return copy;
}