#include <cstring>
#include <iostream>

template <typename T> T max(const T& a, const T& b)
{
    return b < a ? a : b;
}

class String
{
  public:
    const size_t MINIMAL_SIZE = 2;
    const double CHANGE_COEFFICIENT = 2;

    char* buffer = nullptr;
    size_t current_size = 0;
    size_t buffer_size = MINIMAL_SIZE;

    void set_new_size(size_t new_size)
    {
        new_size = max(new_size, MINIMAL_SIZE);
        char* new_buffer = new char[new_size];
        if (!empty()) {
            memcpy(new_buffer, buffer, sizeof(char) * current_size);
        }
        delete[] buffer;
        buffer = new_buffer;
        buffer_size = new_size;
    }

  public:
    String()
    {
        buffer = new char[MINIMAL_SIZE];
    }

    String(char c) : current_size(1), buffer_size(MINIMAL_SIZE)
    {
        buffer = new char[buffer_size];
        *buffer = c;
    }

    String(const char* s)
        : current_size(strlen(s)), buffer_size(max(current_size, MINIMAL_SIZE))
    {
        buffer = new char[buffer_size];
        memcpy(buffer, s, sizeof(char) * current_size);
    }

    String(size_t n, const char& c)
        : current_size(n), buffer_size(max(n, MINIMAL_SIZE))
    {
        buffer = new char[buffer_size];
        memset(buffer, c, n);
    }

    ~String()
    {
        delete[] buffer;
    }

    String(const String& str) : String(str.buffer_size, 'a')
    {
        memcpy(buffer, str.buffer, sizeof(char) * str.current_size);
        current_size = str.current_size;
    }

    void swap(String& str)
    {
        std::swap(buffer, str.buffer);
        std::swap(buffer_size, str.buffer_size);
        std::swap(current_size, str.current_size);
    }

    String& operator=(const String& str)
    {
        String new_str = str;
        swap(new_str);
        return *this;
    }

    char& operator[](size_t ind)
    {
        return *(buffer + ind);
    }

    const char& operator[](size_t ind) const
    {
        return *(buffer + ind);
    }

    size_t length() const
    {
        return current_size;
    }

    bool empty() const
    {
        return current_size == 0;
    }

    void clear()
    {
        delete[] buffer;
        buffer = new char[MINIMAL_SIZE];
        current_size = 0;
        buffer_size = MINIMAL_SIZE;
    }

    bool operator==(const String& str) const
    {
        if (current_size != str.current_size)
            return false;
        char *it_1 = buffer, *it_2 = str.buffer;
        for (; it_1 != buffer + current_size; ++it_1, ++it_2) {
            if (*it_1 != *it_2)
                return false;
        }
        return true;
    }

    String& operator+=(char symbol)
    {
        push_back(symbol);
        return *this;
    }

    String& operator+=(const String& string)
    {
        if (current_size + string.current_size > buffer_size) {
            set_new_size(current_size + string.current_size);
        }
        memcpy(
            buffer + current_size,
            string.buffer,
            sizeof(char) * string.current_size);
        current_size += string.current_size;
        return *this;
    }

    char& front()
    {
        return *buffer;
    }

    char& back()
    {
        return *(buffer + current_size - 1);
    }

    const char& front() const
    {
        return *buffer;
    }

    const char& back() const
    {
        return *(buffer + current_size - 1);
    }

    void push_back(char symbol)
    {
        if (empty()) {
            set_new_size(MINIMAL_SIZE);
        }
        if (current_size == buffer_size) {
            set_new_size(buffer_size * CHANGE_COEFFICIENT);
        }
        *(buffer + current_size) = symbol;
        ++current_size;
    }

    void pop_back()
    {
        --current_size;
        if (current_size > MINIMAL_SIZE &&
            current_size <=
                buffer_size / (CHANGE_COEFFICIENT * CHANGE_COEFFICIENT)) {
            set_new_size(current_size * CHANGE_COEFFICIENT);
        }
    }

    String substr(size_t start, size_t count) const
    {
        String sub_str(count, 'a');
        memcpy(sub_str.buffer, buffer + start, sizeof(char) * count);
        return sub_str;
    }

    size_t find(const String& str) const
    {
        for (size_t i = 0; i < current_size - str.current_size; ++i) {
            if (substr(i, str.current_size) == str) {
                return i;
            }
        }
        return length();
    }

    size_t rfind(const String& str) const
    {
        for (size_t i = 0; i < current_size - str.current_size; ++i) {
            if (substr(current_size - i - str.current_size, str.current_size) ==
                str) {
                return current_size - i - str.current_size;
            }
        }
        return length();
    }
};

String operator+(const String& str_first, const String& str_second)
{
    String str = str_first;
    str += str_second;
    return str;
}

std::ostream& operator<<(std::ostream& out, const String& str)
{
    for (size_t i = 0; i < str.length(); ++i) {
        out << str[i];
    }
    return out;
}

std::istream& operator>>(std::istream& in, String& str)
{
    str.clear();
    bool is_empty = true;
    while (true) {
        int symbol = in.get();
        if (symbol == ' ' || symbol == '\n' || symbol == EOF) {
            if (!is_empty) {
                return in;
            }
            continue;
        }
        str.push_back(symbol);
        is_empty = false;
    }
}