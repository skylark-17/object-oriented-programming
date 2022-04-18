#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

template <typename T> class Deque
{

  private:
    const int minimal_size = 3;

    int buffer_size;
    std::vector<T> buffer;

    int left_iterator, right_iterator;

    int cur_begin_ind;

  public:
    struct iterator;
    struct const_iterator;

    struct iterator {

        int ind;

        Deque* deque_pointer = nullptr;

        iterator()
        {
        }

        iterator(int _ind, Deque* _deque_pointer)
            : ind(_ind), deque_pointer(_deque_pointer)
        {
        }

        bool operator<(const const_iterator& other_it) const
        {
            return ind < other_it.ind;
        }

        bool operator>(const const_iterator& other_it) const
        {
            return other_it < *this;
        }

        bool operator<=(const const_iterator& other_it) const
        {
            return !(other_it < *this);
        }

        bool operator>=(const const_iterator& other_it) const
        {
            return !(*this < other_it);
        }

        bool operator==(const const_iterator& other_it) const
        {
            return ind == other_it.ind;
        }

        bool operator!=(const const_iterator& other_it) const
        {
            return !(*this == other_it);
        }

        int operator-(const const_iterator& other_it) const
        {
            return ind - other_it.ind;
        }

        iterator& operator++()
        {
            ++ind;
            return *this;
        }

        iterator operator++(int)
        {
            iterator copy = *this;
            ++ind;
            return copy;
        }

        iterator& operator--()
        {
            --ind;
            return *this;
        }

        iterator operator--(int)
        {
            iterator copy = *this;
            --ind;
            return copy;
        }

        iterator& operator+=(int x)
        {
            ind += x;
            return *this;
        }

        iterator& operator-=(int x)
        {
            ind -= x;
            return *this;
        }

        iterator operator+(int x)
        {
            iterator copy = *this;
            copy += x;
            return copy;
        }

        iterator operator-(int x)
        {
            iterator copy = *this;
            copy -= x;
            return copy;
        }

        T& operator*()
        {
            return deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }

        T* operator->()
        {
            return &deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }

        const T& operator*() const
        {
            return deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }

        const T* operator->() const
        {
            return &deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }
    };

    struct const_iterator {

        int ind;

        const Deque* deque_pointer = nullptr;

        const_iterator()
        {
        }

        const_iterator(int _ind, const Deque* _deque_pointer)
            : ind(_ind), deque_pointer(_deque_pointer)
        {
        }

        const_iterator(const iterator& other_it)
            : ind(other_it.ind), deque_pointer(other_it.deque_pointer)
        {
        }

        bool operator<(const const_iterator& other_it) const
        {
            return ind < other_it.ind;
        }

        bool operator>(const const_iterator& other_it) const
        {
            return other_it < *this;
        }

        bool operator<=(const const_iterator& other_it) const
        {
            return !(other_it < *this);
        }

        bool operator>=(const const_iterator& other_it) const
        {
            return !(*this < other_it);
        }

        bool operator==(const const_iterator& other_it) const
        {
            return ind == other_it.ind;
        }

        bool operator!=(const const_iterator& other_it) const
        {
            return !(*this == other_it);
        }

        int operator-(const const_iterator& other_it) const
        {
            return ind - other_it.ind;
        }

        const_iterator& operator++()
        {
            ++ind;
            return *this;
        }

        const_iterator operator++(int)
        {
            const_iterator copy = *this;
            ++ind;
            return copy;
        }

        const_iterator& operator--()
        {
            --ind;
            return *this;
        }

        const_iterator operator--(int)
        {
            const_iterator copy = *this;
            --ind;
            return copy;
        }

        const_iterator& operator+=(int x)
        {
            ind += x;
            return *this;
        }

        const_iterator& operator-=(int x)
        {
            ind -= x;
            return *this;
        }

        const_iterator operator+(int x) const
        {
            const_iterator copy = *this;
            copy += x;
            return copy;
        }

        const_iterator operator-(int x) const
        {
            const_iterator copy = *this;
            copy -= x;
            return copy;
        }

        const T& operator*()
        {
            return deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }

        const T* operator->()
        {
            return &deque_pointer->buffer[deque_pointer->cur_begin_ind + ind];
        }
    };

    void reallocate()
    {
        int new_buffer_size = buffer_size * 3;
        std::vector<T> new_buffer(new_buffer_size);
        int new_cur_begin_ind = new_buffer_size / 3;
        int new_left_iterator =
            new_cur_begin_ind + left_iterator - cur_begin_ind;
        int new_right_iterator =
            new_left_iterator + (right_iterator - left_iterator);
        for (int i = left_iterator; i < right_iterator; ++i) {
            new_buffer[new_left_iterator - left_iterator + i] = buffer[i];
        }
        buffer_size = new_buffer_size;
        std::swap(buffer, new_buffer);
        left_iterator = new_left_iterator;
        right_iterator = new_right_iterator;
        cur_begin_ind = new_cur_begin_ind;
    }

    Deque()
        : buffer_size(minimal_size), buffer(buffer_size),
          left_iterator(buffer_size / 3), right_iterator(buffer_size / 3),
          cur_begin_ind(buffer_size / 3)
    {
    }

    Deque(int size)
        : buffer_size(std::max(size * 3, minimal_size)), buffer(buffer_size),
          left_iterator(buffer_size / 3), right_iterator(left_iterator + size),
          cur_begin_ind(left_iterator)
    {
    }

    Deque(int size, T sample)
        : buffer_size(std::max(size * 3, minimal_size)),
          buffer(buffer_size, sample), left_iterator(buffer_size / 3),
          right_iterator(left_iterator + size), cur_begin_ind(left_iterator)
    {
    }

    void swap(Deque& other)
    {
        std::swap(other.buffer_size, buffer_size);
        std::swap(other.buffer, buffer);
        std::swap(other.left_iterator, left_iterator);
        std::swap(other.right_iterator, right_iterator);
        std::swap(other.cur_begin_ind, cur_begin_ind);
    }

    Deque& operator=(const Deque& other)
    {
        Deque new_deque = other;
        swap(new_deque);
        return *this;
    }

    T& operator[](int ind)
    {
        return buffer[left_iterator + ind];
    }

    const T& operator[](int ind) const
    {
        return buffer[left_iterator + ind];
    }

    T& at(int ind)
    {
        if (ind < 0 || ind >= right_iterator - left_iterator) {
            throw std::out_of_range("index is out of range");
        }
        return buffer[left_iterator + ind];
    }

    const T& at(int ind) const
    {
        if (ind < 0 || ind >= right_iterator - left_iterator) {
            throw std::out_of_range("index is out of range");
        }
        return buffer[left_iterator + ind];
    }

    void pop_back()
    {
        --right_iterator;
    }

    void pop_front()
    {
        ++left_iterator;
    }

    void push_back(T x)
    {
        buffer[right_iterator] = x;
        ++right_iterator;
        if (right_iterator == buffer_size) {
            reallocate();
        }
    }

    void push_front(T x)
    {
        buffer[left_iterator - 1] = x;
        --left_iterator;
        if (left_iterator == 0) {
            reallocate();
        }
    }

    iterator begin()
    {
        return iterator(left_iterator - cur_begin_ind, this);
    }

    const_iterator begin() const
    {
        return const_iterator(left_iterator - cur_begin_ind, this);
    }

    const_iterator cbegin() const
    {
        return const_iterator(left_iterator - cur_begin_ind, this);
    }

    iterator end()
    {
        return iterator(right_iterator - cur_begin_ind, this);
    }

    const_iterator end() const
    {
        return const_iterator(right_iterator - cur_begin_ind, this);
    }

    const_iterator cend() const
    {
        return const_iterator(right_iterator - cur_begin_ind, this);
    }

    void insert(const iterator& it, T x)
    {
        if (right_iterator == buffer_size) {
            reallocate();
        }
        for (int i = right_iterator; i > it.ind + cur_begin_ind; --i) {
            buffer[i] = buffer[i - 1];
        }
        buffer[it.ind + cur_begin_ind] = x;
        ++right_iterator;
        if (right_iterator == buffer_size) {
            reallocate();
        }
    }

    void erase(const iterator& it)
    {
        for (int i = it.ind + cur_begin_ind; i < right_iterator; ++i) {
            buffer[i] = buffer[i + 1];
        }
        --right_iterator;
    }

    size_t size() const
    {
        return static_cast<size_t>(right_iterator - left_iterator);
    }
};
