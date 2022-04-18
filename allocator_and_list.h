#include <algorithm>
#include <cassert>
#include <chrono>
#include <iostream>
#include <list>
#include <string>
#include <vector>

template <size_t chunkSize> class FixedAllocator
{

  private:
    std::vector<char*> buffer;
    std::vector<char*> data;

    size_t block_size = 100;

    FixedAllocator()
    {
    }

  public:
    FixedAllocator(const FixedAllocator&) = delete;

    static FixedAllocator& instance()
    {
        static FixedAllocator instance;
        return instance;
    }

    void* allocate()
    {
        if (data.empty()) {
            char* block = new char[block_size * chunkSize];
            buffer.push_back(block);
            for (size_t i = 0; i < block_size; ++i) {
                data.push_back(block + i * chunkSize);
            }
        }
        auto t = data.back();
        data.pop_back();
        return static_cast<void*>(t);
    }

    void deallocate(void* ptr)
    {
        data.push_back(static_cast<char*>(ptr));
    }

    ~FixedAllocator()
    {
        data.clear();
        for (auto i : buffer) {
            delete[] i;
        }
    }
};

template <typename T> struct FastAllocator {

    using value_type = T;

    FastAllocator(){};

    FastAllocator(const FastAllocator&)
    {
    }

    ~FastAllocator(){};

    template <typename U> FastAllocator(const FastAllocator<U>&)
    {
    }

    T* allocate(size_t count) const
    {
        size_t need = count * sizeof(T);
        if (need <= 8) {
            return static_cast<T*>(FixedAllocator<8>::instance().allocate());
        } else if (need <= 16) {
            return static_cast<T*>(FixedAllocator<16>::instance().allocate());
        } else if (need <= 24) {
            return static_cast<T*>(FixedAllocator<24>::instance().allocate());
        } else if (need <= 32) {
            return static_cast<T*>(FixedAllocator<32>::instance().allocate());
        } else {
            return std::allocator<T>().allocate(count);
        }
    }

    void deallocate(T* ptr, size_t count)
    {
        size_t need = count * sizeof(T);
        if (need <= 8) {
            FixedAllocator<8>::instance().deallocate(static_cast<void*>(ptr));
        } else if (need <= 16) {
            FixedAllocator<16>::instance().deallocate(static_cast<void*>(ptr));
        } else if (need <= 24) {
            FixedAllocator<24>::instance().deallocate(static_cast<void*>(ptr));
        } else if (need <= 32) {
            FixedAllocator<32>::instance().deallocate(static_cast<void*>(ptr));
        } else {
            std::allocator<T>().deallocate(ptr, count);
        }
    }
};

template <typename T, typename Allocator = std::allocator<T>> class List
{

  public:
    class Node;

    using value_type = T;
    using pointer = T*;

    using traits = std::allocator_traits<Allocator>;
    using node_alloc_t = typename traits::template rebind_alloc<Node>;
    using node_traits = std::allocator_traits<node_alloc_t>;

  private:
    node_alloc_t alloc;

  public:
    class Node
    {

        friend class List;

        T value;
        Node* next = nullptr;
        Node* prev = nullptr;

      public:
        Node()
        {
        }

        Node(const T& value) : value(value)
        {
        }

        Node(T&& value) : value(value)
        {
        }
    };

  private:
    Node* fict;

    size_t list_size = 0;

  public:
    void push_back(const T& value)
    {
        insert(end(), value);
    }

    void push_back(T&& value)
    {
        insert(end(), std::move(value));
    }

    void pop_back()
    {
        erase(--end());
    }

    void push_front(const T& value)
    {
        insert(begin(), value);
    }

    void push_front(T&& value)
    {
        insert(begin(), std::move(value));
    }

    void pop_front()
    {
        erase(begin());
    }

    explicit List(const Allocator& alloc_ = Allocator()) : alloc(alloc_)
    {
        fict = node_traits::allocate(alloc, 1);
        fict->next = fict;
        fict->prev = fict;
    }

    List(size_t count, const Allocator& alloc_ = Allocator()) : List(alloc_)
    {
        for (size_t i = 0; i < count; ++i) {
            Node* new_node = node_traits::allocate(alloc, 1);
            node_traits::construct(alloc, new_node);
            Node* next = fict->next;
            Node* prev = fict;
            next->prev = new_node;
            new_node->next = next;
            new_node->prev = prev;
            prev->next = new_node;
        }
        list_size = count;
    }

    List(size_t count, const T& value, const Allocator& alloc_ = Allocator())
        : List(alloc_)
    {
        for (size_t i = 0; i < count; ++i) {
            Node* new_node = node_traits::allocate(alloc, 1);
            node_traits::construct(alloc, new_node, value);
            Node* next = fict->next;
            Node* prev = fict;
            next->prev = new_node;
            new_node->next = next;
            new_node->prev = prev;
            prev->next = new_node;
        }
        list_size = count;
    }

    List(const List& other)
        : List(traits::select_on_container_copy_construction(
              other.get_allocator()))
    {
        for (auto it = other.cbegin(); it != other.cend(); ++it) {
            push_back(*it);
        }
    }

    List(List&& other)
        : list_size(other.list_size), fict(other.fict),
          alloc(std::move(other.alloc))
    {
        other.fict = nullptr;
        other.list_size = 0;
    }

    List& operator=(const List& other)
    {
        if (this == &other) {
            return *this;
        }
        while (!empty()) {
            pop_back();
        }
        node_traits::deallocate(alloc, fict, 1);
        if (traits::propagate_on_container_copy_assignment::value) {
            alloc = other.get_allocator();
        }
        fict = node_traits::allocate(alloc, 1);
        fict->next = fict;
        fict->prev = fict;
        for (auto it = other.cbegin(); it != other.cend(); ++it) {
            push_back(*it);
        }
        return *this;
    }

    List& operator=(List&& other)
    {
        if (this == &other) {
            return *this;
        }
        fict = other.fict;
        other.fict = nullptr;
        list_size = other.list_size;
        other.list_size = 0;
        alloc = std::move(other.alloc);
        return *this;
    }

    Allocator get_allocator() const
    {
        return alloc;
    }

    size_t size() const
    {
        return list_size;
    }

    bool empty() const
    {
        return size() == 0;
    }

    template <bool is_const> class base_iterator
    {
        friend class List;

        friend class iterator;

        friend class const_iterator;

      public:
        using difference_type = std::ptrdiff_t;
        using value_type =
            typename std::conditional<is_const, const T, T>::type;
        using pointer = typename std::conditional<is_const, const T*, T*>::type;
        using reference =
            typename std::conditional<is_const, const T&, T&>::type;
        using iterator_category = std::bidirectional_iterator_tag;

      public:
        base_iterator()
        {
        }

      private:
        Node* ptr;

        base_iterator(Node* ptr) : ptr(ptr)
        {
        }

      public:
        bool operator==(const base_iterator& other_it) const
        {
            return ptr == other_it.ptr;
        }

        bool operator!=(const base_iterator& other_it) const
        {
            return !(*this == other_it);
        }

        base_iterator& operator++()
        {
            ptr = ptr->next;
            return *this;
        }

        base_iterator& operator--()
        {
            ptr = ptr->prev;
            return *this;
        }

        base_iterator operator++(int)
        {
            base_iterator copy = *this;
            ptr = ptr->next;
            return copy;
        }

        base_iterator operator--(int)
        {
            base_iterator copy = *this;
            ptr = ptr->prev;
            return copy;
        }

        reference operator*() const
        {
            return ptr->value;
        }

        pointer operator->() const
        {
            return &ptr->value;
        }

        operator base_iterator<true>() const
        {
            return base_iterator<true>(ptr);
        }
    };

    using iterator = base_iterator<false>;
    using const_iterator = base_iterator<true>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    iterator begin()
    {
        return iterator(fict->next);
    }

    iterator end()
    {
        return iterator(fict);
    }

    const_iterator begin() const
    {
        return const_iterator(fict->next);
    }

    const_iterator end() const
    {
        return const_iterator(fict);
    }

    const_iterator cbegin() const
    {
        return const_iterator(fict->next);
    }

    const_iterator cend() const
    {
        return const_iterator(fict);
    }

    reverse_iterator rbegin()
    {
        return std::make_reverse_iterator(end());
    }

    reverse_iterator rend()
    {
        return std::make_reverse_iterator(begin());
    }

    const_reverse_iterator rbegin() const
    {
        return std::make_reverse_iterator(cend());
    }

    const_reverse_iterator rend() const
    {
        return std::make_reverse_iterator(cbegin());
    }

    const_reverse_iterator crbegin() const
    {
        return std::make_reverse_iterator(cend());
    }

    const_reverse_iterator crend() const
    {
        return std::make_reverse_iterator(cbegin());
    }

    void insert(const_iterator it, const T& value)
    {
        Node* new_node = node_traits::allocate(alloc, 1);
        node_traits::construct(alloc, new_node, value);
        Node* next = it.ptr;
        Node* prev = it.ptr->prev;
        next->prev = new_node;
        new_node->next = next;
        new_node->prev = prev;
        prev->next = new_node;
        ++list_size;
    }

    void insert(const_iterator it, T&& value)
    {
        Node* new_node = node_traits::allocate(alloc, 1);
        node_traits::construct(alloc, new_node, std::move(value));
        Node* next = it.ptr;
        Node* prev = it.ptr->prev;
        next->prev = new_node;
        new_node->next = next;
        new_node->prev = prev;
        prev->next = new_node;
        ++list_size;
    }

    void erase(const_iterator it)
    {
        if (list_size == 0) {
            throw std::runtime_error("List is empty");
        }
        Node* next = it.ptr->next;
        Node* prev = it.ptr->prev;
        prev->next = next;
        next->prev = prev;
        node_traits::destroy(alloc, it.ptr);
        node_traits::deallocate(alloc, it.ptr, 1);
        --list_size;
    }

    ~List()
    {
        auto it = begin();
        for (size_t i = 0; i < list_size; ++i) {
            auto prev = it++;
            node_traits::destroy(alloc, prev.ptr);
            node_traits::deallocate(alloc, prev.ptr, 1);
        }
        node_traits::deallocate(alloc, it.ptr, 1);
    }
};