#include <algorithm>
#include <cassert>
#include <chrono>
#include <cmath>
#include <iostream>
#include <list>
#include <string>
#include <vector>

template <size_t chunkSize> class FixedAllocator
{

  private:
    struct Node {
        char data[chunkSize];
        Node* next = nullptr;
    };

    Node* current_free = nullptr;

    std::vector<Node*> buffer;

    size_t size = 1;

    void increase_buffer()
    {
        size *= 2;
        current_free = new Node[size];
        buffer.push_back(current_free);
        for (size_t i = 0; i < size - 1; ++i) {
            current_free[i].next = &current_free[i + 1];
        }
    }

    FixedAllocator()
    {
        increase_buffer();
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
        if (!current_free) {
            increase_buffer();
        }
        Node* return_value = current_free;
        current_free = current_free->next;
        return static_cast<void*>(return_value);
    }

    void deallocate(void* ptr)
    {
        Node* empty_node = static_cast<Node*>(ptr);
        empty_node->next = current_free;
        current_free = empty_node;
    }

    ~FixedAllocator()
    {
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
        if (count == 1 &&
            (sizeof(T) == 4 || sizeof(T) == 8 || sizeof(T) == 16 ||
             sizeof(T) == 24 || sizeof(T) == 32)) {
            return static_cast<T*>(
                FixedAllocator<sizeof(T)>::instance().allocate());
        } else {
            return std::allocator<T>().allocate(count);
        }
    }

    void deallocate(T* ptr, size_t count)
    {
        if (count == 1 &&
            (sizeof(T) == 4 || sizeof(T) == 8 || sizeof(T) == 16 ||
             sizeof(T) == 24 || sizeof(T) == 32)) {
            FixedAllocator<sizeof(T)>::instance().deallocate(
                static_cast<void*>(ptr));
        } else {
            std::allocator<T>().deallocate(ptr, count);
        }
    }

    template <typename... Args> void construct(T* ptr, const Args&... args)
    {
        new (ptr) T(args...);
    }

    void destroy(T* ptr)
    {
        ptr->~T();
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
        Node() = default;

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

    template <typename... Args> void emplace_back(Args&&... args)
    {
        Node* node = traits::allocate(alloc, 1);
        traits::construct(alloc, node, std::forward<Args>(args)...);
        insert_node(end(), node);
    }

    void pop_back()
    {
        erase(--end());
    }

    template <typename... Args> void emplace_front(Args&&... args)
    {
        Node* node = traits::allocate(alloc, 1);
        traits::construct(alloc, node, std::forward<Args>(args)...);
        insert_node(begin(), node);
    }

    void push_front(const T& value)
    {
        insert(begin(), value);
    }

    void pop_front()
    {
        erase(begin());
    }

    explicit List(const Allocator& alloc_ = Allocator()) : alloc(alloc_)
    {
        fict = traits::allocate(alloc, 1);
        fict->next = fict;
        fict->prev = fict;
    }

    List(size_t count, const Allocator& alloc_ = Allocator()) : List(alloc_)
    {
        for (size_t i = 0; i < count; ++i) {
            Node* new_node = alloc.allocate(1);
            traits::construct(alloc, new_node);
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
            Node* new_node = traits::allocate(alloc, 1);
            traits::construct(alloc, new_node, value);
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
        : list_size(other.list_size), alloc(std::move(other.alloc))
    {
        fict = traits::allocate(alloc, 1);
        fict = other.fict;
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
        alloc.deallocate(fict, 1);
        if (traits::propagate_on_container_copy_assignment::value) {
            alloc = other.get_allocator();
        }
        fict = alloc.allocate(1);
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
        while (!empty()) {
            pop_back();
        }
        list_size = other.list_size;
        fict = other.fict;
        other.fict = nullptr;
        other.sz = 0;
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
        Node* new_node = traits::allocate(alloc, 1);
        traits::construct(alloc, new_node, value);
        Node* next = it.ptr;
        Node* prev = it.ptr->prev;
        next->prev = new_node;
        new_node->next = next;
        new_node->prev = prev;
        prev->next = new_node;
        ++list_size;
    }

    void insert_node(const_iterator it, Node* node)
    {
        Node* next = it.ptr;
        Node* prev = it.ptr->prev;
        next->prev = node;
        node->next = next;
        node->prev = prev;
        prev->next = node;
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
        traits::destroy(alloc, it.ptr);
        traits::deallocate(alloc, it.ptr, 1);
        --list_size;
    }

    template <typename... Args> void emplace(const_iterator it, Args&&... args)
    {
        Node* node = traits::allocate(alloc, 1);
        traits::construct(alloc, node, std::forward<Args>(args)...);
        insert_node(it, node);
    }

    void clear()
    {
        while (list_size) {
            pop_back();
        }
    }

    ~List()
    {
        clear();
        traits::destroy(alloc, fict);
        traits::deallocate(alloc, fict, 1);
    }
};

template <
    typename Key,
    typename Value,
    typename Hash = std::hash<Key>,
    typename Equal = std::equal_to<Key>,
    typename Alloc = std::allocator<std::pair<const Key, Value>>>
class UnorderedMap
{
  public:
    using NodeType = std::pair<const Key, Value>;
    using list_allocator =
        typename std::allocator_traits<Alloc>::template rebind_alloc<NodeType*>;
    using list_iterator = typename List<NodeType*, list_allocator>::iterator;
    using const_list_iterator =
        typename List<NodeType*, list_allocator>::const_iterator;

    using iterator_allocator = typename std::allocator_traits<
        Alloc>::template rebind_alloc<list_iterator>;
    using alloc_traits = std::allocator_traits<iterator_allocator>;

    template <bool is_const> class map_iterator;

    using iterator = typename List<NodeType, Alloc>::iterator;
    using const_iterator = typename List<NodeType, Alloc>::const_iterator;

    double max_load_factor_value = 1;

    size_t map_size = 0;
    list_iterator* buffer;

    List<NodeType*, list_allocator> common_list;
    Hash hash_function;
    Equal equal_function;
    Alloc allocator_node;
    iterator_allocator allocator_iter;

    void fill_list()
    {
        std::fill(buffer, buffer + map_size, common_list.end());
    }

    UnorderedMap()
        : map_size(1), buffer(alloc_traits::allocate(allocator_iter, 1))
    {
        fill_list();
    }

    UnorderedMap(const UnorderedMap& other)
        : max_load_factor_value(other.max_load_factor_value),
          map_size(other.map_size),
          buffer(alloc_traits::allocate(allocator_iter, map_size)),
          hash_function(other.hash_function),
          equal_function(other.equal_function),
          allocator_node(other.allocator_node),
          allocator_iter(other.allocator_iter)
    {
        fill_list();
        for (auto it = other.begin(); it != other.end(); ++it) {
            insert(*it);
        }
    }

    UnorderedMap(UnorderedMap&& other)
        : max_load_factor_value(std::move(other.max_load_factor_value)),
          map_size(std::move(other.map_size)), buffer(std::move(other.buffer)),
          common_list(std::move(other.common_list)),
          hash_function(std::move(other.hash_function)),
          equal_function(std::move(other.equal_function)),
          allocator_node(std::move(other.allocator_node)),
          allocator_iter(std::move(other.allocator_iter))
    {
    }

    UnorderedMap& operator=(const UnorderedMap& other)
    {
        if (this == &other)
            return *this;
        alloc_traits::deallocate(allocator_iter, buffer, map_size);
        buffer = nullptr;
        max_load_factor_value = other.max_load_factor_value;
        map_size = other.map_size;
        buffer = other.buffer;
        other.buffer = nullptr;
        common_list = std::move(other.common_list);
        hash_function = std::move(other.hash_function);
        equal_function = std::move(other.equal_function);
        allocator_node = std::move(other.allocator_node);
        allocator_iter = std::move(other.allocator_iter);
    }

    UnorderedMap& operator=(UnorderedMap&& other)
    {
        if (this == &other)
            return *this;
        alloc_traits::deallocate(allocator_iter, buffer, map_size);
        buffer = nullptr;
        max_load_factor_value = other.max_load_factor_value;
        map_size = other.map_size;
        buffer = alloc_traits::allocate(allocator_iter, map_size);
        common_list = other.common_list;
        hash_function = other.hash_function;
        equal_function = other.equal_function;
        allocator_node = other.allocator_node;
        allocator_iter = other.allocator_iter;
        fill_list();
        for (auto it = other.begin(); it != other.end(); ++it) {
            insert(*it);
        }
    }

    ~UnorderedMap()
    {
        alloc_traits::deallocate(allocator_iter, buffer, map_size);
        for (auto it = common_list.begin(); it != common_list.end(); ++it) {
            delete *it;
        }
    }

    iterator find(const Key& key)
    {
        size_t h = hash_function(key) % map_size;
        auto it = buffer[h];
        while (it != common_list.end() &&
               hash_function((*it)->first) % map_size == h) {
            if (equal_function((*it)->first, key)) {
                return iterator(it);
            }
            ++it;
        }
        return end();
    }

    Value& operator[](Key key)
    {
        auto it = find(key);
        if (it == end()) {
            insert({key, Value()});
            it = find(key);
            return it->second;
        }
        return it->second;
    }

    Value& at(const Key& key)
    {
        auto it = find(key);
        if (it == end()) {
            throw std::out_of_range("There are no elements with this key");
        }
        return it->second;
    }

    std::pair<iterator, bool> insert(const NodeType& node)
    {
        NodeType* ptr =
            std::allocator_traits<Alloc>::allocate(allocator_node, 1);
        std::allocator_traits<Alloc>::construct(allocator_node, ptr, node);
        if (find(ptr->first) != end()) {
            std::allocator_traits<Alloc>::destroy(allocator_node, ptr);
            std::allocator_traits<Alloc>::deallocate(allocator_node, ptr, 1);
            return {find(node.first), false};
        }
        check_load_factor();
        size_t h = hash_function(ptr->first) % map_size;
        common_list.insert(buffer[h], ptr);
        --buffer[h];
        return {iterator(buffer[h]), true};
    }

    template <class... Args> std::pair<iterator, bool> emplace(Args&&... args)
    {
        NodeType* ptr =
            std::allocator_traits<Alloc>::allocate(allocator_node, 1);
        std::allocator_traits<Alloc>::construct(
            allocator_node, ptr, std::forward<Args>(args)...);
        if (find(ptr->first) != end()) {
            std::allocator_traits<Alloc>::destroy(allocator_node, ptr);
            std::allocator_traits<Alloc>::deallocate(allocator_node, ptr, 1);
            return {find(ptr->first), false};
        }
        check_load_factor();
        size_t h = hash_function(ptr->first) % map_size;
        common_list.insert(buffer[h], ptr);
        --buffer[h];
        return {iterator(buffer[h]), true};
    }

    std::pair<iterator, bool> insert(NodeType&& node)
    {
        return emplace(std::forward<NodeType>(node));
    }

    template <typename Iter> void insert(Iter beg, Iter en)
    {
        for (; beg != en; ++beg) {
            insert(*beg);
        }
    }

    void erase(const_iterator it)
    {
        if (it.iter == common_list.cend())
            return;
        size_t h = hash_function(it->first) % map_size;
        if (it.iter != buffer[h]) {
            common_list.erase(it.iter);
            return;
        }
        const_iterator copy = it;
        ++copy;
        if (copy != cend() && hash_function(copy->first) % map_size == h) {
            buffer[h] = copy.iter;
        } else {
            buffer[h] = common_list.end();
        }
        common_list.erase(it.iter);
    }

    void erase(const_iterator beg, const_iterator en)
    {
        for (; beg != en; ++beg) {
            erase(beg);
        }
    }

    iterator begin()
    {
        return iterator(common_list.begin());
    }

    iterator end()
    {
        return iterator(common_list.end());
    }

    const_iterator begin() const
    {
        return const_iterator(common_list.cbegin());
    }

    const_iterator end() const
    {
        return const_iterator(common_list.cend());
    }

    const_iterator cbegin() const
    {
        return const_iterator(common_list.cbegin());
    }

    const_iterator cend() const
    {
        return const_iterator(common_list.cend());
    }

    size_t size() const
    {
        return common_list.size();
    }

    double load_factor() const
    {
        return static_cast<double>(common_list.size()) / map_size;
    }

    void max_load_factor(double factor)
    {
        max_load_factor_value = factor;
        check_load_factor();
    }

    double max_load_factor() const
    {
        return max_load_factor_value;
    }

    void reserve(size_t n)
    {
        if (n > map_size) {
            rehash(std::ceil(n / max_load_factor_value));
        }
    }

    void rehash(size_t n)
    {
        alloc_traits::deallocate(allocator_iter, buffer, map_size);
        map_size = n;
        buffer = alloc_traits::allocate(allocator_iter, map_size);
        fill_list();
        for (auto it = common_list.begin(); it != common_list.end(); ++it) {
            size_t h = hash_function((*it)->first) % map_size;
            if (buffer[h] == common_list.end()) {
                buffer[h] = it;
            }
        }
    }

    void check_load_factor()
    {
        if (load_factor() > max_load_factor_value) {
            rehash(map_size * 2);
        }
    }
};
