#include <cassert>
#include <iostream>
#include <type_traits>

struct BaseDeleter {
    virtual void operator()(void* ptr) = 0;

    virtual ~BaseDeleter() = default;
};

template <typename T> struct MyDeleterT : BaseDeleter {
    void operator()(void* ptr) override
    {
        std::allocator<T>().destroy(reinterpret_cast<T*>(ptr));
        std::allocator<T>().deallocate(reinterpret_cast<T*>(ptr), 1);
    }
};

template <typename T, typename U> struct CustomDeleter : BaseDeleter {
    U deleter;

    CustomDeleter(const U& deleter) : deleter(deleter)
    {
    }

    void operator()(void* ptr) override
    {
        deleter(reinterpret_cast<T*>(ptr));
    }
};

template <typename T, typename U> struct AllocatorDeleter : BaseDeleter {
    U alloc;

    AllocatorDeleter(const U& alloc) : alloc(alloc)
    {
    }

    void operator()(void* ptr) override
    {
        using Alloc =
            typename std::allocator_traits<U>::template rebind_alloc<T>;
        Alloc rebind_alloc = alloc;
        std::allocator_traits<Alloc>::destroy(
            rebind_alloc, static_cast<T*>(ptr));
    }
};

struct BaseAlloc {
    virtual void deallocate(void* ptr) = 0;

    virtual ~BaseAlloc() = default;
};

template <typename T, typename U = std::allocator<T>>
struct CustomAlloc : BaseAlloc {
    U alloc;

    CustomAlloc(const U& alloc) : alloc(alloc)
    {
    }

    void deallocate(void* ptr) override
    {
        using Alloc =
            typename std::allocator_traits<U>::template rebind_alloc<char>;
        Alloc rebind_alloc = alloc;
        std::allocator_traits<Alloc>::deallocate(
            rebind_alloc,
            reinterpret_cast<char*>(ptr),
            2 * sizeof(size_t) + sizeof(BaseDeleter) + sizeof(BaseAlloc));
    }
};

template <typename T, typename U = std::allocator<T>>
struct AllocateSharedAlloc : BaseAlloc {
    U alloc;

    AllocateSharedAlloc(const U& alloc) : alloc(alloc)
    {
    }

    void deallocate(void* ptr) override
    {
        using Alloc =
            typename std::allocator_traits<U>::template rebind_alloc<char>;
        Alloc rebind_alloc = alloc;
        std::allocator_traits<Alloc>::deallocate(
            rebind_alloc,
            reinterpret_cast<char*>(ptr),
            sizeof(T) + 2 * sizeof(size_t) + sizeof(BaseDeleter) +
                sizeof(BaseAlloc));
    }
};

template <typename T> class WeakPtr;

template <typename T> class SharedPtr
{

    template <typename U> friend class SharedPtr;

    template <typename U> friend class WeakPtr;

  private:
    T* ptr = nullptr;
    size_t* shared_counter = nullptr;
    size_t* weak_counter = nullptr;
    BaseDeleter* deleter = nullptr;
    BaseAlloc* allocator = nullptr;
    bool isBlock = false;

    template <typename U, typename... Args>
    friend SharedPtr<U> makeShared(Args&&... args);

    template <typename U, typename Alloc, typename... Args>
    friend SharedPtr<U> allocateShared(const Alloc& alloc, Args&&... args);

    void increment()
    {
        if (shared_counter) {
            ++*shared_counter;
        }
    }

    void decrement()
    {
        if (!shared_counter) {
            return;
        }
        --*shared_counter;
        if (*shared_counter != 0) {
            return;
        }
        deleter->operator()(ptr);
        if (*weak_counter != 0) {
            return;
        }
        if (isBlock) {
            allocator->deallocate(ptr);
        } else {
            allocator->deallocate(shared_counter);
        }
        shared_counter = nullptr;
    }

    SharedPtr(
        T* ptr,
        size_t* shared_counter,
        size_t* weak_counter,
        BaseDeleter* deleter,
        BaseAlloc* allocator,
        bool isBlock)
        : ptr(ptr), shared_counter(shared_counter), weak_counter(weak_counter),
          deleter(deleter), allocator(allocator), isBlock(isBlock)
    {
        increment();
    }

    void clear()
    {
        ptr = nullptr;
        shared_counter = nullptr;
        weak_counter = nullptr;
        deleter = nullptr;
        allocator = nullptr;
        isBlock = false;
    }

  public:
    SharedPtr() = default;

    template <typename U, typename Alloc = std::allocator<T>>
    SharedPtr(T* ptr, U other_deleter, Alloc other_alloc = Alloc()) : ptr(ptr)
    {
        using allocateAlloc =
            typename std::allocator_traits<Alloc>::template rebind_alloc<char>;
        allocateAlloc allocate_alloc = other_alloc;
        auto* storage = std::allocator_traits<allocateAlloc>::allocate(
            allocate_alloc,
            2 * sizeof(size_t) + sizeof(BaseDeleter) + sizeof(BaseAlloc));
        shared_counter = reinterpret_cast<size_t*>(storage);
        weak_counter = reinterpret_cast<size_t*>(storage + sizeof(size_t));
        deleter = reinterpret_cast<BaseDeleter*>(storage + 2 * sizeof(size_t));
        allocator = reinterpret_cast<BaseAlloc*>(
            storage + 2 * sizeof(size_t) + sizeof(BaseDeleter));
        *shared_counter = 1;
        *weak_counter = 0;
        new (deleter) CustomDeleter<T, U>(other_deleter);
        new (allocator) CustomAlloc<T, Alloc>(other_alloc);
        isBlock = false;
    }

    SharedPtr(T* ptr)
        : SharedPtr(ptr, std::default_delete<T>(), std::allocator<T>())
    {
    }

    void reset()
    {
        decrement();
        clear();
    }

    void reset(T* other_ptr)
    {
        SharedPtr<T> other(other_ptr);
        swap(other);
    }

    SharedPtr(const SharedPtr& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    template <typename U> SharedPtr(const SharedPtr<U>& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    SharedPtr(SharedPtr&& other)
    {
        ptr = std::move(other.ptr);
        shared_counter = std::move(other.shared_counter);
        weak_counter = std::move(other.weak_counter);
        deleter = std::move(other.deleter);
        allocator = std::move(other.allocator);
        isBlock = std::move(other.isBlock);
        other.clear();
    }

    template <typename U> SharedPtr(SharedPtr<U>&& other)
    {
        ptr = std::move(other.ptr);
        shared_counter = std::move(other.shared_counter);
        weak_counter = std::move(other.weak_counter);
        deleter = std::move(other.deleter);
        allocator = std::move(other.allocator);
        isBlock = std::move(other.isBlock);
        other.clear();
    }

    SharedPtr& operator=(const SharedPtr& other)
    {
        SharedPtr copy = other;
        swap(copy);
        return *this;
    }

    template <typename U> SharedPtr& operator=(const SharedPtr<U>& other)
    {
        SharedPtr copy = other;
        swap(copy);
        return *this;
    }

    SharedPtr& operator=(SharedPtr&& other)
    {
        SharedPtr copy = std::move(other);
        swap(copy);
        return *this;
    }

    template <typename U> SharedPtr& operator=(SharedPtr<U>&& other)
    {
        SharedPtr copy = std::move(other);
        swap(copy);
        return *this;
    }

    size_t use_count() const
    {
        return *shared_counter;
    }

    T* operator->()
    {
        return ptr;
    }

    T& operator*()
    {
        return *ptr;
    }

    const T& operator*() const
    {
        return *ptr;
    }

    T* get()
    {
        return ptr;
    }

    const T* get() const
    {
        return ptr;
    }

    void swap(SharedPtr& other)
    {
        std::swap(ptr, other.ptr);
        std::swap(shared_counter, other.shared_counter);
        std::swap(weak_counter, other.weak_counter);
        std::swap(deleter, other.deleter);
        std::swap(allocator, other.allocator);
        std::swap(isBlock, other.isBlock);
    }

    ~SharedPtr()
    {
        decrement();
    }
};

template <typename T> class WeakPtr
{

    template <typename U> friend class WeakPtr;

    template <typename U> friend class SharedPtr;

  private:
    T* ptr = nullptr;
    size_t* shared_counter = nullptr;
    size_t* weak_counter = nullptr;
    BaseDeleter* deleter = nullptr;
    BaseAlloc* allocator = nullptr;
    bool isBlock = false;

    void increment()
    {
        if (weak_counter) {
            ++*weak_counter;
        }
    }

    void decrement()
    {
        if (!weak_counter) {
            return;
        }
        --*weak_counter;
        if (*shared_counter != 0 || *weak_counter != 0) {
            return;
        }
        if (isBlock) {
            allocator->deallocate(ptr);
        } else {
            allocator->deallocate(shared_counter);
        }
    }

    void clear()
    {
        ptr = nullptr;
        shared_counter = nullptr;
        weak_counter = nullptr;
        deleter = nullptr;
        allocator = nullptr;
        isBlock = false;
    }

  public:
    WeakPtr() = default;

    WeakPtr(const WeakPtr& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    template <typename U> WeakPtr(const WeakPtr<U>& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    WeakPtr(const SharedPtr<T>& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    template <typename U> WeakPtr(const SharedPtr<U>& other)
    {
        ptr = other.ptr;
        shared_counter = other.shared_counter;
        weak_counter = other.weak_counter;
        deleter = other.deleter;
        allocator = other.allocator;
        isBlock = other.isBlock;
        increment();
    }

    size_t use_count() const
    {
        return *shared_counter;
    }

    bool expired() const
    {
        return *shared_counter == 0;
    }

    SharedPtr<T> lock() const
    {
        return SharedPtr<T>(
            ptr, shared_counter, weak_counter, deleter, allocator, isBlock);
    }

    WeakPtr& operator=(const WeakPtr& other)
    {
        WeakPtr copy = other;
        swap(copy);
        return *this;
    }

    WeakPtr& operator=(WeakPtr&& other)
    {
        WeakPtr copy = std::move(other);
        swap(copy);
        return *this;
    }

    WeakPtr& operator=(const SharedPtr<T>& other)
    {
        WeakPtr copy = other;
        swap(copy);
        return *this;
    }

    template <typename U> WeakPtr& operator=(const SharedPtr<U>& other)
    {
        WeakPtr copy = other;
        swap(copy);
        return *this;
    }

    WeakPtr& operator=(SharedPtr<T>&& other)
    {
        WeakPtr copy = std::move(other);
        swap(copy);
        return *this;
    }

    template <typename U> WeakPtr& operator=(SharedPtr<U>&& other)
    {
        WeakPtr copy = std::move(other);
        swap(copy);
        return *this;
    }

    void swap(WeakPtr& other)
    {
        std::swap(ptr, other.ptr);
        std::swap(shared_counter, other.shared_counter);
        std::swap(weak_counter, other.weak_counter);
        std::swap(deleter, other.deleter);
        std::swap(allocator, other.allocator);
        std::swap(isBlock, other.isBlock);
    }

    ~WeakPtr()
    {
        decrement();
    }
};

template <typename T, typename Alloc, typename... Args>
SharedPtr<T> allocateShared(const Alloc& alloc, Args&&... args)
{

    using allocateAlloc =
        typename std::allocator_traits<Alloc>::template rebind_alloc<char>;
    allocateAlloc allocate_alloc = alloc;
    auto* ptr = std::allocator_traits<allocateAlloc>::allocate(
        allocate_alloc,
        sizeof(T) + 2 * sizeof(size_t) + sizeof(BaseDeleter) +
            sizeof(BaseAlloc));

    using constructAlloc =
        typename std::allocator_traits<Alloc>::template rebind_alloc<T>;
    constructAlloc construct_alloc = alloc;
    std::allocator_traits<constructAlloc>::construct(
        construct_alloc,
        reinterpret_cast<T*>(ptr),
        std::forward<Args>(args)...);

    auto* value = reinterpret_cast<T*>(ptr);
    auto* shared_counter = reinterpret_cast<size_t*>(ptr + sizeof(T));
    auto* weak_counter =
        reinterpret_cast<size_t*>(ptr + sizeof(T) + sizeof(size_t));
    auto* deleter =
        reinterpret_cast<BaseDeleter*>(ptr + sizeof(T) + 2 * sizeof(size_t));
    auto* allocator = reinterpret_cast<BaseAlloc*>(
        ptr + sizeof(T) + 2 * sizeof(size_t) + sizeof(BaseDeleter));

    *shared_counter = 0;
    *weak_counter = 0;
    new (deleter) AllocatorDeleter<T, constructAlloc>(construct_alloc);
    new (allocator) AllocateSharedAlloc<T, constructAlloc>(construct_alloc);

    return SharedPtr<T>(
        value, shared_counter, weak_counter, deleter, allocator, true);
}

template <typename T, typename... Args> SharedPtr<T> makeShared(Args&&... args)
{
    return allocateShared<T>(std::allocator<T>(), std::forward<Args>(args)...);
}