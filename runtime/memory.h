#ifndef HAE_MEMORY_H
#define HAE_MEMORY_H

#include <set>
#include <cstdlib>
#include <stdint.h>
// #include "mbed.h" // FIXME: no hard-coding

//#define DEBUG_MEMORY_MBED
#ifdef DEBUG_MEMORY_MBED
#include "mbed.h"
#endif

namespace hae
{

struct Object
{
    virtual void mark() const = 0;
    virtual ~Object() {}
};

class MemoryManager
{

  public:
#ifdef DEBUG_MEMORY_MBED
    MemoryManager()
    {

        LocalFileSystem local("local");
        FILE *fp = fopen("/local/debug.txt", "a");
        fprintf(fp, "---------------------------------------\n\n");
        fclose(fp);
    }
#endif
    /* ~Memory_manager(){ */
    /*   fclose(fp); */
    /* } */

    std::set<intptr_t> allocated_nodes_, allocated_closures_, allocated_tuples_, reachable_;
    size_t total_node_size_, total_closure_size_, total_tuple_size_;

  public:
    void *alloc_node(size_t n);
    void *alloc_closure(size_t n);
    void *alloc_tuple(size_t n);
    inline size_t total_node_size() const { return total_node_size_; }
    inline size_t total_closure_size() const { return total_closure_size_; }
    inline size_t total_tuple_size() const { return total_tuple_size_; }
    void garbage_collect();
    void mark(const void *p);
};

extern MemoryManager *global_memory;
};     // namespace hae
#endif /* end of include guard */
