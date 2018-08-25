#include "memory.h"
#include <cstdio>
#include <vector>
#include "runtime.h"
//#define DEBUG_MEMORY_MBED


namespace hae {

void *MemoryManager::alloc_node(size_t n)
{
  void *p = malloc(n);

#ifdef DEBUG_MEMORY_MBED
   LocalFileSystem local("local");
   FILE *fp = fopen("/local/debug.txt", "a");
if(p == 0){   
   fprintf(fp,"Error: malloc node\n");
   fclose(fp);
}
#endif  

  total_node_size_ += n;
  allocated_nodes_.insert(reinterpret_cast<intptr_t>(p));
#ifdef DEBUG_MEMORY
  std::printf("alloc node %zu: %p\n", n, p);
#endif

#ifdef DEBUG_MEMORY_MBED
   fprintf(fp,"alloc node %d: %p\n", n, p);
   fclose(fp);
#endif
  
  return p;
}

void *MemoryManager::alloc_closure(size_t n)
{
  void *p = malloc(n);
#ifdef DEBUG_MEMORY_MBED
   LocalFileSystem local("local");
   FILE *fp = fopen("/local/debug.txt", "a"); 
if(p == 0){
   fprintf(fp,"Error: malloc closure\n");
   fclose(fp);
}
#endif  


  total_closure_size_ += n;
  allocated_closures_.insert(reinterpret_cast<intptr_t>(p));
#ifdef DEBUG_MEMORY
  std::printf("alloc closure %zu: %p\n", n, p);
#endif
#ifdef DEBUG_MEMORY_MBED
   fprintf(fp,"alloc closure %d: %p\n", n, p);
   fclose(fp);
#endif  

  return p;
}

void *MemoryManager::alloc_tuple(size_t n)
{
  void *p = malloc(n);

#ifdef DEBUG_MEMORY_MBED
   LocalFileSystem local("local");
   FILE *fp = fopen("/local/debug.txt", "a");
if(p == 0){   
   fprintf(fp,"Error: malloc touple\n");
   fclose(fp);
}
#endif  

  total_tuple_size_ += n;
  allocated_tuples_.insert(reinterpret_cast<intptr_t>(p));
#ifdef DEBUG_MEMORY
  std::printf("alloc touple %zu: %p\n", n, p);
#endif
#ifdef DEBUG_MEMORY_MBED
   fprintf(fp,"alloc touple %u: %p\n", n, p);
   fclose(fp);
#endif  

  return p;
}  

void MemoryManager::garbage_collect()
{
  reachable_.clear();
  for (std::set<intptr_t>::const_iterator it = allocated_nodes_.begin(); it != allocated_nodes_.end(); ++it) {
    mark(reinterpret_cast<const void *>(*it));
  }

#ifdef DEBUG_MEMORY
  std::printf("Allocated %zu closures and  %zu tuples, marked %zu closures and tuples\n", allocated_closures_.size(), allocated_tuples_.size(), reachable_.size()); 
#endif

#ifdef DEBUG_MEMORY_MBED
  LocalFileSystem local("local");
  FILE *fp = fopen("/local/debug.txt", "a");
  fprintf(fp, "Allocated %d closures and %d tuples, marked %d closures and tuples\n", allocated_closures_.size(), allocated_tuples_.size(), reachable_.size());
#endif  

  std::vector<intptr_t> garbage;
  for (std::set<intptr_t>::const_iterator it = allocated_closures_.begin(); it != allocated_closures_.end(); ++it) {
    if (!reachable_.count(*it)) {
      garbage.push_back(*it);
    }
  }
  for (std::set<intptr_t>::const_iterator it = allocated_tuples_.begin(); it != allocated_tuples_.end(); ++it) {
    if (!reachable_.count(*it)) {
      garbage.push_back(*it);
    }
  }

#ifdef DEBUG_MEMORY
  std::printf("  %zu garbage closures and tuples\n", garbage.size());
#endif
#ifdef DEBUG_MEMORY_MBED
  fprintf(fp,"  %d garbage closures and tuples\n", garbage.size());
  fclose(fp);
#endif  

  for (std::vector<intptr_t>::const_iterator it = garbage.begin(); it != garbage.end(); ++it) {
    if(allocated_closures_.count(*it)){
      allocated_closures_.erase(*it);
    }else if(allocated_tuples_.count(*it)){
      allocated_tuples_.erase(*it);
    }
    delete reinterpret_cast<Object *>(*it);
    //free(reinterpret_cast<Object *>(*it);
  }
}

void MemoryManager::mark(const void *p)
{
  const intptr_t t = reinterpret_cast<intptr_t>(p);
  if (reachable_.count(t)) {
    return;
  }
  if (allocated_nodes_.count(t)) {
    reinterpret_cast<const Object *>(p)->mark();
  } else if (allocated_closures_.count(t)) {
    reachable_.insert(t);
    reinterpret_cast<const Object *>(p)->mark();
  }else if(allocated_tuples_.count(t)){
    reachable_.insert(t);
    reinterpret_cast<const Object *>(p)->mark();
  }
}
};
