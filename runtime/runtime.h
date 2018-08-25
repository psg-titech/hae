#ifndef HAE_RUNTIME_H
#define HAE_RUNTIME_H

#include <cstdio>
#include <map>
#include <vector>
#include <queue>
#include "memory.h"

namespace hae
{

typedef long long ttime;

int signum(int v);
int abs(int v);

struct Closure;
class Node;

union tup_elem_t {
    int tup_int;
    tup_elem_t() : tup_int(0) {}
    tup_elem_t(int n) : tup_int(n) {}
};

struct Tuple : public Object
{
    tup_elem_t *elems;

    void *operator new(size_t n)
    {
        return ::hae::global_memory->alloc_tuple(n);
    }

    virtual void mark() const {}

    Tuple(tup_elem_t x1, tup_elem_t x2)
    {
        elems = new tup_elem_t[2];
        elems[0] = x1;
        elems[1] = x2;
    }

    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3)
    {
        elems = new tup_elem_t[3];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
    }

    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4)
    {
        elems = new tup_elem_t[4];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
    }

    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5)
    {
        elems = new tup_elem_t[5];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
    }
    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6)
    {
        elems = new tup_elem_t[6];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
        elems[5] = x6;
    }
    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7)
    {
        elems = new tup_elem_t[7];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
        elems[5] = x6;
        elems[6] = x7;
    }
    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8)
    {
        elems = new tup_elem_t[8];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
        elems[5] = x6;
        elems[6] = x7;
        elems[7] = x8;
    }
    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8, tup_elem_t x9)
    {
        elems = new tup_elem_t[9];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
        elems[5] = x6;
        elems[6] = x7;
        elems[7] = x8;
        elems[8] = x9;
    }
    Tuple(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8, tup_elem_t x9, tup_elem_t x10)
    {
        elems = new tup_elem_t[10];
        elems[0] = x1;
        elems[1] = x2;
        elems[2] = x3;
        elems[3] = x4;
        elems[4] = x5;
        elems[5] = x6;
        elems[6] = x7;
        elems[7] = x8;
        elems[8] = x9;
        elems[9] = x10;
    }

    ~Tuple()
    {
        delete[] elems;
    }
};

union value {
    int int_value;
    bool bool_value;
    float float_value;
    double double_value;
    Closure *closure_value;
    Node *node_value;
    Tuple *tup_value;

    value() : int_value(0) {}
    value(int n) : int_value(n) {}
    value(bool b) : bool_value(b) {}
    value(float f) : float_value(f) {}
    value(double d) : double_value(d) {}
    value(Closure *c) : closure_value(c) {}
    value(Node *n) : node_value(n) {}

    value(tup_elem_t x1, tup_elem_t x2)
    {
        tup_value = new Tuple(x1, x2);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3)
    {
        tup_value = new Tuple(x1, x2, x3);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4)
    {
        tup_value = new Tuple(x1, x2, x3, x4);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5, x6);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5, x6, x7);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5, x6, x7, x8);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8, tup_elem_t x9)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5, x6, x7, x8, x9);
    }
    value(tup_elem_t x1, tup_elem_t x2, tup_elem_t x3, tup_elem_t x4, tup_elem_t x5, tup_elem_t x6, tup_elem_t x7, tup_elem_t x8, tup_elem_t x9, tup_elem_t x10)
    {
        tup_value = new Tuple(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10);
    }
};

struct Closure : public Object
{
    void *operator new(size_t n)
    {
        return ::hae::global_memory->alloc_closure(n);
    }

    virtual ~Closure() {}

    virtual value operator()(value x) = 0;

    inline value operator()(value x1, value x2)
    {
        return operator()(x1).closure_value->operator()(x2);
    }

    inline value operator()(value x1, value x2, value x3)
    {
        return operator()(x1, x2).closure_value->operator()(x3);
    }
};

struct Event
{
    bool changed;
    value data;
    Event(bool b, value d) : changed(b), data(d) {}
};

class Node : public Object
{
  protected:
    size_t nreceived_;
    std::vector<Event> received_;
    std::vector<std::pair<Node *, size_t> > kids_;
//    std::vector<Event> prev_; // FIXME: what's this?

  public:
    Node(int argc) : nreceived_(0), received_(), kids_() // , prev_()
    {
        received_.reserve(argc);
#ifdef DEBUG_MEMORY
        std::fprintf(stderr, "Allocate node %p\n", this);
#endif
    }

    virtual ~Node() {}

    void *operator new(size_t n)
    {
        return global_memory->alloc_node(n);
    }

    void add(Node *n)
    {
        kids_.push_back(std::make_pair(n, n->received_.size()));
        n->received_.push_back(Event(false, value()));
    }

    std::vector<std::pair<Node *, size_t> > &kids() { return kids_; }

    void clear_events()
    {
        nreceived_ = 0;
    }

    void receive(int idx, const Event &e)
    {
        received_[idx] = e;
        ++nreceived_;
    }

    bool ready() const
    {
        return nreceived_ == received_.size();
    }

    virtual Event process() = 0;
    virtual void mark() const {};
};

class PrevNode : public Node {
  private:
    bool first_run_;
    value next_;
    value prev_;
  public:
    PrevNode() : Node(1), first_run_(true) {}

    virtual Event process() {
        bool changed = received_.begin()->changed;
		if (changed) {
            if (first_run_) {
                prev_ = received_.begin()->data;
                next_ = prev_;
                first_run_ = false;
            } else {
                prev_ = next_;
                next_ = received_.begin()->data;
            }
        }
        return Event(changed, prev_);
    }
};

class SampleNode : public Node {
  public:
    SampleNode() : Node(2) {}
    virtual Event process() {
        return Event(received_[0].changed && received_[1].changed, received_[0].data);
    }
};

class PrimNode : public Node {
  private:
    value retval_;
    std::vector<value> arg_store_;
  protected:
    virtual value f_(std::vector<value> argv) = 0;
  public:
    PrimNode(int argc) : Node(argc) {
        arg_store_.reserve(argc);
    }

    virtual Event process() {
        bool changed = false;
        for (std::vector<Event>::const_iterator it = received_.begin(); it != received_.end(); it++) {
            if (it->changed) {
                changed = true;
                break;
            }
        }
        if (changed) {
            for (std::vector<Event>::const_iterator it = received_.begin(); it != received_.end(); it++) {
                arg_store_.push_back(it->data);
            }
            retval_ = f_(arg_store_);
            int argc = arg_store_.size();
            arg_store_.clear();
            arg_store_.reserve(argc);
        }
        return Event(changed, retval_);
    }
};

struct Timing {
    ttime period;
    size_t len_past;
    std::vector<ttime> offsets;
    Timing(ttime p, size_t l, const ttime pt[]) : period(p), len_past(l), offsets(pt, pt+l) {}
};

class InputNode : public Node {
  protected:
    std::vector<Timing> timing_;
    value def_;
  public:
    InputNode() : Node(0) {}

    void add_timing(const Timing &t) {
        timing_.push_back(t);
    }

    const std::vector<Timing>& get_timing() {
        return timing_;
    }
};

class IN__fps : public InputNode {
  public:
    IN__fps() : InputNode() {}
  protected:
    virtual Event process() {
        return Event(true, value());
    }
};

/*
class lift_node : public Node
{
    Closure *f_;
    value prev_;

  public:
    lift_node(Closure *f) : f_(f), prev_(0) {}
    virtual ~lift_node() {}
    virtual Event process()
    {
        bool changed = false;
        for (std::vector<Event>::const_iterator it = received_.begin(); it != received_.end(); ++it)
        {
            if (it->changed)
            {
                changed = true;
                break;
            }
        }
        if (changed)
        {
            prev_ = f_;
            for (std::vector<Event>::const_iterator it = received_.begin(); it != received_.end(); ++it)
            {
                prev_ = (*prev_.closure_value)(it->data);
            }
        }
        return Event(changed, prev_);
    }

    virtual void mark() const
    {
        global_memory->mark(f_);
        global_memory->mark(prev_.closure_value);
    }
};

template <unsigned N>
class lift_collect_closure : public closure
{
    closure *f_;
    const std::vector<node *> nodes_;

  public:
    lift_collect_closure(closure *f, const std::vector<node *> &nodes) : f_(f), nodes_(nodes) {}

    value operator()(value s)
    {
        std::vector<node *> nodes = nodes_;
        nodes.push_back(s.node_value);
        if (nodes.size() == N)
        {
            node *n = new lift_node(f_);
            for (unsigned i = 0; i < N; i++)
            {
                nodes[i]->add(n);
            }
            return n;
        }
        else
        {
            return new lift_collect_closure<N>(f_, nodes);
        }
    }

    virtual void mark() const
    {
        global_memory->mark(f_);
        for (std::vector<node *>::const_iterator it = nodes_.begin(); it != nodes_.end(); ++it)
        {
            global_memory->mark(*it);
        }
    }
};

template <unsigned N>
class lift_closure : public closure
{
  public:
    value operator()(value f)
    {
        return new lift_collect_closure<N>(f.closure_value, std::vector<node *>());
    }

    virtual void mark() const
    {
    }
};
*/

/*
class foldp_node : public Node
{
    closure *f_;
    value acc_;

  public:
    foldp_node(closure *f, value n) : f_(f), acc_(n)
    {
    }
    virtual ~foldp_node() {}

    virtual event process()
    {
        if (received_[0].changed)
        {
            acc_ = (*f_)(received_[0].data, acc_);
        }
        return event(received_[0].changed, acc_);
    }

    virtual void mark() const
    {
        global_memory->mark(f_);
        global_memory->mark(acc_.tup_value);
        global_memory->mark(acc_.closure_value);
    }
};

class foldp_closure_2 : public closure
{
    closure *f_;
    value init_;

  public:
    foldp_closure_2(closure *f, value init) : f_(f), init_(init) {}

    value operator()(value sig)
    {
        foldp_node *n = new foldp_node(f_, init_);
        sig.node_value->add(n);
        return n;
    }

    virtual void mark() const
    {
        global_memory->mark(f_);
        global_memory->mark(init_.tup_value);
        global_memory->mark(init_.closure_value);
    }
};

class foldp_closure_1 : public closure
{
    closure *f_;

  public:
    foldp_closure_1(closure *f) : f_(f) {}

    value operator()(value init)
    {
        return new foldp_closure_2(f_, init);
    }

    virtual void mark() const
    {
        global_memory->mark(f_);
    }
};

class foldp_closure : public closure
{
  public:
    value operator()(value f)
    {
        return new foldp_closure_1(f.closure_value);
    }

    virtual void mark() const
    {
    }
};

class switch_node : public node
{
    int input_idx;

  public:
    switch_node() : input_idx(1) {}
    virtual ~switch_node() {}

    virtual event process()
    {
        if (received_[0].changed && input_idx == 1)
        {
            const value &v = received_[0].data;
            if (v.int_value)
            {
                ++input_idx;
                return event(true, received_[input_idx].data);
            }
        }
        return received_[input_idx];
    }
};
*/

class Engine
{
  private:
    ttime now_;
    std::multimap<ttime, std::pair<InputNode*, ttime> > push_timings_;
    std::vector<Node *> input_nodes_;
    std::map<Node *, bool> input_activated_;
  protected:
    virtual void impl_init_() = 0; 
    virtual void impl_set_cb_(ttime now, ttime delay) = 0;
    virtual void impl_yield_() = 0;

  public:
    Engine() : now_(0) {}

    void register_input_node(InputNode *n)
    {
        input_nodes_.push_back(n);
        input_activated_[n] = true;
    }

    void callback_(ttime now) {
        now_ = now;
        typedef std::multimap<ttime, std::pair<InputNode*, ttime>>::iterator ITType;
        std::pair<ITType, ITType> result = push_timings_.equal_range(now);
        std::multimap<ttime, std::pair<InputNode*, ttime> > tmpTimings;
        for (ITType it = result.first; it != result.second; it++) {
            InputNode *n = it->second.first;
            ttime period = it->second.second;
            if (push_timings_.count(now + period) == 0) {
                impl_set_cb_(now, period);
            }
            tmpTimings.insert(std::make_pair(now + period, std::make_pair(n, period)));
            // mark the node as TO BE UPDATED
            input_activated_[n] = true;
        }
        push_timings_.erase(now);
        for (ITType it = tmpTimings.begin(); it != tmpTimings.end(); it++) {
            push_timings_.insert(*it);
        }
        // update the node
        iterate_();
        impl_yield_();
    }

    void dispatch() {
        impl_init_();
        // for each input node
        for (std::vector<Node*>::const_iterator it = input_nodes_.begin(); it != input_nodes_.end(); it++) {
            InputNode* n = (InputNode *)*it;
            // for each timing, prepare for the second round
            for (std::vector<Timing>::const_iterator tit = n->get_timing().begin(); tit != n->get_timing().end(); tit++) {
                Timing t = *tit;
                if (t.len_past == 0) { // timing without offset
                    ttime next = t.period;
                    if (push_timings_.count(next) == 0) {
                        impl_set_cb_(0, t.period);
                    }
                    push_timings_.insert(std::make_pair(next, std::make_pair(n, t.period)));
                } else { // timing with offset
                    for (std::vector<ttime>::const_iterator pit = t.offsets.begin(); pit != t.offsets.end(); pit++) {
                        ttime next = t.period + *pit; // note that the offset is a minus number
                        if (push_timings_.count(next) == 0) {
                            impl_set_cb_(*pit, t.period);
                        }
                        push_timings_.insert(std::make_pair(next, std::make_pair(n, t.period)));
                    }
                }
            }
        }
        // first iteration
        iterate_();
        impl_yield_();
    }

    void iterate_()
    {
        std::queue<Node *> q;
        for (std::vector<Node *>::iterator it = input_nodes_.begin(); it != input_nodes_.end(); ++it)
        {
            q.push(*it);
        }
        while (!q.empty())
        {
            Node *n = q.front();
            q.pop();
            const Event e = input_activated_.count(n) == 1 && input_activated_[n] == false?
                                Event(false, value()) : n->process();
            n->clear_events();
            for (std::vector<std::pair<Node *, size_t>>::iterator it = n->kids().begin(); it != n->kids().end(); ++it)
            {
                Node *next_node = it->first;
                next_node->receive(it->second, e);
                if (next_node->ready())
                {
                    q.push(next_node);
                }
            }
        }
        for (std::map<Node *, bool>::iterator it = input_activated_.begin(); it != input_activated_.end(); it++) {
            it->second = false;
        }
    }
};

}; // namespace hae

#endif /* end of include guard */
