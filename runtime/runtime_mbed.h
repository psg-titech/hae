#ifndef HAE_RUNTIME_MBED_H
#define HAE_RUNTIME_MBED_H

#include "mbed_events.h"
#include "runtime.h"
#include <queue>

namespace hae{

class MBedEngineImpl : public Engine {
  private:
    EventQueue eq_;
    ttime now;
    priority_queue<ttime, vector<ttime>, greater<ttime>> timings_;
  protected:
    virtual void impl_init_() {
        now = 0;
    }
    virtual void impl_set_cb_(ttime now, ttime period) {
        timings_.push(period + now);
    }
    virtual void impl_yield_() {
        ttime latest = timings_.top();
        eq_.call_in(latest - now, callback_, latest);
        now = latest;
        timings_.pop();
        eq_.dispatch();
    }
};

}
#endif