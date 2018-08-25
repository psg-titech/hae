#ifndef HAE_RUNTIME_DEBUG_H
#define HAE_RUNTIME_DEBUG_H

#include "runtime.h"
#include <queue>
#include <unistd.h>

using namespace std;

namespace hae{

class DebugEngineImpl : public Engine {
  private:
    ttime now;
    priority_queue<ttime, vector<ttime>, greater<ttime>> timings_;
  protected:
    virtual void impl_init_() {
        now = 0;
    }
    virtual void impl_set_cb_(ttime now, ttime period) {
        timings_.push(period + now);
        printf("Set CB: %lld %lld\n", now, period);
    }
    virtual void impl_yield_() {
        ttime latest = timings_.top();
        usleep(1000 * (latest - now));
        now = latest;
        timings_.pop();
        callback_(latest);
        printf("impl_yield_()");

    }
};

}
#endif