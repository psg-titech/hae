#include "runtime.h"

namespace hae {

int signum(int v) {
    if (v < 0) return -1;
    if (v > 0) return 1;
    return 0;
}

int abs(int v) {
    return v > 0? v : -v;
}

}