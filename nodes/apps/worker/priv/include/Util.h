#ifndef SS_UTIL
#define SS_UTIL

#include <json_spirit.h>

using namespace json_spirit;

class Util {
public:
    static double to_d(mValue value, double default_value = 0);
};

#endif
