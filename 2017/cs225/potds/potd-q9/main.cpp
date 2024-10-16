#include <iostream>
#include "thing.h"

using namespace potd;

int main() {
    Thing * t1 = new Thing(5);
    Thing * t2 = new Thing(5);

    t1->set_property("name","Kermit");
    t1->set_property("color","Green");

    std::cout << t1->get_property("name") << " is " << t1->get_property("color") << std::endl;

    *t2 = *t1;
    t1->set_property("name","Grover");
    t1->set_property("color","Blue");
    std::cout << t2->props_ct_ << " is " << t1->props_ct_ << std::endl;
    std::cout << t2->get_property("name") << " is " << t2->get_property("color") << std::endl;
    std::cout << t1->get_property("name") << " is " << t1->get_property("color") << std::endl;
    std::cout << &t1 << " is " << &t2 << std::endl;
    delete t1;
    delete t2;
}
