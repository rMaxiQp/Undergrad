#include <iostream>
#include "circle.h"

using namespace std;

// Include the following line in pass_by_value:
//   cout<<"In pass_by_value, the memory address of c is: "<<(&c)<<endl;
void pass_by_value(circle c){
  cout<<"In pass_by_value, the memory address of c is: "<<(&c)<<endl;
}
// Include the following line in pass_by_pointer:
//   cout<<"In pass_by_pointer, the memory address of c is: "<<(c)<<endl;
void pass_by_pointer(circle *c){
  cout<<"In pass_by_pointer, the memory address of c is: "<<(c)<<endl;
}
// Include the following line in pass_by_ref:
//   cout<<"In pass_by_ref, the memory address of c is: "<<(&c)<<endl;
void pass_by_ref(circle &c){
  cout<<"In pass_by_ref, the memory address of c is: "<<(&c)<<endl;
}
