#include <iostream>
#include <string>
#include <iomanip>

using std::string;
using std::cout;
using std::endl;
using std::setw;

int main()
{
   cout << "Hey,";
   string mesg = "this my second C++ program";
   mesg += "!";
   cout << mesg << endl;

  // endl stands for 'end line'
   for(int j = 1; j<17; j++){
   for(int i= 1; i<17; i++) {
     cout <<setw(5) << i*j;
   }
   cout << endl;
 }
   return 0;
}
