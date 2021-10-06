# The proposed MAC unit verilog
## mac_verification.cpp
This is a program for generating test data.

Please download Universal Library from [the link](https://github.com/stillwater-sc/universal).
You can change the following parameters.
```
constexpr size_t nbits    = 8; //numerical precision of posit
constexpr size_t es       = 1; //es size
constexpr size_t capacity = 2; //the size of the guard bits (ceiling(log_2(k)), k: Number of MAC operations)
float FLOAT_MIN = -1; //the range of the input values
float FLOAT_MAX =  1; //the range of the input values
```

## posit_mac_f_tb.v
This is a test bench for the arithmetic unit. 
Please match the following parameters with the generated test data. 
The smaller the quire size, the more errors are occurred. 
```
parameter N = 8; //numerical precision of posit
parameter es = 1; //es size(Don't choose zero.)
parameter qsize = 30; //quire size
parameter ext = 13; //ceiling(log_2(k)), k: Number of MAC operations
```
