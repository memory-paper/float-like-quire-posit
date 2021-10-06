# The proposed MAC unit verilog
## mac_verification.cpp
This is a program for generating test data.

Please download Universal Library from [the link](https://github.com/stillwater-sc/universal).

You can change the numerical precision(nbits), es(es), the size of the guard bits(capacity), and the range of the input values(from FLOAT_MIN to FLOAT_MAX).
```
constexpr size_t nbits    = 8;
constexpr size_t es       = 1;
constexpr size_t capacity = 2;
float FLOAT_MIN = -1;
float FLOAT_MAX =  1;
```

## posit_mac_f_tb.v
This is a test bench for the arithmetic unit. Please match the following parameters with the generated test data.

```
parameter N = 8; //numerical precision of posit
parameter es = 1; //es size(Don't choose zero.)
parameter qsize = 30; //quire size
parameter ext = 13; //ceiling(log_2(k)), k: Number of MAC operations
```
