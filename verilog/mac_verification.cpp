#define POSIT_THROW_ARITHMETIC_EXCEPTION 1
#include <universal/number/posit/posit.hpp>
#include <random>
#include <fstream>
#include <string>
#include <sstream>
#include <bitset>

template<size_t nbits, size_t es>
std::bitset<nbits> twos_complement_conversion(sw::universal::posit<nbits, es> raw){
  std::stringstream ss;
  ss << decoded(raw);
  std::bitset<nbits> bdata(ss.str());
  if(raw < 0){
    ss << decoded(raw);
    std::bitset<nbits> bdata(ss.str());
    bdata = ~bdata;
    int int_bdata = (int)(bdata.to_ulong());
    int_bdata = int_bdata + 1;
    std::bitset<nbits> converted_data(int_bdata);
    converted_data.set(nbits - 1);
    return converted_data;
  }else{
    return bdata;
  }
    
}



int main(){
  using namespace std;
  using namespace sw::universal;

  //parameter
  constexpr size_t nbits    = 8;
  constexpr size_t es       = 1;
  constexpr size_t capacity = 13;
  float FLOAT_MIN = -1;
  float FLOAT_MAX =  1;
  int loop =  1000;
  
  std::string x_filename    = "x_value.txt";
  std::string w_filename    = "w_value.txt";
  std::string o_filename    = "o_value.txt";
  std::string q_filename    = "q_value.txt";
  std::string c_filename    = "cal.txt";

  
  //randomvalue
  //std::random_device rd;
  //std::default_random_engine eng(rd());
  std::mt19937 eng(0);
  std::uniform_real_distribution<float> distr(FLOAT_MIN, FLOAT_MAX);

  
  //output_file
  std::ofstream x_write_file, w_write_file, o_write_file, c_write_file, q_write_file;
  x_write_file.open(x_filename, std::ios::trunc);
  w_write_file.open(w_filename, std::ios::trunc);
  o_write_file.open(o_filename, std::ios::trunc);
  q_write_file.open(q_filename, std::ios::trunc);
  c_write_file.open(c_filename, std::ios::trunc);
  
  posit<nbits, es> px, pw, po;
  quire<nbits, es, capacity> q;
  

  
  for(int i = 0; i < loop; i++){
    px = distr(eng);
    pw = distr(eng);
    convert(q.to_value(), po);
    
    c_write_file << i + 1 << ": " <<  px << "*" << pw << "+" << po;
    
    //q += px * pw;
    q += quire_mul(px, pw);
    convert(q.to_value(), po);
    //po = px * pw;
    c_write_file << "=" << po << "\n";
    
    x_write_file <<  twos_complement_conversion(px) << "\n";
    w_write_file <<  twos_complement_conversion(pw) << "\n";
    o_write_file <<  twos_complement_conversion(po) << "\n";
    q_write_file << i + 1 << ": " << q << "\n";
    
  }
  x_write_file.close();
  w_write_file.close();
  o_write_file.close();
  c_write_file.close();
  q_write_file.close();
  return 0;
}

