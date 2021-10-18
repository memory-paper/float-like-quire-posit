`timescale 1ns / 1ns
module posit_mac0(IN1, IN2, MAC_EN, PURGE, RESULT_REQ_PLS, CLK, RESET, OUT);

function [31:0] log2;
input reg [31:0] value;
	begin
	value = value-1;
	for (log2=0; value>0; log2=log2+1)
        	value = value>>1;
      	end
endfunction

parameter N = 8;
parameter Bs = log2(N);
parameter bias = 2*(N-2);
parameter qsize = (2*bias)+2;
parameter ext = 1;              //余剰ビット
parameter ss = log2(qsize+ext);

input [N-1:0] IN1, IN2;
input MAC_EN, PURGE, RESULT_REQ_PLS;
input CLK, RESET;
output [N-1:0] OUT;

wire s1 = IN1[N-1];
wire s2 = IN2[N-1];
wire zero_tmp1 = |IN1[N-2:0];
wire zero_tmp2 = |IN2[N-2:0];
//wire inf1 = IN1[N-1] & (~zero_tmp1);
//wire inf2 = IN2[N-1] & (~zero_tmp2);
wire zero1 = ~(IN1[N-1] | zero_tmp1);
wire zero2 = ~(IN2[N-1] | zero_tmp2);
//assign inf = inf1 | inf2;
assign zero = zero1 & zero2;

wire rc1, rc2;
wire [Bs-1:0] regime1, regime2;
wire [N-4:0] mant1, mant2;
wire [N-1:0] xin1 = s1 ? -IN1 : IN1;
wire [N-1:0] xin2 = s2 ? -IN2 : IN2;
data_extract #(.N(N)) data_extract1(.in(xin1), .rc(rc1), .regime(regime1), .mant(mant1));
data_extract #(.N(N)) data_extract2(.in(xin2), .rc(rc2), .regime(regime2), .mant(mant2));

wire [N-3:0] m1 = {zero_tmp1,mant1}, m2 = {zero_tmp2,mant2};

wire mult_s = s1 ^ s2;
wire [2*(N-2)-1:0] mult_m = m1 * m2;
wire mult_m_ovf = mult_m[2*(N-2)-1];
wire [2*(N-2)-1:0] mult_mN = ~mult_m_ovf ? mult_m << 1'b1 : mult_m;

wire [Bs+1:0] r1 = rc1 ? {2'b0,regime1} : -regime1;
wire [Bs+1:0] r2 = rc2 ? {2'b0,regime2} : -regime2;
wire [Bs+1:0] mult_e;
add_N_Cin #(.N(Bs+1)) add_N_Cin (r1, r2, mult_m_ovf, mult_e);

wire [Bs+1:0] s_fixed = mult_e + bias;


//仮数部のビット幅分余分にビット幅を確保することでシフト開始位置をLSB側に揃える
wire [2*bias-1+2*(N-2):0] fixed_value;
DLS #(.N(bias), .S(Bs+2), .O(2*bias+2*(N-2))) to_fixed (.a(mult_mN), .b(s_fixed), .c(fixed_value));
wire [qsize-1+ext:0] s_fixed_value = mult_s ? -{1'b0,{ext{1'b0}},fixed_value[2*bias-1+2*(N-2):2*(N-2)-1]} : {1'b0,{ext{1'b0}},fixed_value[2*bias-1+2*(N-2):2*(N-2)-1]};
reg [qsize-1+ext:0] quire;

wire [qsize-1+ext:0] quire_add = quire + s_fixed_value;
wire A = s_fixed_value[qsize-1+ext], B = quire[qsize-1+ext], C = quire_add[qsize-1+ext];
wire ovf = ((A^B==0) && (B^C==1)) ? 1'b1 : 1'b0;
wire [qsize-1+ext:0] quire_value;
assign quire_value = ovf ? ((A==1) ? {1'b1,{(qsize-1+ext){1'b0}}} : {1'b0,{(qsize-1+ext){1'b1}}}) : quire_add;

always @ (posedge CLK or negedge RESET) begin
    if(RESET == 1'b0)
        quire <= 0;
    else if(PURGE)
	      quire <= 0;
    else if(MAC_EN)
        quire <= quire_value;
    else
        quire <= quire;
end

wire [qsize-1+ext:0] quire_masked = RESULT_REQ_PLS ? quire : 0;

wire [qsize-1+ext:0] quire_m = quire_masked[qsize-1+ext] ? -quire_masked : quire_masked;

//オーバーフローチェック
wire [qsize-1+ext:0] tmp_quire_m;
assign tmp_quire_m = |quire_m[qsize-2+ext:qsize-1] ? {{1+ext{1'b0}},1'b1,{(qsize-2){1'b0}}} : quire_m;

wire [ss-1:0] val_quire;
LOD_N #(.N(qsize+ext)) valid_quire(.in(tmp_quire_m), .out(val_quire));

wire [qsize-1+ext:0] quire_frac_s;
DLS #(.N(qsize+ext), .S(ss), .O(qsize+ext)) get_qfrac (.a(tmp_quire_m), .b(val_quire), .c(quire_frac_s));
//wire [2*N+1:0] qfrac = quire_frac_s[qsize-1:(qsize-1)-(2*N+1)];

//取り出す量を限定した場合
//wire [2*(N-2)-1:0] qfrac = quire_frac_s[qsize-1+ext:(qsize-1+ext)-(2*(N-2)-1)];

//全て取り出す場合

//wire [Bs+1:0] quire_exp = ((qsize+ext)-val_quire-(bias+1)) - bias;
wire [Bs+1:0] quire_exp = ((qsize+ext-1)-val_quire)- bias;

wire [Bs:0] r_o;
reg_exp_op #(.Bs(Bs),.N(N)) r_out(quire_exp, r_o);

//wire [2*N-1+3:0] tmp_o = {{N{~quire_exp[Bs+1]}},quire_exp[Bs+1],qfrac[2*N:2*N-(N-2)],qfrac[2*N-(N-2)-1:2*N-(N-2)-2],|qfrac[2*N-(N-2)-3:0]};

//取り出す量を限定した場合
//wire [2*N+1:0] qfrac_tmp = {qfrac,{(2*N+2)-(2*(N-2)){1'b0}}};
//wire [2*N-1+3:0] tmp_o = {{N{~quire_exp[Bs+1]}},quire_exp[Bs+1],qfrac_tmp[2*N:2*N-(N-2)],qfrac_tmp[2*N-(N-2)-1:2*N-(N-2)-2],|qfrac_tmp[2*N-(N-2)-3:0]};

//全て取り出す場合
wire [2*N-1+3:0] tmp_o = {{N{~quire_exp[Bs+1]}},quire_exp[Bs+1],quire_frac_s[qsize-2+ext:(qsize-2+ext)-N],|quire_frac_s[((qsize-2+ext)-N)-1:0]};



wire [3*N-1+3:0] tmp1_o;
//DRS #(.N(3*N+3), .S(Bs+1)) drs (.a({tmp_o,{N{1'b0}}}), .b(r_o[Bs] ? {Bs{1'b1}} : r_o), .c(tmp1_o));
DRS #(.N(3*N+3), .S(Bs+1)) drs (.a({tmp_o,{N{1'b0}}}), .b(r_o), .c(tmp1_o));

wire L = tmp1_o[N+4], G = tmp1_o[N+3], R = tmp1_o[N+2], St = |tmp1_o[N+1:0],
     ulp = ((G & (R | St)) | (L & G & ~(R | St)));
wire [N-1:0] rnd_ulp = {{N-1{1'b0}},ulp};

wire [N:0] tmp1_o_rnd_ulp;
add_N #(.N(N)) uut_add_ulp (tmp1_o[2*N-1+3:N+3], rnd_ulp, tmp1_o_rnd_ulp);
wire [N-1:0] tmp1_o_rnd = (r_o < N-1) ? tmp1_o_rnd_ulp[N-1:0] : tmp1_o[2*N-1+3:N+3];


//wire [N-1:0] tmp1_oN = quire_masked[qsize-1+ext] ? -tmp1_o_rnd : tmp1_o_rnd;
wire [N-2:0] tmp1_oN = quire_masked[qsize-1+ext] ? -tmp1_o_rnd[N-1:1] : tmp1_o_rnd[N-1:1];

//assign out = inf|zero|(~qfrac[2*N+1]) ? {inf,{N-1{1'b0}}} : {quire[qsize-1], tmp1_oN[N-1:1]};
//assign OUT = inf|zero|(~qfrac[2*(N-2)-1]) ? {inf,{N-1{1'b0}}} : {quire_masked[qsize-1+ext], tmp1_oN[N-1:1]};
//assign OUT = {quire_masked[qsize-1+ext], tmp1_oN[N-1:1]};

//assign OUT = (quire_masked) ? ((|tmp1_oN[N-1:1]) ? {quire_masked[qsize-1+ext], tmp1_oN[N-1:1]} : {quire_masked[qsize-1+ext],{(N-2){1'b0}},1'b1}) : 0;

//出力で指数部が大きすぎる場合は最小値と最大値に丸める．
wire [N-1:0] OUT_tmp_p;
wire [N-1:0] OUT_tmp_n;
assign OUT_tmp_p = (|tmp1_oN) ? {quire_masked[qsize-1+ext], tmp1_oN} : {quire_masked[qsize-1+ext],{(N-2){1'b0}},1'b1};
assign OUT_tmp_n = (|tmp1_oN) ? {quire_masked[qsize-1+ext], tmp1_oN} : {N{1'b1}};
wire [N-1:0] OUT_tmp;
assign OUT_tmp = quire_masked[qsize-1+ext] ? OUT_tmp_n : OUT_tmp_p;
assign OUT = (quire_masked) ? OUT_tmp : 0;

endmodule

module data_extract(in, rc, regime, mant);

function [31:0] log2;
input reg [31:0] value;
	begin
	value = value-1;
	for (log2=0; value>0; log2=log2+1)
        	value = value>>1;
      	end
endfunction

parameter N = 8;
parameter Bs = log2(N);
input [N-1:0] in;
output rc;
output [Bs-1:0] regime;
//output [N-1:0] mant;
output [N-4:0] mant;

wire [N-1:0] xin = in;
assign rc = xin[N-2];

wire [N-1:0] xin_r = rc ? ~xin : xin;

wire [Bs-1:0] k;
LOD_N #(.N(N)) xinst_k(.in({xin_r[N-2:0],rc^1'b0}), .out(k));

assign regime = rc ? k-1 : k;

wire [N-1:0] xin_tmp;
DLS #(.N(N), .S(Bs), .O(N)) ls (.a({xin[N-3:0],2'b0}),.b(k),.c(xin_tmp));

//assign mant= xin_tmp;
assign mant = xin_tmp[N-1:3];

endmodule

module LOD_N (in, out);

  function [31:0] log2;
    input reg [31:0] value;
    begin
      value = value-1;
      for (log2=0; value>0; log2=log2+1)
	value = value>>1;
    end
  endfunction

parameter N = 8;
parameter S = log2(N); 
input [N-1:0] in;
output [S-1:0] out;

wire vld;
LOD #(.N(N)) l1 (in, out, vld);
endmodule


module LOD (in, out, vld);

  function [31:0] log2;
    input reg [31:0] value;
    begin
      value = value-1;
      for (log2=0; value>0; log2=log2+1)
	value = value>>1;
    end
  endfunction

  function [31:0] bekizyo;
input reg [31:0] value;
begin
    bekizyo = 1<<value;
end
endfunction


parameter N = 8;
parameter S = log2(N);
parameter Sf = bekizyo(S);

   input [N-1:0] in;
   output [S-1:0] out;
   output vld;

  generate
    if (N == 2)
      begin
	assign vld = |in;
	assign out = ~in[1] & in[0];
      end
    else if (N & (N-1))
      LOD #(Sf) LOD ({in,{((Sf) - N) {1'b0}}},out,vld);
    else
      begin
	wire [S-2:0] out_l, out_h;
	wire out_vl, out_vh;
	LOD #(N>>1) l(in[(N>>1)-1:0],out_l,out_vl);
	LOD #(N>>1) h(in[N-1:N>>1],out_h,out_vh);
	assign vld = out_vl | out_vh;
	assign out = out_vh ? {1'b0,out_h} : {out_vl,out_l};
      end
  endgenerate
endmodule

module DLS(a,b,c);

parameter N=8;
parameter S=3;
parameter O = 8;
input [N-1:0] a;
input [S-1:0] b;
output [O-1:0] c;

wire [O-1:0] tmp [S-1:0];
assign tmp[0]  = b[0] ? a << 7'd1  : a; 
genvar i;
generate
	for (i=1; i<S; i=i+1)begin:loop_blk
		assign tmp[i] = b[i] ? tmp[i-1] << 2**i : tmp[i-1];
	end
endgenerate
assign c = tmp[S-1];

endmodule

module add_N_Cin (a,b,cin,c);
parameter N=8;
input [N:0] a,b;
input cin;
output [N:0] c;
assign c = a + b + cin;
endmodule

module reg_exp_op (exp_o, r_o);
parameter Bs = 2;
parameter N = 8;
input [Bs+1:0] exp_o;
output [Bs:0] r_o;

wire [Bs:0] r_o_tmp;
assign r_o_tmp = exp_o[Bs+1] ? (~exp_o[Bs:0]) + 1 : exp_o[Bs:0] + 1;
wire [Bs:0] r_o;
assign r_o = (r_o_tmp > (N-1)) ? N-1 : r_o_tmp;

endmodule

module add_N (a,b,c);
parameter N=10;
input [N-1:0] a,b;
output [N:0] c;
assign c = {1'b0,a} + {1'b0,b};
endmodule

module DRS(a,b,c);
        parameter N=8;
        parameter S=3;
        input [N-1:0] a;
        input [S-1:0] b;
        output [N-1:0] c;

wire [N-1:0] tmp [S-1:0];
assign tmp[0]  = b[0] ? a >> 7'd1  : a; 
genvar i;
generate
	for (i=1; i<S; i=i+1)begin:loop_blk
		assign tmp[i] = b[i] ? tmp[i-1] >> 2**i : tmp[i-1];
	end
endgenerate
assign c = tmp[S-1];

endmodule