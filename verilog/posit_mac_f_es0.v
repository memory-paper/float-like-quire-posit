`timescale 1ns / 1ns
module posit_mac_f_es0(IN1, IN2, BIAS, MAC_EN, PURGE, RESULT_REQ_PLS, BIAS_EN, CLK, RESET, OUT);

function [31:0] log2;
input reg [31:0] value;
	begin
	value = value-1;
	for (log2=0; value>0; log2=log2+1)
        	value = value>>1;
      	end
endfunction

parameter N = 8;
parameter qsize = 20;
parameter ext = 13;

parameter Bs = log2(N);
parameter bias = 2*(N-2);
parameter qsize2 = (2*bias)+2 + ext;
parameter q_s1 = log2(qsize);
parameter q_s2 = log2(qsize2);
parameter mult_bw = 2*(N-2);
parameter ss = log2(qsize2 - qsize);
parameter quire_bias = qsize - 2;

input [N-1:0] IN1, IN2, BIAS;
input MAC_EN, PURGE, RESULT_REQ_PLS, BIAS_EN;
input CLK, RESET;
output [N-1:0] OUT;

//Bias Decode
wire [N-1:0] bias_in;
assign bias_in = BIAS_EN ? BIAS : 0;
wire [N-1:0] xbias = bias_in[N-1] ? -bias_in : bias_in;
wire rc_b;
wire [Bs-1:0] regime_b;
wire [N-4:0] mant_b;
data_extract #(.N(N)) decode_bias(.in(xbias), .rc(rc_b), .regime(regime_b), .mant(mant_b));
wire [N-3:0] m_b = {1'b1,mant_b};
wire [Bs:0] r_b = rc_b ? {1'b0,regime_b} : -regime_b;
wire [Bs:0] bias_ex = r_b + bias;
wire signed [mult_bw:0] acc_bias_value = bias_in[N-1] ? -m_b <<< (mult_bw - (N-3) - 1) : m_b <<< (mult_bw - (N-3) - 1);

//Decode
wire s1 = IN1[N-1];
wire s2 = IN2[N-1];
wire zero_tmp1 = |IN1[N-2:0];
wire zero_tmp2 = |IN2[N-2:0];
wire zero1 = ~(IN1[N-1] | zero_tmp1);
wire zero2 = ~(IN2[N-1] | zero_tmp2);
assign zero = zero1 & zero2;
wire rc1, rc2;
wire [Bs-1:0] regime1, regime2;
wire [N-4:0] mant1, mant2;
wire [N-1:0] xin1 = s1 ? -IN1 : IN1;
wire [N-1:0] xin2 = s2 ? -IN2 : IN2;
data_extract #(.N(N)) decode1(.in(xin1), .rc(rc1), .regime(regime1), .mant(mant1));
data_extract #(.N(N)) decode2(.in(xin2), .rc(rc2), .regime(regime2), .mant(mant2));
wire [N-3:0] m1 = {zero_tmp1,mant1}, m2 = {zero_tmp2,mant2};

//multiplication
wire mult_s = s1 ^ s2;
wire [mult_bw-1:0] mult_m = m1 * m2;
wire mult_m_ovf = mult_m[mult_bw-1];
wire [mult_bw-1:0] mult_mN_tmp = ~mult_m_ovf ? mult_m << 1'b1 : mult_m;
wire [mult_bw:0] mult_mN = mult_s ? -{1'b0,mult_mN_tmp} : {1'b0,mult_mN_tmp};
wire signed [Bs+1:0] r1 = rc1 ? {2'b0,regime1} : -regime1;
wire signed [Bs+1:0] r2 = rc2 ? {2'b0,regime2} : -regime2;
wire [Bs+1:0] mult_e;
add_N_Cin #(.N(N)) exp_add(r1, r2, mult_m_ovf, mult_e);

reg signed [mult_bw:0] mult_reg;
reg [Bs+1:0] mult_scale_reg;
always @(posedge CLK or negedge RESET)begin
    if(RESET == 1'b0)
        mult_reg <= 0;
    else if(PURGE)
        mult_reg <= 0;
    else if(MAC_EN)
        mult_reg <= mult_mN;
    else
        mult_reg <= 0; 
end
always @(posedge CLK or negedge RESET)begin
    if(RESET == 1'b0)
        mult_scale_reg <= 0;
    else if(PURGE)
        mult_scale_reg <= 0;
    else if(MAC_EN)
        mult_scale_reg <= mult_e;
    else
        mult_scale_reg <= 0;
end

//accumulation
reg signed [mult_bw:0] mult;
reg [Bs+1:0] mult_scale;
reg [ss-1:0] q_scale_reg;
reg signed [qsize-1:0] quire_reg;
reg signed [qsize+mult_bw-2:0] add_value;
reg [ss-1:0] add_scale;
reg signed [qsize-1:0] small_value;
reg [ss-1:0] small_scale;
reg signed [qsize-1:0] big_value;
reg [ss-1:0] big_scale;
reg [ss-1:0] scale_diff;

reg signed [qsize-1:0] quire;
reg signed [qsize-1:0] quire_tmp;
reg [ss-1:0] q_scale;
reg [ss-1:0] q_scale_tmp;
reg signed [ss-1:0] shift_q;
always @(*) begin
    if(BIAS_EN)begin
        mult = acc_bias_value;
        mult_scale = bias_ex;
    end
    else begin
        mult = mult_reg;
        mult_scale = mult_scale_reg;
    end
    if(mult_scale >= quire_bias) begin
        add_value = mult <<< (quire_bias-1);
        add_scale = mult_scale - quire_bias;
    end
    else begin
        add_value = mult <<< mult_scale;
        add_scale = 0;  
    end
    if(q_scale_reg >= add_scale)begin
        scale_diff  = q_scale_reg - add_scale;
        big_value   = quire_reg;
        big_scale   = q_scale_reg;
        small_value = add_value[qsize+mult_bw-2:mult_bw-1];
        small_scale = add_scale;
    end
    else begin
        scale_diff  = add_scale - q_scale_reg;
        big_value   = add_value[qsize+mult_bw-2:mult_bw-1];
        big_scale   = add_scale;
        small_value = quire_reg;
        small_scale = q_scale_reg;
    end
    quire   = big_value + (small_value >>> scale_diff);
    q_scale = big_scale;
    if(quire_reg[qsize-1:qsize-2] == 2'b01 && quire[qsize-1:qsize-2] == 2'b10) begin //plus overflow
        quire = {1'b0,{(qsize - 1){1'b1}}};
    end
    if(quire_reg[qsize-1:qsize-2] == 2'b10 && quire[qsize-1:qsize-2] == 2'b01) begin //minus overflow
        quire = {1'b1,{(qsize - 1){1'b0}}};
    end
    if(q_scale != {(ss){1'b1}} && quire[qsize-1]^quire[qsize-2]) begin
        quire   = quire >>> 1;
        q_scale = q_scale + 1;
    end
end

//MAC_EN
reg acc_en;
always @(posedge CLK or negedge RESET)begin
    if(RESET == 1'b0)
        acc_en <= 0;
    else
        acc_en <= MAC_EN;
end
//quire
always@(posedge CLK or negedge RESET)begin
    if(RESET == 1'b0)
        quire_reg <= 0;
    else if(PURGE)
        quire_reg <= 0;
    else if(acc_en | BIAS_EN)
        quire_reg <= quire;
    else
        quire_reg <= quire_reg;
end
//quire_scale
always@(posedge CLK or negedge RESET)begin
    if(RESET == 1'b0)
        q_scale_reg <= 0;
    else if(PURGE)
        q_scale_reg <= 0;
    else if(acc_en | BIAS_EN)
        q_scale_reg <= q_scale;
    else
        q_scale_reg <= q_scale_reg;
end

//encode
wire [qsize-1:0] quire_masked = RESULT_REQ_PLS ? quire_reg : 0;
wire [qsize-1:0] quire_abs = quire_masked[qsize-1] ? -quire_masked : quire_masked;
wire [q_s1-1:0] val_quire;
LOD_N #(.N(qsize)) valid_quire(.in(quire_abs), .out(val_quire));

wire [qsize-1:0] quire_frac_s;
DLS #(.N(qsize), .S(q_s1), .O(qsize)) get_qfrac (.a(quire_abs), .b(val_quire), .c(quire_frac_s));

wire signed [Bs+1:0] quire_exp = q_scale_reg - (val_quire + 1) + qsize - bias;

wire [Bs:0] r_o;
reg_exp_op #(.Bs(Bs), .N(N)) e_r_out(quire_exp, r_o);

wire [2*N-1+3:0] tmp_o = {{N{~quire_exp[Bs+1]}},quire_exp[Bs+1],quire_frac_s[qsize-2:(qsize-2)-((N-1)+1)],|quire_frac_s[((qsize-2)-((N-1)+1))-1:0]};

wire [3*N-1+3:0] tmp1_o;
DRS #(.N(3*N+3), .S(Bs+1), .O(3*N+3)) drs (.a({tmp_o,{N{1'b0}}}), .b(r_o), .c(tmp1_o));

wire L = tmp1_o[N+4], G = tmp1_o[N+3], R = tmp1_o[N+2], St = |tmp1_o[N+1:0],
     ulp = ((G & (R | St)) | (L & G & ~(R | St)));
wire [N-1:0] rnd_ulp = {{N-1{1'b0}},ulp};

wire [N:0] tmp1_o_rnd_ulp;
add_N #(.N(N)) uut_add_ulp (tmp1_o[2*N-1+3:N+3], rnd_ulp, tmp1_o_rnd_ulp);
wire [N-1:0] tmp1_o_rnd = (r_o < N -1) ? tmp1_o_rnd_ulp[N-1:0] : tmp1_o[2*N-1+3:N+3];

wire [N-2:0] tmp1_oN = quire_masked[qsize-1] ? -tmp1_o_rnd[N-1:1] : tmp1_o_rnd[N-1:1];

wire [N-1:0] OUT_tmp_p;
wire [N-1:0] OUT_tmp_n;
assign OUT_tmp_p = (|tmp1_oN) ? {quire_masked[qsize-1], tmp1_oN} : {quire_masked[qsize-1],{(N-2){1'b0}},1'b1};
assign OUT_tmp_n = (|tmp1_oN) ? {quire_masked[qsize-1], tmp1_oN} : {N{1'b1}};
wire [N-1:0] OUT_tmp;
assign OUT = (|quire_reg | q_scale_reg) ? (quire_masked[qsize-1] ? OUT_tmp_n : OUT_tmp_p) : 0;

endmodule

///////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////
module data_extract(in, rc, regime, mant);

function [31:0] log2;
input reg [31:0] value;
	begin
	value = value-1;
	for (log2=0; value>0; log2=log2+1)
        	value = value>>1;
      	end
endfunction

parameter N=8;
parameter Bs=log2(N);
input [N-1:0] in;
output rc;
output [Bs-1:0] regime;
output [N-4:0] mant;

wire [N-1:0] xin = in;
assign rc = xin[N-2];

wire [N-1:0] xin_r = rc ? ~xin : xin;

wire [Bs-1:0] k;
LOD_N #(.N(N)) xinst_k(.in({xin_r[N-2:0],rc^1'b0}), .out(k));

assign regime = rc ? k-1 : k;

wire [N-1:0] xin_tmp;
DLS #(.N(N), .S(Bs), .O(N)) ls (.a({xin[N-3:0],2'b0}),.b(k),.c(xin_tmp));

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
input signed [Bs+1:0] a,b;
input cin;
output [Bs+1:0] c;
assign c = a + b + cin + bias;
endmodule

module reg_exp_op (exp_o, r_o);
parameter Bs=3;
parameter N = 8;
input [Bs+1:0] exp_o;
output [Bs:0] r_o;

wire [Bs:0] r_o_tmp;
assign r_o_tmp = exp_o[Bs+1] ? (~exp_o[Bs:0]) + 1 : exp_o[Bs:0] + 1;
wire [Bs:0] r_o;
assign r_o = (r_o_tmp > (N-1)) ? N-1 : r_o_tmp;
endmodule

module conv_2c (a,c);
parameter N=10;
input [N:0] a;
output [N:0] c;
assign c = a + 1'b1;
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
        parameter O=8;
        input [N-1:0] a;
        input [S-1:0] b;
        output [O-1:0] c;

wire [O-1:0] tmp [S-1:0];
assign tmp[0]  = b[0] ? a >> 7'd1  : a; 
genvar i;
generate
	for (i=1; i<S; i=i+1)begin:loop_blk
		assign tmp[i] = b[i] ? tmp[i-1] >> 2**i : tmp[i-1];
	end
endgenerate
assign c = tmp[S-1];

endmodule

module DRS_S(a,b,c);
parameter N = 8;
parameter S = 3;
parameter O = 8;
input [N-1:0] a;
input [S-1:0] b;
output [O-1:0] c;
wire signed [N-1:0] a2;
wire signed [S-1:0] b2;
wire signed [O-1:0] c2;
assign a2 = a;
assign b2 = b;
assign c2 = a2 >>> b2;
assign c = c2;
endmodule