`timescale 1ns / 1ns
module mac_f_tb;

reg [N-1:0] IN1, IN2, BIAS;
reg MAC_EN, PURGE, RESULT_REQ_PLS, BIAS_EN, CLK, RESET, REG_START, START;
wire [N-1:0] OUT;
parameter STEP = 10;

parameter N = 8; //bit width of posit
parameter es = 1; //es size(Don't choose zero.)
parameter qsize = 30; //quire size
parameter ext = 13; //ceil(log_2(k)), k: Number of MAC operations

posit_mac_f #(.N(N), .es(es), .qsize(qsize), .ext(ext)) posit_mac_f_test (IN1, IN2, BIAS, MAC_EN, PURGE, RESULT_REQ_PLS, BIAS_EN, CLK, RESET, OUT);

always begin
  CLK = ~CLK; #(STEP/2);
end

reg [N-1:0] data1 [0:1000];
reg [N-1:0] data2 [0:1000];
initial $readmemb("in1.txt",data1);
initial $readmemb("in2.txt",data2);

integer outfile;

reg [19:0] i;
initial begin
  $dumpfile("wave.vcd");
  $dumpvars(0,mac_f_tb);
  i = 0;
  CLK = 1;
  MAC_EN = 0;
  PURGE = 0;
  RESULT_REQ_PLS = 0;
  RESET = 0;
  REG_START = 0;
  BIAS_EN = 0;

  #(STEP + 1)
  RESET = 1;

  #STEP
  REG_START = 1;

  #STEP
  MAC_EN = 1;
  RESULT_REQ_PLS = 1;

  #(STEP*2)
  START = 1;


  #(STEP*1000)
  #(STEP*1)
  MAC_EN = 0;
 
  #(STEP*3)
  START = 0;
  #(STEP*20)
  $fclose(outfile); 
  $finish;
end

always @(negedge RESET, posedge CLK)begin
  if(!RESET) begin
    IN1 = 0;
    IN2 = 0;
  end
  else begin
    IN1 = data1[i];
    IN2 = data2[i];
    if(i == 20'h3EB)
      $finish;
    else if(REG_START) i = i + 1;
  end
end

initial outfile = $fopen("out.txt", "wb");

reg [N-1:0] result [0:1000];
initial $readmemb("testdata.txt",result);
always @(posedge CLK) begin
	if(START)begin
    $fwrite(outfile, "%b\n",OUT);
  end
end

endmodule