/* AUTOMATICALLY GENERATED VERILOG-2001 SOURCE CODE.
** GENERATED BY CLASH 1.6.4. DO NOT MODIFY.
*/
`timescale 100fs/100fs
module xasted
    ( // Inputs
      input  MAX10_CLK1_50 // clock
    , input [1:0] KEY

      // Outputs
    , output wire [7:0] HEX0
    , output wire [7:0] HEX1
    , output wire [7:0] HEX2
    , output wire [7:0] HEX3
    , output wire [7:0] HEX4
    , output wire [7:0] HEX5
    , output wire [9:0] LEDR
    );
  wire [9:0] result_0;
  wire [1:0] lives;
  wire [9:0] c$case_alt;
  reg [9:0] c$app_arg;
  wire [0:0] c$app_arg_0;
  wire [0:0] c$app_arg_1;
  wire [0:0] c$app_arg_2;
  wire [7:0] result_1;
  wire [7:0] result_2;
  wire [3:0] c$case_alt_0;
  wire [3:0] c$case_alt_1;
  wire  c$app_arg_3;
  reg [2:0] c$ds8_app_arg = 3'd0;
  wire [7:0] result_3;
  wire [2:0] bh;
  wire  bv;
  wire [4:0] ah;
  wire  av;
  wire  ahide;
  wire [7:0] c$case_alt_2;
  wire [7:0] c$case_alt_3;
  wire [7:0] c$case_alt_4;
  wire [7:0] c$app_arg_4;
  wire [7:0] c$case_alt_5;
  wire [7:0] result_4;
  wire [2:0] bh_0;
  wire  bv_0;
  wire [4:0] ah_0;
  wire  av_0;
  wire  ahide_0;
  wire [7:0] c$case_alt_6;
  wire [7:0] c$case_alt_7;
  wire [7:0] c$case_alt_8;
  wire [7:0] c$app_arg_5;
  wire [7:0] c$case_alt_9;
  wire [7:0] result_5;
  wire [2:0] bh_1;
  wire  bv_1;
  wire [4:0] ah_1;
  wire  av_1;
  wire  ahide_1;
  wire [7:0] c$case_alt_10;
  wire [7:0] c$case_alt_11;
  wire [7:0] c$case_alt_12;
  wire [7:0] c$app_arg_6;
  wire [7:0] c$case_alt_13;
  wire [7:0] result_6;
  wire [2:0] bh_2;
  wire  bv_2;
  wire [4:0] ah_2;
  wire  av_2;
  wire  ahide_2;
  wire [7:0] c$case_alt_14;
  wire [7:0] c$case_alt_15;
  wire [7:0] c$case_alt_16;
  wire [7:0] c$app_arg_7;
  wire [7:0] c$case_alt_17;
  wire [7:0] result_7;
  wire [2:0] bh_3;
  wire  bv_3;
  wire [4:0] ah_3;
  wire  av_3;
  wire  ahide_3;
  wire [7:0] c$case_alt_18;
  wire [7:0] c$case_alt_19;
  wire [7:0] c$case_alt_20;
  wire [7:0] c$app_arg_8;
  wire [7:0] c$case_alt_21;
  wire [10:0] positionsH;
  wire [0:0] bv_4;
  wire  result_8;
  wire [25:0] result_9;
  reg [24:0] maxVal;
  wire [1:0] level;
  wire [1:0] ds;
  wire [5:0] c$case_alt_22;
  wire  reset;
  wire [5:0] c$case_alt_23;
  wire  shot;
  wire [1:0] result_10;
  wire [1:0] c$case_alt_24;
  wire [1:0] c$case_alt_25;
  wire [3:0] c$app_arg_9;
  wire [1:0] c$app_arg_10;
  wire [1:0] c$case_alt_26;
  wire [1:0] c$case_alt_27;
  wire  c$app_arg_11;
  wire  c$ds6_app_arg;
  reg  old = 1'b0;
  wire  result_11;
  wire  c$shot_app_arg;
  reg  old_0 = 1'b0;
  wire  c$s_case_alt;
  wire  asteroidVPos;
  wire  bulletVPos;
  reg [2:0] c$ds_app_arg = 3'd0;
  wire [2:0] bulletHPos;
  wire [3:0] ds1;
  wire [7:0] c$case_alt_28;
  wire  remove;
  wire [7:0] c$case_alt_29;
  wire [7:0] c$case_alt_30;
  wire [7:0] c$case_alt_31;
  wire  move;
  wire [7:0] c$case_alt_32;
  wire  v;
  wire [7:0] c$case_alt_33;
  wire  fireTop;
  wire  fireBottom;
  wire  c$app_arg_12;
  wire [2:0] h;
  reg  c$ds8_app_arg_0 = 1'b0;
  reg  playingP = 1'b0;
  wire  playing;
  wire [1:0] ds1_0;
  wire [5:0] c$case_alt_34;
  wire [5:0] c$case_alt_35;
  wire  restart;
  wire [5:0] c$case_alt_36;
  wire  shipHit;
  wire [1:0] lives_0;
  wire  c$app_arg_13;
  wire  playing_0;
  wire  hitShip;
  wire [8:0] ds1_1;
  wire [12:0] c$case_alt_37;
  wire  reset_0;
  wire [12:0] c$case_alt_38;
  wire  iv;
  wire [4:0] ih;
  wire [12:0] c$case_alt_39;
  wire  move_0;
  wire  c$app_arg_14;
  wire [0:0] c$app_arg_15;
  wire  flipV;
  wire [4:0] c$app_arg_16;
  wire  v_0;
  wire [4:0] h_0;
  wire [15:0] c$ds8_app_arg_1;
  wire [15:0] rand;
  wire [15:0] counter;
  wire [15:0] randomValue;
  wire [15:0] \randomValue' ;
  wire [47:0] result_12;
  reg [31:0] c$ds9_app_arg = {16'b0000000000000000,   16'b0000000000000000};
  wire [5:0] ds_0;
  wire [2:0] c$case_alt_40;
  wire [2:0] c$case_alt_41;
  wire [2:0] c$case_alt_42;
  wire [2:0] c$case_alt_43;
  wire  c$app_arg_17;
  wire [2:0] pos;
  reg  c$app_arg_18;
  wire  tick;
  reg  c$app_arg_19;
  reg  c$app_arg_20;
  wire [1:0] level_0;
  wire [4:0] asteroidHPos;
  reg [1:0] c$ds7_app_arg = 2'd0;
  reg  c$ds8_app_arg_2 = 1'b0;
  reg  c$ds8_app_arg_3 = 1'b0;
  wire  c$ds8_app_arg_4;
  reg  old_1 = 1'b0;
  wire  asteroidTick;
  wire [4:0] flashCt;
  wire [4:0] \flashCount' ;
  wire [9:0] result_13;
  wire [4:0] \c$flashCount'_case_alt ;
  wire  flashTick;
  reg [4:0] c$ds4_app_arg = 5'd8;
  reg [5:0] c$ds8_app_arg_5 = {5'd9,   1'b0};
  reg [2:0] c$ds8_app_arg_6 = {1'b0,   2'd0};
  reg [24:0] result_14;
  reg [23:0] ds_1 = 24'd0;
  reg [3:0] c$ds8_app_arg_7 = {3'd0,   1'b0};
  reg [3:0] c$ds6_app_arg_0 = 4'd0;
  reg [24:0] count = 25'd0;
  reg [21:0] result_15;
  reg [20:0] ds_2 = 21'd0;
  wire  result_16;
  wire  c$fireTop_app_arg;
  reg  old_2 = 1'b0;
  wire  press0;
  wire  result_17;
  wire  c$fireBottom_app_arg;
  reg  old_3 = 1'b0;
  wire  press1;
  wire  c$old_app_arg;
  wire [2:0] c$bv;
  wire [4:0] c$bv_0;
  wire signed [63:0] c$i_148;
  wire [5:0] c$bv_1;
  wire [57:0] result;

  assign result = {result_7,   result_6,
                   result_5,   result_4,   result_3,   result_1,
                   result_0};

  assign result_0 = playing ? c$case_alt : 10'b0100000000;

  assign lives = c$case_alt_34[1:0];

  assign c$case_alt = ({7'b0000000,(({(c$app_arg_2),(({(c$app_arg_1),(c$app_arg_0)}))}))}) | c$app_arg;

  always @(*) begin
    case(level)
      2'b00 : c$app_arg = 10'b0000010000;
      2'b01 : c$app_arg = 10'b0000100000;
      2'b10 : c$app_arg = 10'b0000110000;
      default : c$app_arg = 10'b0001000000;
    endcase
  end

  assign c$app_arg_0 = (lives > 2'd0) ? 1'b1 : 1'b0;

  assign c$app_arg_1 = (lives > 2'd1) ? 1'b1 : 1'b0;

  assign c$app_arg_2 = (lives > 2'd2) ? 1'b1 : 1'b0;

  assign result_1 = playing ? result_2 : 8'b10110110;

  assign result_2 = c$case_alt_0[0:0] ? 8'b11111111 : 8'b11000110;

  assign c$case_alt_0 = hitShip ? {3'd7,
                                   c$app_arg_3} : c$case_alt_1;

  assign c$case_alt_1 = (flashTick & (c$ds8_app_arg > 3'd0)) ? {c$ds8_app_arg - 3'd1,
                                                                c$app_arg_3} : {c$ds8_app_arg,   c$app_arg_3};

  assign c$bv = (c$ds8_app_arg);

  assign c$app_arg_3 = (c$bv[64'sd0]) == (1'b1);

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds8_app_arg_register
    if ( c$old_app_arg) begin
      c$ds8_app_arg <= 3'd0;
    end else begin
      c$ds8_app_arg <= c$case_alt_0[3:1];
    end
  end
  // register end

  assign result_3 = playing ? c$case_alt_2 : 8'b10001000;

  assign bh = positionsH[10:8];

  assign bv = positionsH[7:7];

  assign ah = positionsH[6:2];

  assign av = positionsH[1:1];

  assign ahide = positionsH[0:0];

  assign c$case_alt_2 = c$app_arg_4 & c$case_alt_3;

  assign c$case_alt_3 = ((~ ahide) & (5'd1 == ah)) ? c$case_alt_4 : 8'b11111111;

  assign c$case_alt_4 = (av == 1'b0) ? 8'b10011100 : 8'b10100011;

  assign c$app_arg_4 = (3'd1 == bh) ? c$case_alt_5 : 8'b11111111;

  assign c$case_alt_5 = (bv == 1'b0) ? 8'b11111110 : 8'b11110111;

  assign result_4 = playing ? c$case_alt_6 : 8'b10010010;

  assign bh_0 = positionsH[10:8];

  assign bv_0 = positionsH[7:7];

  assign ah_0 = positionsH[6:2];

  assign av_0 = positionsH[1:1];

  assign ahide_0 = positionsH[0:0];

  assign c$case_alt_6 = c$app_arg_5 & c$case_alt_7;

  assign c$case_alt_7 = ((~ ahide_0) & (5'd2 == ah_0)) ? c$case_alt_8 : 8'b11111111;

  assign c$case_alt_8 = (av_0 == 1'b0) ? 8'b10011100 : 8'b10100011;

  assign c$app_arg_5 = (3'd2 == bh_0) ? c$case_alt_9 : 8'b11111111;

  assign c$case_alt_9 = (bv_0 == 1'b0) ? 8'b11111110 : 8'b11110111;

  assign result_5 = playing ? c$case_alt_10 : 8'b10000111;

  assign bh_1 = positionsH[10:8];

  assign bv_1 = positionsH[7:7];

  assign ah_1 = positionsH[6:2];

  assign av_1 = positionsH[1:1];

  assign ahide_1 = positionsH[0:0];

  assign c$case_alt_10 = c$app_arg_6 & c$case_alt_11;

  assign c$case_alt_11 = ((~ ahide_1) & (5'd3 == ah_1)) ? c$case_alt_12 : 8'b11111111;

  assign c$case_alt_12 = (av_1 == 1'b0) ? 8'b10011100 : 8'b10100011;

  assign c$app_arg_6 = (3'd3 == bh_1) ? c$case_alt_13 : 8'b11111111;

  assign c$case_alt_13 = (bv_1 == 1'b0) ? 8'b11111110 : 8'b11110111;

  assign result_6 = playing ? c$case_alt_14 : 8'b10000110;

  assign bh_2 = positionsH[10:8];

  assign bv_2 = positionsH[7:7];

  assign ah_2 = positionsH[6:2];

  assign av_2 = positionsH[1:1];

  assign ahide_2 = positionsH[0:0];

  assign c$case_alt_14 = c$app_arg_7 & c$case_alt_15;

  assign c$case_alt_15 = ((~ ahide_2) & (5'd4 == ah_2)) ? c$case_alt_16 : 8'b11111111;

  assign c$case_alt_16 = (av_2 == 1'b0) ? 8'b10011100 : 8'b10100011;

  assign c$app_arg_7 = (3'd4 == bh_2) ? c$case_alt_17 : 8'b11111111;

  assign c$case_alt_17 = (bv_2 == 1'b0) ? 8'b11111110 : 8'b11110111;

  assign result_7 = playing ? c$case_alt_18 : 8'b10100001;

  assign bh_3 = positionsH[10:8];

  assign bv_3 = positionsH[7:7];

  assign ah_3 = positionsH[6:2];

  assign av_3 = positionsH[1:1];

  assign ahide_3 = positionsH[0:0];

  assign c$case_alt_18 = c$app_arg_8 & c$case_alt_19;

  assign c$case_alt_19 = ((~ ahide_3) & (5'd5 == ah_3)) ? c$case_alt_20 : 8'b11111111;

  assign c$case_alt_20 = (av_3 == 1'b0) ? 8'b10011100 : 8'b10100011;

  assign c$app_arg_8 = (3'd5 == bh_3) ? c$case_alt_21 : 8'b11111111;

  assign c$case_alt_21 = (bv_3 == 1'b0) ? 8'b11111110 : 8'b11110111;

  assign positionsH = {bulletHPos,   bulletVPos,
                       asteroidHPos,   asteroidVPos,   result_8};

  assign c$bv_0 = ((flashCt));

  assign bv_4 = (( c$bv_0[0] ));

  assign result_8 = bv_4 == 1'b1;

  assign result_9 = (count >= maxVal) ? {25'd0,
                                         1'b1} : {count + 25'd1,   1'b0};

  always @(*) begin
    case(level)
      2'b00 : maxVal = 25'd29999999;
      2'b01 : maxVal = 25'd14999999;
      2'b10 : maxVal = 25'd7499999;
      default : maxVal = 25'd3749999;
    endcase
  end

  assign level = c$case_alt_22[1:0];

  assign ds = {result_11,
               c$ds6_app_arg & playing};

  assign c$case_alt_22 = reset ? {4'd0,
                                  2'd0} : c$case_alt_23;

  assign reset = ds[0:0];

  assign c$case_alt_23 = (shot & c$app_arg_11) ? {c$app_arg_9,
                                                  result_10} : {c$ds6_app_arg_0,   c$app_arg_10};

  assign shot = ds[1:1];

  assign result_10 = (c$app_arg_9 < 4'd5) ? 2'd0 : c$case_alt_24;

  assign c$case_alt_24 = (c$app_arg_9 < 4'd10) ? 2'd1 : c$case_alt_25;

  assign c$case_alt_25 = (c$app_arg_9 < 4'd15) ? 2'd2 : 2'd3;

  assign c$app_arg_9 = c$ds6_app_arg_0 + 4'd1;

  assign c$app_arg_10 = (c$ds6_app_arg_0 < 4'd5) ? 2'd0 : c$case_alt_26;

  assign c$case_alt_26 = (c$ds6_app_arg_0 < 4'd10) ? 2'd1 : c$case_alt_27;

  assign c$case_alt_27 = c$app_arg_11 ? 2'd2 : 2'd3;

  assign c$app_arg_11 = c$ds6_app_arg_0 < 4'd15;

  assign c$ds6_app_arg = old ? 1'b0 : 1'b1;

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : old_register
    if ( c$old_app_arg) begin
      old <= 1'b0;
    end else begin
      old <= playing;
    end
  end
  // register end

  assign result_11 = c$shot_app_arg & c$s_case_alt;

  assign c$shot_app_arg = old_0 ? 1'b0 : 1'b1;

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : old_0_register
    if ( c$old_app_arg) begin
      old_0 <= 1'b0;
    end else begin
      old_0 <= c$s_case_alt;
    end
  end
  // register end

  assign c$s_case_alt = (c$ds_app_arg > 3'd0) & ((asteroidHPos == ({{(5-3) {1'b0}},c$ds_app_arg})) & (asteroidVPos == bulletVPos));

  assign asteroidVPos = c$case_alt_37[1:1];

  assign bulletVPos = c$case_alt_28[0:0];

  // delay begin
  always @(posedge MAX10_CLK1_50) begin : c$ds_app_arg_delay
    c$ds_app_arg <= bulletHPos;
  end
  // delay end

  assign bulletHPos = c$case_alt_28[3:1];

  assign ds1 = {result_14[0:0] & playingP,
                c$ds8_app_arg_0,   result_16,   result_17};

  assign c$case_alt_28 = remove ? {{3'd0,   v},
                                   {3'd0,   v}} : c$case_alt_29;

  assign remove = ds1[2:2];

  assign c$case_alt_29 = (move & c$app_arg_12) ? {{3'd0,
                                                   v},   {3'd0,   v}} : c$case_alt_30;

  assign c$case_alt_30 = (move & (h == 3'd5)) ? {{3'd0,
                                                  v},   {3'd0,   v}} : c$case_alt_31;

  assign c$case_alt_31 = move ? {{h + 3'd1,   v},
                                 {h + 3'd1,   v}} : c$case_alt_32;

  assign move = ds1[3:3];

  assign c$case_alt_32 = (fireTop & c$app_arg_12) ? {{3'd1,
                                                      1'b0},   {3'd1,   1'b0}} : c$case_alt_33;

  assign v = c$ds8_app_arg_7[0:0];

  assign c$case_alt_33 = (fireBottom & c$app_arg_12) ? {{3'd1,
                                                         1'b1},   {3'd1,   1'b1}} : {c$ds8_app_arg_7,
                                                                                     c$ds8_app_arg_7};

  assign fireTop = ds1[1:1];

  assign fireBottom = ds1[0:0];

  assign c$app_arg_12 = h == 3'd0;

  assign h = c$ds8_app_arg_7[3:1];

  // delay begin
  always @(posedge MAX10_CLK1_50) begin : c$ds8_app_arg_0_delay
    c$ds8_app_arg_0 <= result_11;
  end
  // delay end

  // delay begin
  always @(posedge MAX10_CLK1_50) begin : playingP_delay
    playingP <= playing;
  end
  // delay end

  assign playing = c$case_alt_34[2:2];

  assign ds1_0 = {hitShip,
                  result_16 | result_17};

  assign c$case_alt_34 = (restart & c$app_arg_13) ? {{1'b1,
                                                      2'd3},   {1'b1,   2'd3}} : c$case_alt_35;

  assign c$case_alt_35 = (c$app_arg_13 | (lives_0 == 2'd0)) ? {{1'b0,
                                                                2'd0},   {1'b0,   2'd0}} : c$case_alt_36;

  assign restart = ds1_0[0:0];

  assign c$case_alt_36 = shipHit ? {{1'b1,
                                     lives_0 - 2'd1},   {1'b1,
                                                         lives_0 - 2'd1}} : {c$ds8_app_arg_6,
                                                                             c$ds8_app_arg_6};

  assign shipHit = ds1_0[1:1];

  assign lives_0 = c$ds8_app_arg_6[1:0];

  assign c$app_arg_13 = ~ playing_0;

  assign playing_0 = c$ds8_app_arg_6[2:2];

  assign hitShip = c$case_alt_37[0:0];

  assign ds1_1 = {asteroidTick & (playingP & (~ (flashCt > 5'd0))),
                  (c$ds8_app_arg_4 & playingP) | (c$ds8_app_arg_3 | c$ds8_app_arg_2),
                  c$case_alt_40[0:0],
                  (({2'b00,(c$ds8_app_arg_1[3 : 1])})) + 5'd5,
                   c$ds8_app_arg_1[0] };

  assign c$case_alt_37 = reset_0 ? {{ih,   iv},
                                    {ih,   iv,   1'b0}} : c$case_alt_38;

  assign reset_0 = ds1_1[7:7];

  assign c$case_alt_38 = ((h_0 == 5'd0) & move_0) ? {{5'd0,
                                                      v_0},   {5'd0,   v_0,   1'b0}} : c$case_alt_39;

  assign iv = ds1_1[0:0];

  assign ih = ds1_1[5:1];

  assign c$case_alt_39 = move_0 ? {{c$app_arg_16,
                                    c$app_arg_14},   {c$app_arg_16,
                                                      c$app_arg_14,
                                                      h_0 == 5'd1}} : {c$ds8_app_arg_5,   {h_0,
                                                                                           v_0,   1'b0}};

  assign move_0 = ds1_1[8:8];

  assign c$app_arg_14 = v_0 ^ ((c$app_arg_15));

  assign c$app_arg_15 = flipV ? 1'b1 : 1'b0;

  assign flipV = ds1_1[6:6];

  assign c$app_arg_16 = h_0 - 5'd1;

  assign v_0 = c$ds8_app_arg_5[0:0];

  assign h_0 = c$ds8_app_arg_5[5:1];

  assign c$ds8_app_arg_1 = rand;

  assign rand = result_12[15:0];

  assign counter = c$ds9_app_arg[31:16];

  assign randomValue = c$ds9_app_arg[15:0];

  assign \randomValue'  = (result_16 | result_17) ? (randomValue ^ counter) : randomValue;

  assign result_12 = {{counter + 16'b0000000000000001,
                       \randomValue' },   \randomValue' };

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds9_app_arg_register
    if ( c$old_app_arg) begin
      c$ds9_app_arg <= {16'b0000000000000000,   16'b0000000000000000};
    end else begin
      c$ds9_app_arg <= result_12[47:16];
    end
  end
  // register end

  assign c$i_148 = ($unsigned({{(64-5) {1'b0}},asteroidHPos}));

  assign ds_0 = {level,
                 $unsigned(c$i_148[0+:3]),
                 asteroidTick & (asteroidHPos <= 5'd5)};

  assign c$case_alt_40 = (c$app_arg_20 & tick) ? {c$ds7_app_arg + 2'd1,
                                                  c$app_arg_17} : c$case_alt_41;

  assign c$case_alt_41 = (c$app_arg_19 & (tick & (c$ds7_app_arg == 2'd2))) ? {2'd0,
                                                                              c$app_arg_17} : c$case_alt_42;

  assign c$case_alt_42 = (c$app_arg_19 & tick) ? {c$ds7_app_arg + 2'd1,
                                                  c$app_arg_17} : c$case_alt_43;

  assign c$case_alt_43 = (c$app_arg_18 & tick) ? {c$ds7_app_arg + 2'd1,
                                                  c$app_arg_17} : {c$ds7_app_arg,   1'b0};

  assign c$bv_1 = 6'b101010;

  assign c$app_arg_17 = (c$bv_1[($unsigned({{(64-3) {1'b0}},pos}))]) == (1'b1);

  assign pos = ds_0[3:1];

  always @(*) begin
    case(level_0)
      2'b11 : c$app_arg_18 = 1'b1;
      default : c$app_arg_18 = 1'b0;
    endcase
  end

  assign tick = ds_0[0:0];

  always @(*) begin
    case(level_0)
      2'b10 : c$app_arg_19 = 1'b1;
      default : c$app_arg_19 = 1'b0;
    endcase
  end

  always @(*) begin
    case(level_0)
      2'b01 : c$app_arg_20 = 1'b1;
      default : c$app_arg_20 = 1'b0;
    endcase
  end

  assign level_0 = ds_0[5:4];

  assign asteroidHPos = c$case_alt_37[6:2];

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds7_app_arg_register
    if ( c$old_app_arg) begin
      c$ds7_app_arg <= 2'd0;
    end else begin
      c$ds7_app_arg <= c$case_alt_40[2:1];
    end
  end
  // register end

  // delay begin
  always @(posedge MAX10_CLK1_50) begin : c$ds8_app_arg_2_delay
    c$ds8_app_arg_2 <= (flashCt == 5'd1);
  end
  // delay end

  // delay begin
  always @(posedge MAX10_CLK1_50) begin : c$ds8_app_arg_3_delay
    c$ds8_app_arg_3 <= hitShip;
  end
  // delay end

  assign c$ds8_app_arg_4 = old_1 ? 1'b0 : 1'b1;

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : old_1_register
    if ( c$old_app_arg) begin
      old_1 <= 1'b0;
    end else begin
      old_1 <= playingP;
    end
  end
  // register end

  assign asteroidTick = result_9[0:0];

  assign flashCt = result_13[4:0];

  assign \flashCount'  = result_11 ? 5'd29 : \c$flashCount'_case_alt ;

  assign result_13 = {\flashCount' ,
                      \flashCount' };

  assign \c$flashCount'_case_alt  = ((~ flashTick) | (c$ds4_app_arg == 5'd0)) ? c$ds4_app_arg : (c$ds4_app_arg - 5'd1);

  assign flashTick = result_15[0:0];

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds4_app_arg_register
    if ( c$old_app_arg) begin
      c$ds4_app_arg <= 5'd8;
    end else begin
      c$ds4_app_arg <= result_13[9:5];
    end
  end
  // register end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds8_app_arg_5_register
    if ( c$old_app_arg) begin
      c$ds8_app_arg_5 <= {5'd9,   1'b0};
    end else begin
      c$ds8_app_arg_5 <= c$case_alt_37[12:7];
    end
  end
  // register end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds8_app_arg_6_register
    if ( c$old_app_arg) begin
      c$ds8_app_arg_6 <= {1'b0,   2'd0};
    end else begin
      c$ds8_app_arg_6 <= c$case_alt_34[5:3];
    end
  end
  // register end

  always @(*) begin
    case(ds_1)
      24'd0 : result_14 = {24'd9999999,   1'b1};
      default : result_14 = {ds_1 - 24'd1,   1'b0};
    endcase
  end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : ds_1_register
    if ( c$old_app_arg) begin
      ds_1 <= 24'd0;
    end else begin
      ds_1 <= result_14[24:1];
    end
  end
  // register end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds8_app_arg_7_register
    if ( c$old_app_arg) begin
      c$ds8_app_arg_7 <= {3'd0,   1'b0};
    end else begin
      c$ds8_app_arg_7 <= c$case_alt_28[7:4];
    end
  end
  // register end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : c$ds6_app_arg_0_register
    if ( c$old_app_arg) begin
      c$ds6_app_arg_0 <= 4'd0;
    end else begin
      c$ds6_app_arg_0 <= c$case_alt_22[5:2];
    end
  end
  // register end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : count_register
    if ( c$old_app_arg) begin
      count <= 25'd0;
    end else begin
      count <= result_9[25:1];
    end
  end
  // register end

  always @(*) begin
    case(ds_2)
      21'd0 : result_15 = {21'd1999999,   1'b1};
      default : result_15 = {ds_2 - 21'd1,   1'b0};
    endcase
  end

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : ds_2_register
    if ( c$old_app_arg) begin
      ds_2 <= 21'd0;
    end else begin
      ds_2 <= result_15[21:1];
    end
  end
  // register end

  assign result_16 = c$fireTop_app_arg & press0;

  assign c$fireTop_app_arg = old_2 ? 1'b0 : 1'b1;

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : old_2_register
    if ( c$old_app_arg) begin
      old_2 <= 1'b0;
    end else begin
      old_2 <= press0;
    end
  end
  // register end

  assign press0 = ~ ((KEY[64'sd0]) == (1'b1));

  assign result_17 = c$fireBottom_app_arg & press1;

  assign c$fireBottom_app_arg = old_3 ? 1'b0 : 1'b1;

  // register begin
  always @(posedge MAX10_CLK1_50 or  posedge  c$old_app_arg) begin : old_3_register
    if ( c$old_app_arg) begin
      old_3 <= 1'b0;
    end else begin
      old_3 <= press1;
    end
  end
  // register end

  assign press1 = ~ ((KEY[64'sd1]) == (1'b1));

  assign c$old_app_arg = 1'b0;

  assign HEX0 = result[57:50];

  assign HEX1 = result[49:42];

  assign HEX2 = result[41:34];

  assign HEX3 = result[33:26];

  assign HEX4 = result[25:18];

  assign HEX5 = result[17:10];

  assign LEDR = result[9:0];


endmodule
