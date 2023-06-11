// Xasted - A 6 x 7 segment shooter game
// game play:
// use key buttons to fire upper or lower guns to destroy
// asteroids before they hit your ship.

`define MERGE_BULLET_BOMB(pos, letter) \
   game_over > 0 ? letter : ( \
   (bomb_pos == pos ? bomb_sym : blank_sym) & \
   (bullet_pos == pos ? fire_sym : blank_sym))
// XASTED letters
`define CODE_A 8'b1000_1000
`define CODE_D 8'b1010_0001
`define CODE_E 8'b1000_0110
`define CODE_S 8'b1001_0010
`define CODE_T 8'b1000_0111
`define CODE_X 8'b1011_0110 // greek Xi

module xasted(
  input                   MAX10_CLK1_50,
  output         [7:0]    HEX0,
  output         [7:0]    HEX1,
  output         [7:0]    HEX2,
  output         [7:0]    HEX3,
  output         [7:0]    HEX4,
  output         [7:0]    HEX5,
  input          [1:0]    KEY,
  output         [9:0]    LEDR,
  input          [9:0]    SW
);

// symbols
localparam fighter_sym = 8'b1100_0110;
localparam low_bomb_sym = 8'b1010_0011;
localparam high_bomb_sym = 8'b1001_1100;
localparam low_bullet_sym = 8'b1111_0111;
localparam high_bullet_sym = 8'b1111_1110;
localparam blank_sym = 8'b1111_1111;
localparam flash_count = 15;
localparam start_lives = 3;
localparam pressed_high_BIT = 1, pressed_low_BIT = 0;
localparam FLASH_CLK_BIT = 21;

//=======================================================
//  REG/WIRE declarations
//=======================================================

reg [2:0] bullet_pos;
reg [2:0] bomb_pos;
reg bullet_lh;
reg bomb_lh;
wire flash_clk;
reg [2:0] died;
reg [7:0] game_over = 1;
reg [2:0] bomb_shot = 0;
reg [5:0] next_bomb = 3;
reg [1:0] game_tick;
// latched events
reg pressed_high;
reg pressed_low;
reg [2:0] lives = 0;
wire [7:0] bomb_sym;
wire [7:0] fire_sym;
reg [FLASH_CLK_BIT:0] game_clk_cnt;


initial begin
  game_clk_cnt = 0;
  died = 0;
  game_over = 0;
  bomb_shot = 0;
end

//=======================================================
//  Structural coding
//=======================================================

always @ (posedge MAX10_CLK1_50) begin
  game_clk_cnt = game_clk_cnt + 1;
end

// move and dynamics
always @(posedge flash_clk) begin
  // remember key presses until next game clock
  // TODO pull these out as key presses (falling edge)
  if (pressed_high || pressed_low) begin
    // do nothing
  end else if (~KEY[0]) begin
    pressed_high = 1;
  end else if (~KEY[1]) begin
    pressed_low = 1;
  end
  if (flash_clk) begin
    game_tick = game_tick + 1;
    if (died > 0) died = died - 1;
    if (bomb_shot == 1) begin
      bomb_pos = 0;
      next_bomb = 5;
    end
    if (bomb_shot > 0) bomb_shot = bomb_shot - 1;
    // fired bullet
    if (game_over > 0) begin
     // not playing game so can't shoot
    end if (bullet_pos == 0 && pressed_low) begin
     bullet_pos = 1;
     bullet_lh = 0;
    end else if (bullet_pos == 0 && pressed_high) begin
      bullet_pos = 1;
      bullet_lh = 1;
    end
    
    if (game_over > 0) begin
      if (game_over > 1) begin
        game_over = game_over - 1;
      end else if (game_over == 1) begin
        if (pressed_low || pressed_high) begin
          game_over = 0;
          lives = start_lives;
          // give time for first bullet to clear screen
          next_bomb = 10;
        end
      end
    end else if (game_tick == 0) begin
      // move or remove fire
      if (bullet_pos > 5) begin
       bullet_pos = 0;
       bomb_lh = ~bomb_lh;
      end else if(bullet_pos > 0) begin
       bullet_pos = bullet_pos + 1;
       // check bullet hit bomb
       if (bullet_pos > 0 && bullet_pos == bomb_pos && bullet_lh == bomb_lh) begin
         bomb_shot = flash_count;
         bullet_pos = 0;
       end
      end
      // check if bomb hit fighter or move
      if (bomb_shot > 0) begin
      end else if (bomb_pos == 1) begin
        died = flash_count;
        lives = lives - 1;
        bomb_pos = 0;
        // TODO generate random number
        bomb_lh = ~bomb_lh;
        next_bomb = 5;
      end else if (bomb_pos > 0) begin
       bomb_pos = bomb_pos - 1;
       if (bullet_pos > 0 && bullet_pos == bomb_pos && bullet_lh == bomb_lh) begin
         bomb_shot = flash_count;
         bullet_pos = 0;
       end
      end
      // prepare next bomb
      if (bomb_pos == 0) begin
       next_bomb = next_bomb - 1;
       if (next_bomb == 0) begin
        bomb_pos = 5;
       end
      end
      if (lives == 0) begin
        game_over = 8;
      end
    end
    pressed_low = 0;
    pressed_high = 0;
  end
end

always @(negedge KEY[0], negedge KEY[1], posedge flash_clk) begin
end

assign flash_clk = game_clk_cnt[FLASH_CLK_BIT];
assign bomb_sym = bomb_shot > 0 && flash_clk ? blank_sym : 
  (bomb_lh ? high_bomb_sym : low_bomb_sym);
assign fire_sym = bullet_lh ? high_bullet_sym : low_bullet_sym;

assign LEDR[0] = lives > 0;
assign LEDR[1] = lives > 1;
assign LEDR[2] = lives > 2;
assign LEDR[9:3] = 0;
assign HEX5 = game_over > 0 ? `CODE_X : (died[0] ? blank_sym : fighter_sym);
assign HEX4 = `MERGE_BULLET_BOMB(1, `CODE_A);
assign HEX3 = `MERGE_BULLET_BOMB(2, `CODE_S);
assign HEX2 = `MERGE_BULLET_BOMB(3, `CODE_T);
assign HEX1 = `MERGE_BULLET_BOMB(4, `CODE_E);
assign HEX0 = `MERGE_BULLET_BOMB(5, `CODE_D);

endmodule
