
module b58_cliky
	(
		CLOCK_50,						//	On Board 50 MHz
		// Your inputs and outputs here
    KEY,
    SW,
    HEX0, HEX1, HEX2, HEX3, HEX4, HEX5, HEX6, HEX7,
    LEDR,
		// The ports below are for the VGA output.  Do not change.
		VGA_CLK,   						//	VGA Clock
		VGA_HS,							//	VGA H_SYNC
		VGA_VS,							//	VGA V_SYNC
		VGA_BLANK_N,						//	VGA BLANK
		VGA_SYNC_N,						//	VGA SYNC
		VGA_R,   						//	VGA Red[9:0]
		VGA_G,	 						//	VGA Green[9:0]
		VGA_B   						//	VGA Blue[9:0]
	);

	input			CLOCK_50;				//	50 MHz
	input   [9:0]   SW;
	input   [3:0]   KEY;
    output	[2:0]	LEDR;
    output 	[6:0]   HEX0, HEX1, HEX2, HEX3, HEX4, HEX5, HEX6, HEX7;

	// Declare your inputs and outputs here
	// Do not change the following outputs
	output			VGA_CLK;   				//	VGA Clock
	output			VGA_HS;					//	VGA H_SYNC
	output			VGA_VS;					//	VGA V_SYNC
	output			VGA_BLANK_N;				//	VGA BLANK
	output			VGA_SYNC_N;				//	VGA SYNC
	output	[9:0]	VGA_R;   				//	VGA Red[9:0]
	output	[9:0]	VGA_G;	 				//	VGA Green[9:0]
	output	[9:0]	VGA_B;   				//	VGA Blue[9:0]
   wire resetn;
   assign resetn = SW[0];
	
	// Create the colour, x, y and writeEn wires that are inputs to the controller.
	wire [2:0] colour;
	wire [7:0] x;
        wire [7:0] y;
	wire writeEn;
	// Create an Instance of a VGA controller - there can be only one!
	// Define the number of colours as well as the initial background
	// image file (.MIF) for the controller.
	
	vga_adapter VGA(
			.resetn(resetn),
			.clock(CLOCK_50),
			.colour(colour),
			.x(x),
			.y(y),
			.plot(writeEn),
			/* Signals for the DAC to drive the monitor. */
			.VGA_R(VGA_R),
			.VGA_G(VGA_G),
			.VGA_B(VGA_B),
			.VGA_HS(VGA_HS),
			.VGA_VS(VGA_VS),
			.VGA_BLANK(VGA_BLANK_N),
			.VGA_SYNC(VGA_SYNC_N),
			.VGA_CLK(VGA_CLK));
		defparam VGA.RESOLUTION = "160x120";
		defparam VGA.MONOCHROME = "FALSE";
		defparam VGA.BITS_PER_COLOUR_CHANNEL = 1;
		defparam VGA.BACKGROUND_IMAGE = "black.mif";
  
   // ******************************************
   // WIRING
   // ****************************************** 
   wire [2:0] next_square, curr_square;
   wire [6:0] next_colour;
   wire [2:0] curr_colour;
   
   wire [3:0] next_mode, curr_mode;
   
   wire [15:0] score, high_score;
   wire [3:0] lives;
   
   wire times_up, have_pressed, correct_answer;
   
   wire [9:0] squares_drawn;
   wire [19:0] current_state, next_state;
   wire [4:0] pressed_key;
   
   // ******************************************
   // TOP MODULE INSTANTIATED
   // ****************************************** 
   top_clicky top(
   		.clk(CLOCK_50),
   		.restart(SW[9]),
   		.key1(~KEY[3]), .key2(~KEY[2]), .key3(~KEY[1]), .key4(~KEY[0]),
   	  //.key1(SW[7]), .key2(SW[6]), .key3(SW[5]), .key4(SW[4]),
   		.next_square(next_square), 
		.curr_square(curr_square),
   		.next_colour(next_colour), 
   		.curr_colour(curr_colour),
   		.next_mode(next_mode), 
   		.curr_mode(curr_mode),
   		.score(score),
  		.high_score(high_score),
  		.lives(lives),
  		.x(x), 
  		.y(y), // X,Y for PLOT
  		.colour(colour), // COLOUR for PLOT
  		.times_up(times_up), 
  		.have_pressed(have_pressed), 
  		.correct_answer(correct_answer),
  		.squares_drawn(squares_drawn),
   		.current_state(current_state), 
   		.next_state(next_state),
   		.pressed_key(pressed_key),
   		.plot(writeEn) // writeEn for PLOT
   		);

   // ******************************************
   // SCORE TO HEX
   // ****************************************** 
      hex_display score_1(
        .IN(score[3:0]),
        .OUT(HEX0)
      );
  
      hex_display score_2(
        .IN(score[7:4]),
        .OUT(HEX1)
      );

    // HIGH SCORE
  		
	hex_display high_score_1(
        .IN(high_score[3:0]),
        .OUT(HEX6)
      );
		
	hex_display high_score_2(
        .IN(high_score[7:4]),
        .OUT(HEX7)
      );

   // ******************************************
   // LIVES TO HEX
   // ****************************************** 
	
      hex_display lives_count(
        .IN(lives),
        .OUT(HEX4)
      );
		
	//HEX OTHERS
	assign HEX5 = 7'b1000111;
	assign HEX3 = 7'b0010010;
	assign HEX2 = 7'b1000110;
    
endmodule

// ******************************************
// HEX DISPLAY
// ****************************************** 
module hex_display(IN, OUT);
   input [3:0] IN;
	 output reg [7:0] OUT;
	 
	 always @(*)
	 begin
		case(IN[3:0])
			4'b0000: OUT = 7'b1000000;
			4'b0001: OUT = 7'b1111001;
			4'b0010: OUT = 7'b0100100;
			4'b0011: OUT = 7'b0110000;
			4'b0100: OUT = 7'b0011001;
			4'b0101: OUT = 7'b0010010;
			4'b0110: OUT = 7'b0000010;
			4'b0111: OUT = 7'b1111000;
			4'b1000: OUT = 7'b0000000;
			4'b1001: OUT = 7'b0011000;
			4'b1010: OUT = 7'b0001000;
			4'b1011: OUT = 7'b0000011;
			4'b1100: OUT = 7'b1000110;
			4'b1101: OUT = 7'b0100001;
			4'b1110: OUT = 7'b0000110;
			4'b1111: OUT = 7'b0001110;
			
			default: OUT = 7'b0111111;
		endcase

	end
endmodule


// ******************************************
// TOP MODULE AND LOWER MODULES HERE ....
// ******************************************
 module top_clicky(
   input clk, restart,
   
   input key1, key2, key3, key4,
   
   output wire [2:0] next_square, curr_square,
   output wire [6:0] next_colour, 
   output wire [2:0] curr_colour,
   
   output wire [3:0] next_mode,
   output wire [3:0] curr_mode,
   
   output wire [15:0] score,
   output wire [15:0] high_score,
   output wire [3:0] lives,
   
   output wire [7:0] x, y,
   output wire [2:0] colour,
   
   output wire times_up, have_pressed, correct_answer,
   
   output wire [9:0] squares_drawn,
   output wire [19:0] current_state, next_state,
   output wire [4:0] pressed_key,
   output wire plot
   );
	
    wire [4:0] ld_w_square;
    wire ld_next_square;
    wire update;
	
    clicky_datapath d0(
      .clk(clk),
      .start(1'd1),
      .reset(reset),
      .ld_w_square(ld_w_square),
      .ld_next_square(ld_next_square),
      .plot(plot),
      .pressed_key(pressed_key),
      .update(update),
      .set_answer(set_answer),
      .drawing_bar(drawing_bar),
      .times_up(times_up),
      .have_pressed(have_pressed), 
      .correct_answer(correct_answer),
      .squares_drawn(squares_drawn),
      .out_x(x),
      .out_y(y),
      .colour(colour),
      .score(score),
      .high_score(high_score),
      .lives(lives),
      .next_square(next_square),
      .next_colour(next_colour),
      .curr_colour(curr_colour),
      .curr_square(curr_square),
      .next_mode(next_mode),
      .curr_mode(curr_mode)
    );
  
    clicky_control c0(
      .start(1'd1),
      .restart(restart),
      .times_up(times_up),
      .clk(clk),
      .key1(key1), 
      .key2(key2), 
      .key3(key3), 
      .key4(key4),
      .have_pressed(have_pressed), 
      .correct_answer(correct_answer),
      .squares_drawn(squares_drawn),
      .lives(lives), 
      .score(score),
      .ld_w_square(ld_w_square),
      .ld_next_square(ld_next_square),
      .plot(plot),
      .pressed_key(pressed_key),
      .update(update),
      .current_state(current_state), 
      .next_state(next_state),
      .reset(reset),
      .set_answer(set_answer),
      .drawing_bar(drawing_bar)
    );

endmodule

module clicky_control(
    input start, 
    input restart,
    input times_up,
    input clk,
    input key1, key2, key3, key4,
    input have_pressed, correct_answer,
    input [9:0] squares_drawn,
    input [3:0] lives, 
    input [15:0] score,
    output reg [4:0] ld_w_square,
    output reg ld_next_square,
    output reg plot,
    output reg [4:0] pressed_key,
    output reg update,
    output reg [19:0] current_state, next_state,
    output reg reset,
    output reg set_answer,
    output reg drawing_bar
    );
    
    localparam
  	    
    START		           = 20'd1,
  
    LOAD_W_SQUARE_1		    =	20'd2,
    DRAW_W_SQUARE_1		    =	20'd3,
    DRAW_W_SQUARE_1_WAIT	    =	20'd4,

    LOAD_W_SQUARE_2		    =	20'd5,
    DRAW_W_SQUARE_2		    =	20'd6,
    DRAW_W_SQUARE_2_WAIT	    =	20'd7,

    LOAD_W_SQUARE_3		    =	20'd8,
    DRAW_W_SQUARE_3		    =	20'd9,
    DRAW_W_SQUARE_3_WAIT	    =	20'd10,

    LOAD_W_SQUARE_4		    =  20'd11,
    DRAW_W_SQUARE_4		    =  20'd12,
    DRAW_W_SQUARE_4_WAIT	    =	20'd13,

    LOAD_NEXT_SQUARE		    =  20'd14,
    DRAW_NEXT_SQUARE		    =	20'd15,
    DRAW_NEXT_SQUARE_WAIT	    =	20'd16,
    
    LOAD_LEFT_BAR           =    20'd30,
    DRAW_LEFT_BAR           =    20'd31,
    DRAW_LEFT_BAR_WAIT      =    20'd32,
    
    LOAD_RIGHT_BAR          =    20'd33,
    DRAW_RIGHT_BAR          =    20'd34,
    DRAW_RIGHT_BAR_WAIT     =    20'd35,

    WAIT_ANSWER		    =	20'd17,

    PRESSED_KEY_1_WAIT	    =	20'd19,
    PRESSED_KEY_2_WAIT	    =	20'd21,
    PRESSED_KEY_3_WAIT	    =	20'd23,
    PRESSED_KEY_4_WAIT	    =	20'd25,

    TIMES_UP			    =	20'd26,
    UPDATE_SCORE		    =	20'd27,
    RESTART			   	    =	20'd28,
    RESTART_WAIT	        =   20'd29,
	UPDATE_SCORE_WAIT		=   20'd36;
  

    always@(*)
    begin: state_table 
            case (current_state)
                
              START: next_state = start ? LOAD_W_SQUARE_1 : START;
              
              LOAD_W_SQUARE_1: next_state = DRAW_W_SQUARE_1;
              DRAW_W_SQUARE_1: next_state = (squares_drawn < 10'd256) ? DRAW_W_SQUARE_1 : DRAW_W_SQUARE_1_WAIT;
              DRAW_W_SQUARE_1_WAIT: next_state = LOAD_W_SQUARE_2;
              
              LOAD_W_SQUARE_2: next_state = DRAW_W_SQUARE_2;
              DRAW_W_SQUARE_2: next_state = (squares_drawn < 10'd256) ? DRAW_W_SQUARE_2 : DRAW_W_SQUARE_2_WAIT;
              DRAW_W_SQUARE_2_WAIT: next_state = LOAD_W_SQUARE_3;
              
              LOAD_W_SQUARE_3: next_state = DRAW_W_SQUARE_3;
              DRAW_W_SQUARE_3: next_state = (squares_drawn < 10'd256) ? DRAW_W_SQUARE_3 : DRAW_W_SQUARE_3_WAIT;
              DRAW_W_SQUARE_3_WAIT: next_state = LOAD_W_SQUARE_4;
              
              LOAD_W_SQUARE_4: next_state = DRAW_W_SQUARE_4;
              DRAW_W_SQUARE_4: next_state = (squares_drawn < 10'd256) ? DRAW_W_SQUARE_4 : DRAW_W_SQUARE_4_WAIT;
              DRAW_W_SQUARE_4_WAIT: next_state = LOAD_NEXT_SQUARE;
              
              LOAD_NEXT_SQUARE: next_state = DRAW_NEXT_SQUARE;
              DRAW_NEXT_SQUARE: next_state = (squares_drawn < 10'd256) ? DRAW_NEXT_SQUARE : DRAW_NEXT_SQUARE_WAIT;
              DRAW_NEXT_SQUARE_WAIT: next_state = LOAD_LEFT_BAR;
              
              LOAD_LEFT_BAR: next_state = DRAW_LEFT_BAR;
              DRAW_LEFT_BAR: next_state = (squares_drawn < 10'd480) ? DRAW_LEFT_BAR : DRAW_LEFT_BAR_WAIT;
              DRAW_LEFT_BAR_WAIT: next_state = LOAD_RIGHT_BAR;

              LOAD_RIGHT_BAR: next_state = DRAW_RIGHT_BAR;
              DRAW_RIGHT_BAR: next_state = (squares_drawn < 10'd480) ? DRAW_RIGHT_BAR : DRAW_RIGHT_BAR_WAIT;
              DRAW_RIGHT_BAR_WAIT: next_state = WAIT_ANSWER;
              
              WAIT_ANSWER:
                    begin
		              if (times_up == 1'b1)
                            next_state = TIMES_UP;
                      else if ((key1 == 1'b1) && (have_pressed == 1'b0))
                        	next_state = PRESSED_KEY_1_WAIT;
                      else if ((key2 == 1'b1) && (have_pressed == 1'b0))
                        	next_state = PRESSED_KEY_2_WAIT;
                      else if ((key3 == 1'b1) && (have_pressed == 1'b0))
                        	next_state = PRESSED_KEY_3_WAIT;
                      else if ((key4 == 1'b1) && (have_pressed == 1'b0))
                        	next_state = PRESSED_KEY_4_WAIT;
                      else
                        	next_state = WAIT_ANSWER;	
                    end
		
		    PRESSED_KEY_1_WAIT:
			begin
				if ((times_up == 1'b0) && (key1 == 1'b0))
					next_state = WAIT_ANSWER;
				else if (times_up == 1'b1)
					next_state = TIMES_UP;
				else
					next_state = PRESSED_KEY_1_WAIT;
			end
		    
            PRESSED_KEY_2_WAIT:
			begin
				if ((times_up == 1'b0) && (key2 == 1'b0))
					next_state = WAIT_ANSWER;
				else if (times_up == 1'b1)
					next_state = TIMES_UP;
				else
					next_state = PRESSED_KEY_2_WAIT;
			end
		    
            PRESSED_KEY_3_WAIT:
			begin
				if ((times_up == 1'b0) && (key3 == 1'b0))
					next_state = WAIT_ANSWER;
				else if (times_up == 1'b1)
					next_state = TIMES_UP;
				else
					next_state = PRESSED_KEY_3_WAIT;
			end
			
            PRESSED_KEY_4_WAIT:
			begin
				if ((times_up == 1'b0) && (key4 == 1'b0))
					next_state = WAIT_ANSWER;
				else if (times_up == 1'b1)
					next_state = TIMES_UP;
				else
					next_state = PRESSED_KEY_4_WAIT;
			end
 
                TIMES_UP: next_state = UPDATE_SCORE;
                UPDATE_SCORE: next_state = UPDATE_SCORE_WAIT;
					 UPDATE_SCORE_WAIT: next_state = (lives != 4'd0) ? LOAD_W_SQUARE_1 : RESTART;
              
                RESTART: next_state = restart ? RESTART_WAIT : RESTART;
                RESTART_WAIT: next_state = restart ? RESTART_WAIT : START;
              
            default:     next_state = START;
        endcase
    end
   
    always @(*)
    begin: enable_signals
	    set_answer = 1'b0;
      	reset = 1'b0;
      	ld_w_square = 5'd0;
		ld_next_square = 1'b0;
      	plot = 1'b0;
		pressed_key = 5'd0;
		update = 1'b0;
		drawing_bar = 1'b0;
      
        case (current_state)
           START:
             	begin
                reset = 1'b1;
                end
           LOAD_W_SQUARE_1: 
                begin
                set_answer = 1'b1;
                ld_w_square = 5'd1;
                end
           LOAD_W_SQUARE_2: 
                begin
                ld_w_square = 5'd2;
                end
           LOAD_W_SQUARE_3: 
                begin
                ld_w_square = 5'd3;
                end
           LOAD_W_SQUARE_4: 
                begin
                ld_w_square = 5'd4;
                end
           LOAD_NEXT_SQUARE: 
                begin
                ld_next_square = 1'b1;
                end
                
           LOAD_LEFT_BAR:
                begin
                ld_w_square = 5'd5;
                end
           LOAD_RIGHT_BAR:
                begin
                ld_w_square = 5'd6;
                end
          
           DRAW_W_SQUARE_1:
             	begin
                plot = 1'b1;
                end
           DRAW_W_SQUARE_2:
             	begin
                plot = 1'b1;
                end
           DRAW_W_SQUARE_3:
             	begin
                plot = 1'b1;
                end
           DRAW_W_SQUARE_4:
             	begin
                plot = 1'b1;
                end
           DRAW_NEXT_SQUARE:
             	begin
                plot = 1'b1;
                end
                
           DRAW_LEFT_BAR:
             	begin
                plot = 1'b1;
                drawing_bar = 1'b1;
                end
           DRAW_RIGHT_BAR:
             	begin
                plot = 1'b1;
                drawing_bar = 1'b1;
                end
                
           PRESSED_KEY_1_WAIT:
                begin
                pressed_key = 5'd1;
                end
           PRESSED_KEY_2_WAIT:
                begin
                pressed_key = 5'd2;
                end
           PRESSED_KEY_3_WAIT:
                begin
                pressed_key = 5'd3;
                end
           PRESSED_KEY_4_WAIT:
           	    begin
                pressed_key = 5'd4;
                end
          
           UPDATE_SCORE:
             	begin
                update = 1'b1;
                end
        endcase
    end 

    always@(posedge clk)
    begin: state_FFs
      if(!start)
            current_state <= START;
        else
            current_state <= next_state;
    end 
endmodule


module clicky_datapath(
   input clk,
   input start,
   input reset,
   input [4:0] ld_w_square,
   input ld_next_square,
   input plot,
   input [4:0] pressed_key,
   input update,
   input set_answer,
   input drawing_bar,
   output times_up,
   output reg have_pressed, correct_answer,
   output reg [9:0] squares_drawn,  
   output reg [7:0] out_x, out_y,
   output reg [2:0] colour, 
   output reg [15:0] score,
   output reg [15:0] high_score,
   output reg [3:0] lives,  
   output reg [2:0] next_square,
   output reg [6:0] next_colour,
   output reg [2:0] curr_colour,
   output reg [2:0] curr_square,
   output reg [3:0] next_mode,
   output reg [3:0] curr_mode
    );
    
    localparam
	  
    w_square_1_x		=	8'd30,
    w_square_1_y		=	8'd52,

    w_square_2_x		=	8'd60,
    w_square_2_y		=	8'd52,

    w_square_3_x		=	8'd90,
    w_square_3_y		=	8'd52,

    w_square_4_x		=	8'd120,
    w_square_4_y		=	8'd52,
    
    left_bar_x          =   8'd0,
    right_bar_x         =   8'd156,
    bar_y               =   8'd0,
    
    black			=      3'b000,
    blue			=      3'b001,
    green			=      3'b010,
    cyan			=      3'b011,
    red			    =	   3'b100,
    pink			=      3'b101,
    yellow			=	   3'b110,
    white		    =	   3'b111;

    reg [30:0] max_count;
    reg inc_life, dec_life, inc_speed, dec_speed, inc_score, dec_score;
	
   rate_divider rate_div(
   			.clk(clk), 
   			.reset(reset), 
   			.max_count(max_count),
   		    .enable(times_up)
   		    );
  
   reg [7:0] x,y;
   reg [2:0] c;  
   reg [9:0] ct;
  
   initial lives = 4'd3;
   initial score = 16'd0;
   initial have_pressed = 1'b0;
   initial next_square = 2'd1;
   initial next_colour = 1'd0;
   initial squares_drawn = 9'd0;
   initial max_count = 29'd180_000_000;
   initial high_score = 16'd0;
   initial inc_life = 0;
   initial dec_life = 0;
   initial inc_speed = 0;
   initial dec_speed = 0;
   initial inc_score = 0;
   initial dec_score = 0;
   initial curr_mode = 0;
   initial next_mode = 0;
   
   // DRAWING SQUARES
   wire [3:0] x_add = ct[3:0];
   wire [3:0] y_add = ct[7:4];
   
   // DRAWING BARS
   wire [1:0] x_add_bar = ct[1:0];
   wire [7:0] y_add_bar = ct[9:2];
  

   always @(posedge clk)
   begin
		
		 // RANDOM SELECTOR : SQUARE POSITION
		if (next_square >= 3'd4)
		    next_square <= 3'd0;
		else
		    next_square <= next_square + 3'd1;
       
                // RANDOM RANGE SELECTOR : COLOUR
		if (next_colour >= 7'd100)
		    next_colour <= 7'd0;
		else
		    next_colour <= next_colour + 7'd1;
		
		// RANDOM SELECTOR : NEXT MODE
		if (next_mode >= 4'd10)
		    next_mode <= 4'd0;
		else
		    next_mode <= next_mode + 7'd1;
		    
       // ******************************************
       // SET ANSWER
       // *****************************************
       if (set_answer == 1'b1)
           correct_answer <= 1'b0;

       // ******************************************
       // RESET
       // ******************************************
        if (reset == 1'b1)
        begin
        x <= 8'd0;
        y <= 8'd0;
        c <= 3'd0;
        
        inc_speed <= 0;
        dec_speed <= 0;
        inc_life <= 0;
		  dec_life <= 0;
		  inc_score <= 0;
		  dec_score <= 0;
		  
        have_pressed <= 1'd0;
        lives <= 4'd3;
        score <= 16'd0;
		  max_count <= 29'd180_000_000;
		  curr_mode <= 0;
        end
      	
        // ******************************************
        // UPDATE
        // ******************************************
        if (update == 1'b1)
        begin
  
	        

            // ******************************************
            // !!!! REGULAR MODE !!!
            // ******************************************
	       // if (curr_mode == 4'd0)
				
					 if (inc_speed)
    	            max_count <= max_count - (29'd10_000_000);
                if (dec_speed)
                    max_count <= max_count + (29'd10_000_000);
                if (inc_score)
					     begin
                    score <= score + 1;
						  if (high_score <= score+1)
								high_score = score +1;
						  end
                if (dec_score)
                    score <= score - 1;
                if (inc_life)
                    lives <= lives + 1;
                if ((dec_life) & (lives != 0)) 
                    lives <= lives - 1;
					
					 if ((have_pressed == 1'b0) && ((curr_colour == 3'd0) || (curr_colour == 3'd1)) && (lives != 3'd0))
					     lives <= lives -1;

   
				
            // ******************************************
            // !!! SWITCH MODE !!!
            // ******************************************
		    
		    // CASES FOR SWITCH MODE
		    if (curr_mode == 4'd0)     // switch mode is OFF
		    begin
		        if (next_mode == 4'd1)  // 10% chance to turn on switch mode
		            curr_mode <= 4'd1;
		    end
		    else if (curr_mode == 4'd3) // switch mode about to turn OFF
		        curr_mode <= 4'd0;
		    else                        // switch mode is ON
		        curr_mode <= curr_mode + 4'd1;
  

        end
      
        // ******************************************
        // DETERMINE CORRECT ANSWER :
        // ******************************************

 	// ************** PRESSED KEY 1 *************
        if (pressed_key == 5'd1)
            begin

            // INITIALIZE VALUES

            inc_speed <= 0;
            dec_speed <= 0;
            inc_life <= 0;
            dec_life <= 0;
            inc_score <= 0;
            dec_score <= 0;

            //CASES FOR COLORED SQUARES
            if (curr_mode == 4'd0)
            begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd1)
                begin
                	inc_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd1)
                begin
                        dec_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd1)
                begin
                        inc_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square == 3'd1)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square != 3'd1)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
            end
         	else
         	begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd1)
                begin
                    dec_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd1)
                begin
                    inc_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd1)
                begin
                     dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square == 3'd1)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square != 3'd1)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
         	end    
            
            have_pressed <= 1'b1;
            end

        // ************** PRESSED KEY 2 *************
        if (pressed_key == 5'd2)
            begin

            // INITIALIZE VALUES

            inc_speed <= 0;
            dec_speed <= 0;
            inc_life <= 0;
            dec_life <= 0;
            inc_score <= 0;
            dec_score <= 0;

            //CASES FOR COLORED SQUARES
            if (curr_mode == 4'd0)
            begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd2)
                begin
                	inc_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd2)
                begin
                        dec_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd2)
                begin
                        inc_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square == 3'd2)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square != 3'd2)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
            end
         	else
         	begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd2)
                begin
                    dec_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd2)
                begin
                    inc_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd2)
                begin
                     dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square == 3'd2)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square != 3'd2)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
         	end    
            
            have_pressed <= 1'b1;
            end

		// ************** PRESSED KEY 3 *************
        if (pressed_key == 5'd3)
            begin

            // INITIALIZE VALUES

            inc_speed <= 0;
            dec_speed <= 0;
            inc_life <= 0;
            dec_life <= 0;
            inc_score <= 0;
            dec_score <= 0;

            //CASES FOR COLORED SQUARES
            if (curr_mode == 4'd0)
            begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd3)
                begin
                	inc_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd3)
                begin
                        dec_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd3)
                begin
                        inc_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square == 3'd3)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square != 3'd3)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
            end
         	else
         	begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd3)
                begin
                    dec_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd3)
                begin
                    inc_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd3)
                begin
                     dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square == 3'd3)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square != 3'd3)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
         	end    
            
            have_pressed <= 1'b1;
            end
            
         // ************** PRESSED KEY 4 *************
        if (pressed_key == 5'd4)
            begin

            // INITIALIZE VALUES

            inc_speed <= 0;
            dec_speed <= 0;
            inc_life <= 0;
            dec_life <= 0;
            inc_score <= 0;
            dec_score <= 0;

            //CASES FOR COLORED SQUARES
            if (curr_mode == 4'd0)
            begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd4)
                begin
                	inc_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd4)
                begin
                        dec_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd4)
                begin
                        inc_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square == 3'd4)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square != 3'd4)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
            end
         	else
         	begin
                // ************ INCREASE SPEED **************
                if (curr_colour == 3'd4 && curr_square == 3'd4)
                begin
                    dec_speed <= 1'b1;
                end
                // ************ DECREASE SPEED **************
                else if (curr_colour == 3'd3 && curr_square == 3'd4)
                begin
                    inc_speed <= 1'b1;
                end
                // ************ INCREASE LIVES **************
                else if (curr_colour == 3'd2 && curr_square == 3'd4)
                begin
                     dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd0)
                begin
                    if (curr_square == 3'd4)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
                // ************ INCREASE SCORE **************
                else if (curr_colour == 3'd1)
                begin
                    if (curr_square != 3'd4)
                        inc_score <= 1'b1;
                    else
                        dec_life <= 1'b1;
                end
         	end    
            
            have_pressed <= 1'b1;
            end
             
        // ******************************************
        // LOADING WHITE SQUARE POSITION
        // ******************************************
        if (ld_w_square == 5'd1)
        begin
		      have_pressed <= 1'd0;
            x <= w_square_1_x;
            y <= w_square_1_y;
            c <= white;
				
				inc_speed <= 0;
            dec_speed <= 0;
            inc_life <= 0;
            dec_life <= 0;
            inc_score <= 0;
            dec_score <= 0;
        end
      	
        else if (ld_w_square == 5'd2)
        begin
            x <= w_square_2_x;
            y <= w_square_2_y;
            c <= white;
        end
      
        else if (ld_w_square == 5'd3)
        begin
            x <= w_square_3_x;
            y <= w_square_3_y;
            c <= white;
        end
      
        else if (ld_w_square == 5'd4)
        begin
            x <= w_square_4_x;
            y <= w_square_4_y;
            c <= white;
        end
        
        // LEFT BAR
        else if (ld_w_square == 5'd5)
        begin
            x <= left_bar_x;
            y <= bar_y;
            if (curr_mode == 4'd0)
                c <= black;
            else
                c <= red;
        end
        
        // RIGHT BAR
        else if (ld_w_square == 5'd6)
        begin
            x <= right_bar_x;
            y <= bar_y;
            if (curr_mode == 4'd0)
                c <= black;
            else
                c <= red;
        end

       // ******************************************
       // LOADING VARB SQUARE POSITION
       // ******************************************
       if (ld_next_square == 1'b1)
       begin
		 /*
	    // ******* SET HIGH SCORE *******
	    if (high_score <= score)
	    begin
	        high_score <= score;
	    end*/
           // ******** INCREMENT SPEED FOR SCORE INC *******
	    if (score%5 == 4)
	    begin
	        max_count <= max_count - (29'd10_000_000);
	    end
	    
           // ******* SET THE NEXT SQUARE *******
	    if (next_colour >= 7'd0 && next_colour <= 7'd44)
	    begin
	            c <= red;
                curr_colour <= 3'd1;
        end
	    else if (next_colour >= 7'd45 && next_colour <= 7'd89)
	    begin
	            c <= yellow;
                curr_colour <= 3'd0;
        end
	    else if (next_colour >= 7'd90 && next_colour <= 7'd94)
	    begin
	            c <= green;
                curr_colour <= 3'd2;
        end
	    else if (next_colour >= 7'd95 && next_colour <= 7'd97)
	    begin
                c <= pink;
                curr_colour <= 3'd4;
        end
	    else if (next_colour >= 7'd98 && next_colour <= 7'd100)
	    begin
		        c <= cyan;
                curr_colour <= 3'd3;
        end
  
          

            if (next_square == 3'd1)
            begin
            	x <= w_square_1_x;
            	y <= w_square_1_y;
              	curr_square <= 3'd1;
            end
          
            else if (next_square == 3'd2)
            begin
            	x <= w_square_2_x;
            	y <= w_square_2_y;
                curr_square <= 3'd2;
            end
          
            else if (next_square == 3'd3)
            begin
            	x <= w_square_3_x;
            	y <= w_square_3_y;
                curr_square <= 3'd3;
            end
          
            else if (next_square == 3'd4)
            begin
            	x <= w_square_4_x;
            	y <= w_square_4_y;
                curr_square <= 3'd4;
            end
        end
      
        // ******************************************
        // SQUARE PLOTTER
        // ******************************************
        if (plot == 1'b0)
            ct <= 0;
        else if (ct == 10'd1023)
            begin
            ct <= 0;
            end
        else if (plot == 1'b1)
            ct <= ct + 1'b1;
        else
            ct <= ct;
      
        // ******************************************
        // PLOT OUTPUT
        // ******************************************
      
        if (plot == 1'b0) 
        begin
            out_x <= 8'b0000_0000;
            out_y <= 8'b0000_0000;
            colour <= 3'b000;
            squares_drawn <= 10'd0;
        end
        else
        begin
            // SWITCH MODE IS OFF
            if (drawing_bar == 1'b1)
            begin
                out_x <= x + x_add_bar;
                out_y <= y + y_add_bar;
                colour <= c;
            end
            else
            begin
                out_x <= x + x_add;
                out_y <= y + y_add;
                colour <= c;
            end
            squares_drawn <= squares_drawn + 10'd1;
        end
    end
endmodule

// ******************************************
// RATE DIVIDER
// ******************************************
module rate_divider(
  	input clk,
	input reset,
  	input [30:0] max_count, 
  	output reg enable
    );
  	
  	reg [30:0] ct;
  
    always @(posedge clk)
    begin
		  if (reset)
		  begin
		      ct <= max_count - 1;
				enable <= 1'b0;
		  end
        else if (ct > 0)
        begin
            ct <= ct - 29'd1;
      		enable <= 1'b0;
        end
      
        else
        begin
            ct <= max_count;
          	enable <= 1'b1;
        end
    end
endmodule
