module MyThing(
    input S_AXI_ACLK,                 
    input S_AXI_ARESETN,
    input [31:0] S_AXI_ARADDR,
    output S_AXI_ARREADY,         //
    input S_AXI_ARVALID,
    input [31:0] S_AXI_AWADDR,
    output  S_AXI_AWREADY,         //
    input S_AXI_AWVALID,
    input S_AXI_BREADY,
    output  reg [1:0] S_AXI_BRESP,     //
    output  S_AXI_BVALID,          //
    output reg [31:0] S_AXI_RDATA,    //
    input S_AXI_RREADY,
    output reg [1:0] S_AXI_RRESP,           //
    output  S_AXI_RVALID,          //
    input [31:0] S_AXI_WDATA,
    output  S_AXI_WREADY,          //
    input [3:0] S_AXI_WSTRB,
    input  S_AXI_WVALID,
    input [7:0] SWITCH,
    output reg [7:0] LED,
    
    output reg [31:0] M_AXI_ARADDR,
    input M_AXI_ARREADY,
    output  M_AXI_ARVALID,
    output reg [31:0] M_AXI_AWADDR,
    input M_AXI_AWREADY,
    output  M_AXI_AWVALID,
    output  M_AXI_BREADY,
    input [1:0] M_AXI_BRESP,
    input M_AXI_BVALID,
    input [63:0] M_AXI_RDATA,
    output M_AXI_RREADY,
    input [1:0] M_AXI_RRESP,
    input M_AXI_RVALID,
    output [63:0] M_AXI_WDATA,
    input M_AXI_WREADY,
    output [7:0] M_AXI_WSTRB,
    output M_AXI_WVALID,
    input M_AXI_RLAST,
    output M_AXI_WLAST
);

reg [2:0] state;

parameter IDLE = 3'd0,  WRITE_BEGIN = 3'd1, READ_WAIT = 3'd2, WRITE_WAIT = 3'd3, BRESP_WAIT = 3'd4;
parameter OK = 2'b00, SLVERR = 2'b10, DECERR = 2'b11;
assign S_AXI_ARREADY = (state == IDLE);
assign S_AXI_RVALID = (state == READ_WAIT);
assign S_AXI_WREADY = (state == WRITE_WAIT);
assign S_AXI_AWREADY = (state == WRITE_BEGIN);
assign S_AXI_BVALID = (state == BRESP_WAIT) || (state == WRITE_WAIT);

assign sentaddrgo = (state == WRITE_WAIT) && S_AXI_WVALID;
assign sentaddrgoread = sentaddrgo && !S_AXI_WDATA[0];
assign sentaddrgowrite = sentaddrgo && S_AXI_WDATA[0];

reg [31:0] addr;
reg [63:0] cyclecount;

//slave logic: read address 70000000 returns the cyclecount
//             write address 7000000 with an address, causes the machine to to sum up the next 1024 64-bit numbers and put the result on the LEDS
always @(posedge S_AXI_ACLK) begin
    if (S_AXI_ARESETN == 0) begin
        state <= IDLE;
        S_AXI_RRESP <= 2'b0;
        S_AXI_RDATA <= 32'b0; 
        S_AXI_BRESP <= 2'b0;
    end else case(state)
        IDLE: begin
            if (S_AXI_ARVALID == 1) begin
                S_AXI_RRESP <= (S_AXI_ARADDR == 32'h70000000) ? OK : SLVERR;
                S_AXI_RDATA <= cyclecount[31:0];
                state <= READ_WAIT;
            end else if (S_AXI_AWVALID == 1) begin
                state <= WRITE_BEGIN;
            end
        end
        READ_WAIT: begin
            if (S_AXI_RREADY == 1) begin
                state <= IDLE;
            end
        end
        WRITE_BEGIN: begin 
            S_AXI_BRESP <= (S_AXI_AWADDR == 32'h70000000) ? OK : SLVERR;
            state <= WRITE_WAIT; 
        end
        WRITE_WAIT: begin
            if (S_AXI_WVALID == 1) begin
                state <= (S_AXI_BREADY == 1) ? IDLE : BRESP_WAIT;
            end
        end
        BRESP_WAIT: begin
            if (S_AXI_BREADY == 1) begin
                state <= IDLE;
            end
        end
    endcase
end

parameter ADDR_WAIT  = 3'd1, READY_WAIT = 3'd1;


reg [2:0] amstate;
reg [2:0] rmstate;

assign M_AXI_ARVALID = (amstate == ADDR_WAIT);
assign M_AXI_RREADY = (rmstate == READ_WAIT);

reg [11:0] count;
reg [11:0] acount;

always @(posedge S_AXI_ACLK) begin
    if (S_AXI_ARESETN == 0) begin
        amstate <= IDLE;
        M_AXI_ARADDR <= 0;
        acount <= 12'd0;
    end else case(amstate)
        IDLE: begin
            if(sentaddrgoread) begin
                M_AXI_ARADDR <= {S_AXI_WDATA[31:1],1'b0};
                acount <= 12'd64;
                amstate <= ADDR_WAIT;
            end
        end
        ADDR_WAIT: begin
            if (M_AXI_ARREADY == 1) begin
                if(acount - 1 == 0)
                    amstate <= IDLE;
                acount <= acount - 1;
                M_AXI_ARADDR <= M_AXI_ARADDR + 128; 
            end
        end
    endcase
end


always @(posedge S_AXI_ACLK) begin
    if (S_AXI_ARESETN == 0) begin
        rmstate <= IDLE;
        count <= 12'd0;
    end else case(rmstate)
        IDLE: begin
            if(sentaddrgoread) begin
                LED[6:0] <= 0;
                count <= 12'd1024;
                rmstate <= READ_WAIT;
            end
        end
        READ_WAIT: begin
            if (M_AXI_RVALID == 1) begin
                LED[6:0] <= LED[6:0] + M_AXI_RDATA[6:0];
                if(M_AXI_RLAST == 1)
                    if (count - 1 == 0)
                        rmstate <= IDLE;
                count <= count - 1;
            end
        end
    endcase
end


assign M_AXI_BREADY = 1;


reg [11:0] wcount;

assign M_AXI_WDATA = {52'b0,wcount};
assign M_AXI_WSTRB = 8'b11111111;

reg [2:0] wmastate;
reg [11:0] awcount;
assign M_AXI_AWVALID = (wmastate == READY_WAIT);
always @(posedge S_AXI_ACLK) begin
    if (S_AXI_ARESETN == 0) begin
        wmastate <= IDLE;
        awcount <= 0;
    end else case(wmastate) 
        IDLE: begin
            if(sentaddrgowrite) begin
                M_AXI_AWADDR <= {S_AXI_WDATA[31:1],1'b0};
                awcount <= 12'd63;
                wmastate <= READY_WAIT;
            end
        end
        READY_WAIT: begin 
            if(M_AXI_AWREADY == 1) begin
                if(awcount == 0)
                    wmastate <= IDLE;
                else
                    awcount <= awcount - 1;
                M_AXI_AWADDR <= M_AXI_AWADDR + 128;
            end
        end    
    endcase
end

reg [2:0] wmstate;
assign M_AXI_WVALID = (wmstate == READY_WAIT);
assign M_AXI_WLAST = (wcount[3:0] == 4'b0000);
 
always @(posedge S_AXI_ACLK) begin
    if (S_AXI_ARESETN == 0) begin
        wmstate <= IDLE;
        wcount <= 0;
    end else case(wmstate) 
        IDLE: begin
            if(sentaddrgowrite) begin
                wcount <= 12'd1023;
                wmstate <= READY_WAIT;
            end
        end
        READY_WAIT: begin 
            if(M_AXI_WREADY == 1) begin
                if(wcount == 0)
                    wmstate <= IDLE;
                else
                    wcount <= wcount - 1;
            end
        end    
    endcase
end



// count how long it takes to fulfill a full copy
always @(posedge S_AXI_ACLK) begin    
    if(sentaddrgo)
        cyclecount <= 0;
    else if (count != 0 || wcount != 0)
        cyclecount <= cyclecount + 1;
end



//change LED every 100M cycles, changes 1/sec at 100Mhz
reg [63:0] clockcounter;
always @(posedge S_AXI_ACLK) begin
    if(clockcounter == 100000000) begin
        LED[7] <= ~LED[7];
        clockcounter <= 0;
    end else
        clockcounter <= clockcounter + 1;
end



endmodule