module RAMReader(
    //AXI port
    input ACLK,
    input ARESETN,
    output reg [31:0] M_AXI_ARADDR,
    input M_AXI_ARREADY,
    output  M_AXI_ARVALID,
    input [63:0] M_AXI_RDATA,
    output M_AXI_RREADY,
    input [1:0] M_AXI_RRESP,
    input M_AXI_RVALID,
    input M_AXI_RLAST,
    output [3:0] M_AXI_ARLEN,
    output [1:0] M_AXI_ARSIZE,
    output [1:0] M_AXI_ARBURST,
    
    //Control config
    input VALID,
    output READY,
    input [31:0] START_ADDR,
    input [31:0] NBYTES,
    
    //RAM port
    output reg [8:0] RAM_ADDR,
    output RAM_WE,
    output [63:0] RAM_DI,
    input [12:0] BYTES_FREE,
    output WROTE_BYTES
);

assign M_AXI_ARLEN = 4'b1111;
assign M_AXI_ARSIZE = 2'b11;
assign M_AXI_ARBURST = 2'b01;
parameter IDLE = 0, RWAIT = 1;
    
//ADDR logic
reg [31:0] a_count;
reg a_state;  
assign M_AXI_ARVALID = (a_state == RWAIT);
always @(posedge ACLK) begin
    if (ARESETN == 0) begin
        a_state <= IDLE;
        M_AXI_ARADDR <= 0;
        a_count <= 0;
    end else case(a_state)
        IDLE: begin
            if(VALID) begin
                M_AXI_ARADDR <= START_ADDR;
                a_count <= NBYTES[31:7];
                a_state <= RWAIT;
            end
        end
        RWAIT: begin
            if (M_AXI_ARREADY == 1) begin
                if(a_count - 1 == 0)
                    a_state <= IDLE;
                a_count <= a_count - 1;
                M_AXI_ARADDR <= M_AXI_ARADDR + 128; 
            end
        end
    endcase
end
    
assign ram_has_room = BYTES_FREE >= 8;

//READ logic
reg [31:0] b_count;
reg r_state;
assign M_AXI_RREADY = (r_state == RWAIT) && ram_has_room;
always @(posedge ACLK) begin
    if (ARESETN == 0) begin
        r_state <= IDLE;
        b_count <= 0;
    end else case(r_state)
        IDLE: begin
            if(VALID) begin
                b_count <= {NBYTES[31:7],7'b0};
                r_state <= RWAIT;
                RAM_ADDR <= 0;
            end
        end
        RWAIT: begin
            if (M_AXI_RVALID && ram_has_room) begin
                //use M_AXI_RDATA
                if(b_count - 8 == 0)
                    r_state <= IDLE;
                b_count <= b_count - 8;
                RAM_ADDR <= RAM_ADDR + 9'b1;
            end
        end
    endcase
end

assign RAM_DI = M_AXI_RDATA;
assign RAM_WE = M_AXI_RREADY && M_AXI_RVALID;
assign WROTE_BYTES = RAM_WE;
assign READY = (r_state == IDLE) && (a_state == IDLE);

endmodule