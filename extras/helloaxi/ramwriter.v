module RAMWriter(
    //AXI port
    input ACLK,
    input ARESETN,
    output reg [31:0] M_AXI_AWADDR,
    input M_AXI_AWREADY,
    output M_AXI_AWVALID,
    
    output [63:0] M_AXI_WDATA,
    output [7:0] M_AXI_WSTRB,
    input M_AXI_WREADY,
    output M_AXI_WVALID,
    output M_AXI_WLAST,
    
    input [1:0] M_AXI_BRESP,
    input M_AXI_BVALID,
    output M_AXI_BREADY,
    
    output [3:0] M_AXI_AWLEN,
    output [1:0] M_AXI_AWSIZE,
    output [1:0] M_AXI_AWBURST,
    
    //Control config
    input VALID,
    output READY,
    input [31:0] START_ADDR,
    input [31:0] NBYTES,
    
    //RAM port
    output [8:0] RAM_ADDR,
    output RAM_RE,
    input [63:0] RAM_DO,
    input [12:0] BYTES_FREE,
    output READ_BYTES
);

assign M_AXI_AWLEN = 4'b1111;
assign M_AXI_AWSIZE = 2'b11;
assign M_AXI_AWBURST = 2'b01;
assign M_AXI_WSTRB = 8'b11111111;

parameter BUFFER_SIZE = 4096;
parameter IDLE = 0, RWAIT = 1;
    
//ADDR logic
reg [31:0] a_count;
reg a_state;  
assign M_AXI_AWVALID = (a_state == RWAIT);
always @(posedge ACLK) begin
    if (ARESETN == 0) begin
        a_state <= IDLE;
        M_AXI_AWADDR <= 0;
        a_count <= 0;
    end else case(a_state)
        IDLE: begin
            if(VALID) begin
                M_AXI_AWADDR <= START_ADDR;
                a_count <= NBYTES[31:7];
                a_state <= RWAIT;
            end
        end
        RWAIT: begin
            if (M_AXI_AWREADY == 1) begin
                if(a_count - 1 == 0)
                    a_state <= IDLE;
                a_count <= a_count - 1;
                M_AXI_AWADDR <= M_AXI_AWADDR + 128; 
            end
        end
    endcase
end
    
assign ram_has_room = BYTES_FREE + {pipe_valid[0],3'b0} + {pipe_valid[1],3'b0} + {pipe_valid[2],3'b0} <= (13'd4096 - 13'd8);

//WRITE logic



wire stalled;

reg [63:0] ram_buffer;

reg [2:0] pipe_valid;
reg [8:0] ram_addr;
reg [8:0] ram_addr_r;

assign stalled = pipe_valid[2] && !M_AXI_WREADY;

assign RAM_RE = pipe_valid[0];
always @(posedge ACLK) begin
    if (ARESETN == 0 || (READY && VALID)) begin
        pipe_valid <= 3'b000;
        ram_addr_r <= 9'b0;
        ram_addr <= 9'b0;
    end else begin
        if (!stalled) begin
            pipe_valid <= {pipe_valid[1:0], ram_has_room};
            ram_addr_r <= ram_addr;
            if (ram_has_room)
                ram_addr <= ram_addr + 9'b1;
            ram_buffer <= RAM_DO;
        end
    end
end

assign RAM_ADDR = ram_addr_r;


reg [31:0] b_count;
reg w_state;
always @(posedge ACLK) begin
    if (ARESETN == 0) begin
        w_state <= IDLE;
        b_count <= 0;
    end else case(w_state)
        IDLE: begin
            if(VALID) begin
                b_count <= {NBYTES[31:7],7'b0};
                w_state <= RWAIT;
                last_count <= 4'b1111;
            end
        end
        RWAIT: begin
            if (M_AXI_WREADY && M_AXI_WVALID) begin
                //use M_AXI_WDATA
                if(b_count - 8 == 0) begin
                    w_state <= IDLE;
                end
                last_count <= last_count - 4'b1;
                b_count <= b_count - 8;
            end
        end
    endcase
end

reg [3:0] last_count;
assign M_AXI_WLAST = last_count == 4'b0000;

assign M_AXI_WVALID = (w_state == RWAIT) && pipe_valid[2];

assign READY = (w_state == IDLE) && (a_state == IDLE);

assign M_AXI_BREADY = 1;

assign M_AXI_WDATA = ram_buffer;

assign READ_BYTES = M_AXI_WREADY && M_AXI_WVALID;

endmodule