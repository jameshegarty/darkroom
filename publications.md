---
layout: post
title: Publications
---
Publications
============


**[Darkroom: Compiling High-Level Image Processing Code into Hardware Pipelines](darkroom14-low.pdf)** <br/>
_James Hegarty, John Brunhaver, Zachary DeVito, Jonathan Ragan-Kelley, Noy Cohen, Steven Bell, Artem Vasilyev, Mark Horowitz, and Pat Hanrahan_<br/>
SIGGRAPH '14

Specialized image signal processors (ISPs) exploit the structure of image processing pipelines to minimize memory bandwidth using the architectural pattern of line-buffering, where all intermediate data between each stage is stored in small on-chip buffers. This provides high energy efficiency, allowing long pipelines with tera-op/sec. im- age processing in battery-powered devices, but traditionally requires painstaking manual design in hardware. Based on this pattern, we present Darkroom, a language and compiler for image processing. The semantics of the Darkroom language allow it to compile pro- grams directly into line-buffered pipelines, with all intermediate values in local line-buffer storage, eliminating unnecessary com- munication with off-chip DRAM. We formulate the problem of optimally scheduling line-buffered pipelines to minimize buffering as an integer linear program. Finally, given an optimally scheduled pipeline, Darkroom synthesizes hardware descriptions for ASIC or FPGA, or fast CPU code. We evaluate Darkroom implementations of a range of applications, including a camera pipeline, low-level fea- ture detection algorithms, and deblurring. For many applications, we demonstrate gigapixel/sec. performance in under 0.5mm2 of ASIC silicon at 250 mW (simulated on a 45nm foundry process), real- time 1080p/60 video processing using a fraction of the resources of a modern FPGA, and tens of megapixels/sec. of throughput on a quad-core x86 processor.