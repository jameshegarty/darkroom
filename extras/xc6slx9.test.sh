sudo kextunload -v -bundle com.apple.driver.AppleUSBFTDI  #mavs
papillio-prog -f out/$1.bit
sudo kextload -v -bundle com.apple.driver.AppleUSBFTDI
terra ../fpgatest.t out/$1.metadata.lua out/$1.fpga.bmp
