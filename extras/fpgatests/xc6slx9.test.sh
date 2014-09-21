sudo kextunload -v -bundle com.apple.driver.AppleUSBFTDI  #mavs
papillio-prog -f out/$1.bit
sudo kextload -v -bundle com.apple.driver.AppleUSBFTDI
$(TERRA) $1.lua test frame_128.bmp
