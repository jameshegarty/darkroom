(terralib.loadfile("test.t"))()
import "darkroom"

im a(x,y) darkroom.vectorSelect( {[uint8](100),[uint8](0),[uint8](100)} > [uint8](10), {[uint8](100),[uint8](0),[uint8](100)}, [uint8[3]](testinput)) end

im c(x,y) {a[0]+a[1],a[1],a[2]} end
--im b(x,y) darkroom.dot(a,{[uint8](2),[uint8](2),[uint8](2)}) end
test({{c,"uart",darkroom.type.array(darkroom.type.uint(8),3)}})
