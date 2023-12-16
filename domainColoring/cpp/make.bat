rem generate a static .dll 
g++ -static -shared -O3 -o dc.dll DomainColoring.cpp
move dc.dll ..
