#include "Waterman.h"

int main()
{
	float rad=45;
	
    auto wp = WatermanPoly();
	
	printf("generating a %f waterman poly...",rad);
	
    auto qh = wp.genHull(rad);
    auto faces=qh.getFaces();
    auto vertices = qh.getVertices();
    
	puts("done");
	
	printf("# vertexes:%d, #faces:%d\n", vertices.size(), faces.size());
	return 0;
}