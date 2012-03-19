#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <libintl.h>


void kernel_smooth(double *x, double *ypts,  double *xpts, int *n, int *nxpts, double *h, double *results){
	int i, j;
	

	for(i = 0; i < *n; i++){

		double nsum = 0;
		double dsum = 0;
        
        double z = x[i] + *h;
        double y = x[i] - *h;
	                
		for(j = 0; j < *nxpts; j++){
						
			if(xpts[j] < y){
				continue;
			}	
			if(xpts[j] > z){
				break;
			}	
			double d = (fabs(xpts[j] - i)) / *h;

		    double r = dnorm(d, 0, 1, 0);
			nsum += r * ypts[j];
			dsum += r;	
		}	

			double v = nsum / dsum;

		 results[i] = v;  
		 
	}
	
}