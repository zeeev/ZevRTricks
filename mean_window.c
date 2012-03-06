#include <R.h>
#include <Rmath.h>
#include <stdio.h>
#include <libintl.h>


void gslide_mean(double *ypts, double *xpts, int *nxpts, int *window_size, double *results){
	int i, j;
	double half_window = *window_size / 2;	
	
	for(i = 0 ; i < *nxpts; i++){
	
		int lower = xpts[i] - half_window;
		int upper = xpts[i] + half_window;
	
		double total = 0;
		double count = 0;

	   if(upper > xpts[*nxpts -1]) upper = xpts[*nxpts -1];	
	   if(lower < xpts[0]) lower = xpts[0];
	
		for(j = 0; j < *nxpts; j++){
			if(xpts[j] > upper) continue;
			if(xpts[j] < lower) continue;
			total += ypts[j];	
			count++;
		}
		
		double ave = total / count;
		results[i] = ave;
	}			
}