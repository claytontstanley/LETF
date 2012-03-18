/*
 *  parabola2d.c
 *  Hurricane Test (borrowed from Quick)
 *
 *  Created by Rick on 6/26/07.
 *  Copyright 2007 PALM. All rights reserved.
 *
 */

#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <unistd.h>
#include "Functions.h"

#define USE_STDIN
//#define SLOW	

int main( int argc, char **argv ) {
	if( argc != 2 ) {
		fprintf( stderr, "Usage: parabola2d work-file\n" );
		exit( -1 );
	}
	
#ifdef USE_STDIN
	FILE *fp = stdin;
#else
	FILE *fp = fopen( argv[ 1 ], "r" );
	if( ! fp ) {
		fprintf( stderr, "parabola2d: Could not open work file \"%s\"\n", argv[ 1 ] );
		exit( -1 );
	}
#endif
	
	char buffer[ BUFFER_SIZE ];
	double x, y;
	while( fgets( buffer, BUFFER_SIZE, fp ) != NULL ) {
//		sleep( 1 );
		sscanf( buffer, "%lg %lg\n", &x, &y );

#ifdef SLOW
		sleep( 7 );
/*
		struct timespec ratp;
		ratp.tv_sec = 0;
		ratp.tv_nsec = 500000000;
		ratp.tv_nsec = 200000000;
		nanosleep( &ratp, NULL );
 */
#endif
		
		printf( "Evaluating %lg %lg\n", x, y );
		printf( "height=%lg\n", 1 - (4*x*x+4*y*y));
		printf( "height1=%lg\n", 1- (2*(x+.5)*x+2*y*y));
		printf( "height2=%lg\n", sin(x*x + 3*y*y)/(.1 + x*x + y*y) + x*x + 5 *y*y);
	}
	fclose( fp );
	
	exit(0);
}
