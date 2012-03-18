/*
 *  Functions.c
 *  Hurricane
 *
 *  Created by Rick on 11/5/09.
 *  Copyright 2009 Lockheed. All rights reserved.
 *
 */

#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <math.h>
//#include "Pointxd.h"
//#include "Cube.h"

double frandom() {
	return((double) rand() / RAND_MAX );
}

int irandom( int range ) {
	return frandom() * range;
}

void Log( const char *message, ... ) {
	va_list ap;
	
	va_start( ap, message );
	vfprintf( stderr, message, ap );
	va_end( ap );
	
	fprintf( stderr, "\n" );
}

int isseparator( int c ) {
	return isspace( c ) || ( c == ',' );
}

char *strdellastpath( const char *path ) {
    char *lastSlash = strrchr( path, '/' );
    char *result = NULL;
    
    if( lastSlash == NULL ) {
        // No slash in path
        result = strdup( "" );
    } else if( ! strcmp( path, "/" )) {
        // Root
        result = strdup( "/" );
    } else if( path[ strlen( path ) - 1 ] == '/' ) {
        // Ends with slash
        result = strdup( path );
        
        // Remove trailing slash
        result[ strlen( result ) - 1] = 0;
        
        // Now find the next slash in
        lastSlash = strrchr( result, '/' );
        
        // Remove that slash, if it is not the root slash
        if( lastSlash > result )
            *lastSlash = 0;
    } else {
        // Should be a normal path
        result = strdup( path );
        
        // Now find the next slash in
        lastSlash = strrchr( result, '/' );
        
        // Remove that slash, if it is not the root slash
        if( lastSlash > result )
            *lastSlash = 0;
    }
    
    return result;
}

char *strlastpath( const char *path ) {
    char *result = NULL;
    
    if( ! strcmp( path, "/" )) {
        // Root path
        result = strdup( "/" );
    } else {
        char *lastSlash = strrchr( path, '/' );
        
        if( lastSlash == NULL ) {
            // No slash at all
            result = strdup( path );
        } else if( path[ strlen( path - 1 ) ] == '/' ){
            char *temp = strdup( path );
                
            // Remove trailing slash
            temp[ strlen( temp ) - 1 ] = 0;
            
            lastSlash = strrchr( temp, '/' );
            result = strdup( lastSlash + 1 );
//#ifdef NOTBROKEN
            
            free( temp );
//#endif
        } else {
            result = strdup( lastSlash + 1 );
        }
    }
    
    return result;
}

char *strreplace( const char *string, const char *find, const char *replace ) {
    int findLength = strlen( find );
    int replaceLength = strlen( replace );
    int findInstances = 0;

    // First count the number of replacements so we can allocate memory
    for( const char *stringPointer = string; *stringPointer; stringPointer++ ) 
        if( ! strncmp( stringPointer, find, findLength )) {
            findInstances++;
            stringPointer += findLength - 1;
        }

    // Allocate memory for the result
    char *result = (char *) malloc( strlen( string ) + findInstances * ( replaceLength - findLength ) + 1 );

    // Make the replacements
    const char *stringPointer = string;
    char *resultPointer = result;
    while( *stringPointer ) {
        if( ! strncmp( stringPointer, find, findLength )) {
            strcpy( resultPointer, replace );
            resultPointer += replaceLength;
            stringPointer += findLength;
        } else {
            *resultPointer = *stringPointer;
            resultPointer++;
            stringPointer++;
        }
    }
    *resultPointer = 0;

    return result;
}

const char **strcomps( const char *string ) {
    int arrayCount = 5;
    int arrayIndex = 0;
    const char **parts = (const char **) malloc( sizeof( const char * ) * ( arrayCount + 1 ));
	int index = 0;
    
	for(;;) {
		// Find start of component, skipping whitespace
		while( isseparator( string[ index ] ))
			index++;
		int start = index;
		
		// Find end of component
		while(( ! isseparator( string[ index ] )) && ( string[ index ] ))
			index++;
        
		// If start == end we are at the end of the string
		if( start == index )
			break;
		
		// Create part and add to list
        char *component = (char *) malloc( 1 + index - start );
        memcpy( component, (string + start), index - start );
        component[ index - start ] = 0;

        // Make sure there is room in the array for this component
        if( arrayIndex >= arrayCount ) {
            arrayCount += 5;
            parts = (const char **) realloc( parts, sizeof( const char * ) * ( arrayCount + 1 ));
        }
        
        // Add to array
        parts[ arrayIndex ] = component;
        arrayIndex++;
    }
    
	parts[ arrayIndex ] = NULL;
	return parts;
}

int strcompscount( const char **comps ) {
    int count = 0;
    
    while( *comps ) {
        comps++;
        count++;
    }
    return count;
}

void delcomps( const char **comps ) {
    
//#ifdef NOTBROKEN

    const char **string = comps;
    
    while( *string ) {
        free((void *) *string );
        string++;
    }
    free( comps );
//#endif
}

const char *stradd( const char *string, const char *addString ) {
    int length = ( string ? strlen( string ) : 0 ) + strlen( addString ) + 1;
    char *newString = (char *) malloc( length );
    newString[ 0 ] = 0;
    if( string )
        strcpy( newString, string );
    strcat( newString, addString );

//#ifdef NOTBROKEN
    if( string )
		free((void *) string );
//#endif
    return newString;
}

const char *strtrim( const char *string ) {
	const char *start = string;
	
	while( isspace( *start ))
		start++;
	
	char *result = strdup( start );
	while( isspace( result[ strlen( result ) - 1 ] ))
		result[ strlen( result ) - 1 ] = 0;
		
	return result;
}


/*
void destroyArrayWithPoints( Array *array ) {
//#ifdef NOTBROKEN

	for( int i = 0; i < array->count; ++i )
		delete (Pointxd *) array->array[ i ];
	destroyArray( array );
//#endif
}

void destroyArrayWithCubes( Array *array ) {
//#ifdef NOTBROKEN
	for( int i = 0; i < array->count; ++i )
		delete (Cube *) array->array[ i ];
	destroyArray( array );
//#endif
}

void removePointFromArray( Array *array, Pointxd *point ) {
	for( int i = 0; i < array->count; ++i )
		if(((Pointxd *) array->array[ i ])->isEqual( point )) {
            removeArrayIndex( array, i );
            break;
        }
}
*/
