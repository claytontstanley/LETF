/*
 *  Function.h
 *  Hurricane
 *
 *  Created by Rick on 11/5/09.
 *  Copyright 2009 Lockheed. All rights reserved.
 *
 */

#ifndef FUNCTIONSH
#define FUNCTIONSH

// Generic buffer size for reading strings, etc.
#define BUFFER_SIZE			65535

//#include "Array.h"

class Pointxd;
class Samplexd;

double frandom();
int irandom( int range );
int isseparator( int c );

void Assert( int condition, const char *message, ... );
void Log( const char *message, ... );
void sampleNotification( Samplexd *sample );

char *strdellastpath( const char *path );
char *strlastpath( const char *path );
char *strreplace( const char *string, const char *find, const char *replace );
const char **strcomps( const char *string );
int strcompscount( const char **comps );
void delcomps( const char **comps );
const char *stradd( const char *string, const char *addString );
const char *strtrim( const char *string );

// void destroyArrayWithPoints( Array *array );
// void destroyArrayWithCubes( Array *array );
// void removePointFromArray( Array *array, Pointxd *point );

#endif

