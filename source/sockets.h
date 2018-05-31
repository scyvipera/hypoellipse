/* BEGIN sockets.h */
/********************************************************************/
/****                   Include file sockets.h                   ****/
/********************************************************************/
/****       A header file for my socket based interporcess       ****/
/****   A header file for my socket based remote interprocess    ****/
/****                  communications routines.                  ****/
/********************************************************************/
/****       Written by G. H. Cole Sonafarnk, May 25, 1989.       ****/
/********************************************************************/

#include <fcntl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <errno.h>

#define MAX_STRING		256	/* Arbitrary */
#define SOCKET_BACKLOG		5	/* Current system limit */
#define MAX_HOST_LENGTH		32	/* Find what include this is in */

extern int errno;

