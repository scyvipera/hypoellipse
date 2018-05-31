/* BEGIN setup_server.c      [] */
/********************************************************************/
/****                   setup_server_socket()                    ****/
/********************************************************************/
/****    This is an interprocess communication function which    ****/
/****     establishes a network socket on the local host for     ****/
/****  the specified service.  This service (name) and its port  ****/
/**** number and protocol (TCP) must exist in the services data  ****/
/****         base (e.g. /etc/net/services and the YP).          ****/
/********************************************************************/
/****   A descriptor of an open file (the socket) is returned.   ****/
/****     If an error occurs, a messages will be written to      ****/
/****           stderr and a negative value returned.            ****/
/********************************************************************/
/****       Written by G. H. Cole Sonafrank, May 25, 1989.       ****/
/********************************************************************/
/****  Copyright (c) University of Alaska, 1989,90,91,92,93,94   ****/
/****                    All rights reserved.                    ****/
/********************************************************************/

#include <stdio.h>
#include <string.h>
#include "sockets.h"

int setup_server_socket (tcp_service)
  char *tcp_service;			/* Service's name string */
{

  extern char *program_name;
  struct sockaddr_in socket_name;
  struct servent *server_ptr;
  struct hostent *host_entry;

  char this_host[MAX_HOST_LENGTH];
  char *inet_ntoa(), *char_ptr;
  int  addrlen = sizeof(struct sockaddr);
  int sockname_len, i;
  int  sd;			/* Socket descriptors */

  /* Get the standard name for the current processor (this host) */
  if (gethostname(this_host, sizeof(this_host)) < 0) {
    fprintf (stderr, "%s:\t", program_name);
    perror("setup_socket_server:gethostname");
    return(-1);
    }

  /* Get the host data base entry for the current host */
  if ((host_entry = gethostbyname(this_host)) == NULL) {
    fprintf (stderr, "%s:\t", program_name);
    perror("setup_socket_server:gethostbyname");
    return(-1);
    }

  /* Get the service data base entry to obtain the port */
  /* number associated with it (e.g. from /etc/net/services). */
  if ((server_ptr = getservbyname (tcp_service, "tcp")) == NULL) {
    fprintf (stderr, "%s:\t", program_name);
    perror("setup_socket_server:getservbyname");
    return(-1);
    }

  char_ptr = (char *)&socket_name;
  for (i=0; i<sizeof(socket_name); i++) *char_ptr++ = '\0';
  /* (void) bzero((char *)&socket_name, sizeof(socket_name)); */

  socket_name.sin_family = AF_INET;
  socket_name.sin_port = server_ptr->s_port;
  (void) strncpy ((char *)&socket_name.sin_addr,
                  (char *)host_entry->h_addr,
                  sizeof(socket_name.sin_addr));
 
  /* bcopy used previously is not appropriate under Solaris */
  /* (void) bcopy((char *)host_entry->h_addr, */
               /* (char *)&socket_name.sin_addr, */
               /* sizeof(socket_name.sin_addr)); */

  /* Obtain a socket descriptor */
  if ((sd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
    fprintf (stderr, "%s:\t", program_name);
    perror("setup_socket_server:socket");
    return(-1);
    }

  /* Request that a name be assigned to the socket */
  sockname_len = sizeof(socket_name);
  /* Note: must typecast pointer of socket_name */
  if (bind(sd, (struct sockaddr *)&socket_name, sockname_len) < 0) {
    fprintf (stderr, "%s:\t", program_name);
    perror("setup_socket_server:bind");
    return(-1);
    }

  fprintf (stderr, "%s:\tSetup socket server \"%s:%s\"\n",
          program_name, this_host, tcp_service);
  fprintf (stderr, "%s:\tSocket address: %s, port number: %d\n",
           program_name, inet_ntoa(socket_name.sin_addr), server_ptr->s_port);
  fflush (stderr);

  return (sd);

} /* End setup_server_socket() function */

