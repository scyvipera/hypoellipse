/* BEGIN listen_serv.c     []   */
/********************************************************************/
/****                   listen_server_socket()                   ****/
/********************************************************************/
/****    This is an interprocess communication function which    ****/
/****     allows (possibly remote) connections to be made to     ****/
/****       the specified (by descriptor argument) socket.       ****/
/****     Multiple connections are allowed, with each being      ****/
/****      Multiple connections are accepted, but they are       ****/
/****   handled (active) one at a time in the order they were    ****/
/****    made.  The calling process is blocked (waits) until     ****/
/****      a client establishes a connection to the socket.      ****/
/********************************************************************/
/****   A descriptor of an open file (the socket) is returned.   ****/
/****     If an error occurs, a messages will be written to      ****/
/****           stderr and a negative value returned.            ****/
/********************************************************************/
/****       Written by G. H. Cole Sonafrank, May 25, 1989.       ****/
/********************************************************************/

#include <stdio.h>
#include "sockets.h"

int listen_server_socket(sd)
  int sd;
{
  struct sockaddr_in client_sockaddr;
  struct hostent *client_hostent;
  int fd;
  int  addrlen = sizeof(struct sockaddr_in);

  /* Allow incomming connections to the socket and set */
  /* up a "backlog" (queue) for multiple connections. */
  if (listen(sd, SOCKET_BACKLOG) < 0) {
    perror("listen_server_socket:listen");
    return(-1);
    }

#if DEBUG_FLAG
  fprintf (stderr, "Waiting until a client connects...\n");
#endif

  /* Block until some client is "connect"ed (unless FNDELAY */
  /* non-blocking is set), then get a file descriptor. */
  do {
     /* note: must typecast the pointer of client_sockaddr, for correctness */
    if ((fd = accept(sd, (struct sockaddr *)&client_sockaddr, &addrlen)) < 0) {
      if (errno == EWOULDBLOCK) return (-2);
      else if (errno != 4) {
        perror("listen_server_socket:accept");
        return(-1);
        }
      }
    } while (errno == 4);	/* Interrupted system call */

  /* Try to determine which host (client) made the connection */
  client_hostent = gethostbyaddr ((char *)&client_sockaddr.sin_addr,
                                  sizeof(client_sockaddr.sin_addr),
                                  client_sockaddr.sin_family);

  if (client_hostent != NULL) {
    fprintf (stderr, "Client \"%s\" is connected.\n", client_hostent->h_name);
    }
  else {
    fprintf (stderr, "Client \"%s\" is connected.\n",
             inet_ntoa(client_sockaddr.sin_addr));
    }

  return (fd);

} /* End listen_server_socket() */

