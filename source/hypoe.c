/* BEGIN Hypoe.h     []   */
#define EXIT_SUCCESS    0
#define EXIT_FAILURE    1
#define RETURN_SUCCESS  1
#define RETURN_FAILURE  0
#define MAX_PATH        256  /* Arbitrary value */
#define ALARM_SECONDS   18000
#define PHASE_TYPE      'p'
#define ARCHIVE_TYPE    'a'
#define BACKUP_TYPE     'q'

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sockets.h"

/* BEGIN Hypoe.c */
char *program_name = {"Hypoe"};

/********************************************************************/
/****                   /usr/Hypoe/src/Hypoe.c                   ****/
/********************************************************************/
/****   A C version of the Hypoellipse main routine which uses   ****/
/****      network sockets to allow client programs to send      ****/
/****   Hypoellipse the name of each input file as it is to be   ****/
/****     processed.  Between the processing of these files,     ****/
/****   Hypoellipse simply waits patiently.  10-Oct-89, GHCS.    ****/
/********************************************************************/
void MAIN_() {
 	/* this corrects a f77 library cludge introduce in v4.1.2 */
 	/* and v4.1.3 SUN OS */
}

int tcp_sd= -1; /* TCP socket descriptor returned by setup_server_socket() */

main (argc, argv) 
  int argc;
  char *argv[];
{
  extern void hypoe_exit();
  extern int tcp_sd;
  extern char *program_name;
  void  lissum_();

  int  i, process_input();
  int  mode=1;              /* initial toggle: 1 first time, 2 other times */
  char filename[MAX_PATH];
  FILE * fileptr;
   
  /* Network socket related declarations */
  int fd, listen_ok;
  int setup_server_socket(), listen_server_socket();
  char tcp_service[MAX_STRING], last_string[MAX_STRING];
  char *status, *fdgetstr();
  unsigned alarm_seconds=ALARM_SECONDS, set_alarm();

  f_init();
#if DEBUG
  fprintf (stderr, "This is my version (%s)...\n", argv[0]);
  fflush (stderr);
#else
  if (strcmp(argv[0],"Hypoe"))
    fprintf (stderr, "This is %s\n", argv[0]);
#endif

  tcp_service[0] = '\0'; /* Make certain we start without this option */
  filename[0] = '\0';    /* Make certain we start with null filename */

  /************************************************************/
  /* Obtain any command line switches and arguments expected. */
  /* This is ready for whatever else you want to do later...  */
  /************************************************************/

  for (i=1; i<argc; i++) {
#if DEBUG
    fprintf (stderr, "argv[%d]=%s\n", i, argv[i]);
    fflush (stderr);
#endif
    if (argv[i][0] == '-') {

      switch (argv[i][1]) {

        case 's':
        case 'S':
          if (argv[i][2] != '\0')
            (void) strcpy (tcp_service, &argv[i][2]);
          else if (argv[++i] != '\0')
            (void) strcpy (tcp_service, argv[i]);
          else fprintf (stderr, "Error in Service (-s<service>) argument.\n");
          break;

        default:
          fprintf (stderr, "Warning: unexpected argument \"%s\"\n", argv[i]);
          /* USAGE */
          fprintf (stderr, "Usage: Hypoe [filename] [-s<service>]\n");

        } /* End switch */
      } /* End if argument was a switch */

    else {
      /* Presume that any non-switch arguments are the filename. */
      (void) strcpy (filename, argv[i]);
      } /* End else argument was not a switch */

    } /* End for each argument i */

  /******************************************************************/
  /* If the tcp_service argument was provided, then...              */
  /* Set up a TCP network socket and listen for incomming messages. */
  /* Each message is presently presumed to be the name of a file    */
  /* to be processed by Hypoellipse.  If no message is received     */
  /* within ALARM_SECONDS, the signal handler set by set_alarm()    */
  /* will be called (it should probably terminate Hypoellipse).     */
  /******************************************************************/
  if (tcp_service[0] != '\0') {
    tcp_sd = setup_server_socket (tcp_service);
    }
  fflush (stderr); fflush (stdout);

  if (tcp_sd > 0) {

    do {	/* Loop until we recieve and exit command, */
                /* or until the alarm times out.           */
  
      (void) set_alarm(0);
      listen_ok = (fd = listen_server_socket(tcp_sd)) > 0;
      fflush (stderr); fflush (stdout);

      if (listen_ok) {
  
        do {	/* Get records from the socket until EOF */
        
          (void) set_alarm(alarm_seconds);

          status = fdgetstr (fd, filename, MAX_PATH);
        
          if (status != NULL) {
            fprintf (stderr,"Server received \"%s\"\n", filename);
	    if(strcmp("EXIT", filename) != 0) {
              if ((fileptr = fopen(filename, "r")) == NULL) {
   	             (void)fprintf(stderr,"\nCan't open file '%s'!\n\n",filename);
		     continue;
                }
              else {
                fclose (fileptr);
              }
	    }

            fflush (stderr);
            (void) strcpy(last_string, filename);

            /******************************************/
            /* Process the next input file.           */
            /* Terminate if return value is negative. */
            /******************************************/
            if (strcmp(filename,"EXIT")) {
              if (process_input (mode, 1, filename) < 0)
                hypoe_exit (0);
              mode = 2;
              }
            }

#if DEBUG
          else if (strcmp(last_string,"EXIT")) {
            fprintf (stderr, "Server received EOF without EXIT.\n");
            fflush (stderr);
            }
          else {
            fprintf (stderr, "Server received EXIT & EOF.\n");
            fflush (stderr);
            }
#endif
        
          } while (status != NULL && listen_ok);
  
        close (fd);

        } /* End if listening ok */

        fflush (stderr); fflush (stdout);

      } while (listen_ok && strcmp(last_string,"EXIT"));

    if (listen_ok) {
      fprintf (stderr, "Server processing client exit command (%s).\n",
               last_string);
      mode = 2;
      lissum_(&mode);
      hypoe_exit(EXIT_SUCCESS);
      }
    else {
      fprintf (stderr,
        "Hypoe: Bad file descriptor (%d) from listen_server_socket()\n", fd);
      hypoe_exit(EXIT_FAILURE);
      }

    } /* End if TCP option was selected and setup got sd ok */
  
  else if (tcp_service[0] == '\0') {
#if DEBUG
    fprintf (stderr,
            "Hypoellipse's network socket option was not selected.\n");
    fflush (stderr);
#endif
    (void) process_input (mode, -1, filename);
    hypoe_exit(EXIT_SUCCESS);
    }
  else {
    fprintf (stderr, "Setup of \"%s\" server failed.\n", tcp_service);
    hypoe_exit(EXIT_FAILURE);
    }

} /* End main() Hypoe.c */

/* BEGIN proc_input.c */
/********************************************************************/
/****                 /usr/Hypoe/src/proc_input.c                ****/
/********************************************************************/
/****    The process_input() routine initializes Hypoellipse     ****/
/****      and processes the specified input file, if any.       ****/
/****                      10-Oct-89, GHCS                       ****/
/********************************************************************/
int process_input (mode_arg, socket_arg, filename)
  int mode_arg, socket_arg;
  char *filename;
{
  int mode, status, rename_arc();
  void initial_(), input1_(), lissum_(), locate_(), cleanup_();

  mode = mode_arg;
  status = socket_arg;
#if DEBUG
  fprintf (stderr, "\nHypoe:process_input: mode=%d, file=%s\n",
           mode, filename);
#endif

  /**************************/
  /* Initialize Hypoellipse */
  /**************************/
  fflush (stderr); fflush (stdout);
  initial_(&mode, filename, strlen(filename));         

  if (mode == 1) {
    input1_();            /* get filenames, open files,and write greeting */
    lissum_(&mode);       /* initialize summary of residuals */
    mode = 2;
    if (socket_arg == 1) {
      cleanup_();
      initial_(&mode, filename, strlen(filename));         
      }
    }

  /**********************/
  /* Locate earthquakes */
  /**********************/
  locate_(&status);
  cleanup_();
  fprintf (stderr, "\n");
  fprintf (stdout, "\n");
  if (socket_arg == 1 && filename[0] != '\0') {
    (void) rename_arc (filename);
    }

#if DEBUG
  fprintf (stderr, "\nHypoe:process_input: status=%d\n", status);
#endif
  return (status);

} /* End process_input() */

/* BEGIN set_alarm.c */
/********************************************************************/
/****                 /usr/Hypoe/src/set_alarm.c                 ****/
/********************************************************************/
/**** This routine sets an alarm clock to ring, thereby running  ****/
/****  the handle_alarm() routine, after the time specified by   ****/
/****  the seconds argument has elapsed.  The clock can be (and  ****/
/**** usually is) reset at any time by subsequent calls to this  ****/
/****                  routine.  10-Oct-89, GHCS                 ****/
/********************************************************************/
#include <stdio.h>
#include <signal.h>

unsigned set_alarm (seconds)
  unsigned seconds;
{
  unsigned alarm();
  extern void handle_alarm();
#if MASSCOMP
  int (*syshandler)();

  syshandler = signal(SIGALRM, handle_alarm);
#else
  signal(SIGALRM, handle_alarm);
#endif

  return(alarm(seconds));
}

/* BEGIN handle_alarm.c */
/********************************************************************/
/****                /usr/Hypoe/src/handle_alarm.c               ****/
/********************************************************************/
/****      The signal handler for the set_alarm() routine.       ****/
/****  This routine does whatever is required when Hypoellipse   ****/
/****  fails to reset the alarm clock before the timeout period  ****/
/**** elapses (e.g. performs a controlled exit). 10-Oct-89, GHCS ****/
/********************************************************************/
void handle_alarm()
{
  void hypoe_exit();

#if DEBUG
  fprintf (stderr, "Hypoe:handle_alarm: Processing alarm signal\n");
#endif
  fprintf (stdout, "Hypoellipse terminating with timeout signal.\n");
  hypoe_exit (0);  /* Clean-up and exit with normal status */
}

/* BEGIN rename_arc.c */
/********************************************************************/
/****                 /usr/Hypoe/src/rename_arc.c                ****/
/********************************************************************/
/****    The intent of this routine is to rename the original    ****/
/****   phase filename to a backup name (e.g. eventp to eventq   ****/
/****  or event to eventq).  It then renames the newly created   ****/
/****   archive file to the original filename (e.g. eventa to    ****/
/****       eventp or eventa to event).  12-Oct-89, GHCS.        ****/
/********************************************************************/
int rename_arc (old_filename)
  char *old_filename;
{
  int length, rename();
  char new_filename[MAX_PATH];

  (void) strcpy (new_filename, old_filename);

  if ((length = strlen(old_filename) - 1) > 0) {

    if (old_filename[length] != PHASE_TYPE) {
      new_filename[(++length)+1] = '\0';
      }
    new_filename[length] = BACKUP_TYPE;
#if DEBUG
    printf ("\nfrom: %s, to:%s\n", old_filename, new_filename);
#endif
  
    if (rename(old_filename, new_filename) != 0) {
      fprintf (stderr, "Failed to rename %s to %s\n",
               old_filename, new_filename);
      perror ("Hypoe:rename_arc");
      return (RETURN_FAILURE);
      }

    else {
      new_filename[length] = ARCHIVE_TYPE;
#if DEBUG
      printf ("from: %s, to:%s\n", new_filename, old_filename);
#endif
      if (rename(new_filename, old_filename) != 0) {
        fprintf (stderr, "Failed to rename %s to %s\n",
                 new_filename, old_filename);
        perror ("Hypoe:rename_arc");
        return (RETURN_FAILURE);
        }

      } /* End else renamed phase file ok */
    } /* End if old_filename was not null */

  else {
    fprintf (stderr, "Hypoe:rename_arc: Filename was null.\n");
    return (RETURN_FAILURE);
    }

  return (RETURN_SUCCESS);
  
} /* End rename_arc() */

/* BEGIN hypoe_exit.c */
/********************************************************************/
/****                 /usr/Hypoe/src/hypoe_exit.c                ****/
/********************************************************************/
/**** A routine which does whatever house cleaning is necessary  ****/
/****  prior to terminating Hypoellipse and then exits with the  ****/
/****       status passed as an argument.  10-Oct-89, GHCS       ****/
/********************************************************************/
void hypoe_exit (status)
  int status;
{
  int mode;
/*  void lissum_(); */
  extern int tcp_sd;

  /* mode = 2; */
  /* lissum_(&mode); */

  if (tcp_sd > 0) {
    shutdown (tcp_sd, 0);
    close (tcp_sd);
    }

  fflush (stderr);
  fflush (stdout);
  f_exit();
  exit (status);
} 
