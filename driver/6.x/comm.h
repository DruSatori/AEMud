
#include <sys/socket.h>
#ifdef _AIX
#include <sys/socketvar.h>
#endif
#include <netinet/in.h>
#include <arpa/inet.h>

#ifdef linux
#define	FD_SETSIZE	__FD_SETSIZE
#endif

#define MAX_WRITE_SOCKET_SIZE	8192

struct interactive {
    void *tp;
    struct object *ob;		/* Points to the associated object */
    struct sentence *input_to;	/* To be called with next input line ! */
    struct sockaddr_in addr;
    char *prompt;
    int closing;		/* True when closing this socket. */
    int do_close;		/* This is to be closed down. */
    struct interactive *snoop_on, *snoop_by;
    int noecho;			/* Don't echo lines */
    int last_time;		/* Time of last command executed */
    char *default_err_message;	/* This or What ? is printed when error */
    int trace_level;		/* Debug flags. 0 means no debugging */
    char *trace_prefix;		/* Trace only object which has this as name prefix */
    struct ed_buffer *ed_buffer;	/* Local ed */
    char *rname;
    int lport;
    int rport;
#ifdef SUPER_SNOOP
    int snoop_fd;
#endif
#ifdef WORD_WRAP
    unsigned screen_width;   /* If 0, no wordwrap */
    unsigned current_column; /* Where the cursor should be */
#endif
#ifdef COLOUR_SUPPORT
    char     default_bg;
    char     default_fg;
    unsigned colour_method;  /* 0 - no colour, 1 - ansi, 2 - debug */
#endif
};

void add_ip_entry(unsigned long, char *);
void remove_interactive(struct interactive *, int);
void interactive_input(struct interactive *, char *);
void *new_player(void *, struct sockaddr_in *, size_t);
